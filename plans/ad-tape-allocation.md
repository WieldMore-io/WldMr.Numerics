# Reverse-mode AD allocates ~2.5x what it needs to

**Status: steps 1 and 2 landed 2026-08-07 (PR #18); step 4 followed the same
day — see the step-4 section at the end. Step 3 is done as option (d) of
`ad-allocation-redesign.md`, which now carries the direction; the gather op
(`ad-gather.md`) is implemented and verified, awaiting the Analytics adoption
PR after release.**

History: diagnosed 2026-08-05 with a harness (`consoles/BenchmarkAdTape`) and
the cause confirmed at three specific lines. Step 1, the `adjoint`/`.A` consumer
audit, ran the same day; its corrections supersede the original review's
`jacobian'` claim and are applied throughout the body. Step 2 — buffer reuse on
reset plus the guarded copy in `adjoint` — shipped 2026-08-07.

**Numbers *and line references* in the body below are pre-step-2.** They are
kept because they are what motivated the work, but steps 2 and 4 both inserted
lines into `AD.Lite.fs`, so every `:NNNN` in the body is stale — find the sites
by name with `rg` instead. The step-4 section at the end carries the current
allocation baseline and the verification recipe. Use those.

## Why this is worth doing

The finding came from outside this repo. `WldMr.Analytics` profiled its
`MarketBuild` rule — the reactive store's most expensive computation, fired
three times per incoming price — and found:

- One fit allocates **100.8 MB across ~2.23 million objects**, averaging 48 bytes.
- **26.5% of all of it is inside the `reverseProp` subtree** (~27 MB a fit).
  This is measured by bucketing every allocation-sample by whether
  `reverseProp`/`pushRec` appears anywhere on its stack, so it captures the
  allocation that per-frame attribution scatters across `op_Addition`,
  `CsrMat.mulV` and `DV.op_DotMultiply`.
- GC pause is **13.9% of wall time** there, at 12.8 gen0 collections per fit.

One ratio from that profile points straight at the mechanism: the fit creates
only **18,610 `DVR` reverse nodes** but **233,500 `System.Double[]`** — about
**12.5 zero-vectors allocated per reverse node per fit**, each averaging 156 B
(~19 doubles). Small-array churn, not bandwidth.

The full downstream analysis is `WldMr.Analytics/plans/marketbuild-cost.md`.
This repo is where the fix belongs, and the fix benefits every DiffSharp
consumer, not just that one rule.

## What the local harness already shows

`consoles/BenchmarkAdTape` builds a synthetic curve-solve graph (40 pillars →
320 points; per layer: sparse `DM * DV` via CSR, `.*`, negate, `exp`, `.*`,
sparse aggregate, `+`) chosen to match the op mix the Analytics profile is
dominated by.

```bash
cd consoles/BenchmarkAdTape && dotnet run -c Release
# also: -- census [depth] | -- phases [depth] [reps] | -- scale [reps]
#       -- width [reps] | -- seeds [depth] [reps] | -- profile [n] [depth]
```

BenchmarkDotNet `Allocated` per operation, and the ratio that matters:

| method | depth 8 | vs forward |
| --- | ---: | ---: |
| Forward (builds the tape) | 223.67 KB | 1.00 |
| ForwardAndReset | 336.62 KB | 1.50 |
| ForwardResetAndPush | 558.72 KB | 2.50 |
| ForwardThenSeedPasses (x8) | 2904.05 KB | 12.98 |

Phase split at depth 8 from `GC.GetTotalAllocatedBytes(true)`, independent of
BenchmarkDotNet and agreeing with it to 0.1%:

| phase | B/pass | B/node | B/adjoint slot |
| --- | ---: | ---: | ---: |
| forward (builds tape) | 229,040 | 3949 | 16.99 |
| reset — *measured* | 115,656 | 1994 | 8.58 |
| reset+push (`reverseProp`) | 343,088 | 5915 | 25.45 |
| push — *derived by subtraction* | 227,432 | 3921 | 16.87 |

**The decisive experiment is `-- width`.** Hold the node count fixed at 58 and
vary the vector width: reset allocation still grows **47x**, converging on 8
bytes per adjoint slot — a `reset / zero-vector-floor` ratio of **1.03**. So the
reset cost is the `float[]` payload itself, not a per-node header. That is the
proof, not an inference.

`-- seeds 8` shows one forward pass (229 KB) followed by 8 reverse passes
(343,088 B each): **92.3% of the total is the reverse side.**

## The three sites

All line numbers verified against the current `src/WldMr.Numerics.DiffSharp/AD.Lite.fs`.

**1. `:3428` — reset allocates a fresh zero vector per node.**

```fsharp
| DVR(dPrimal, dARef, o, dFanOutRef, _) ->
    dARef.Value <- DV.ZeroN dPrimal.Length     // DV.ZeroN n = DV(Array.zeroCreate n)  (:648)
```

The adjoint is a `DV ref` (`:516`), so the existing buffer is thrown away and
replaced rather than cleared. `:3530` is the identical twin for `DM`
(`DM.ZeroMN d.Rows d.Cols`).

**2. `:576` — the same zero vector is already allocated at node construction.**

```fsharp
static member R(d, op, ai) = DVR(d, ref (DV.ZeroN d.Length), op, ref 0u, ai)
```

So a node's adjoint buffer is allocated once when the node is built and again on
every reset. That is why "forward" is not free of adjoint allocation, and it is
why the waste is paid twice per node per reverse pass.

**3. `:3746` — push allocates a whole `DV` per adjoint contribution.**

```fsharp
dARef.Value <- dARef.Value + v
```

Every contribution allocates a new `DV` (and its `float[]`) and discards the
zero vector the reset just made. This is why push costs ~2x reset, and it is the
`Tuple<dobj,dobj>` / `FSharpList<...>` traffic the Analytics trace attributes to
`op_Addition$cont`. The in-place machinery already exists at both layers:
`Backend.Add_V_V_Inplace` (`Backend.Lite.fs:34`, a `daxpy`) and
`Backend.AlphaAdd_M_M_Inplace'` (`Backend.Lite.fs:128`) — and, better, a
*dormant* `DV.Add_V_V_Inplace` (`:906`, zero call sites) whose `Op_DV_DV_DV`
dispatch is exactly the case-guarded fast path this fix needs: destructive
`daxpy` when both sides are plain `DV`, allocating fallback for the `DVF`/`DVR`
cases (constraint 5). Note the backend in-place add treats an empty first
argument as a no-op, preserving the `DV.Zero`-means-no-contribution convention
that `Item_DV` relies on (`:3706`).

**And the multiplier: a full reset per reverse pass, `:3996`.**

`reverseProp` unconditionally calls `reverseReset` first, so every reverse pass
re-zeroes **every node on the tape**, however little changed but the seed. Two
callers turn a modest per-node cost into the numbers above, and they are not the
same caller.

*Downstream — and this is where the 27 MB actually is.* `WldMr.Analytics`'
`CurveSolver.ff'` (`CurveFit/CurveSolver.fs:83-104`) builds the market tape once
per Newton iteration (`:85`–`:86`) and then runs one `reverseProp D.One` **per
fit instrument** (`:89`) over that shared tape. One fit is therefore
(iterations × instruments) full tape resets, which is the signature the
12.5-zero-vectors-per-node ratio carries. Note what it is *not*: nothing
downstream calls `jacobianTv''`. A sweep of Analytics, Web, Excel and
Excel.Addin finds **zero** call sites for `jacobianTv`, `jacobianTv'` or
`jacobianTv''`, so the downstream multiplier is CurveSolver's own hand-rolled
loop and no change to `DiffOps` will move it.

*In-library.* `:4069`, `jacobianTv''`, is the API built to avoid exactly this —
one forward pass, then `r2` per row:

```fsharp
let r2 = fun (v:DV) -> z |> reverseProp v
                       xa |> adjoint
```

It has **no callers anywhere**, here or downstream. `jacobianTv'`/`jacobianTv`
(`:4080`, `:4086`) wrap it, but are three-argument functions, so each
application re-runs the whole body — forward pass included. That is why
`jacobian'` (`:4093`) pays N+1 forward passes for an N-row Jacobian rather than
one; see the audit's correction 1.

Minor, while you are in the file: `:112`/`:114` make `D.Zero` and `D.One`
*properties*, so every access allocates a `D`. `static member val` would be free.

## Five constraints that make this harder than it looks

**1. Three compile targets, and `System.Array.Clear` is not one of them under
Fable.** These libs must build for .NET, Fable→JS and Fable→Python, and the
`.fsproj`s ship their sources in the nupkg under `fable/`, so a .NET-only change
breaks downstream Fable consumers at *their* compile time, not here. The repo
already has the pattern for exactly this — `LinAlg/CsrMat.fs:38-50`:

```fsharp
#if !FABLE_COMPILER
  System.Array.Clear(colTotal, 0, colTotal.Length)
#else
  for i in 0 .. colTotal.Length-1 do
    colTotal.[i] <- 0
#endif
```

Follow it. See this repo's `CLAUDE.md` for the rest of the multi-target rules.

**2. Reset and push are coupled through fan-out counters.** `resetRec` bumps
each node's counter and recurses only when it hits exactly 1 (`:3430`); `pushRec`
decrements and proceeds to children at 0. Consequences:

- A *second* reset with no push in between walks only the root, reports ~0, and
  **silently disarms the push that follows**. Any benchmark or test that resets
  twice is measuring nothing and will mislead you.
- `reverseProp` is safely repeatable, because a complete push returns every
  counter to 0.
- Any restructuring must preserve this protocol.

**3. The adjoint *is* the result.** `adjoint` (`:3346`) returns `d.A` — a
reference to the tape's adjoint buffer, not a copy. If a caller keeps that value
rather than copying out of it, clearing the buffer in place on the next reset
silently zeroes their gradient. **This was the central correctness question for
the obvious fix at `:3428`**, and the audit at the end of this document settles
it. The short version:

- The `DiffOps` surface (`grad'`, `jacobian'` and friends) is **immune**: every
  call makes its own `makeReverse` and hands back a buffer nothing touches
  again. `jacobian'`'s `let r = jacobianTv f x` is a partial application, so
  each row re-runs the entire forward pass on a fresh tape — see the audit's
  correction 1 (an earlier revision of this plan claimed the opposite).
- The dangerous shape is one `makeReverse` hoisted out of a loop, with
  `reverseProp` + `adjoint` per iteration and rows copied out only after the
  loop. Exactly one consumer in the family has it — `WldMr.Analytics`
  `CurveFit/CurveSolver.fs:90` (`ff'`), which feeds every curve fit — and a
  probe build reproduced the corruption: earlier rows come back zero. See the
  audit's correction 2.
- Containment is a guarded copy in `adjoint` itself (`:3346`), which every
  escape point in the family already funnels through: `grad'` (`:4045`),
  `jacobianTv''`'s `r2` (`:4076`), and all external readers. A copy placed only
  in `r2` would miss `grad'` and `CurveSolver`.
- Inside `pushRec`, **sixteen** sites write `.A` directly, bypassing the
  `:3746` accumulate (line list in the audit) — that is step 4's real
  inventory. The FixedPoint block (`:3715`–`:3737`) additionally re-enters
  `reverseProp` on a sub-tape in a loop and reads adjoints between nested
  resets; it survives buffer reuse — the reads are snapshotted into fresh
  values by arithmetic before the next reset — but its seed at `:3723` aliases
  the enclosing node's adjoint buffer, and any restructuring must re-verify the
  block. `Item_DV`/`Item_DM` (`:3705`/`:3709`) push an *empty* `DV.Zero`
  (`:646`) as the contribution, so an in-place accumulate that asserts equal
  lengths crashes on every indexing op; empty must stay an identity.

**4. Push cannot be isolated through the public API.** `pushRec` is local to
`reverseProp` (`:3642`/`:3648`), and `reverseProp` unconditionally calls
`reverseReset` first (`:3996`). `reverseReset` is public and `[<AutoOpen>]`, so
reset can be measured directly, but push is only ever available as
`reverseProp − reset`. The harness reports it as `[derived]` and cross-checks it
with `ReversePassOnPrebuiltTape`, which equals `ForwardResetAndPush − Forward`
exactly.

**5. Nested AD stores non-primal adjoints, so "right length" is not a
sufficient reuse condition.** Adjoint refs start as `DV.ZeroN`, but push does
`dARef.Value <- dARef.Value + v`, and under forward-on-reverse
(`gradhessian`/`hessian`, `:4110`) the contributions are `DVF` duals — after one
push the ref holds a `DVF`, which has a `.Length` too. A length-only check at
reset would pass and then mutate a tangent-carrying value in place. Both fast
paths must pattern-match: reuse/accumulate in place only when the existing value
is a plain `DV` (and, for push, `v` too), falling back to allocation otherwise.
`DV.Add_V_V_Inplace` (`:906`) already encodes exactly this dispatch.

## Suggested order

1. ~~**Audit `adjoint` / `.A` consumers**~~ — **done 2026-08-05**, full findings in
   the audit section at the end. All eleven repos swept; contract adopted: an
   adjoint obtained through `adjoint` is the caller's own value, `.A` is the
   raw accessor and aliases the tape. Nothing below is blocked any more.
2. ~~**`:3428` + `:3530` + copy-at-boundary, as one indivisible change**~~ —
   **done 2026-08-07, PR #18.** Reuse guarded on a plain `DV`/`DM(ColMajor)` of
   matching shape (constraint 5), behind the Fable guard, with `adjoint`
   (`:3346`) returning a copy for exactly the same case split, in the same
   commit. Reset allocation went 115,656 → 2,136 B/pass and is now flat in
   vector width. Cross-runtime tests in `tests/ExpectoTests/AdjointLifetimeTests.fs`.
3. ~~**`:576`**~~ — **done 2026-08-07** as option (d) of
   `ad-allocation-redesign.md`: `DV.R`/`DM.R` seed the node's adjoint with a shared
   empty sentinel and reset's shape-mismatch arm materialises on first visit.
   The "buffers are full-length by push time" invariant is pinned by a test, and
   the harness's phases/seeds modes now report the first pass separately.
   Measured outcome in the redesign note.
4. ~~**`:3746`** — in-place adjoint accumulation.~~ — **done 2026-08-07.** The
   `DV` and `DM` central accumulates now go through the previously dormant
   `Add_V_V_Inplace`/`Add_M_M_Inplace`; push went 227,432 → 115,472 B/pass
   (16.87 → 8.57 B/slot, i.e. each slot's value is now allocated once — the
   derivative payload — instead of twice). Details in the step-4 section at
   the end.
5. **Reset per reverse pass (`:3996`)** — the multiplier, potentially worth more
   than 2-4 combined for multi-seed workloads. It has two separable levers, and
   they live in different repos:
   - *In-library.* `jacobian'` (`:4093`) re-runs the **entire forward pass per
     row** — N+1 forward passes plus N reverse passes for an N-row Jacobian —
     because `let r = jacobianTv f x` is a partial application of a
     three-argument function. Holding `jacobianTv''`'s `r2` instead removes N
     forward passes outright. Self-contained, cheap, testable here. Separately,
     whether `r2` needs a *reset* per row at all is the open question `:4069`
     poses.
   - *Downstream, where the measured 27 MB is.* `CurveSolver.ff'` pays a full
     tape reset per instrument per Newton iteration, and reaches `reverseProp`
     directly rather than through `DiffOps` (zero `jacobianTv*` call sites in
     the family). **Optimising `:4069` alone will not move the Analytics
     profile.** That fix is in `WldMr.Analytics` — or here, as a many-seed API
     that does not reset per seed, which is what `jacobianTv''` was meant to be
     and has never been used as.

## How to verify a fix

- **Correctness, .NET:** `dotnet test` (three projects).
  `tests/WldMr.Numerics.DiffSharp.Checks` is FsCheck property tests comparing the
  Lite backend against reference implementations — that is the real net for an AD
  change. Six are `[<Ignore("Not implemented")>]`; expect them skipped.
- **Correctness, Fable:** `npm test` (JS) and the Fable→Python command in
  `CLAUDE.md`. `tests/ExpectoTests` is the only Fable-compiled suite, so a
  cross-runtime regression test belongs there.
- **New tests to add with the fix:** a cross-runtime contract test in
  `tests/ExpectoTests` for the `adjoint`-direct shape that actually breaks —
  one `makeReverse`, two `reverseProp`s, read the first adjoint after the
  second pass and assert it survives. Assert on **values, never reference
  identity**: with step 2 alone the stale rows come back zero in distinct
  arrays; with steps 2+4 they alias the last row (the audit's two failure
  signatures). `jacobianTv''`'s `r2` called twice is a strict special case,
  worth covering too. The audit confirmed the entire existing suite stays green
  under a probe of the step-2 bug, so this behaviour is load-bearing and
  CI-blind until these tests exist. Also a guard for the constraint-2 trap:
  reset twice, then push — today that silently yields a zero gradient.
- **Allocation:** `consoles/BenchmarkAdTape`. `-- width` and `-- scale` are the
  ones that show whether the per-node/per-slot cost actually moved; the BDN
  ratios are the headline.
- **Downstream:** `WldMr.Analytics`' `BenchmarkMarketBuild` has a `-- fingerprint`
  mode asserting that a fit produces byte-identical `MarketDefResults` across
  processes. That is the end-to-end guard against an in-place-mutation bug, and
  it is stronger than anything in this repo. Use `local-feed.sh` / the
  `wldmr-cross-repo-dev` skill to get a modified package into Analytics before
  merging, then re-run its `-- stages` and compare the MB columns.

## Housekeeping for whoever picks this up

- `consoles/BenchmarkAdTape` and its supporting changes (`paket.dependencies`,
  `paket.lock`, `WldMr.Numerics.sln`, one `.gitignore` line) are **uncommitted**.
  The lock change is additive — no existing package version moved.
- **`paket.dependencies` pins `Pragmastat ~> 3.2` deliberately.** BenchmarkDotNet
  0.15.8 pulls Perfolizer 0.6.1, which needs `Pragmastat >= 3.2.4`; paket
  otherwise resolves 13.0.1, whose `MeasurementUnit` constructor changed, and
  every run dies with `MissingMethodException` in `TimeUnit`'s static constructor
  before a single benchmark executes. The reason is recorded in the file. After
  touching it, `rm -rf bin obj` on the benchmark project — an incremental build
  keeps the stale DLL and reproduces the same crash.
- BenchmarkDotNet labels the `[Host]` row `DEBUG`. That is an F#-metadata quirk,
  not an unoptimised build: `Optimize` is `true` in Release (checked via
  `dotnet msbuild -getProperty:Optimize`), the measuring `DefaultJob` row is not
  flagged, and the allocations agree with the independent GC-counter numbers to
  0.1%.

## Audit of `adjoint` / `.A` consumers — step 1, done 2026-08-05

**Constraint 3 is settled: the obvious fix at `:3428` is not safe as the plan
describes it, but for a different reason than the plan gives.** `jacobian'` is
*immune*; the casualty is `WldMr.Analytics`' `CurveSolver.ff'`, which reads the
adjoint directly and never touches `DiffOps`. The containment the plan proposes
— copy in `jacobianTv''`'s `r2` — does **not** cover it. Copy in `adjoint`
(`:3346`) instead; that one line covers every escape point in the family.

Everything below is measured, not reasoned: a probe patch implementing the
step-2 reset was applied, built and run, then reverted. `AD.Lite.fs` is
unmodified — `git status` shows only the pre-existing uncommitted benchmark
changes.

### The probe

Five lines at `:3428`, the constraint-5-guarded form step 2 asks for:

```fsharp
| DVR(dPrimal, dARef, o, dFanOutRef, _) ->
    match dARef.Value with
    | DV a when a.Length = dPrimal.Length ->
        for i = 0 to a.Length - 1 do a.[i] <- 0.
    | _ -> dARef.Value <- DV.ZeroN dPrimal.Length
    dFanOutRef.Value <- dFanOutRef.Value + 1u
```

Built clean, and every scenario below was run against it and against stock.

### The complete consumer list

Swept all eleven repos in `wldmr-dev/repos.txt` for `adjoint`, `.A`,
`reverseProp`, `reverseReset`, `makeReverse` and the whole `DiffOps` gradient
surface, excluding `fable_modules/`, `build*/`, `bin`, `obj`, `js-build`,
`py-build`. Outside `AD.Lite.fs` there are exactly **two** direct adjoint
readers in the entire family, plus the benchmark console:

| site | shape | verdict |
| --- | --- | --- |
| `WldMr.Analytics` `CurveFit/CurveSolver.fs:90` | one `makeReverse` hoisted out of the loop (`:85`), `reverseProp` per instrument (`:89`), `adjoint` read each pass, rows assembled only at `:103` | **breaks** |
| `WldMr.Analytics` `tests/…/DiffSharpTests.fs:38` | scalar `D`, one pass, `D.toFloat` immediately | safe |
| `consoles/BenchmarkAdTape` `Benchmarks.fs:61,89,97,109`, `Phases.fs:39,75,171,194,205` | reads once after the last pass, purely to defeat dead-code elimination | safe |

`WldMr.Web`, `WldMr.Excel`, `WldMr.Excel.Addin`, `WldMr.Web.Datastore.Cli`,
`WldMr.Serialize`, `WldMr.Auth`, `WldMr.CommonDataLogic`, `WldMr.PhysicalRisk`:
**zero** hits. Many files `open WldMr.Numerics.DiffSharp.AD.Float64`, but only
for `D` as a number type.

Indirect readers, all through `DiffOps` and all safe because each call makes its
own `makeReverse` and hands back a buffer nothing else ever touches again:
`Sabr/GaussNewtonAlgorithm.fs:28` (`grad'` per residual, then `Mat.ofRowsArray`
— the shape that *looks* like the dangerous one and is not),
`Market/MktRiskConfig.fs:154,182` (`grad`), plus `grad'`/`grad`/`jacobian'` in
`WldMr.Analytics/tests` (`LegPricerTests.fs:165,168,220,229`,
`FixedBondTest.fs:320`, `InterpolationTests.fs:40,150,165`) and
`WldMr.Web` `admin/DateQueriesPage.fs:26`.

Inside `AD.Lite.fs`, an adjoint escapes to a caller at exactly two places:
`:4045` (`grad'`) and `:4076` (`jacobianTv''`'s `r2`). Every other `.A` is
`pushRec`'s own read-modify-write — `:3705`, `:3709`, `:3824`, `:3827`,
`:3833`, `:3834`, `:3837`, `:3840`, `:3843`, `:3853`, `:3856`, `:3910`,
`:3915`, `:3969`, `:3972`, `:3982` all do `a.A <- a.A + …` and bypass the
`:3746` accumulate, so step 4 has sixteen sites to convert, not the two the
plan names. The FixedPoint reads (`:3731`, `:3735`, `:3737`) are unchanged from
the plan's description.

### Correction 1: `jacobian'` is immune, and the plan's reason is wrong

The plan says each `r` call at `:4093` returns an alias of one shared `xa`. It
does not. `let r = jacobianTv f x` is a **partial application of a
three-argument function**, so the body of `jacobianTv` — including
`jacobianTv'' f x`, the entire forward pass — re-runs on every `r v`. Each row
builds its own tape with its own `xa` and its own adjoint buffer.

Measured, counting evaluations of `f` for a 2→3 function (the reverse branch,
`2 * 2 > 3`):

```
forward evaluations of f for a 3-row jacobian: 4
```

One for the primal at `:4090`, then one per row. And under the probe,
`jacobian` still returns the correct matrix. `jacobian'`, `jacobianT`,
`gradhessian` and `hessian'` are therefore **not** casualties of buffer reuse.

Two consequences:

- The plan's containment sentence — copy in `r2` "makes buffer reuse invisible
  to everything that goes through the `DiffOps` API" — is wrong as written. What
  protects the `DiffOps` surface is *one `makeReverse` per returned adjoint*,
  which every one of `grad'`, `jacobian'` and friends already has. `grad'` at
  `:4045` is a second escape point that never routes through `jacobianTv''` at
  all, so a copy placed only in `r2` would have missed it.
- **This belongs in step 5, not step 3.** `jacobian'` costs N+1 forward passes
  *and* N reverse passes for an N-row Jacobian, not one forward pass and N
  reverse. The harness's `-- seeds` mode models 1 forward + N reverse (see the
  comment at `Phases.fs:188`), so it understates what `jacobian'` actually
  does. The `jacobianTv''` "reverse evaluator" exists precisely to avoid this
  and nothing in the library uses it that way.

### Correction 2: the real casualty is `CurveSolver.ff'`

`WldMr.Analytics/src/WldMr.Analytics/CurveFit/CurveSolver.fs:83-104`:

```fsharp
let drFwds = dFwds |> makeReverse GlobalTagger.Next          // :85 — ONCE, outside the loop
let gradient' (cb: MktData) (xa: DV) (f: FitInstrument) (price: float) =
  let z = f |> FitInstrument.evaluate cb price
  z |> reverseProp D.One                                     // :89 — per instrument
  (z |> primal, xa |> adjoint)                               // :90 — same xa every time

let res, gradients =
  (insts, prices) ||> Array.zip
  |> Array.map (fun (i, p) ->
      let r, g = gradient' drCb drFwds i p
      D.toFloat r, DV.toFloats g)                            // :98 — NOT a copy
  |> Array.unzip
let y's = gradients |> Mat.ofRowsArray                        // :103 — after every pass has run
```

Three facts make this fatal, each verified:

1. `DV.toFloats` (`:650`) returns the backing array — `| DV(p) -> p`, no copy.
   Mutating the result mutates the `DV`.
2. `drFwds` is created once and reverse-propagated once per instrument, so
   every pass resets and rewrites *the same node's* adjoint.
3. Nothing is copied out until `Mat.ofRowsArray` at `:103`, after the loop.

Reproduced with the probe, on a two-output version of exactly this shape:

```
stock:  collected = [| [|5.0; 3.0|]; [|1.0; 1.0|] |]     correct
probe:  collected = [| [|0.0; 0.0|]; [|1.0; 1.0|] |]     first row silently zeroed
```

The failure *signature* depends on which steps have landed, which matters for
writing the test:

- **Step 2 alone** (reset reuses, push still allocates at `:3746`): each pass's
  reset zeroes the buffer the previous pass handed out, then push installs a
  fresh `DV` in the ref. Earlier rows come back **zero**, and the row arrays are
  still distinct objects — a reference-identity assertion would not catch it.
- **Steps 2 and 4 together** (push accumulates in place, so the ref keeps one
  object): every row **aliases the last** gradient.

So assert on values, never on identity.

Blast radius: `ff'` is the only Jacobian source for `CurveSolver.solve` and
`solveWithJac`, which `CurveFit.fs` calls at `:37`, `:50`, `:74`, `:101`,
`:126`, `:318` and `:369` — every curve fit, `MarketBuild` included. Analytics
is Fable-compiled into `WldMr.Web`'s clients, so this same source breaks in JS
and Python too, not only .NET.

Expected downstream signal, reasoned but **not run**: zeroed rows make the
Jacobian singular, `Mat.solveMV` fails, and `solve`/`solveWithJac` return
`Error "Singular matrix - conflicting instruments are likely"`, so Analytics'
curve-fit suites should go red loudly rather than drift numerically. Worth
confirming with `local-feed.sh` before step 2 rather than trusting it.

### No in-repo test catches this

Full `dotnet test -c Release` with the probe applied:

```
WldMr.Numerics.LinAlg.Tests    16 passed, 0 failed
ExpectoTests                    7 passed, 0 failed
WldMr.Numerics.DiffSharp.Checks 35 passed, 0 failed, 6 skipped
```

Green. The FsCheck property suite compares backend numerics against reference
implementations and never exercises adjoint *lifetime*, so it cannot see this
class of bug at all. The plan's "load-bearing and untested" is confirmed, and
CI offers no protection.

### Verdict: the contract, and where to put the copy

Adopt: **an adjoint obtained through `adjoint` is the caller's own value; `.A`
is the raw accessor and aliases the tape.**

Implement it by copying in `DOps.adjoint` (`:3346`) — *not* in `r2` (`:4076`) as
the plan proposes. Every escape point in the family already funnels through
`adjoint`: `grad'` (`:4045`), `r2` (`:4076`), `CurveSolver.fs:90`,
`DiffSharpTests.fs:38` and the benchmark console. One guarded copy there covers
all of them, needs no change in Analytics, and leaves `pushRec`'s sixteen
internal `.A` sites untouched.

Mirror the constraint-5 guard: copy only when the adjoint is a plain `DV`/`DM`,
pass a `DVF`/`DVR` through unchanged — the same case split that decides whether
reset may reuse, so the two stay consistent under nesting.
`Array.copyFast` (`Util.fs:138`) and `Mat.copy`/`GenMat.copy`
(`LinAlg/Mat.fs:228`, `GenMat.fs:86`) are the existing multi-target primitives
and already compile under Fable.

Cost: one `DV` per `adjoint` call — per *reverse pass*, not per node. At depth 8
that is a ~40-double array against the 343 KB a pass currently costs. Noise.

The alternative — keep aliasing and fix `CurveSolver` to `Array.copy` — is
worse: it needs a coordinated Numerics + Analytics release with Web and
Excel.Addin bumps behind it, and it leaves the trap armed for the next consumer
of a public `[<AutoOpen>]` API.

### What this changes in the plan above

- Step 2 loses its "and make `jacobianTv''`'s `r2` return a copy in the same
  commit" clause; it becomes "and make `adjoint` (`:3346`) return a copy in the
  same commit". The indivisibility argument is unchanged and still holds.
- Constraint 3's "The first casualty is not downstream — it is `jacobian'`"
  paragraph is wrong and should be replaced by correction 1 above.
- The new-test bullet under verification should cover the `adjoint`-direct shape
  (one `makeReverse`, two `reverseProp`s, read the first result after the second
  pass) rather than only `jacobianTv''`'s `r2`, since `r2` is a strict special
  case of it and the shape that actually breaks is the general one. Assert on
  values, not on reference identity — see the two signatures above.
- Step 5 gains the `jacobian'` finding: N+1 forward passes per N-row Jacobian.

All four applied to the body, 2026-08-05.

## Step 4: in-place adjoint accumulation — landed 2026-08-07

This section replaces the step-4 hand-over that previously stood here; it keeps
only what later steps still need.

### What changed

The `DV` and `DM` central accumulates in `pushRec` — nothing else:

```fsharp
dARef.Value <- DV.Add_V_V_Inplace(v, dARef.Value)   // was: dARef.Value + v
dARef.Value <- DM.Add_M_M_Inplace(v, dARef.Value)
```

The previously dormant `*_Inplace` members pass the same `fd`/`df_*`/`r_*`
lambdas as `(+)`, so every mixed case (`DVF`/`DVR`/`DMF`/`DMR` on either side —
constraint 5) dispatches exactly as before; only both-plain changes, to a
destructive `daxpy` into the buffer reset leaves in place. They are destructive
of their **second** argument, hence `v` first.

Two facts made this smaller than the plan feared:

- **The preconditions were already the site's own.** `Backend.Add_V_V` /
  `GenMat.addM` at the same site already errored on the same shape mismatches —
  `addM` even required `ColMajor` on *both* sides where in-place needs it only
  on the destination. The one narrowing: an empty destination receiving a
  non-empty contribution now errors instead of copying. A consistent graph
  cannot produce that today, but it becomes the live error path if step 3
  removes the eager allocation at `DV.R` — that is the step-3 interaction to
  mind.
- **Identity pushes were secretly expensive.** `Add_V_V`/`Mat.addM` with an
  empty operand return a full copy of the other side, so every
  `bxv DV.Zero a` / `bxm DM.Zero a` bookkeeping push (all the slicing ops emit
  them) copied the entire adjoint. They are now no-ops.

Deliberately left alone: the scalar `D` accumulate (an immutable scalar cannot
accumulate in place) and the sixteen `.A <-` bypass sites in `pushRec`
(`rg '\.A <-' AD.Lite.fs`). Push now costs ~8 B per adjoint slot — exactly one
allocation per slot, the derivative payload itself (`dA .* b.P`, `-dA`, …).
Going below that means fusing derivative computation into the accumulate (the
structured-expression TODO at the `Sub_DM_DM` push site), a different and much
larger change; in this op mix the bypass sites are inside the noise. Whoever
does convert them must re-establish what made the central sites safe: the
destination buffer is uniquely owned by its node, and a pushed contribution is
only ever read.

### Current baseline — what step 3 measures against

`-- phases 8`, B/pass: forward 229,040 · reset 2,136 · push 115,472 ·
reverseProp 117,608. Push was 227,432 (16.87 B/slot) before this step.

BDN depth 8 `Allocated`: Forward 223.67 KB (1.00) · ForwardAndReset 225.76
(1.01) · ForwardResetAndPush 338.88 (1.52) · ForwardThenSeedPasses ×8 1142.84
(5.11).

Downstream (`BenchmarkMarketBuild -- stages 20`, warm shared world): whole fit
**74.0 MB**, `fitMktData` **47.0 MB** — from 86.6 / 59.8 before this work
started, −14.6% cumulative, all of it inside curve fitting.

### The failure signature changed — keep the tests honest

With an in-place push, a broken `adjoint` copy no longer reads stale zeros; it
**aliases the live buffer** — and a same-seed rerun refills an aliased buffer
with identical values, which reads as green. That caught the DM lifetime test,
which seeded both passes with `D.One`; it now seeds the second pass `D 3.0`.
The rule for any test here: assert on values, distinct seeds (or functions) per
pass, never reference identity. The copy-neutralisation drill — temporarily
drop the copies in `DOps.adjoint`, expect the three survival tests red —
verifies a test is load-bearing; it was re-run after the seeding fix.

### Verification recipe (worked for steps 2 and 4, unchanged)

```bash
# in-repo
dotnet test                                        # 16 + 11 + 35 pass, 6 skipped
npm test                                           # Fable→JS, 11 passing
dotnet fable tests/ExpectoTests --lang python -o py-build/tests/ExpectoTests \
  && python3 py-build/tests/ExpectoTests/main.py   # 11 passing

cd consoles/BenchmarkAdTape
dotnet run -c Release -- phases 8
dotnet run -c Release -- width
dotnet run -c Release                              # BDN headline, ~7 min

# downstream
/git/wldmr-dev/scripts/local-feed.sh pack WldMr.Numerics WldMr.Analytics
/git/wldmr-dev/scripts/local-feed.sh use  WldMr.Analytics
# in WldMr.Analytics: dotnet test (445), npm test (222), then in
# consoles/BenchmarkMarketBuild: -- fingerprint and -- stages 20
/git/wldmr-dev/scripts/local-feed.sh clear         # always
```

**Expected fingerprint:
`15a6fee33a346e31be9e6de14de49e968ea71958bdd5193885939d9d2c0cf52d`** — stable
across stock, step 2 and step 4; any change to it is a real numerical
regression, not noise.

Gotchas that cost time, all still live: `local-feed.sh` is not on PATH (use the
full path above); `-- stages 5` is JIT-contaminated, use 20 reps; don't pipe
the BDN run through `tail`; Analytics' `npm test` runs three suites, redirect
to a file and grep; `GenMat.addM`/`mulM` are `failwith "todo"` off the
ColMajor+ColMajor diagonal, so DM experiments hit holes unrelated to AD.

### Then what

Step 3 (`DV.R`'s eager zero-vector) — mind the empty-destination interaction
above. Step 5's in-library half is independent and cheap: `jacobian'` re-runs
the whole forward pass per Jacobian row because `let r = jacobianTv f x` is a
partial application; holding `jacobianTv''`'s `r2` fixes it.

Adoption: `WldMr.Analytics` pins with `lowest_matching`, so it moves onto none
of this by itself — it needs an explicit `>= x.y.z` minimum. See the
`wldmr-cross-repo-dev` and `wldmr-analytics-release` skills.
