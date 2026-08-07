# A first-class reverse-mode `gather` for `DV`

**Status: design, 2026-08-07. Nothing implemented. Written against master at
`1af3889` ("accumulate adjoints in place on reverse push").**

Line references are against that commit. `AD.Lite.fs` means
`src/WldMr.Numerics.DiffSharp/AD.Lite.fs` (4,224 lines) unless said otherwise.

## Why

`WldMr.Analytics`' `LinearInterpolator.InterpolateV`
(`src/WldMr.Analytics/Curve/LinearInterpolation.fs:80-104`) needs, per call:

```
result[i] = c0[ks[i]] + (ts[i] - x[ks[i]]) * c1[ks[i]]
```

with `ks: int[]` segment indices, `x: float[]` constants, `c0, c1 : DV` on the
reverse tape. Because no differentiable gather exists, it builds **two** CSR
selection matrices per call — `csrS` (:82-86: an all-ones values array, `ks` as
columns, an `n+1` row-index array) and a hand-built transpose `csrST` (:88-97:
another ones array, an `n` rows array, an `m+1` column-index scan) — wraps them
as `DM (SparseDouble (csrS, csrST))` (:99) and runs two sparse mat-vec products
(:102-103). `csrST` exists **only** so that `GenMat.transpose`
(`src/WldMr.Numerics.LinAlg/GenMat.fs:97`, a pair swap) is free when the
reverse pass of `Mul_DMCons_DV` computes `DM.Transpose(cons) * dA`
(`AD.Lite.fs:3813`) — i.e. it is the reverse rule, precomputed by hand on every
forward call.

Measured cost, from the MarketBuild CPU/allocation profile
(`WldMr.Analytics/plans/marketbuild-cost.md:753-769`): **~5.5 MB/fit**, with
49.6 MB of `Double[]` attributed to `InterpolateV` across a 20-fit trace, plus
the `Int32[]` index arrays, the `SparseDouble`/`DM` wrappers, and
`CsrMat.mulV` cost downstream (76.7 MB of `Double[]` across all callers). It
runs per pricing evaluation inside the curve-fit solver loop.

A gather primitive — forward: pick elements by index; reverse: scatter-add the
incoming adjoint into the source's adjoint — collapses the whole ceremony to
one captured index array.

## 1. API surface

Repo convention for `DV` ops is tupled static members, data first:
`DV.AddSubVector (a, i, b)` (:1164), `DV.Split (d, n)` (:685),
`DV.Append (a, b)` (:1377). Follow it:

```fsharp
/// result.[i] = a.[ks.[i]]
static member Gather (a: DV, ks: int[]) : DV

/// The adjoint pair: length-n vector with result.[ks.[i]] accumulating b.[i]
static member Scatter (b: DV, ks: int[], n: int) : DV
```

plus a pipe-friendly helper in `module DV` (:2924), matching `DV.ofArray` /
`DV.toArray` style: `let inline gather (ks: int[]) (v: DV) = DV.Gather(v, ks)`.
(The prompt's curried `int[] -> DV -> DV` shape is what the module helper
provides; the static member follows the file's existing tupled form.)

`Scatter` is public because it is `Gather`'s reverse rule and the two are each
other's adjoints — exactly the pairing `Slice_DV` / `AddSubVector` already
have (push at :3893-3895 calls `DV.AddSubVector`). It must be a first-class op
anyway for nested AD (see §2), so hiding it buys nothing.

**Single-index `D` variant: do not add one.** It already exists as the indexer
`d.[i]` (:584-589), trace op `Item_DV` (:2713), push at :3741-3743.

**Bounds validation: always on, in both debug and release, on all three
targets.** The check is one integer compare per element against arithmetic
that then does O(n) double work — noise. And it is not optional off .NET:
under Fable→JS a `Float64Array` read out of bounds yields `undefined` (NaN
after arithmetic) and an out-of-bounds *write is silently discarded*; under
Fable→Python a negative index silently wraps to the end of the array. Only
.NET throws. Validate `0 <= ks.[i] < a.Length` in `DV.Gather` (and `< n`,
plus `ks.Length = b.Length`, in `DV.Scatter`) with an `invalidArg` following
the `ErrorMessages` pattern (`DiffSharp/Util.fs:107-110`). Empty `ks` returns
`DV.Zero` (:646) without building a node, matching `Append`'s empty-operand
early-outs (:1378-1381).

The index array is captured **without copying**, like `csrS.Columns = ks`
aliases it today (`LinearInterpolation.fs:84`); document that the caller must
not mutate `ks` after the call, and that the tape keeps it alive until the
tape dies (against six arrays today — a net reduction).

## 2. Tape representation, forward, reverse

### Trace ops

Two new cases in `TraceOp` (:2669), in the vector-valued section next to
`Slice_DV` (:2810):

```fsharp
| Gather_DV               of DV * int[]     // source node, indices
| Scatter_DV              of DV * int[]     // source node, indices
```

`Gather_DV` captures the source `DV` (the node reference the walkers recurse
into, as every case does) and the index array. Only one case each: the index
array is a constant, so there is no `*Cons` triplet as for two-`D`-argument
ops.

### Forward

Both are unary linear ops and fit `Op_DV_DV` (:700-704) exactly, the same
mould as `DV.Sum`/`ReshapeToDM` (:1369, :1393):

```fsharp
static member Gather (a: DV, ks: int[]) =
    // validate ks against a.Length here (always)
    let inline ff(a) = Backend.Gather_V(a, ks)
    let inline fd(a) = DV.Gather(a, ks)
    let inline df(cp, ap, at) = DV.Gather(at, ks)      // linear: tangent gathers
    let inline r(a) = Gather_DV(a, ks)
    DV.Op_DV_DV (a, ff, fd, df, r)
```

`Scatter` is symmetric (`ff = Backend.Scatter_V(b, ks, n)`,
`df = DV.Scatter(bt, ks, n)`, `r = Scatter_DV(b, ks)`).

The array kernels go in `Backend.Lite.fs` per this repo's rule ("numeric
behaviour changes belong here, not in `AD.Lite.fs`" — `CLAUDE.md`), alongside
`Sum_V`/`Add_V_V`:

```fsharp
static member inline Gather_V(x: float[], ks: int[]) =
    let r = Array.zeroCreate ks.Length
    for i in 0 .. ks.Length - 1 do r.[i] <- x.[ks.[i]]
    r
static member inline Scatter_V(x: float[], ks: int[], n: int) =
    let r = Array.zeroCreate n
    for i in 0 .. ks.Length - 1 do
        let k = ks.[i]
        r.[k] <- r.[k] + x.[i]           // duplicates ADD; ascending i — see §7-adjacent bit-identity note
    r
```

The DVR branch of `Op_DV_DV` builds the node via `DV.R` (:576), which still
allocates the eager `DV.ZeroN` adjoint — same as every op; that is
`ad-tape-allocation.md` step 3's business, not this op's.

### Reverse

`resetRec`, in the `DVR` branch's op dispatch (the list at :3459-3547):

```fsharp
| Gather_DV(a, _)  -> resetRec (bxd a :: t)
| Scatter_DV(b, _) -> resetRec (bxd b :: t)
```

`pushRec`, in the `DV` branch (:3792-3900):

```fsharp
| Gather_DV(a, ks)  -> pushRec ((bxv (DV.Scatter(dA, ks, a.Length)) a) :: t)
| Scatter_DV(b, ks) -> pushRec ((bxv (DV.Gather(dA, ks)) b) :: t)
```

Design choice, deliberate: **materialize the scatter vector and push it
through the central channel** — the style of `AddSubVector_DV_DV`'s push
(:3889) — rather than the `.A <-` bypass style of `Slice_DV` (:3893-3895,
`a.A <- DV.AddSubVector(a.A, i, dA)` followed by an identity `bxv DV.Zero a`
push). Reasons:

- **It composes with `1af3889` for free.** The central accumulate
  (:3782-3788) does `dARef.Value <- DV.Add_V_V_Inplace(v, dARef.Value)` — a
  destructive `daxpy` into the buffer `reverseReset` leaves in place when both
  sides are plain `DV`, with the `(+)` dispatch for every nested case
  (`DV.Add_V_V_Inplace` :906-915, `Backend.Add_V_V_Inplace`
  `Backend.Lite.fs:34-40`). A pushed contribution is only ever read, which is
  precisely the ownership contract the step-4 commit message establishes for
  central-channel pushes. No new `.A` bypass site is added to the sixteen the
  audit counted.
- **Fan-out stays textbook.** `resetRec` bumps the source's counter once per
  gather node (:3456-3457); `pushRec` decrements once and delivers exactly one
  contribution (:3782). If one source feeds two gathers, the counter reaches 2
  and the source's own parents are visited only after both contributions have
  arrived — the protocol from `ad-tape-allocation.md` constraint 2, untouched.
  **Duplicate indices inside one `ks` are internal to the node**: they fold
  inside `Scatter_V`'s `+=`, and contribute nothing extra to fan-out. That is
  the correct accounting — one op, one use.
- **Nested AD works without special cases.** Under forward-on-reverse the
  incoming `dA` can be a `DVF` (constraint 5 in `ad-tape-allocation.md`);
  `DV.Scatter` is itself a proper op, so the `DVF` dispatches through
  `Op_DV_DV`'s middle branch and produces the right dual — the same reason
  `Slice_DV`'s push calls the differentiable `DV.AddSubVector` rather than an
  array loop. This is also why `Scatter_DV` needs its own reset/push cases
  (reverse-on-reverse puts scatter nodes on an outer tape).
- **Allocation is a wash versus the bypass.** The bypass would call an
  `AddScatter`-style op whose `ff` copies the whole adjoint
  (`Array.copyFast`, as `AddSubVector`'s `ff` does at :1165-1169); the
  materialized vector is the same one full-length `float[]` — and it is
  byte-for-byte the vector the CSR path already allocates as
  `DM.Transpose(cons) * dA`. A later zero-allocation variant (guarded in-place
  scatter into `a.A`) is possible but changes summation order — see §7; do not
  do it in the first cut.

Also note the empty-adjoint invariant: the contribution flows through the same
channel as every other op, so the step-3 interaction documented in
`ad-tape-allocation.md` ("an empty destination receiving a non-empty
contribution is `Add_V_V_Inplace`'s error path; reset runs before every push,
so buffers are full-length by push time") applies to gather unchanged, with
nothing new to mind.

**Implementation trap, worth stating:** both walkers end every branch with a
wildcard (`| _ -> resetRec t` at :3436/:3551, `| _ -> pushRec t` at :3901).
`src/Directory.Build.props`' `WarningsAsErrors FS0025` does **not** protect a
new `TraceOp` case — forgetting the `pushRec` case compiles clean and yields a
silent zero gradient; forgetting `resetRec` leaves the source un-reset and
un-armed. The §4 tests are chosen to catch exactly these two failure shapes.

## 3. The consumer rewrite

`InterpolateV` (`LinearInterpolation.fs:80-104`) becomes:

```fsharp
member li.InterpolateV (ts: float[]): DV =
  let ks = li.LeftSegmentIndex(ts)
  let vX = Array.init ts.Length (fun i -> x.[ks.[i]])   // x is constant — plain float loop, no AD
  let dTs_minus_X = Blas.sub_V_V(ts, vX)
  let a = DV.Gather(c0, ks)
  let b = DV.Gather(c1, ks) .* (DV dTs_minus_X)
  DV.Add_V_V_Inplace(a, b)
```

Expression topology is 1:1 with today's (`Gather_DV` replaces each
`Mul_DMCons_DV`; the `.*` node and the final `Add_V_V_Inplace` node are
unchanged), which matters for §7.

Gone per call (n = `ts.Length`, m = `x.Length`):

- `csrS`: the ones array (n doubles) and `RowIndices` (n+1 ints) — `Columns`
  aliased `ks` and cost nothing;
- `csrST` entirely: ones (n doubles), rows (n ints), colIndices (m+1 ints),
  plus the O(m+n) scan loop that builds it (:92-96);
- the `SparseDouble` `GenMat` case and the `DM` wrapper (:99), and their
  lifetime on the tape — the two `Mul_DMCons_DV` trace nodes each held the
  whole six-array structure alive until the tape dropped; `Gather_DV` holds
  one `int[]`;
- two `CsrMat.mulV` runs (n multiply-adds each, `CsrMat.fs:119-130`) become
  two n-element copy loops; `vX`'s mulV (:100) becomes the plain loop above.

Stays: `ks`, `vX`, `dTs_minus_X` (same three arrays as today), the two gather
result vectors (the mat-vec products allocated the same), the two `DVR` nodes
with their eager adjoints, and — on the reverse side — one materialized
length-m vector per gather per pass, same as `csrST * dA` costs today.

Side benefit: the CSR-transpose construction (:92-96) silently requires `ks`
non-decreasing (and `LeftSegmentIndex(ts)` at :48-62 requires `ts` sorted).
Gather itself has no ordering requirement, so this rewrite removes one of the
two hidden sortedness assumptions.

## 4. Correctness testing

Layout per `CLAUDE.md`: `tests/ExpectoTests` is the only Fable-compiled suite
(new lists register in `all`, `Main.fs:15-21`; the file is added to the
explicit `<Compile>` list); `tests/WldMr.Numerics.DiffSharp.Checks` is the
.NET FsCheck net (`[<Property>]` style, `AD.Float64.fs:14`).

**`tests/ExpectoTests/GatherTests.fs`** — cross-runtime, deterministic values
(gather is exact index-picking, so integer-valued expectations through
`Expect.floatClose` with the existing `accuracy`, per
`AdjointLifetimeTests.fs:22`):

1. Forward values, including duplicate and *unsorted* `ks`.
2. Reverse with duplicates: `f v = DV.Sum(DV.Gather(v, [|1;1;0|]))` on
   `makeReverse`'d input; gradient must be `[|1.; 2.|]` — **the scatter must
   ADD**. This is also the test that goes red if the `pushRec` case is
   forgotten (silent zero) or if a future in-place variant overwrites instead
   of accumulating.
3. Fan-out: one source feeding **two** gather nodes combined by `+`; the
   gradient is the sum of both scatters. Red if the `resetRec` case is
   forgotten (the counter never arms; stale adjoints or a stopped push).
4. Nested: forward-over-reverse through a gather (a `DVF`-seeded pass, the
   constraint-5 shape), exercising `Scatter`'s `DVF` dispatch.
5. Bounds: index `= a.Length` **and** index `= -1` both raise, asserted on
   all three runtimes — the JS-silent/Python-wrap trap from §1 is exactly why
   this test must run under Fable, not only .NET.

**`tests/WldMr.Numerics.DiffSharp.Checks`** (new `[<Property>]`s next to
`AD.Float64.fs`):

6. Finite differences: `grad (fun v -> DV.Sum(DV.Gather(v, ks) .* w))` versus
   `Numerical.Float64` `DiffOps.grad'` (`Numerical.Float64.fs:51-59`) via
   `Util.(=~)`, over random `v`, random weights, `ks` mapped into range by
   `abs k % v.Length`. Linearity means a weighted-sum probe covers the whole
   Jacobian.
7. CSR equivalence, the adoption guard in miniature: build the exact
   `InterpolateV` pair of formulations (selection CSR + `SparseDouble`/`DM`
   versus gather) over random sorted inputs; assert primal **and**
   reverse-mode gradients equal with `=`, **not** `=~` — this is the in-repo
   canary for the §7 bit-identity claim. (Generator must produce
   non-decreasing `ks`; the CSR-transpose construction requires it, gather
   does not.)

Also run the adjoint-lifetime suite unchanged — gather goes through `adjoint`
(:3352-3365) like everything else, and its copy contract is what makes the
consumer's hoisted-`makeReverse` loop safe.

## 5. Benchmark

`consoles/BenchmarkAdTape` gains one BDN class and one phases mode; the
existing `AdTapeBenchmark` rows stay untouched for cross-run comparability.

- `Benchmarks.fs`: `[<MemoryDiagnoser>] type GatherBenchmark` over the
  `InterpolateV` shape at the harness's standard width (m=40 pillars, n=320
  points, `Graph.spec`'s ratio; sorted synthetic `ts`, `c0`/`c1`
  `makeReverse`'d per iteration like `Graph.forward`). Four rows:
  `CsrForward`, `GatherForward`, `CsrForwardAndReverse`,
  `GatherForwardAndReverse`. Read `Allocated`, as the file header says.
- `Phases.fs`/`Program.fs`: a `-- gather [reps]` mode printing forward/reverse
  B/pass for both formulations from `GC.GetTotalAllocatedBytes`, and
  asserting value and gradient max-abs-diff is exactly `0.0` — a standing
  bit-parity check that runs in seconds, unlike the BDN suite.

Expected shape of the result: forward-side saving ≈ the six CSR arrays and
wrappers (~2n doubles + (2n+m+2) ints ≈ 6.5 KB per call at n=320) plus the
mulV arithmetic; reverse side ≈ unchanged (the materialized scatter vector
replaces `csrST * dA` one-for-one). The end-to-end number that matters is
downstream: `WldMr.Analytics`' `BenchmarkMarketBuild -- stages 20`
(`fitMktData` column; 47.0 MB is the current post-`1af3889` baseline per
`ad-tape-allocation.md`) via the local feed.

## 6. Fable notes

- **No BCL guards needed.** Both kernels are index loops plus
  `Array.zeroCreate`; the `System.Array.Clear` guard precedent
  (`LinAlg/CsrMat.fs:45-50`) was checked and is not required — nothing
  platform-specific appears.
- **The bounds check is a correctness feature under Fable, not hygiene** —
  JS typed arrays don't throw and Python wraps negatives (§1). This is the
  one place the three targets genuinely diverge.
- `int[]` compiles to `Int32Array` (JS); capturing it in a union case is fine.
- New test file goes into `ExpectoTests.fsproj`'s explicit `<Compile>` list in
  dependency order; new members in `AD.Lite.fs` keep the fork's 4-space
  indentation (repo `CLAUDE.md`: do not reformat forked files).
- The Python target is not in CI — run
  `dotnet fable tests/ExpectoTests --lang python -o py-build/tests/ExpectoTests
  && python3 py-build/tests/ExpectoTests/main.py` by hand; the bounds test (5)
  is the one that can only fail there.

## 7. Versioning, rollout, and the fingerprint

**Version:** the package version is CI-composed — `major: 1` / `minor: 3`
with a patch counter keyed on `major.minor`
(`azure-pipelines.yml:13-24,35`) — so merging to master publishes **1.3.16**
with no file edit. Recommended over bumping to 1.4.0: the change is purely
additive, and `17e9147` ("Constrain dependencies so consumers can adopt
1.3.x") deliberately set consumers up for the 1.3 line. Analytics resolves
with `lowest_matching` and currently pins/locks `>= 1.3.15`
(`WldMr.Analytics/paket.dependencies:55-56`, `paket.lock:284-289`), so it
moves only via an explicit bump to `>= 1.3.16` in the same PR as the
`InterpolateV` rewrite. Pre-merge validation via
`local-feed.sh` / the `-pr` packages (`wldmr-cross-repo-dev` skill), release
via `wldmr-analytics-release`.

**The fingerprint must not move:**
`BenchmarkMarketBuild -- fingerprint` must stay
`15a6fee33a346e31be9e6de14de49e968ea71958bdd5193885939d9d2c0cf52d`
(`ad-tape-allocation.md`, verification recipe). The claim: **the gather
formulation is bit-identical to the CSR formulation**, on three grounds, each
checked against the code:

1. *Forward.* `CsrMat.mulV` (`CsrMat.fs:119-130`) computes a one-entry row as
   `0.0 + c0[k] * 1.0`. `x * 1.0 = x` exactly and `0.0 + x = x` exactly for
   every double **except `x = -0.0`** (which normalizes to `+0.0`); gather
   returns `c0[k]` verbatim. So the primals agree bitwise unless an
   interpolated coefficient is exactly negative zero — unreachable from the
   exp/division arithmetic these curves are made of, but it is the one stated
   exception rather than a proof of full equality.
2. *Reverse.* The CSR reverse contribution is `csrST * dA`: for source `j`,
   `mulV` left-folds `dA[i]` over `csrST`'s row-`j` entries, which the
   construction at `LinearInterpolation.fs:88-97` lists in ascending `i` (it
   requires `ks` non-decreasing). `Scatter_V` iterates `i` ascending into a
   zero-initialized vector, producing the identical left fold per `j`
   (`0.0 + d1` is exact). Both vectors then enter the same central accumulate
   (:3787). Same numbers, same order, same `daxpy`.
3. *Topology.* §3's rewrite preserves the expression graph node-for-node, so
   the worklist delivers contributions to `c0`/`c1` in the same sequence, and
   the (non-associative) accumulate sums in the same order.

Condition 2 is exactly why §2 rejects the in-place `.A` scatter for the first
cut: writing `dA[i]` directly into a non-empty adjoint re-associates the sum
(`((A+d1)+d2)` versus `A+(d1+d2)`) whenever the source's fan-out is above one
— which it always is in a fit, where `c0` feeds every instrument on the shared
tape. That variant is a real fingerprint risk and is deferred until the
materialized version is validated end-to-end. **If the fingerprint moves
anyway, stop and treat it as a bug in this analysis, not as noise** — the plan
doc's rule.

Verification order: in-repo tests (§4, all three runtimes) → `-- gather`
parity mode → local-feed into Analytics → its 445 .NET + 222 JS tests →
`-- fingerprint` → `-- stages 20`.

## 8. Open questions, with recommendations

1. **Names `Gather`/`Scatter`?** Yes — standard AD/array vocabulary, zero
   collisions in the family (`rg Gather` finds nothing), and the pair names
   its own adjoint relationship. Alternative `Select`/`SelectAdd` says less.
2. **Materialized-scatter push versus in-place `.A` scatter?** Materialized
   (§2, §7). Revisit in-place only after the fingerprint has a green history,
   and then behind the constraint-5 guard (`DV`-and-`DV` only, matching
   lengths), accepting that it re-associates sums and so likely moves the
   fingerprint — it would need Analytics to re-baseline, a much bigger ask
   than the saving (one length-m array per gather per pass) justifies today.
3. **Validate bounds in release?** Yes, unconditionally (§1). The Fable
   targets turn a bad index into silent NaN or a silently-dropped adjoint
   write; there is no `#if DEBUG` convention in this file to hide behind.
4. **A `DM` gather (rows/columns by index)?** Defer. No consumer need;
   `SliceRow_DM`/`SliceCol_DM` cover the single-row cases, and the `GenMat`
   off-diagonal `failwith "todo"` holes make DM work disproportionately
   expensive to test.
5. **`D`-returning single-index variant?** No — `d.[i]` / `Item_DV` is that
   op already (:584, :3741).
6. **Copy `ks` defensively?** No — matches the existing aliasing of `ks` into
   `csrS.Columns`, and the callers build a fresh array per call anyway.
   Document the no-mutation contract on the member.
7. **Where do the kernels live?** `Backend.Lite.fs` (`Gather_V`,
   `Scatter_V`), per the repo rule that numeric behaviour belongs in the
   backend; `AD.Lite.fs` gets only the op plumbing.
8. **Should `InterpolateV` validate `ks` sortedness now that gather doesn't
   need it?** Out of scope here — an Analytics decision. Note only that the
   gather rewrite removes the silent dependency from the differentiable path;
   `LeftSegmentIndex(ts: float[])` still assumes sorted `ts`.
