# Reducing per-op AD allocation: direction note

> **Re-sequenced 2026-08-08 against an object-COUNT profile — read
> "The worklist" below before acting on the sequencing at the end of this file.**
> Everything above that section was written from a *byte* profile, and bytes rank
> these options wrongly: the reverse pass's per-node bookkeeping is invisible in
> bytes at wide nodes and dominant in counts at every width.
>
> **All of it is now removed**, plus two allocations that bought nothing, the two
> `ref` cells per node, and half the zeroing work in the reset. A MarketBuild fit
> is 1,059,787 -> **771,313 objects (-27.2%)**, 49.8 -> **42.2 MB (-15.3%)** and
> 18.9 -> **17.5 ms (-7.4%)**, fingerprint unchanged throughout. Shipped as
> WieldMore-io/WldMr.Numerics#23. What is left of the tape is the 3 objects per
> node that construction costs; see "The ceiling, quantified".
>
> **Two queued steps were withdrawn by measurement, not by choice** — read those
> sections before picking either back up. Adjoint pooling has no lifetime to pool
> in ("The adjoint-pooling costing"), and fusion's ceiling is ~5 objects per node
> *eliminated*, not the wrapper total ("The TraceOp census, re-measured").

> [!NOTE]
> **Line numbers in the 2026-08-07 sections point at the file as it was then.**
> `AD.Lite.fs` has since moved by hundreds of lines and both traversals were
> rewritten, so treat `:3473`-style references in "Where the bytes go per op" and
> "Candidate directions" as archaeology. References added from 2026-08-08 onward
> name symbols instead, which is what to do from here.

**Status: direction-setting, 2026-08-07.** Follows `ad-tape-allocation.md`, whose
steps 2 and 4 (buffer reuse on reset, `d1f4435`; in-place accumulation on push,
`1af3889`) are landed. After those, the AD/LinAlg cluster is still roughly half
of an Analytics `MarketBuild` fit's allocation: in a 20-fit trace, `Double[]`
alone is 414.7 MB (31.6%), led by `CsrMat.mulV` (76.7 MB), `DV.op_DotMultiply`
(74.3 MB) and `DV.ZeroN` (32.0 MB), plus ~140 MB of `D`/`DV` wrappers, ~78 MB of
worklist tuples and cons cells, and 16.6 MB of `FSharpRef<uint32>` fan-out
counters. The remaining cost is structural — every eager op allocates — so this
note sets direction rather than designing a fix.

## Where the bytes go per op

Trace `a .* b` on two same-tag `DVR`s (`AD.Lite.fs:944`, dispatched through
`Op_DV_DV_DV`, DVR×DVR case `:748`). The forward op allocates **seven objects**:
the result `float[]` (`Backend.Map2_Mul_V_V`, `Backend.Lite.fs:401`) and its `DV`
wrapper; then `DV.R` (`AD.Lite.fs:576`) adds a full-length zero adjoint `float[]`
(`DV.ZeroN`, `:648`) with its own `DV` wrapper, a `ref` cell for it, a `ref 0u`
fan-out cell, and the `DVR` node itself — plus the `Mul_Had_DV_DV(a, b)` TraceOp
object (`:2736`). Each reverse pass then adds, per node: cons cells in reset
(`:3473`), a `(dobj*dobj)` tuple plus cons cell per contribution in push
(`:3685`), and one derivative-payload array per slot (`dA .* b.P`, `:3806` — the
one array 1af3889 left, since the accumulate itself is now in place, `:3787`).
The tape pins every buffer until reset, so nothing is reclaimed mid-fit. Sparse
matvec pays the same twice over: `CsrMat` `MulV` allocates its result
(`CsrMat.fs:70`) once per layer forward and once per layer per reverse pass
(`Mul_DMCons_DV` push, `AD.Lite.fs:3813`, via `GenMat.mulV`, `GenMat.fs:136`).

## Candidate directions

**a. Buffer pooling / arena tied to the tape lifecycle.** Rent result and
adjoint arrays from a pool owned by the tape generation; return them wholesale.
Payoff ceiling is the largest — it covers the result arrays that dominate
`Double[]` — but there is no lifecycle hook to anchor it: the "tape" is the
implicit graph hanging off `DVR` nodes, and the only sweep points are
reverse-reset (`:3372`, which runs over a still-live graph, many times per tape
in `CurveSolver.ff'`) and GC. Recycling therefore needs an explicit tape-scope
API, a new public surface. The specific sinking risk is aliasing with
caller-held arrays: `DV.toFloats` (`:650`) hands out the backing array, and
primals escape through far more paths than adjoints ever did. The
`adjoint`-copies precedent (`:3352`) bounds the shape of the fix — copy at every
escape accessor, enumerated by audit, exactly as step 1 did for adjoints — but
the primal escape surface is much wider, so the audit is the real cost.
Reorder-free (same values, recycled storage). **Weeks.**

**b. In-place ops via fan-out (refcount-guided reuse of an input buffer).**
Mostly not viable at construction time: fan-out counters are populated during
*reset* (`:3456`), not during the forward pass, so an op cannot know its
operand is dead — and even a dead-looking operand's primal is retained by the
TraceOp precisely because push needs it (`Mul_Had_DV_DV` push reads `a.P` and
`b.P`, `:3806`). Overwriting an input corrupts the recorded primal. The viable
narrow form is push-side: the contribution temporaries (`dA .* b.P` etc.) are
uniquely owned by `pushRec` and die within the pass — but that is a pooling
problem with a clean lifecycle, i.e. a sub-case of (a), not a separate
direction. **Days for the narrow form (as part of a); weeks and fragile for the
general form — do not pursue the general form.**

**c. Fusing the hot patterns.** Two flavours. *Forward fusion*: new ops for the
axpy-like chains the curve code produces (e.g. `exp`∘`.*`, `.*`-then-`+`), each
replacing 2–3 nodes with one — one result array, one TraceOp, one `DVR`, one
zero adjoint instead of two or three of each. This is the only lever that
reduces *node count*, which is what the ~140 MB of wrappers, ~78 MB of
tuples/cons and 16.6 MB of refs all scale with. Cost: each fused op needs
forward-dual, reset and push cases (FS0025 exhaustive matching helps), and
consumer adoption in Analytics' curve code — a cross-repo change. *Push-site
fusion*: compute the contribution directly into the child's adjoint buffer
instead of materialising it — e.g. the sparse `Transpose(cons) * dA` temp at
`:3813` folded into the accumulate (a sparse variant of the dense-only
`Backend.Mul_M_V_Add_V'`, `Backend.Lite.fs:219`, which today also copies). This
is the structured-contribution worklist flagged at the end of
`ad-tape-allocation.md` — the only way below the current ~8 B/slot push floor.
Fingerprint: fusion is reorder-free **only if** the fused kernel performs the
identical scalar operations in the identical per-element order — no FMA, no
reassociation, no `dgemv` `beta`-accumulation reordering. That is achievable
(element-wise chains and CSR row loops have a natural order) but must be
verified per kernel with Analytics' `-- fingerprint`. **Days per fused op
family; the structured worklist is weeks.**

**d. `DV.R`'s eager zero vector → lazy.** The known open item (step 3 of
`ad-tape-allocation.md`): `:576` allocates a full-length zero adjoint at node
construction, 32.0 MB in the trace, and reset re-zeroes or replaces it anyway.
Initialise the ref with the empty `DV.Zero` sentinel (`:646`) instead; reset's
existing fallback arm (`:3455`) already materialises full-length on the shape
mismatch, so the 1af3889 invariant — "buffers are full-length by push time,
because reset runs before every push" (`:4040`) — holds without new code at the
push site. `Add_V_V_Inplace`'s empty-destination error path (`Backend.Lite.fs:34`)
stays unreachable; add a test asserting exactly that invariant, plus the
nested-AD (`DVF` adjoint) case. Do **not** go further and have push install the
first contribution directly without a zero fill: contributions like `bxv dA a`
alias the parent's own adjoint buffer, and installing one uncopied aliases two
nodes' adjoints. Reorder-free. **Hours, plus the usual tri-target test and
local-feed verification day.**

**Done 2026-08-07 (in-tree, with invariant tests; both `DV.R` and `DM.R`, via
shared empty sentinels).** Measured: in-repo forward B/pass halves (229,040 →
118,464 at depth 8) and the materialisation moves to the tape's *first* reverse
pass — the multi-seed total is byte-identical, so a fully-visited tape nets
zero, exactly as the ad-tape plan's step-4 section predicted. Downstream the
effect on MarketBuild is **nil today** (54.7 MB whole-fit with and without,
A/B'd on the same Analytics commit): the z-spread-on-floats work had already
removed the forward-only tapes this would have saved. Two corrections to the
paragraph above: the `DV.ZeroN` trace line does not vanish — `ZeroN` inlines
into its callers, and for visited nodes the allocation reappears under
`resetRec`'s fallback arm — and the payoff is best understood as hardening:
any consumer path that evaluates reverse-typed values without differentiating
stops paying for adjoints it never uses, which is precisely the population the
Analytics bond work was eliminating by hand.

**e. Not worth pursuing now.**
- *Full lazy expression-graph rewrite* — months, destroys diffability against
  upstream DiffSharp, and every consumer inherits the fingerprint risk at once.
- *Span/stackalloc/ArrayPool/SIMD widening* — the arrays here escape into the
  tape, so stack lifetimes don't fit; `ArrayPool` isn't available under
  Fable→JS/Python (the tri-target guard pattern, `CsrMat.fs:45-50`, covers small
  divergences, not a pooling substrate — that substrate is option (a), written
  portably).
- *Struct `D` / merging `DVR`'s two ref cells into one state object* — the refs
  are ~1% of the trace; a struct DU can't be recursive, and the merge rewrites
  every `DVR(...)` pattern in a 4,200-line forked file for a marginal win.
  **The merge half of this was wrong and is now done** — 4.2% of the fit's
  objects, in a morning. The struct-`D` half still stands. See "Done: the ref
  merge" for both the correction and why a byte reading produced it.

## The worklist: what an object-count profile says (2026-08-08)

Measured in the consumer, where the op mix is real:
`WldMr.Analytics/plans/marketbuild-cost.md`'s count profile, differenced between a
20-fit and a 2-fit trace so no fixed startup cost is smeared in. A MarketBuild fit
creates **1.06 M objects, ~65% of them AD**, and decomposes as:

| | per fit | per node |
| --- | ---: | ---: |
| reverse nodes (`DVR` 18,637 + `DR` 15,229) | **33,866** | 1 |
| node construction — node + fan-out ref + adjoint ref + TraceOp | ~135,000 | 4 |
| reset worklist cons cells | 56,831 | 1.68 |
| push worklist cons cells | 56,842 | 1.68 |
| push contribution tuples | 56,738 | 1.68 |
| **traversal bookkeeping total** | **170,411** | **5.03** |

This is the **1.3.20 baseline**, i.e. the diagnosis, not the current state: the
traversal-bookkeeping rows are now zero and node construction is 3 per node, not 4.

The fan-out refs (33,892) and adjoint refs (18,603 + 15,282) match the node count
to within 0.1%, which is the check that this decomposition is reading the right
objects. The 1.68 is reverse passes per node per fit — lower than
`ad-tape-allocation.md`'s `jacobianTv''` discussion implies, because most tapes are
driven once or twice, not once per Jacobian row.

**So a reverse node costs ~9 objects a fit, and 5 of the 9 are worklist
bookkeeping** — no arithmetic, no values, nothing a caller can observe.
170,411 objects is **16.1% of everything a MarketBuild allocates**, ~1.1 CPU-ms a
fit at the consumer's measured ~6.2 ns an allocation.

> [!WARNING]
> **A byte profile cannot see this, and `BenchmarkAdTape`'s default shape hides it
> further.** Per node-visit the reverse pass allocates 3 bookkeeping objects (32 B
> each) and 1 derivative payload array — and only the payload's size scales with
> node width. So bookkeeping is a fixed **75% of the reverse pass's objects at any
> width**, while its byte share collapses as nodes get wider:
>
> | pillars (harness `width`) | slots/node | bookkeeping share of reverse bytes |
> | ---: | ---: | ---: |
> | 5 | 29 | ~22% |
> | 40 (harness default) | 232 | ~5% |
> | 320 | 1,859 | ~0.7% |
>
> MarketBuild's mean `Double[]` is 159 B — about 20 doubles — so the real consumer
> sits at the *narrow* end, near pillars=5, and the harness default is ~10× wider
> than the workload it stands for. **Run `width` and read the low rows**, or add a
> narrow default. This is why the byte-led analysis above put fusion next.

### Done: struct-tuple push worklist (2026-08-08)

`pushRec`'s worklist was `(dobj * dobj) list` — a cons cell *plus* a reference
tuple per contribution. Making the pair a struct tuple puts it inside the cons
cell. Six lines: the three `bx`/`bxv`/`bxm` helpers, `pushRec`'s signature, its
`| struct (v, d) :: t ->` pattern, and the `pushRec [struct (v, d)]` entry.

Measured in-repo (`BenchmarkAdTape phases`/`width`): push **−1,584 B/pass at every
node width**, confirming it is per-contribution and not per-element. Measured
downstream through the local feed, on the same Analytics commit:

| | before | after |
| --- | ---: | ---: |
| objects a fit | 1,059,787 | **1,004,545** (−55,242, −5.2%) |
| allocated a fit | 49.8 MB | **48.5 MB** (−1.3 MB, −2.6%) |
| gen0 a fit | 6.38 | 6.21 |
| wall (2,000 fits) | 18.9 / 18.9 / 18.9 ms | 18.8 / 18.6 / 18.8 ms |

`Tuple<dobj,dobj>` leaves the type census entirely and the cons cell grows 32 → 40 B,
which is where the predicted 1.37 MB net (measured 1.3) comes from. **Reorder-free
by construction** — the change is the worklist slot's representation, nothing
touches traversal order or arithmetic — and Analytics' `-- fingerprint` returns
`15a6fee3…f52d`, unchanged. All three targets green (18 .NET Expecto, 16 LinAlg,
37 Checks, 18 Fable/JS, 18 Fable/Python).

### Done: array-backed worklists (2026-08-08) — and the trap that nearly sank it

Both traversals now use an index-managed array stack instead of a cons list.
Measured downstream on the same Analytics commit, against the released 1.3.20:

| | before | after |
| --- | ---: | ---: |
| objects a fit | 1,059,787 | **897,595** (−162,192, −15.3%) |
| allocated a fit | 49.8 MB | **45.1 MB** (−9.4%) |
| wall (2,000 fits, ×3) | 18.9 / 18.9 / 18.9 ms | **18.1 / 18.1 / 18.1 ms** |
| GC pause a fit | 3.10 ms | 2.82 ms |
| gen0 a fit | 6.38 | 5.79 |

In-repo (`BenchmarkAdTape width`, at pillars=5): reset 2,136 → 592 B/pass, push
23,072 → 19,928. Both `FSharpList` rows and `Tuple<dobj,dobj>` leave the object
census entirely. Fingerprint `15a6fee3…f52d` unchanged; tri-target green
(18/16/37 .NET, 18 JS, 18 Python) and downstream Analytics 446 .NET + 222 JS green.

**The trap, and it cost a full measure-and-back-out cycle.** The obvious first cut
uses `ResizeArray<dobj>` — and `dobj` is `interface end` (`AD.Lite.fs:2973`). An
interface-typed array makes **every store a `stelem.ref` with a real assignability
check**. Measured on a CPU profile of the regressed build:
`CastHelpers.StelemRef_Helper` 0.738 + `StelemRef` 0.277 = **1.0 CPU-ms a fit that
did not exist before**, plus the write-barrier and cast slow paths under it —
`libcoreclr` +2.5 ms/fit and `System.Private.CoreLib` +1.4 ms/fit. Allocation fell
9% and **the fit still regressed 18.9 → 21.6 ms/fit**, consistently across three
runs. Storing struct slots (`[<Struct>] type ResetSlot = { RD: dobj }`,
`PushSlot = { PV: dobj; PD: dobj }`) into struct arrays skips the check entirely and
turned the same change into −4%.

Three design points follow from that, all recorded at the declaration site:

- **Struct arrays, not `ResizeArray<dobj>`** — for the reason above. The initial
  capacity is 16; 64 measured identically on the clock and cost 1.2 MB a fit more.
- **A class with methods, not `inline` closures over mutable locals.** The push is
  expanded at ~230 sites in `reverseProp`; one small method lets the JIT inline the
  hot path at its own discretion and keeps the growth branch out of line, instead of
  stamping a copy of it into every case arm.
- **One generic `SlotStack<'T>`** serves both traversals — `ResetSlot` for reset and
  `struct (dobj * dobj)`, the pair the call sites already build, for push. .NET
  specialises generic instantiations over value types, so the struct-array store is
  preserved; re-measured after the merge at 43.1 MB and 17.7 ms, unchanged.

> [!WARNING]
> **The struct-slot reasoning is .NET's alone.** Under Fable a `[<Struct>]` record is
> an ordinary object and a plain array store is a plain array store, so the slot
> allocates about as the cons cell did, and `Array.blit`/`Array.zeroCreate` become
> interpreted element loops rather than a memmove. Neither Fable target runs this
> workload, so this is parity-not-regression territory and was left alone — but a
> `#if FABLE_COMPILER` `ResizeArray<dobj>` would allocate *zero* per push there
> (native `push`/`pop`), beating both the cons list and the struct slots. Worth doing
> only if a Fable consumer ever drives a tape hard.

**The general lesson is worth more than the change**: on a metric of *object count*
this looked like a pure win, and it was — the count fell 15%. The cost showed up
only on the clock, in a helper neither the byte profile nor the count profile can
see. Allocation-reduction work needs a wall-clock check as well as an allocation
one, and the CPU profile is what identifies the cause when they disagree.

#### How it was done, kept for the next mechanical edit of this kind

The remaining 112,000 objects a fit were the cons cells themselves, in both
`resetRec` (`dobj list`) and `pushRec`. Each became a reusable array-backed stack.

**Order must be preserved exactly**, or adjoint accumulation reorders and results
move. The code was already a stack discipline — `pushRec (x :: y :: t)`
processes `x`, then `y`, then `t` — so a LIFO array stack reproduces it by pushing
in reverse: `push y; push x; loop ()`. The transformation is uniform:

| now | becomes |
| --- | --- |
| `\| (v, d) :: t ->` | pop, bind |
| `resetRec (X :: t)` / `pushRec (X :: t)` | `push X; loop ()` |
| `resetRec (X :: Y :: t)` / `pushRec (X :: Y :: t)` | `push Y; push X; loop ()` |
| `resetRec t` / `pushRec t` | `loop ()` |

**Size of the edit, as counted beforehand and as it turned out**: `resetRec` had
184 one-child and 51 two-child sites, `pushRec` 235 sites ending `:: t)`, plus
**8 irregular sites** that build a list and
`List.append` it (`Make_DV_ofDs`, `Make_DM_ofDs`, `Make_DM_ofMatD`,
`Make_DMRows_ofDVs`, in both functions) — those append in order, so they push in
reverse. ~470 mechanical sites in a 4,290-line forked file: scriptable, but it must
be reviewed site by site, and `FS0025`-as-error plus the tri-target suites plus
Analytics' `-- fingerprint` are what make it safe rather than the diff being small.

The shape, on `resetRec` — note the `| _ -> resetRec t` arms become `| _ -> ()`,
so most sites get *shorter*, and the `while` replaces a tail recursion the original
comment says is deliberate (Fable→JS guarantees no TCO, so a loop is strictly
safer):

```fsharp
let reverseReset (d: dobj) =
    let stack = ResizeArray<dobj>(16)
    let mutable n = 0                      // logical top; the buffer only grows
    let inline push (x: dobj) =
        if n < stack.Count then stack.[n] <- x else stack.Add x
        n <- n + 1
    push d
    while n > 0 do
        n <- n - 1
        let d = stack.[n]
        match d with
        | :? D as d ->
            match d with
            | DR(_, dARef, o, dFanOutRef, _) ->
                dARef.Value <- D.Zero
                dFanOutRef.Value <- dFanOutRef.Value + 1u
                if dFanOutRef.Value = 1u then
                    match o with
                    | Add_D_D(a, b) -> push (bxd b); push (bxd a)   // NB reversed
                    | Add_D_DCons(a) -> push (bxd a)
                    | _ -> ()
            | _ -> ()
        | _ -> ()
```

`push`/`pop` by index rather than `RemoveAt` keeps it O(1) on every target and lets
the buffer be reused for the whole traversal.

### Done: two allocations that bought nothing at all (2026-08-08)

Found by attributing the `D` and `DV` wrapper counts by allocating frame — the
first time either had been looked at on the count axis. Both are pure waste rather
than a design cost, and together they are **92,258 objects and 2.0 MB a fit**.

**1. `Add_V_V_Inplace` allocated a wrapper around the array it had just mutated —
42,090 `DV` a fit, 29.6% of all of them.** The 1.3.15 in-place accumulate stopped
allocating the *array* per adjoint contribution, but it routes through the generic
`Op_DV_DV_DV`, whose plain/plain arm is `DV(ff(ap, bp))`. `ff` daxpys into `bp` and
returns `bp` — so the dispatcher wraps `b`'s own array in a fresh `DV` and hands
back something indistinguishable from the `b` already in hand. `Add_V_V_Inplace`
now pattern-matches the plain/plain case and returns `b` itself, falling through to
the dispatcher for every nested-AD case. `Backend.Add_V_V_Inplace` mutates `y` in
both its non-error branches (no-op when `x` is empty, daxpy when lengths match,
`Backend.Lite.fs:34-40`), so the two are equivalent by inspection, not just by
measurement.

**2. `D.Zero`, `D.One`, `DV.Zero` and `DM.Zero` were properties, so every access
allocated — ~50,000 a fit.** `reverseReset` assigns `D.Zero` once per scalar node
per reverse pass; that alone was 20,239 `D` a fit, 16.8% of all `D` allocation, to
produce a value that is immutable and always identical. They are now
`static member val`, the same pattern 1.3.18 already used for `ZeroSentinel` two
lines below `DV.Zero` — which is why this one stings a little.

Downstream, on the same Analytics commit:

| | after worklists | after these |
| --- | ---: | ---: |
| objects a fit | 897,595 | **805,337** (−92,258, −10.3%) |
| allocated a fit | 45.1 MB | **43.1 MB** |
| wall (2,000 fits, ×3) | 18.1 / 18.1 / 18.1 ms | **17.8 / 17.7 / 17.6 ms** |
| GC pause a fit | 2.82 ms | 2.59 ms |

`DV` 142,181 → 92,435 and `D` 120,486 → 82,324. Fingerprint unchanged, tri-target
green, Analytics 446 + 222 green.

A grep for the same two shapes elsewhere in `AD.Lite.fs` comes back empty: no
allocating `static member X = …` properties remain.

**`DM.Add_M_M_Inplace` got the same short-circuit on review**, after an initial
decision to skip it on the grounds that `AlphaAdd_M_M_Inplace'` only updates
`ColMajor` in place. That reasoning was wrong: the generic path calls the *same*
`ff`, so the backend's non-`ColMajor` `failwith` fires identically whether or not
the dispatcher wraps the result, and the short-circuit is exactly as valid as the
`DV` one. It is unmeasured — `DM` does not appear in a MarketBuild census at all —
and is there so the two siblings do not silently diverge.

### Three traps found while costing the worklists (2026-08-08)

**1. `reverseProp` is re-entrant, so the buffers must be per-call.** The
`FixedPoint_D` push case (`AD.Lite.fs:3818-3840`, Christianson 1994) calls
`reverseProp` *from inside* `pushRec` — twice, in a loop — and then continues with
`pushRec ((bx bfirst.A b) :: t)`. With a cons list the outer `t` survives that for
free because it is immutable. With a shared or pooled mutable stack it does not:
the nested call would push and pop on the same buffer and any imperfect unwind
silently corrupts the outer traversal. **Allocate both stacks per `reverseProp` /
`reverseReset` call.** That is 2 objects per call against 3 per node-visit removed,
so it cannot regress on count — `CurveSolver.ff'` calls `reverseProp` once per fit
instrument per solver iteration, i.e. hundreds to low thousands of calls a fit
against 56,895 node-visits. It *can* regress on bytes if calls are numerous and
short, which is what the initial capacity is for; measure with the harness's
`phases`/`seeds` rather than guessing. (`D.FixedPoint` has no caller in Analytics,
so this path is cold for MarketBuild — but it has to keep working, and it is the
reason pooling across calls is not a free upgrade later.)

**2. Pushing in reverse flips the order the contributions are *evaluated* in.**
`pushRec ((bx (dA * b.P) a) :: (bx (dA * a.P) b) :: t)` evaluates `a`'s
contribution first, then `b`'s, then recurses. Rewritten as
`push (bx (dA * a.P) b); push (bx (dA * b.P) a)`, the two multiplications happen in
the opposite order. For plain values that is unobservable — same inputs, same
results, only the allocation order differs. **Under nested AD it is not**: when
`dA` is itself a reverse node, each multiplication *constructs nodes on the outer
tape*, so flipping them flips the outer tape's construction order and therefore the
order its own traversal later sums contributions in. Bind both in the original
order first, then push in reverse:

```fsharp
| Mul_D_D(a, b) ->
    let ca = bx (dA * b.P) a
    let cb = bx (dA * a.P) b
    push cb; push ca
```

Costs nothing and removes the class of bug entirely. This would show up only in a
nested forward-over-reverse fingerprint and nowhere else — `tests/ExpectoTests`
already has such cases (`nested forward-over-reverse works over lazy adjoints`,
`… through a gather`), which is where it would be caught, if at all.

**3. It subsumes the struct-tuple change already landed.** With two parallel
`ResizeArray<dobj>` for the push stack, both the cons cell and the pair go, so the
08-08 six-liner is a strict subset of this. That was still worth landing: it is
shippable now, it is six lines, and it banked 5.2% while this one is a day's work.

Tri-target: `ResizeArray` (`System.Collections.Generic.List`) is fine under Fable.
Prefer two parallel `ResizeArray<dobj>` over `ResizeArray<struct(dobj*dobj)>` —
value-tuple *element types* in a generic collection are the part worth not betting
on across JS and Python.

Predicted beforehand: ~112,000 objects and ~3.6 MB a fit. Actual: **162,192
objects and 4.7 MB** against the released package — the estimate omitted that the
struct-tuple change's cons cells (40 B) also go, and that `reverseReset` is called
once per `reverseProp` rather than once per tape. Both scripts are in the session
scratch; the transformation is uniform enough to re-derive in an hour if the
ref-merge (step 5) wants the same treatment.

## What a compact index-tape design would and would not buy here

The textbook low-allocation reverse-mode design is a contiguous arena-backed tape
of trivially-copyable records — `struct { uint32 lhs, rhs, out; uint16 opcode }`,
structure-of-arrays, adjoints in one flat gradient buffer, a reverse sweep that is
a `for` loop over a `switch` with no dispatch and no allocation. Measured against
what this fork actually does, that checklist splits three ways.

**Already done, and the measurements say so.**

- *Accumulate adjoints in place, never a new gradient object per reverse op.*
  1.3.15, −6.0 MB a fit downstream.
- *Coarse primitives rather than tracing every scalar element.* The fork is
  `DV`/`DM`-level: 55% of a fit's 33,866 nodes are vector nodes. **Tested
  2026-08-08 and the answer is negative** — the 15,229 scalar `DR` nodes come from
  `SwapBricks.Pricer.price_oises_vector`/`price_df_vector` collapsing an already
  vectorised leg to a scalar NPV, plus `Money.op_Multiply` aggregating by currency.
  That is a reduction tail of a few nodes per leg evaluation, not a loop over
  elements, so there is no scalar-tracing pathology to fix.
- *Minimal residuals — save only what the pullback needs.* Already the case where
  it matters: `Exp_D`'s pullback reads `d.P`, the output, not the input.
- *Activity analysis.* Present at op granularity as the `..._DCons` variants
  (`Mul_D_DCons` 4,338 a fit, `Mul_Had_DV_DVCons` 4,489). The whole-subgraph
  version has no automated form here, but the largest win of the whole campaign was
  exactly that analysis applied by hand in the consumer: Analytics' float z-spread
  solve noticed every input was an AD constant and dropped the subgraph to floats
  (bondCurves 10.7 → 8.2 MB, ~23.6 → ~17.8 ms).
- *Fusion.* Landed once, as `DV.Gather`/`DV.Scatter` replacing two CSR selection
  matrices per interpolation — the largest single step in the series.

**Structurally unavailable, and it is worth being explicit about why.**

- *The index tape itself.* A full rewrite of a 4,290-line file that must compile to
  .NET, Fable→JS **and** Fable→Python, and it is the one item here that **cannot**
  keep byte-identical results — a flat sweep accumulates each node's contributions
  in tape order, the current worklist in graph-traversal order, and float addition
  is not associative. Parked under (e) as months. Two notes on the constraints,
  corrected 2026-08-08:
  - *Diffability against upstream is not a constraint* and never will be again —
    0.8.3 was rewritten upstream shortly after the fork, so there is no merge to
    take. `CLAUDE.md` said otherwise until 08-08. This makes the large mechanical
    edits (worklists, ref merge) cheaper, since restructuring is allowed; it does
    not change the size or the risk of the rewrite itself.
  - *What byte-identity would actually cost* is priced under "Spending the
    fingerprint" below. Short version: less than it looks, but not nothing, and the
    bill is downstream rather than numerical.
- *Expression templates* are a C++ mechanism with no F# equivalent — `inline` plus
  SRTP will not build a fused kernel across statements.
- *Source-transformation AD* (Enzyme-style, at IR level) has no F#/Fable route.
- *Checkpointing / rematerialisation* is the wrong trade here. It buys memory with
  FLOPs; tapes are ~34k nodes and **memory is not the binding constraint — the
  allocation rate is**. Recomputing intermediates would add work to fix a problem
  we do not have.
- *Arena / `ArrayPool`* is (a) above: unavailable under Fable→JS/Python as a
  substrate, and gated on the primal-escape audit rather than on the pooling.

**Live, and the checklist sharpens two of them.**

- *"Bookkeeping cost can exceed floating-point cost for scalar-heavy code."* This
  is precisely the measured result: 5 of the 9 objects a node costs are worklist
  bookkeeping, and 75% of the reverse pass's objects are bookkeeping at any node
  width. The array-backed worklist gets the "no allocation per reverse step" half
  of the textbook sweep without the contiguity half.
- *"Pre-size or reuse tapes; allocate only when the high-water mark increases."*
  Directly applicable to the worklist buffers — with the constraint already noted,
  that `reverseProp` runs under `Array.Parallel.map` in Analytics' `fitOisCurves`,
  so a retained buffer must be per-call or thread-local, not module-level.

### What was left after the first four changes (2026-08-08) — a superseded snapshot

> [!NOTE]
> This table is the state at 805,337 objects a fit, i.e. **before** the ref merge
> and the first-visit fix. It is kept because it is what the next two decisions
> were made from, and because three of its five rows turned out to be wrong in
> instructive ways. The current state is 771,313; the corrected figures are in the
> "lever" column and in the two costing sections that follow.

Of that 805,337, the AD cluster was ~420,000 (52%, down from ~65%):

| item | objects/fit | share of fit | the lever |
| --- | ---: | ---: | --- |
| `D` + `DV` wrappers | 174,759 | 21.7% | ~~fusion — one per op result~~ **not fusion's to take — only ~1 wrapper in 5 belongs to a tape node; see "fusion's real ceiling"** |
| `Double[]` (AD's ~75% of 110,101) | ~83,000 | 10.3% | ~~pooling~~ **withdrawn — see "The adjoint-pooling costing"** |
| ref cells | 66,604 | 8.3% | ~~merge the two per node~~ **done, −34,024** |
| TraceOps | ~25,000 | 3.1% | **re-measured: 33,029, 4.3%** — fusion |
| `DVR` + `DR` nodes | 33,320 | 4.1% | fusion, or the index tape |

**The `Double[]` split is new information and it promotes one option.** By
allocating frame:

| frame | arrays/fit | |
| --- | ---: | --- |
| `reverseReset` | 25,426 | **the lazy adjoint materialising on first reset.** A counter later put the true figure at **20,096** — this row was 27% high, and a second trace read 28,465, 42% high the other way |
| `DV.op_DotMultiply` | 14,309 | per-op result |
| `ArrayModule.ZeroCreate` | 13,824 | per-op result (leaf is generic) |
| `ff@712-19` | 10,404 | per-op result |
| `CstFwdCurve.MinusTotalRates` | 8,631 | *Analytics, not AD* |
| `ResolvedFixings.dailySeries` | 7,288 | *Analytics, not AD* |
| `Scatter` + `Gather` + `GatherNoCheck` | 10,884 | the gather primitive's own |
| `DayCount.YearFraction` | 2,667 | *Analytics, not AD* |

**`reverseReset` is the single largest producer of `Double[]` in the fit at 23%** —
one adjoint buffer per node per tape, allocated at first reset since 1.3.18 made
seeding lazy. Reset already reuses it on every *subsequent* pass (`Array.Clear` on
a length match); what is unpooled is the first one per node.

> [!WARNING]
> **The paragraph below is the reasoning that promoted adjoint pooling, and it is
> wrong.** It is kept because the way it fails is the lesson: it argues from
> *ownership* when the thing that decides pooling is *lifetime*. "The
> adjoint-pooling costing" has the measurement that overturned it.

That matters because it splits option (a) in two, and the halves are not equally
hard. **Adjoint buffers have a clean owner and an existing escape discipline** —
they belong to the node, and `DOps.adjoint` already hands out a copy (the 1.3.13
precedent). Pooling them by length across a tape needs no primal-escape
audit at all, which was the thing that made (a) "weeks". The per-op *result* arrays
are the hard half: those escape through `DV.toFloats` and everywhere else, and
still need the audit. Worth ~25,000 objects a fit (3.1%) on its own.

Also worth recording: **~25% of `Double[]` is Analytics, not AD** —
`CstFwdCurve.MinusTotalRates` 8,631, `ResolvedFixings.dailySeries` 7,288,
`DayCount.YearFraction` 2,667 a fit. Unexamined, and in-repo for that consumer.

### The TraceOp census, re-measured (2026-08-09) — and fusion's real ceiling

Step 8 said to re-measure before discussing fusion rather than reuse the chain
named in step 2, since the gather adoption replaced the CSR-matvec layer that
referred to. Done, on the current build. **24 of the 240 defined `TraceOp` cases
are used at all**, totalling 33,029 a fit — 4.3%, one per reverse node, as it
should be.

| op | count/fit | of ops |
| --- | ---: | ---: |
| `Gather_DV` | 5,872 | 17.8% |
| `Mul_Had_DV_DVCons` | 4,425 | 13.4% |
| `Mul_D_DCons` | 4,332 | 13.1% |
| `Add_DV_DV` | 2,907 | 8.8% |
| `Exp_DV` | 2,793 | 8.5% |
| `Add_D_DCons` | 1,653 | 5.0% |
| `Neg_D` | 1,530 | 4.6% |
| *18 more* | 9,517 | 28.8% |

Top five are 61.5%. The scalar ops together (`Mul_D_DCons`, `Add_D_DCons`, `Neg_D`,
`Div_D_D`, `Sub_D_DCons`, `Div_D_DCons`, `Add_D_D`, `Exp_D`, `Sub_D_D`,
`Pow_D_DCons`) are 12,189, 37% of all ops. **`Gather_DV` is now the single largest
op**, which it was not when step 2 was written — it did not exist.

**The ceiling is lower than this plan has been claiming.** The "What is left" table
credits fusion with the `D` + `DV` wrappers, 174,616 a fit, 21.7%. That is wrong as
an upper bound on what fusion removes. There are 174,616 wrappers against 33,181
reverse nodes — **5.3 wrappers per node** — because most wrappers come from primal
arithmetic that never becomes a tape node at all. Fusion removes objects only for
the nodes it *eliminates*, and eliminating one node saves the node (48 B), its
`NodeState` (32 B), its `TraceOp` (32–40 B), its adjoint buffer, and one result
wrapper — about 5 objects. So:

> **fusion's prize = (nodes eliminated) x ~5 objects**, and nodes eliminated is
> bounded by the *smaller* member of each fused chain.

The obvious chain is the discount-factor shape, `Exp_DV(Mul_Had_DV_DVCons(...))`,
bounded by `Exp_DV` at 2,793 — so ~14,000 objects, **1.8% of the fit**, for one
fused kernel plus its fingerprint argument. Fusing every scalar chain in the census
is optimistically another 3–4%. That is the honest shape of step 8: a grind of 1–2%
steps, each needing a per-kernel numerical argument, not one 20% move.

> [!WARNING]
> **Per-frame counts in these traces are unreliable to ±50%, even though per-type
> totals are good to ~3%.** `reverseReset` allocates exactly one `float[]` and one
> `DV` per materialised adjoint — the counter says both are 20,096 a fit — yet the
> trace attributes 28,465 `Double[]` (+42%) and 10,378 `DV` (−48%) to that same
> frame. Type totals aggregate every sample for a type; a frame split divides the
> same samples among call sites and runs out of them. **Rank with frames, size with
> types, and count with a counter.** Every per-frame figure in this note and in
> `marketbuild-cost.md` carries this error bar.

### The adjoint-pooling costing (2026-08-08) — measured, and it kills step 7 as written

`reverseReset` was instrumented with counters (temporary, reverted) and run over
20 MarketBuild fits. Per fit:

| | per fit | |
| --- | ---: | --- |
| `reverseReset` calls | 1,197 | ~17 `DV` nodes per pass — the tapes are small |
| `DV` node visits | 40,532 | mean fan-out 2.02 |
| adjoint allocations | 20,096 | **every one from the empty sentinel** |
| adjoint clears | 20,436 | **every one redundant** — see below |
| wrong-length / `DVF` adjoints | **0** | the case the reuse guard exists for never fires |
| `DM` adjoint allocations | **0** | there are no `DM` nodes in MarketBuild at all |

**Three things follow, and the second one is the important one.**

**1. The prize is smaller than the trace said.** 20,096 a fit, 2.6%, not the 25,426
(3.1%) carried in the table below — and the by-frame attribution said 28,465, 42%
high. `GCSampledObjectAllocationHigh` is a sampler; it ranks well and counts badly.
Anything about to be costed to the nearest percent wants a counter, not a trace.

**2. Pooling has no scope to exist in.** Each node allocates exactly one adjoint,
on its first reset, and holds it for the life of the tape. Reset allocates *all* of
them before `reverseProp` starts, so within a tape every adjoint is live at once and
there is nothing to hand back. Reuse would have to be **across** tapes — 1,197 of
them a fit — which needs a tape-lifetime signal that does not exist today and would
have to cross the package boundary into Analytics to get one. That is option (a)'s
hard half wearing a smaller hat, not the "narrow, no-audit-needed" half this plan
promised. The claim that adjoint buffers are the easy half was right about
*ownership* and wrong about *lifetime*, and only the lifetime matters for pooling.

Three ways forward, none of them cheap:
- **(i) an explicit tape scope + release protocol** — the honest version of step 7,
  and it is days-to-weeks, not the hours the plan implied.
- **(ii) defer allocation to `reverseProp` and free on fire.** A node's adjoint is
  dead once it has fired and pushed to its children, so a liveness-ordered pool
  *would* have intra-tape reuse. But it moves allocation into the hot traversal and
  needs the reset/prop split reworked.
- **(iii) reduce node count instead**, which is fusion — step 8 — and it takes the
  buffers with it for free, because the buffer is one of the ~5 objects a node costs.

**(iii) is the recommendation.** Pooling 20,096 arrays is 2.6%; fusing the ops that
create the nodes removes the array, the node, the state and the TraceOp together.

**3. A third of the adjoint buffers are `float[1]`.** 40 distinct lengths, and
length 1 is 33.4% of the allocations, lengths ≤4 about half. A `double[1]` is 32
bytes of which 24 is header. Worth a separate look at *why* MarketBuild builds so
many length-1 `DV`s — that is an Analytics modelling question, not an AD one, and
it is not on this list yet.

### Done: zero the adjoint on the first visit only (2026-08-08)

The measurement above found 20,436 redundant clears a fit against 20,096 real
ones — **half the zeroing work in `reverseReset` did nothing**. A node is visited
once per incoming edge, and every visit ran the type test, the length check and the
`Array.Clear`, over a buffer the first visit had already zeroed.

`st.F = 1u` already identifies the first visit, so the fix deletes a guard rather
than adding one: the zeroing moves inside the branch that pushes the children.

```fsharp
st.F <- st.F + 1u
if st.F = 1u then
    match st.A with ...        // was above the increment, running every visit
    match o with ...
```

Order-preserving (nothing runs between the two statements, and `st.A`/`st.F` are
independent), so the fingerprint holds. Applied to all three arms.

Wall **17.82 -> 17.53 ms** over six `profile 2000` runs each side (A: 17.7 17.5 17.9
18.0 17.6 18.2; B: 17.7 17.5 17.5 17.6 17.5 17.4) — 1.6%, and worth stating
carefully: it is about two standard errors, so it is real but only just, and B's
spread is visibly tighter than A's. Allocation is unchanged by construction.

> [!NOTE]
> The instrumentation was worth more than the fix. Three of the five facts in the
> table above — no `DM` nodes, no `DVF` adjoints, every allocation from the sentinel
> — are things no trace would have shown, and the second of them is what overturned
> the plan. When a step is about to be costed, a counter behind `#if !FABLE_COMPILER`
> and a `ProcessExit` dump is an hour and answers the question exactly.

### Done: the ref merge (2026-08-08) — and why it costed exactly right

Predicted −33,300 objects a fit and 4.1%; measured **−34,024 and −4.2%**
(805,337 -> 771,313), against a prediction made from a count profile rather than a
byte one. That is the point worth keeping: the count profile's arithmetic is
*additive and checkable* — 66,604 refs against 33,320 nodes is exactly two per
node, so exactly half of them go, and nothing about the estimate needed a
measurement to confirm. Byte profiles do not offer that, which is how these
same refs sat under "the refs are ~1% of the trace" for two rounds.

```fsharp
type NodeState<'T>(a: 'T) =
    member val A = a with get, set     // accumulated adjoint
    member val F = 0u with get, set    // fan-out counter

| DR of primal: D * state: NodeState<D> * parentOperation: TraceOp * tag: uint32
```

One generic class over the three node types, not three concrete ones: `'T` is
always a reference type, so .NET shares a single canonical instantiation and there
is no code duplication to pay for. It compiles on all three targets unchanged —
`member val` was already in Fable-compiled code (`Util.fs`'s `GlobalTagger`).

**The byte arithmetic, and the half of it that is not the refs.** Measured per
node: `FSharpRef<D>` 24 B + `FSharpRef<uint32>` 24 B -> `NodeState<D>` 32 B, and
`DR` itself 56 B -> 48 B because the case lost a field. So 104 -> 80 B, a 24 B
saving, and 34,024 x 24 = 0.78 MB — exactly the measured 43.1 -> 42.3. Worth
separating because **8 of those 24 bytes are the DU shrinking, not the merge**:
any future field removed from `DR`/`DVR`/`DMR` is worth ~8 B a node on its own,
and scoring this change on the ref pair alone would have undersold it by a third.

`member val` was checked against the alternative rather than assumed: the emitted
IL is 7–8 bytes per accessor, non-virtual, so RyuJIT inlines all of them to a bare
`ldfld`/`stfld` and a `val mutable` field would produce identical machine code —
while costing the verbose explicit-constructor form, a public mutable field on a
package-public type, and a re-verification on two Fable targets. The Fable output
was checked too: JS emits a plain class with `this["A@"]`, and Python emits
`self._A` as a **plain attribute**, not a `@property`, so even the slowest target
has no descriptor protocol in the way.

Results: fingerprint `15a6fee3…f52d` **unchanged**, 43.1 -> 42.3 MB/fit, wall
17.7 ms (unmoved, 17.5–17.9 across three runs), GC pause 2.59 -> 2.59 ms. Green on
18 Expecto + 16 LinAlg + 37 Checks (.NET), 18 Fable/JS, 18 Fable/Python, and
downstream 446 + 222.

> [!NOTE]
> **The edit was 92 mechanical sites and 6 real ones**, and separating them is what
> made it a morning rather than a day. Every read-only pattern has `_` in *both*
> the adjoint and the fan-out slot, so one regex requiring `_` in each — dropping
> the fourth field — could not touch a site that actually binds either ref. That
> left the six `reverseReset`/`reverseProp` blocks, which were the only places to
> read carefully. It compiled first try; `WarningsAsErrors=FS0025` in
> `src/Directory.Build.props` is what makes an arity change in a DU safe to do this
> way at all, since a missed pattern is a build error and not a runtime surprise.

> [!NOTE]
> The in-repo `BenchmarkAdTape -- phases` signal was 2042.1 -> 2018.1 B/node on the
> forward pass and *exactly* unchanged on reset and push — which is what a
> construction-side-only change should look like, and a useful shape to check
> before spending the downstream loop.

**Left on the table deliberately.** The three `reverseProp` blocks re-read `st.F`
after the adjoint accumulation, and the JIT cannot forward the stored value across
`D.op_Addition` / `DV.Add_V_V_Inplace` / `DM.Add_M_M_Inplace` — it has no way to
prove those don't touch this node's state. Hoisting to `let f = st.F - 1u` is
order-preserving and saves one L1 load a node a pass: ~40 µs against a 17.7 ms fit
whose run-to-run spread is 400 µs, so **0.25%, an order of magnitude under the
noise**. It touches a fingerprint-gated block, so it is not worth a release loop of
its own — fold it in when these blocks are next opened for something real. The
matching double-reads in `reverseReset` are genuinely free (nothing sits between
the store and the load, so store-to-load forwarding removes them) and want no
change at all.

> [!WARNING]
> The object trace's own MB/fit read 41.0 against `-- profile`'s 42.3, a 3% gap
> where the script's header asks for ~1%. `GCSampledObjectAllocationHigh` samples,
> so a few percent is expected drift and it does not move any ranking here — but if
> that gap widens, re-check it before trusting a *count* from the same trace.

### Correction (now closed): the ref cells were a count item, not a byte item

Option (e) dismissed "merging `DVR`'s two ref cells into one state object" because
"the refs are ~1% of the trace". That was a **byte** judgement and the count profile
overturns it — 24 bytes each is exactly why bytes hid them.

Measured after the worklist and Zero work, so these are the numbers to beat:
fan-out refs 33,284 a fit, adjoint refs 18,356 + 14,964 = 33,320, i.e. **66,604
objects a fit, 8.3% of everything a MarketBuild allocates**, against 33,320 nodes —
exactly two per node. Merging the pair into one small mutable node-state object
removes ~33,300 a fit, **4.1%**, and takes a node from 4 construction objects to 3.

**Size of the edit**: 49 `DVR(`, 30 `DR(` and 39 `DMR(` occurrences — construction
sites and match patterns together, ~118 in all, against the ~470 the worklists took.
Only 3 `ref 0u` fan-out initialisers, all in the three `R` constructors, so the
construction side is nearly free; the bulk is the patterns. Unlike the worklists this
is a node-*representation* change rather than a traversal one, so it cannot perturb
traversal or evaluation order at all — but it still wants its own fingerprint gate,
because the `.A`/fan-out accessors are touched. It is the one piece of the
compact-tape idea that can be taken incrementally.

### Spending the fingerprint

Analytics' `-- fingerprint` (SHA-256 of the encoded `MarketDefResults`) has gated
every change in this campaign. It is worth being precise about what it is and what
abandoning it would cost, because "we could accept small numerical differences" is
true and is not the whole picture.

**It is not a business requirement. It is a very cheap proof technique.** One
command, no tolerance to argue, no per-field judgement — it either matches or the
change is not what you thought. That is why nineteen changes could be shipped
quickly. Give it up as a general rule and every future change needs a bespoke
numerical argument instead, which costs far more per change than the hash does.

**Nothing on the current path needs to spend it.** Both worklist changes and the
ref merge are order-preserving, so they keep byte-identity for free, and they take
a node from 9 objects to 3. The question only becomes live at the index tape.

If it is spent there, three things are true and worth separating:

1. **Determinism survives; equality with the past does not.** A tape swept in
   reverse construction order accumulates each node's contributions in a fixed,
   reproducible sequence — just a different one from today's fan-out-gated
   traversal. So WldMr.Web's content-addressed dedup of `MarketDefResults` keeps
   working exactly as before; an identical re-fit still does not mint a new blob.
   What happens is a **one-time re-blob of every stored `MarketDefResults`**.
   Sizing that is the first thing to do, and it is a WldMr.Web question
   (`plans/datastore-rewrite/current/08-live-watcher-6c.md`), not a Numerics one.
2. **The right size to claim is ~1e-8, not ~1e-16.** The tempting argument — "float
   reassociation, differences in the last ulp" — is wrong for a *solved* quantity.
   `CurveSolver.solve` is called with `tol = 1e-8` on |price difference|
   (`CurveFit.fs:37`, `:50`, `:74`, `:101`, `:126`, `:331`). Perturbing a gradient
   in its last bits changes the iteration path and where the stopping criterion
   fires, so fitted values move by up to the solver tolerance, not by an ulp. 1e-8
   in price terms is far below anything economically meaningful, so the claim is
   easy to defend — but it has to be stated at 1e-8. **A deviation materially
   larger than the solver tolerance is a bug, not reassociation**, and that makes
   it a usable acceptance test rather than a hand-wave.
3. **Build the replacement oracle before the tape, not after.** Most of it exists:
   `WldMr.Analytics.Positions/Diff/MarketDefDiff.fs` already walks a `MarketDef`
   key by key and pulls out each curve's pillars and values (and is Fable-portable,
   no Thoth — see `docs/object-diff.md`). What it lacks is a numeric mode: it
   renders floats through `DiffFormat.floatStr` at `%.10f` and compares the
   strings, which is an exact comparison wearing a tolerance's clothing. Adding a
   max/rms-deviation report over the same walk, plus a `-- compare <baseline>` mode
   on `BenchmarkMarketBuild` that fits and diffs against a stored reference, gives
   a gate with the same one-command ergonomics as the hash. Also report the
   **solver iteration counts**: if those move much, the tape has an accuracy
   problem rather than an ordering one.

**Recommendation.** Keep byte-identity as the default gate — not out of
conservatism, but because it costs nothing to keep on the current path and buys a
free correctness oracle. Spend it once, deliberately, on the index tape if that is
ever taken, and only after (1) is sized and (3) is built.

### The ceiling, quantified

After both worklist changes a node still cost **4 objects: the node, two refs and
the TraceOp** — ~135,000 a fit, 12.8% of a MarketBuild's allocation. The ref merge
took that to **3 and ~101,000**, which is now the measured state, not a projection.
Below it the object graph *is* the tape, and only replacing it with an index tape
removes the rest. So the incremental path has a floor of roughly 10% of the fit's
objects — 13.1% of the smaller 771,313 total it is now measured against — and that
number is what a rewrite would have to be worth to justify itself.

## Sequencing

1. ~~**(d) first.**~~ **Done 2026-08-07** — see the outcome note under (d):
   in-repo forward halves, downstream currently nil (Analytics' bond work got
   there first), `-- fingerprint` unchanged, all 445 + 222 downstream tests
   green against the local feed.
2. ~~**(c), forward-fusion flavour.**~~ **Demoted 2026-08-08.** It was next
   because the byte profile showed wrappers and tuples/cons scaling with node
   count — true, and the count profile confirms node count is the lever. But
   fusion attacks the *4 construction objects per node* and needs a fused op
   family, consumer adoption in Analytics' curve code, and a per-kernel
   fingerprint argument each time; the worklist attacks the *5 traversal objects
   per node*, is bigger, is confined to two functions, and cannot move a result.
   Do the cheaper, larger, safer one first.
3. ~~**Struct-tuple push worklist.**~~ **Done 2026-08-08** — 55,242 objects and
   1.3 MB a fit off MarketBuild, six lines, fingerprint unchanged. See
   "The worklist" above.
4. ~~**Array-backed worklists in `resetRec` and `pushRec`.**~~ **Done 2026-08-08**
   — 162,192 objects and 4.7 MB a fit, 18.9 -> 18.1 ms, fingerprint unchanged. Read
   the `stelem.ref` trap before attempting anything similar.
5. ~~**Two allocations that bought nothing** — the `Add_V_V_Inplace` wrapper and
   the uncached `Zero`/`One` properties.~~ **Done 2026-08-08**, 92,258 objects and
   2.0 MB a fit.
6. ~~**Merge `DVR`/`DR`'s two ref cells into one node-state object.**~~ **Done
   2026-08-08** — 34,024 objects a fit (**−4.2%**, 805,337 -> 771,313) and
   0.8 MB, fingerprint unchanged, wall unmoved. Re-promoted from (e), which had
   dismissed it on a byte reading. See "Done: the ref merge".
7. ~~**Pool adjoint buffers by length across a tape.**~~ **Withdrawn 2026-08-08,
   measured.** The prize is 20,096 arrays a fit (2.6%, not 3.1%), and there is no
   scope to pool them in: every adjoint in a tape is live at once, so reuse has to
   cross tapes and needs a lifetime signal that does not exist. See "The
   adjoint-pooling costing". What came out of the measurement instead was the
   redundant-clear fix, which is done.

   Kept for whoever revisits it: the mechanics *would* be cheap now — a pool handle
   is one `member val` on `NodeState` and zero pattern sites, and `NodeState<'T>`
   has 4 bytes of tail padding so any field of 4 bytes or fewer is free (widening
   `F` past `uint32`, or a second reference, costs 8). The pool itself must be
   per-traversal-call for the same re-entrancy reason `SlotStack` is — `FixedPoint_D`
   re-enters while an outer traversal is live. And `A: 'T` is the wrapper (`DV`),
   not the storage (`float[]`), so a length-keyed pool wants the `DM` path's length
   without unwrapping `GenMat`/`ColMajor`. It is the *lifetime*, not the mechanics,
   that stops this.

7b. ~~**Zero the adjoint on the first visit only.**~~ **Done 2026-08-08** — half the
   zeroing work in `reverseReset` was re-clearing already-zero buffers; 17.82 ->
   17.53 ms, fingerprint unchanged.
   The narrow half of (a): adjoint buffers are node-owned and `adjoint` already
   copies on the way out, so this needs none of the primal-escape audit that makes
   the full arena expensive. See "What is left, measured".
8. **(c) forward fusion — now the recommended next move**, promoted by step 7's
   withdrawal: it is the only lever that takes the adjoint buffer, the node, the
   state object and the TraceOp together, and after the four done items it is what
   the remaining 13% of the fit's objects is made of. For whatever the count profile then shows
   leading — re-measure the TraceOp census first rather than reusing the chain
   named in step 2, since the gather adoption already replaced the CSR-matvec
   layer it referred to. Gate each fused op on a byte-identical fingerprint.
9. **(a)'s hard half only if still needed** — the per-op *result* arrays behind an
   explicit tape scope, preceded by the primal-escape audit. This is what is left of
   (a) once step 7 has taken the adjoint buffers.
10. The structured-contribution worklist (deep flavour of (c)) stays parked until
   the cheap levers are exhausted; re-evaluate against whatever the trace then
   shows.

Steps 4 and 6 together take a node from 9 objects to 4; see "The ceiling,
quantified" for why nothing incremental goes below that.

Reorder-free and fingerprint-safe by construction: (a), (b), (d), and both
worklist changes. Requires per-kernel fingerprint proof: (c), both flavours.

## Effort

| option | class |
| --- | --- |
| struct-tuple push worklist | **done** — six lines + the verification day |
| array-backed worklists | **done** — a day, as costed, plus one measure-and-fix cycle |
| uncached `Zero` / redundant `Add_V_V_Inplace` wrapper | **done** — an hour, once the counts were attributed by frame |
| e. merge the two ref cells per node | **done** — a morning: 92 sites by regex, 6 by hand |
| zero the adjoint on the first visit only | **done** — minutes, once a counter showed half the clears were redundant |
| a. pool adjoint buffers | **withdrawn** — no lifetime to pool in; the mechanics were the easy part |
| d. lazy `DV.R` adjoint | hours (+ a day of tri-target and downstream verification) |
| c. forward fusion, per op family | days |
| a. push-temp pool | days |
| a. full arena + tape scope + primal audit | weeks |
| c. structured contribution worklist | weeks |
| b. general refcount-guided in-place | not recommended |
