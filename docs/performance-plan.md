# WldMr.Numerics — Performance Improvement Plan

Findings from a read-through of the numerical-differentiation and reverse-mode
AD hot paths. Organized into two tracks (Numerical diff, Reverse AD), each
ranked by return-on-investment. File/line references point at the current code.

Relevant files:
- `src/WldMr.Numerics.DiffSharp/Numerical.Float64.fs` — numerical differentiation
- `src/WldMr.Numerics.DiffSharp/Backend.Lite.fs` — vector/matrix backend ops
- `src/WldMr.Numerics.LinAlg/LiteBlas.fs` — BLAS kernels
- `src/WldMr.Numerics.DiffSharp/AD.Lite.fs` — forward/reverse AD engine

---

## Track A — Numerical differentiation

### A1. Eliminate `O(n²)`/`O(n³)` allocation from `standardBasisVal` + `Add_V_V` (highest ROI)

Affects `grad'`, `jacobianT'`, `gradhessian'`.

In `grad'` (`Numerical.Float64.fs:54`):
```fsharp
Array.init x.Length (fun i -> f (Lite.Backend.Add_V_V(x, standardBasisVal x.Length i GlobalConfig.Float64Epsilon)))
```
Each coordinate `i` allocates a fresh length-`n` zero basis vector
(`standardBasisVal`, `Util.fs:52`) **and** a second length-`n` array via
`Add_V_V` (`Backend.Lite.fs:21`). A dimension-`n` gradient therefore does `2n`
allocations / `O(n²)` memory traffic and `n` full vector adds, just to perturb
one coordinate at a time. For `jacobianT'`/`gradhessian'` (`:116`, `:66`) it is
`O(n³)`.

**Fix:** reuse a single perturbation buffer — copy `x` once, then per
coordinate `xp.[i] <- xi + eps; eval; xp.[i] <- xi`. Converts the perturbation
overhead from `O(n²)` to `O(n)` for `grad`.
```fsharp
let xp = Array.copyFast x
Array.init x.Length (fun i ->
    let xi = xp.[i]
    xp.[i] <- xi + eps
    let r = f xp
    xp.[i] <- xi
    r)
```
**Caveat:** only safe if `f` does not retain the array reference between calls.
If that cannot be guaranteed, the conservative variant (`let xp = Array.copyFast x`
inside the lambda, then bump one element) still removes the basis-vector
allocation and the full `Add_V_V`.

### A2. Fuse `grad'` post-processing (removes 3 array allocations)

`Numerical.Float64.fs:51-55` allocates a baseline array `g = Array.create n fx`,
a `Sub_V_V(gg, g)` result, and a `Mul_S_V` result — three length-`n` arrays for
one fused map. Collapse into the perturbation loop:
```fsharp
let g = Array.init x.Length (fun i -> ((* perturbed eval *) - fx) * epsRec)
(fx, g)
```

### A3. Stop materializing the `n×n` baseline in Hessian/Jacobian

`gradhessian'` (`:64-68`) and `jacobianT'` (`:114-118`) build
`array2D (Array.create x.Length g)` — an `n×n` matrix whose every row is the
baseline gradient `g` — then `Sub_M_M` (another `n×n`) then `Mul_S_M` (a third).
Subtract `g` and scale while filling the result, removing **three `n×n`
allocations and two full matrix passes**.

### A4. Vectorize the scalar BLAS kernels (SIMD)

`daxpy` is already SIMD (`LiteBlas.fs:80`); these on the hot paths are still
scalar loops:
- `Backend.Mul_S_V` (`Backend.Lite.fs:53`) — on the gradient scaling path; the
  SIMD/`BlockCopy` version is commented out at `:61-63`.
- `ddot` (`LiteBlas.fs:320`), `dnrm2` (`:342`), `dasum` (`:330`) — marked `// IMPROVE`.
- `dscal` (`:136`) — in the LU hot path (`dgetf2`).

Wrap in `System.Numerics.Vector<double>` like `daxpy`; typically 2–4× on these
kernels.

### A5. Optional parallelism

`grad'`/`jacobianT'`/`gradhessian'` evaluate `f` at independent perturbed points
but use serial `Array.init`. `Array.Parallel.init` is already wired up
(`Util.fs:153`). Near-linear speedup when `f` is expensive and thread-safe.
**Trade-off:** conflicts with the single-buffer reuse in A1 (needs one buffer
per thread); favor A1 for cheap-`f`/many-dims, parallel for expensive `f`.

### A6. (Correctness, not perf) `dgemm` off-by-one

The two transposed-`A` branches loop `for l = 0 to k do` (`LiteBlas.fs:397`,
`:423`) — `k+1` iterations over a length-`k` contraction, an out-of-bounds read.
Not on the numerical-diff path, but fix if `dgemm` with transposed A is ever used.

---

## Track B — Reverse-mode AD (`pushRec` hot in profiler)

`pushRec` (`AD.Lite.fs:3648`) is the backward sweep — inherently `O(#ops)` — but
the implementation adds 2–3 heap allocations per graph edge plus per-node type
tests that are pure overhead.

### B1. In-place adjoint accumulation (highest ROI, esp. `DV`/`DM`)

Every arm begins with (`:3657` for `D`, `:3746` for `DV`, `DM` likewise):
```fsharp
dARef.Value <- dARef.Value + v
let dA = dARef.Value
```
For `DV`/`DM` this allocates a **brand-new dense vector/matrix per incoming
contribution**. A node with fan-in `k` does `k` full-length allocations + `k`
passes, when the operation is conceptually `adjoint += contribution`.

**Fix:** in-place axpy into the (already zeroed) adjoint buffer:
```fsharp
DV.AddInplace(dARef.Value, v)   // daxpy; no allocation
```
Backed by existing `Blas.daxpy` / `Add_V_V_Inplace`. Go further and **fuse the
pullback into the accumulation**: `Add_DV_DV(a,b)` is just `a.A += dA; b.A += dA`
(no temporary); `Mul_Had_DV_DV` is a fused multiply-add into `a.A` instead of
allocating the product *and* the sum.

**Caveat:** breaks second-order *reverse-over-reverse* nesting (the adjoint `DV`
would need to stay a live tape node). Safe for first-order reverse AD; gate on
the non-nested level if higher-order is supported.

### B2. Replace the heterogeneous worklist with typed struct stacks

The worklist is `(dobj*dobj) list` (`:3648`). Each edge allocates a reference
`System.Tuple<dobj,dobj>` (via `bx`/`bxd`, `:3643`/`:2919`) **and** a cons cell —
~2 heap objects per edge, all short-lived → the GC churn the profiler attributes
to `pushRec`. Same pattern in `reverseReset`/`resetRec` (`:3360`), which runs a
**full extra pass** before every push.

Additionally, because items are stored as the marker interface
`dobj` (`:2917`), every pop recovers the concrete type with runtime `isinst`
tests (`:3653`, `:3742`, `:3864`) — ~2 tests for a scalar `D` node, up to 4 for
`DM`. (Note: the `match d, v with` tuple match itself does **not** allocate; F#
compiles it to direct nested tests.)

**Fix:** three *typed* stacks instead of one boxed list:
```fsharp
let dStack  = System.Collections.Generic.Stack<struct(D  * D )>()
let dvStack = System.Collections.Generic.Stack<struct(DV * DV)>()
let dmStack = System.Collections.Generic.Stack<struct(DM * DM)>()
```
Each child's kind is **statically known** from the `TraceOp` case
(`Sum_DV(a)` knows `a : DV`, etc.), so children are pushed onto the correctly
typed stack with no runtime test, and each pop is already concrete. Drain all
three until empty — ordering across stacks is irrelevant because the fan-out
counter (not worklist order) guarantees a node descends only after all parents
have contributed.

This single change removes, together:
- the per-edge tuple + cons allocations,
- all `isinst` type tests per pop, and
- enables JIT devirtualization of the `D`/`DV`/`DM` operator calls (static type
  now concrete, not `dobj`).

Apply to both `pushRec` and `resetRec`. Cost: more code (each arm lifts into its
own typed loop), but mostly mechanical since the arms are already written out.

### B3. Mutable-class reverse nodes instead of union + `ref` cells

`DR(primal, adjoint: D ref, op, fanOut: uint ref, tag)` (`:52`; `DVR`/`DMR`
likewise) allocate two separate `FSharpRef` heap objects per node at
forward-trace time, and every `pushRec`/`resetRec` visit chases
`dARef.Value` / `dFanOutRef.Value` through an extra pointer hop.

**Fix:** model the reverse node as a small mutable class with `mutable Adjoint` /
`mutable FanOut` fields — removes two allocations per node (big on the forward
build) and one indirection per access on the reverse side. Pairs naturally with
B1.

### B4. Cache local partials at forward time (avoid transcendental recompute)

Pullbacks recompute functions already evaluated in the forward pass:
```fsharp
| Sin_D(a) -> ... (dA * cos a.P) ...      // :3681  recomputes cos
| Pow_D_D(a, b) -> ... (a.P ** (b.P - D.One)) ...   // :3672  recomputes pow
| Log_D(a) -> ... (dA / a.P) ...
```
Some ops already cache (`Exp_D` reuses `d.P`, `:3680`; `Sqrt_D` reuses `d.P`,
`:3686`), but `sin/cos/tan/pow/log` recompute. Store the per-edge local partial
in the trace node at forward time and just multiply by `dA` in reverse. Trades
forward memory for removing transcendental calls from the backward pass — worth
it for trig/exp/pow-heavy functions where the reverse pass is the hotspot.

### B5. Specific allocation hotspots in the `DV`/`DM` arms

- `dA.GetCols() |> Seq.iter (fun v -> b.A <- b.A + v)` (`:3910`, `:3969`):
  allocates an enumerator **and a fresh `b.A` vector per column** — `O(cols)`
  full-vector allocations for one matrix→vector reduction. Replace with a single
  in-place column sum (`DV` accumulation, or `dgemv` against a ones-vector).
- `Make_DV_ofDs(a)` (`:3822`): `Array.mapi … |> List.ofArray |> List.append`
  builds an intermediate array, a list, and appends per occurrence. Becomes a
  simple push loop with no list churn under the typed-stack worklist (B2).

### B6. Structural option — flat tape (Wengert list)

`reverseProp` runs **two full graph traversals per gradient** — `reverseReset`
then `pushRec` (`:3996-3997`). A flat tape design (record ops into a contiguous
array on the forward pass; reverse pass = single linear scan) eliminates the
reset pass (adjoints become a contiguous array cleared with one `memset`),
eliminates the worklist and its type tests, makes in-place accumulation (B1) the
default, and gives sequential memory access instead of pointer chasing. Largest
possible speedup, but a genuine rewrite — only if B1–B5 are insufficient.

---

## Suggested sequencing

Reverse AD (driven by the `pushRec` profile):
1. **B1** in-place adjoint accumulation (gated to non-nested level) — biggest for
   `DV`/`DM`.
2. **B2** typed struct-stack worklist — removes per-edge allocation *and* type
   tests together; apply to `pushRec` and `resetRec`.
3. **B3** mutable-class nodes.
4. **B4** cache local partials (if trig/exp/pow heavy).
5. **B5** targeted hotspots.
6. **B6** tape rewrite only if needed.

Numerical diff:
1. **A1 + A2 + A3** (remove per-coordinate basis-vector and `n×n` allocations) —
   pure wins, order-`n` reduction in overhead.
2. **A4** SIMD kernels.
3. **A5** optional parallel path.
4. **A6** `dgemm` correctness fix.

Each item is independent; land them as separate commits and re-profile after
each so the before/after is apples-to-apples. Whether B1 or B2 lands first
depends on whether the workloads are `DV`/`DM`-heavy (B1) or scalar-`D`-heavy
(B2).
