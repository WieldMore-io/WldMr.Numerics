# Reducing per-op AD allocation: direction note

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

## Sequencing

1. ~~**(d) first.**~~ **Done 2026-08-07** — see the outcome note under (d):
   in-repo forward halves, downstream currently nil (Analytics' bond work got
   there first), `-- fingerprint` unchanged, all 445 + 222 downstream tests
   green against the local feed.
2. **(c), forward-fusion flavour, for the top two chains** in the MarketBuild
   profile (the CSR-matvec → `.*` → `exp` layer). Justified if, after (d), the
   trace still shows wrappers + tuples/cons (~220 MB combined) scaling with node
   count — they will. Gate each fused op on a byte-identical fingerprint before
   adoption.
3. **(a) only if still needed after fusion**, starting with the narrow push-temp
   pool (which subsumes the useful part of (b)), then the full arena behind an
   explicit tape scope — preceded by the primal-escape audit, run exactly like
   step 1's adjoint audit.
4. The structured-contribution worklist (deep flavour of (c)) stays parked until
   the cheap levers are exhausted; re-evaluate against whatever the trace then
   shows.

Reorder-free and fingerprint-safe by construction: (a), (b), (d). Requires
per-kernel fingerprint proof: (c), both flavours.

## Effort

| option | class |
| --- | --- |
| d. lazy `DV.R` adjoint | hours (+ a day of tri-target and downstream verification) |
| c. forward fusion, per op family | days |
| a. push-temp pool | days |
| a. full arena + tape scope + primal audit | weeks |
| c. structured contribution worklist | weeks |
| b. general refcount-guided in-place | not recommended |
