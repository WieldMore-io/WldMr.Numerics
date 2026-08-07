module BenchmarkAdTape.Graph

open WldMr.Numerics.LinAlg
open WldMr.Numerics.LinAlg.CsrMat
open WldMr.Numerics.DiffSharp.Util
open WldMr.Numerics.DiffSharp.AD.Float64


/// Observation points generated per curve pillar. Eight keeps the sparse
/// interpolation matrix realistically thin (two non-zeros a row) while giving
/// the layer enough width that the `DV`-sized allocations dominate the
/// per-node object headers.
[<Literal>]
let PointsPerPillar = 8

/// Reverse passes the `SeedPasses` benchmark makes over one tape. A Newton step
/// on a curve solve takes one transposed-Jacobian-vector product per residual
/// row, and `jacobianTv''` (`AD.Lite.fs:4069`) serves them all from a single
/// forward pass — but each one goes through `reverseProp`, which resets the
/// whole tape first. This is the multiplier that turns a per-node reset cost
/// into the 119 MB the Analytics profile charges to `DOps.resetRec`.
[<Literal>]
let SeedPasses = 8

/// Shape of the synthetic curve-solve graph.
type GraphSpec =
  {
    /// Number of unknowns — curve pillars. The gradient has this length.
    Pillars: int
    /// Number of observation points the pillars are interpolated onto, per layer.
    Points: int
    /// Number of stacked interpolate / discount / aggregate layers.
    Depth: int
  }

/// A graph of `pillars` unknowns and `depth` layers.
///
/// The two knobs move different things, and separating them is what tells the
/// per-node story apart from the per-slot one. `depth` changes the number of
/// reverse nodes at fixed node width; `pillars` changes the width of every node
/// at a fixed node count. If a reset allocates one zero vector a node, bytes
/// scale with the product — i.e. linearly in each.
let specOf (pillars: int) (depth: int) =
  {
    Pillars= pillars
    Points= pillars * PointsPerPillar
    Depth= depth
  }

/// The standard shape at a given depth: 40 pillars, 320 observation points —
/// an OIS curve solve's order of magnitude.
let spec (depth: int) = specOf 40 depth

/// Constant data the tape is built against. None of it is a reverse node, so
/// building it is not charged to any measured phase.
type Fixture =
  {
    Spec: GraphSpec
    /// Sparse interpolation matrix, `Points` x `Pillars`, two non-zeros a row.
    Interp: DM
    /// Sparse aggregation matrix, `Pillars` x `Points`, one non-zero a column.
    Aggregate: DM
    /// Year fractions at the observation points.
    Times: DV
    /// Cashflow weights at the observation points.
    Weights: DV
    /// The unknowns the tape is differentiated with respect to.
    X: DV
  }

/// Pack rows of `(column, value)` pairs into a CSR matrix.
let private csr (nCols: int) (rows: (int * float)[][]) : CsrMat =
  {
    Values= rows |> Array.collect (Array.map snd)
    Columns= rows |> Array.collect (Array.map fst)
    RowIndices= rows |> Array.map Array.length |> Array.scan (+) 0
    NCols= nCols
  }

/// Wrap a CSR matrix as the `SparseDouble` pair `GenMat` wants: the matrix and
/// its transpose, both in CSR, so that `GenMat.transpose` — which the push side
/// of `Mul_DMCons_DV` calls on every visit — is a swap rather than a copy.
let private sparse (m: CsrMat) =
  SparseDouble(m, m.ToCscMat().Transpose()) |> DM

/// Build the constant data for one graph shape.
let build (spec: GraphSpec) : Fixture =
  let nP = spec.Pillars
  let nO = spec.Points

  // Linear interpolation of the pillar curve onto the observation points. This
  // is the shape that puts both the forward pass and the push into
  // `CsrMat.mulV`, which the Analytics profile charges 47.6 MB.
  let interp =
    Array.init nO (fun i ->
      let u = float i * float (nP - 1) / float (nO - 1)
      let j = min (nP - 2) (int u)
      let w = u - float j
      [| (j, 1.0 - w); (j + 1, w) |])
    |> csr nP

  // Aggregation back onto the pillars: each observation point feeds exactly one
  // pillar. The 0.01 scale makes a layer a contraction, so `Depth` can be raised
  // without the primal values running away and turning `exp` into an overflow.
  let aggregate =
    Array.init nP (fun j ->
      [| for i in 0 .. nO - 1 do
           if i * nP / nO = j then
             yield (i, 0.01 / float PointsPerPillar) |])
    |> csr nO

  {
    Spec= spec
    Interp= sparse interp
    Aggregate= sparse aggregate
    Times= DV(Array.init nO (fun i -> 0.25 + 30.0 * float i / float (nO - 1)))
    Weights= DV(Array.init nO (fun i -> 0.5 + 0.25 * float (i % 4)))
    X= DV(Array.init nP (fun i -> 0.01 + 0.04 * float i / float (nP - 1)))
  }

/// One layer: interpolate the pillars onto the observation points, discount,
/// weight, aggregate back onto the pillars, add to the incoming curve.
///
/// Seven reverse nodes per layer — five of length `Points` (`Mul_DMCons_DV`,
/// `Mul_Had_DV_DVCons`, `Neg_DV`, `Exp_DV`, `Mul_Had_DV_DVCons`) and two of
/// length `Pillars` (`Mul_DMCons_DV`, `Add_DV_DV`). Those are the same ops the
/// `fitOisCurves` profile is dominated by, which is the point of this shape.
let private layer (fx: Fixture) (x: DV) : DV =
  let y = fx.Interp * x
  let df = exp (-(y .* fx.Times))
  let cf = fx.Weights .* df
  let s = fx.Aggregate * cf
  s + x

/// Run the forward pass, building the reverse tape. Returns the scalar root and
/// the input node, which is where the gradient lands after a push.
///
/// **This is phase (a), and it is not free of adjoint allocation.** `DV.R`
/// (`AD.Lite.fs:576`) gives every node a fresh `DV.ZeroN` at construction, so
/// the zero vector a reset later rewrites has already been allocated once here.
let forward (fx: Fixture) : D * DV =
  let x = fx.X |> makeReverse GlobalTagger.Next
  let mutable acc = x
  for _ in 1 .. fx.Spec.Depth do
    acc <- layer fx acc
  DV.Sum acc, x

/// Number of reverse nodes on the tape: the input, seven a layer, and the
/// scalar root.
let nodeCount (spec: GraphSpec) = 1 + spec.Depth * 7 + 1

/// `float` slots a single `reverseReset` writes as zero, summed over the tape.
///
/// `resetRec` does `dARef.Value <- DV.ZeroN dPrimal.Length` per `DVR`
/// (`AD.Lite.fs:3428`), so if the per-node-zero-vector reading is right, one
/// reset allocates at least `8 *` this many bytes plus an object header a node.
/// Measured reset bytes divided by that floor is the number that confirms or
/// kills the hypothesis — `Phases.phases` prints the ratio.
let adjointSlots (spec: GraphSpec) =
  spec.Pillars + spec.Depth * (5 * spec.Points + 2 * spec.Pillars)
