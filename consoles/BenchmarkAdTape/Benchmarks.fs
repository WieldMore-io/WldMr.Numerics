module BenchmarkAdTape.Benchmarks

open BenchmarkDotNet.Attributes

open WldMr.Numerics.DiffSharp.AD.Float64


/// What a reverse-mode AD pass over a curve-solve-shaped tape allocates.
///
/// The claim under test comes from profiling `MarketBuild` in WldMr.Analytics
/// (`plans/marketbuild-cost.md`): across 20 fits, `DOps.resetRec` accounts for
/// 119 MB and `DOps.pushRec` 52 MB — 23% of the `fitOisCurves` stage, and pure
/// reverse-mode tape bookkeeping rather than arithmetic. The reading to be
/// tested is that a reset should zero adjoints in place, and this one allocates
/// a fresh zero vector per node instead (`AD.Lite.fs:3428`,
/// `dARef.Value <- DV.ZeroN dPrimal.Length`).
///
/// **Allocated bytes are the metric here, not time.** The `Mean` column is
/// incidental; read `Allocated`.
///
/// The phase split is by subtraction, and the rows are ordered to make it
/// readable directly:
///
/// - (a) forward             = `Forward`
/// - (b) reset               = `ForwardAndReset` - `Forward`
/// - (c) push                = `ForwardResetAndPush` - `ForwardAndReset`
/// - (b)+(c) cross-check     = `ReversePassOnPrebuiltTape`, which should equal
///   `ForwardResetAndPush` - `Forward`. If it does not, the subtraction is
///   measuring something other than the phases.
///
/// There is deliberately no bare-reset benchmark on a prebuilt tape. It would be
/// wrong: `resetRec` increments each node's fan-out counter and only recurses
/// when it reaches 1 (`AD.Lite.fs:3430`), so a second reset with no intervening
/// push walks only the root and reports near-zero. A tape can be reset repeatedly
/// only through `reverseProp`, whose push returns every counter to zero.
///
/// `[<MemoryDiagnoser>]` and nothing else. Deliberately no `SimpleJob` /
/// `RuntimeMoniker`: two benchmarks in WldMr.Analytics pinned `Net50`/`Net60`
/// and can no longer run at all.
[<MemoryDiagnoser>]
type AdTapeBenchmark() =

  let mutable fixture = Unchecked.defaultof<Graph.Fixture>
  let mutable prebuiltRoot = Unchecked.defaultof<D>
  let mutable prebuiltInput = Unchecked.defaultof<DV>

  /// Layers stacked into the tape — the graph-size knob. Node count is
  /// `7 * Depth + 2`, so allocation must be linear in this if the cost is per
  /// node. That linearity is the evidence; a flat column would kill the reading.
  [<Params(2, 8, 32)>]
  member val Depth = 8 with get, set

  [<GlobalSetup>]
  member this.Setup() =
    fixture <- Graph.build (Graph.spec this.Depth)
    let root, x = Graph.forward fixture
    // A full pass here, so that JIT, the DiffSharp statics and the tag counter
    // are not charged to iteration 1 — and so that a fixture whose push never
    // reaches the input fails the run rather than publishing a fast row.
    reverseProp (D.One :> dobj) (root :> dobj)
    let g: DV = x |> adjoint
    if DV.toFloats g |> Array.forall (fun v -> v = 0.0) then
      failwith "gradient is identically zero — the push never reached the input"
    prebuiltRoot <- root
    prebuiltInput <- x

  /// (a) alone: the forward evaluation that builds the tape, no reverse pass.
  ///
  /// Not free of adjoint allocation — `DV.R` gives every node a fresh
  /// `DV.ZeroN` at construction (`AD.Lite.fs:576`), so the zero vector a reset
  /// later rewrites has already been allocated once, here.
  [<Benchmark(Baseline = true)>]
  member _.Forward() = Graph.forward fixture

  /// (a) + (b): the tape build and one bare `reverseReset`. `DOps.reverseReset`
  /// is public, so this is the one phase boundary the API exposes directly.
  [<Benchmark>]
  member _.ForwardAndReset() =
    let root, x = Graph.forward fixture
    reverseReset (root :> dobj)
    struct (root, x)

  /// (a) + (b) + (c): the tape build and one full `reverseProp`. This is what
  /// `DiffOps.grad` costs.
  [<Benchmark>]
  member _.ForwardResetAndPush() =
    let root, x = Graph.forward fixture
    reverseProp (D.One :> dobj) (root :> dobj)
    x |> adjoint: DV

  /// (b) + (c) on a tape built once in `GlobalSetup`. Repeatable because a
  /// complete push leaves every fan-out counter back at zero, which is exactly
  /// the state a fresh tape is in.
  [<Benchmark>]
  member _.ReversePassOnPrebuiltTape() =
    reverseProp (D.One :> dobj) (prebuiltRoot :> dobj)
    prebuiltInput |> adjoint: DV

  /// The curve-solve shape: one forward pass, then `Graph.SeedPasses` reverse
  /// passes over the same tape. `jacobianTv''` (`AD.Lite.fs:4069`) exists so a
  /// whole Jacobian can be had from a single forward pass — but every row goes
  /// through `reverseProp`, which resets the entire tape first. This row is
  /// where a per-node reset cost gets multiplied.
  [<Benchmark>]
  member _.ForwardThenSeedPasses() =
    let root, x = Graph.forward fixture
    for _ in 1 .. Graph.SeedPasses do
      reverseProp (D.One :> dobj) (root :> dobj)
    x |> adjoint: DV
