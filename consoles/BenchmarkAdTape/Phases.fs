module BenchmarkAdTape.Phases

open System
open System.Diagnostics

open WldMr.Numerics.DiffSharp.AD.Float64


/// Process-wide allocated bytes, with `precise = true` so the per-thread
/// allocation buffers are flushed first. This console is single threaded and
/// nothing else allocates in it, so process-wide is exact here.
let private alloc () = GC.GetTotalAllocatedBytes true

/// Push a unit adjoint through the tape rooted at `root`. Resets first — that
/// is what `reverseProp` does, and there is no public entry point that does not.
let private push (root: D) = reverseProp (D.One :> dobj) (root :> dobj)

/// Zero the adjoints on the tape rooted at `root`, without pushing.
let private reset (root: D) = reverseReset (root :> dobj)

/// Fail loudly rather than publish fast numbers for a tape that never computed
/// anything. A graph that silently produced `nan`, or a push that never reached
/// the input, would otherwise look like a very cheap reverse pass.
let private guard (root: D) (g: DV) =
  let v = D.toFloat root
  if Double.IsNaN v || Double.IsInfinity v then
    failwithf "graph root is not finite (%f) — the fixture is broken" v
  let ga = DV.toFloats g
  if ga.Length = 0 || ga |> Array.forall (fun x -> x = 0.0) then
    failwith "gradient is empty or identically zero — the push never reached the input"

/// Everything about the graph and the tape it produces, so a run that measured
/// the wrong thing cannot pass unnoticed. Printed as the header of the default
/// BenchmarkDotNet run for that reason.
let census (spec: Graph.GraphSpec) =
  let fx = Graph.build spec
  let root, x = Graph.forward fx
  push root
  let g: DV = x |> adjoint
  guard root g
  let ga = DV.toFloats g
  let slots = Graph.adjointSlots spec
  printfn "graph   pillars=%d points=%d depth=%d" spec.Pillars spec.Points spec.Depth
  printfn "  reverse nodes         %d  (1 input + 7 per layer + 1 root)" (Graph.nodeCount spec)
  printfn "  adjoint float slots   %d  (%.1f KB of zero vectors per reset, at 8 B a slot)"
    slots (float slots * 8.0 / 1024.0)
  printfn "  interp matrix         %d x %d sparse, %d non-zeros"
    spec.Points spec.Pillars (2 * spec.Points)
  printfn "  aggregate matrix      %d x %d sparse, %d non-zeros"
    spec.Pillars spec.Points spec.Points
  printfn "  root value            %.9f" (D.toFloat root)
  printfn "  gradient              length=%d  min=%.6g  max=%.6g"
    ga.Length (Array.min ga) (Array.max ga)

/// One tape, three measurements: the forward pass, one full `reverseProp`, then
/// one bare `reverseReset`.
///
/// **The order is not arbitrary.** A tape is pushable only while every fan-out
/// counter is zero, which holds when it is fresh and again after a complete push
/// (`pushRec` decrements each counter back to 0). A bare `reverseReset` leaves
/// the counters at each node's in-degree instead, and a *second* reset then
/// finds them non-zero, declines to recurse (`AD.Lite.fs:3430`) and touches only
/// the root — so a reset measured before the push would both mis-measure itself
/// and silently disarm the push that followed. Hence: forward, push, reset,
/// throw the tape away.
let private measureOnce (fx: Graph.Fixture) =
  let a0 = alloc ()
  let root, x = Graph.forward fx
  let a1 = alloc ()
  push root
  let a2 = alloc ()
  // Checked here, between the push and the reset: the gradient is live only in
  // this window, since the bare reset below is about to erase it. Its own
  // allocation is excluded by restarting the counter afterwards.
  guard root (x |> adjoint)
  let a3 = alloc ()
  reset root
  let a4 = alloc ()
  a1 - a0, a2 - a1, a4 - a3

/// Allocated bytes for (a) the forward evaluation that builds the tape,
/// (b) the reset, and (c) the push — with (c) derived, not measured.
///
/// `DOps.reverseReset` is public, so (b) is a direct measurement. (c) is not
/// available on its own: `pushRec` is a local function inside `reverseProp`
/// (`AD.Lite.fs:3648`) and `reverseProp` calls `reverseReset` before it
/// (`AD.Lite.fs:3997`), so the only public push includes a reset. (c) is
/// therefore `reverseProp` minus the bare reset, and is labelled derived.
let phases (spec: Graph.GraphSpec) (reps: int) =
  let fx = Graph.build spec
  measureOnce fx |> ignore // warm: JIT, the DiffSharp statics, the tagger

  let mutable fwd = 0L
  let mutable rp = 0L
  let mutable rst = 0L
  let sw = Stopwatch.StartNew()
  for _ in 1 .. reps do
    let f, p, r = measureOnce fx
    fwd <- fwd + f
    rp <- rp + p
    rst <- rst + r
  sw.Stop()

  let n = float reps
  let fwd, rp, rst = float fwd / n, float rp / n, float rst / n
  let nodes = float (Graph.nodeCount spec)
  let slots = Graph.adjointSlots spec
  let floorBytes = float slots * 8.0

  printfn "phases  pillars=%d points=%d depth=%d, %d reps, %.1f ms"
    spec.Pillars spec.Points spec.Depth reps sw.Elapsed.TotalMilliseconds
  printfn "  %-28s %12s %12s %12s" "phase" "B/pass" "B/node" "B/slot"
  let row (label: string) (bytes: float) =
    printfn "  %-28s %12.0f %12.1f %12.2f" label bytes (bytes / nodes) (bytes / float slots)
  row "(a) forward, builds tape" fwd
  row "(b) reset      [measured]" rst
  row "(a)+(b)+(c) reverseProp" rp
  row "(c) push       [derived]" (rp - rst)
  printfn "  ---"
  printfn "  zero-vector floor         %12.0f B  (%d slots x 8 B)" floorBytes slots
  printfn "  reset / floor             %12.2f x" (rst / floorBytes)
  printfn "  reverseProp / forward     %12.2f x" (rp / fwd)

/// The same phase split across a list of graph shapes, one line each.
///
/// This is the evidence that confirms or kills the per-node reading. If a reset
/// allocates a fresh zero vector per node, then reset bytes track the slot count
/// and `reset/floor` stays flat at a little over 1. An in-place reset would show
/// a column that barely moves and a `reset/floor` collapsing towards zero.
let sweep (varying: string) (label: Graph.GraphSpec -> string) (specs: Graph.GraphSpec list)
          (reps: int) =
  printfn "%-7s %8s %8s %14s %14s %14s %14s %11s"
    varying "nodes" "slots" "forward B" "reset B" "revProp B" "push B (der)" "reset/floor"
  for spec in specs do
    let fx = Graph.build spec
    measureOnce fx |> ignore
    let mutable fwd = 0L
    let mutable rp = 0L
    let mutable rst = 0L
    for _ in 1 .. reps do
      let f, p, r = measureOnce fx
      fwd <- fwd + f
      rp <- rp + p
      rst <- rst + r
    let n = float reps
    let fwd, rp, rst = float fwd / n, float rp / n, float rst / n
    let slots = Graph.adjointSlots spec
    printfn "%-7s %8d %8d %14.0f %14.0f %14.0f %14.0f %11.2f"
      (label spec) (Graph.nodeCount spec) slots fwd rst rp (rp - rst)
      (rst / (float slots * 8.0))

/// Sweep the number of layers at a fixed node width: more nodes, same size each.
let scale (depths: int list) (reps: int) =
  sweep "depth" (fun s -> string s.Depth) (depths |> List.map Graph.spec) reps

/// Sweep the node width at a fixed layer count: same nodes, more slots each.
/// The other half of the evidence — depth and width should move the reset by the
/// same per-slot amount if the cost really is one zero vector a node.
let width (pillars: int list) (reps: int) =
  sweep "pillars" (fun s -> string s.Pillars) (pillars |> List.map (fun p -> Graph.specOf p 8)) reps

/// A bare loop of `n` forward-plus-reverse passes, for an external profiler.
///
/// The shape `dotnet-trace collect --profile gc-verbose` wants: one process, no
/// BenchmarkDotNet warm-up or child-process machinery, every allocation on the
/// path under test.
let profile (spec: Graph.GraphSpec) (n: int) =
  let fx = Graph.build spec
  let root0, x0 = Graph.forward fx
  push root0
  guard root0 (x0 |> adjoint)

  printfn "profile: %d forward+reverse passes, depth=%d" n spec.Depth
  let before = alloc ()
  let pause0 = GC.GetTotalPauseDuration()
  let sw = Stopwatch.StartNew()
  for _ in 1 .. n do
    let root, _ = Graph.forward fx
    push root
  sw.Stop()
  let mb = float (alloc () - before) / 1048576.0
  let pause = (GC.GetTotalPauseDuration() - pause0).TotalMilliseconds
  printfn "  %.1f ms total, %.3f ms/pass, %.1f MB total, %.1f KB/pass"
    sw.Elapsed.TotalMilliseconds (sw.Elapsed.TotalMilliseconds / float n) mb (mb * 1024.0 / float n)
  printfn "  GC pause %.1f ms (%.1f%% of wall)" pause (100.0 * pause / sw.Elapsed.TotalMilliseconds)

/// One forward pass followed by `SeedPasses` reverse passes over the same tape —
/// the curve-solve shape, where `jacobianTv''` serves a whole Jacobian from one
/// forward pass and pays a full tape reset for every row of it.
let seedPasses (spec: Graph.GraphSpec) (reps: int) =
  let fx = Graph.build spec
  let warmRoot, warmX = Graph.forward fx
  push warmRoot
  guard warmRoot (warmX |> adjoint)

  let mutable fwd = 0L
  let mutable passes = 0L
  for _ in 1 .. reps do
    let a0 = alloc ()
    let root, x = Graph.forward fx
    let a1 = alloc ()
    for _ in 1 .. Graph.SeedPasses do
      push root
    let a2 = alloc ()
    guard root (x |> adjoint)
    fwd <- fwd + (a1 - a0)
    passes <- passes + (a2 - a1)

  let n = float reps
  let fwd, passes = float fwd / n, float passes / n
  printfn "seeds   depth=%d, %d reverse passes over one tape, %d reps"
    spec.Depth Graph.SeedPasses reps
  printfn "  forward (once)            %12.0f B" fwd
  printfn "  %d reverse passes          %12.0f B  (%.0f B each)"
    Graph.SeedPasses passes (passes / float Graph.SeedPasses)
  printfn "  total                     %12.0f B  (%.1f%% of it reverse)"
    (fwd + passes) (100.0 * passes / (fwd + passes))
