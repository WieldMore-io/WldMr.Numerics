module GatherTests

#if FABLE_COMPILER_PYTHON
open Fable.Pyxpecto
#endif
#if FABLE_COMPILER_JAVASCRIPT
open Fable.Mocha
#endif
#if !FABLE_COMPILER
open Expecto
open Expecto.Flip
#else
type TestsAttribute() =
  inherit System.Attribute()
#endif

open WldMr.Numerics.DiffSharp.AD.Float64

open MochaFlip

let accuracy = { absolute = 1e-9; relative = 0. }

let private raises (f: unit -> 'a) =
  try
    f () |> ignore
    false
  with _ -> true

/// `Gather`/`Scatter` per plans/ad-gather.md. The reverse-pass cases sit behind
/// `pushRec`/`resetRec` wildcards, so a forgotten case compiles clean — tests 2
/// and 3 are the tripwires for exactly those two omissions.
[<Tests>]
let tests =
  testList "gather" [

    testCase "forward picks by index, duplicates and unsorted allowed" <| fun _ ->
      let v = DV [| 3.0; 5.0; 7.0 |]
      let g = DV.Gather(v, [| 2; 0; 0; 1 |]) |> DV.toFloats
      g.Length |> Expect.equal "length follows ks" 4
      g.[0] |> Expect.floatClose "picked [2]" accuracy 7.0
      g.[1] |> Expect.floatClose "picked [0]" accuracy 3.0
      g.[2] |> Expect.floatClose "picked [0] again" accuracy 3.0
      g.[3] |> Expect.floatClose "picked [1]" accuracy 5.0
      (v |> DV.gather [| 1 |] |> DV.toFloats).[0]
        |> Expect.floatClose "module helper" accuracy 5.0
      (DV.Gather(v, [||]) |> DV.toFloats).Length
        |> Expect.equal "empty ks is DV.Zero, no node" 0
      // Scatter forward: duplicates ADD into the slot
      let s = DV.Scatter(DV [| 1.0; 2.0; 3.0 |], [| 1; 1; 0 |], 4) |> DV.toFloats
      s.[0] |> Expect.floatClose "slot 0" accuracy 3.0
      s.[1] |> Expect.floatClose "slot 1 accumulates 1+2" accuracy 3.0
      s.[2] |> Expect.floatClose "untouched slot" accuracy 0.0
      s.[3] |> Expect.floatClose "untouched slot" accuracy 0.0

    testCase "reverse with duplicate indices: the scatter must ADD" <| fun _ ->
      // d/dv of sum(gather(v, [1;1;0])) = scatter of ones = [|1; 2|]. Goes red
      // if the pushRec case is forgotten (silent zero through the wildcard) or
      // if a future in-place scatter overwrites instead of accumulating.
      let g = grad (fun (v: DV) -> DV.Sum(DV.Gather(v, [| 1; 1; 0 |]))) (DV [| 3.0; 5.0 |])
              |> DV.toFloats
      g.[0] |> Expect.floatClose "index 0 picked once" accuracy 1.0
      g.[1] |> Expect.floatClose "index 1 picked twice" accuracy 2.0

    testCase "fan-out: one source feeding two gather nodes" <| fun _ ->
      // Red if the resetRec case is forgotten: the source never arms, and with
      // lazy adjoints the push then meets an unmaterialised buffer.
      let g = grad (fun (v: DV) -> DV.Sum(DV.Gather(v, [| 0; 1 |]) + DV.Gather(v, [| 1; 0 |])))
                   (DV [| 3.0; 5.0 |])
              |> DV.toFloats
      g.[0] |> Expect.floatClose "both scatters contribute" accuracy 2.0
      g.[1] |> Expect.floatClose "both scatters contribute" accuracy 2.0

    testCase "nested forward-over-reverse through a gather" <| fun _ ->
      // Constraint-5 shape: a DVF adjoint dispatches through Scatter's own
      // Op_DV_DV case. f v = sum(gather(v,[1;1;0]) .* gather(v,[1;1;0]))
      // = v0^2 + 2 v1^2, so H = diag(2, 4).
      let f (v: DV) =
        let s = DV.Gather(v, [| 1; 1; 0 |])
        DV.Sum(s .* s)
      let x = DV [| 3.0; 5.0 |]
      let hcol0 = jacobianv (grad f) x (DV [| 1.0; 0.0 |]) |> DV.toFloats
      let hcol1 = jacobianv (grad f) x (DV [| 0.0; 1.0 |]) |> DV.toFloats
      hcol0.[0] |> Expect.floatClose "H[0,0]" accuracy 2.0
      hcol0.[1] |> Expect.floatClose "H[1,0]" accuracy 0.0
      hcol1.[0] |> Expect.floatClose "H[0,1]" accuracy 0.0
      hcol1.[1] |> Expect.floatClose "H[1,1]" accuracy 4.0

    testCase "bounds are validated on every target" <| fun _ ->
      // Under Fable→JS an out-of-bounds typed-array read is `undefined` and a
      // write is silently dropped; under Fable→Python a negative index wraps.
      // This test running under Fable is the point — .NET throws on its own.
      let v = DV [| 3.0; 5.0; 7.0 |]
      raises (fun () -> DV.Gather(v, [| 3 |])) |> Expect.isTrue "index = length"
      raises (fun () -> DV.Gather(v, [| -1 |])) |> Expect.isTrue "negative index"
      raises (fun () -> DV.Scatter(DV [| 1.0 |], [| 2 |], 2)) |> Expect.isTrue "scatter index = n"
      raises (fun () -> DV.Scatter(DV [| 1.0; 2.0 |], [| 0 |], 3)) |> Expect.isTrue "length mismatch"
  ]
