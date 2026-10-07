module JacobianReverseTests

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

/// `jacobian'`'s reverse branch holds the `jacobianTv''` reverse evaluator, so the
/// forward pass of the reverse mode AD runs once, not once per Jacobian row (the
/// N+1 evaluation cost a partial application of `jacobianTv` had before). The
/// evaluation-count test pins that contract: it fails at 4 on the old code and
/// CI offered no other protection. See plans/ad-tape-allocation.md, step 5.
[<Tests>]
let tests =
  testList "jacobian reverse" [

    testCase "reverse branch evaluates f once, not once per row" <| fun _ ->
      let count = ref 0
      let f (v: DV) =
        count.Value <- count.Value + 1
        DV.ofSeqD [| v.[0] * v.[1]; v.[0] + v.[1]; v.[0] - v.[1] |]
      let o, j = jacobian' f (DV [| 3.0; 5.0 |])
      count.Value |> Expect.equal "one forward pass for the whole Jacobian" 1
      let ov = o |> DV.toFloats
      ov.[0] |> Expect.floatClose "primal x*y" accuracy 15.0
      ov.[1] |> Expect.floatClose "primal x+y" accuracy 8.0
      ov.[2] |> Expect.floatClose "primal x-y" accuracy -2.0
      j.[0, 0] |> Expect.dfloatClose "d(x*y)/dx" accuracy 5.0
      j.[0, 1] |> Expect.dfloatClose "d(x*y)/dy" accuracy 3.0
      j.[1, 0] |> Expect.dfloatClose "d(x+y)/dx" accuracy 1.0
      j.[1, 1] |> Expect.dfloatClose "d(x+y)/dy" accuracy 1.0
      j.[2, 0] |> Expect.dfloatClose "d(x-y)/dx" accuracy 1.0
      j.[2, 1] |> Expect.dfloatClose "d(x-y)/dy" accuracy -1.0

    testCase "reverse branch rows survive later passes (values, distinct seeds)" <| fun _ ->
      // The evaluator re-propagates over the shared tape once per row; with the
      // in-place push an aliased buffer refills identically under a same seed,
      // so the second pass here runs a genuinely different seed through r2.
      let f (v: DV) = DV.ofSeqD [| v.[0] * v.[1]; v.[0] + v.[1] |]
      let _, r2 = jacobianTv'' f (DV [| 3.0; 5.0 |])
      let row0 = r2 (DV [| 2.0; 0.0 |]) |> DV.toFloats
      let row1 = r2 (DV [| 0.0; 3.0 |]) |> DV.toFloats
      row0.[0] |> Expect.floatClose "seed [2;0], x*y component" accuracy 10.0
      row0.[1] |> Expect.floatClose "seed [2;0], x+y component" accuracy 6.0
      row1.[0] |> Expect.floatClose "seed [0;3], x*y component" accuracy 3.0
      row1.[1] |> Expect.floatClose "seed [0;3], x+y component" accuracy 3.0

    testCase "forward branch values are unchanged" <| fun _ ->
      // Wide output, narrow input: 2*x.Length <= o.Length takes the forward
      // branch. Its evaluation count is one probe plus one pass per basis
      // vector and is inherent to forward mode — assert values only.
      // `exp` on a D fails to resolve under Fable (see BasicTests' note); D.Exp is
      // the portable form.
      let f (v: DV) = DV.ofSeqD [| v.[0] * v.[0]; v.[0] * v.[0] * v.[0]; D.Exp v.[0] |]
      let o, j = jacobian' f (DV [| 2.0 |])
      let ov = o |> DV.toFloats
      ov.[0] |> Expect.floatClose "primal x^2" accuracy 4.0
      ov.[1] |> Expect.floatClose "primal x^3" accuracy 8.0
      ov.[2] |> Expect.floatClose "primal exp x" accuracy (exp 2.0)
      j.[0, 0] |> Expect.dfloatClose "d(x^2)/dx" accuracy 4.0
      j.[1, 0] |> Expect.dfloatClose "d(x^3)/dx" accuracy 12.0
      j.[2, 0] |> Expect.dfloatClose "d(exp x)/dx" accuracy (exp 2.0)

    testCase "jacobianT matches jacobian'" <| fun _ ->
      let f (v: DV) = DV.ofSeqD [| v.[0] * v.[1]; v.[0] + v.[1]; v.[0] - v.[1] |]
      let jt = jacobianT f (DV [| 3.0; 5.0 |])
      jt.[0, 0] |> Expect.dfloatClose "transpose [0,0]" accuracy 5.0
      jt.[0, 2] |> Expect.dfloatClose "transpose [0,2]" accuracy 1.0
      jt.[1, 0] |> Expect.dfloatClose "transpose [1,0]" accuracy 3.0
      jt.[1, 2] |> Expect.dfloatClose "transpose [1,2]" accuracy -1.0
  ]
