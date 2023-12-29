module BasicTests

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

let accuracy = { absolute = 0.0001; relative = 0.}

[<Tests>]
let tests =
  testList "Basic tests" [
    testCase "D addition" <| fun _ ->
      let x = 1.0 |> D
      let y = 1.0 |> D
      let s = x + y |> D.toFloat
      s |> Expect.floatClose "D 1 + D 1 failed" accuracy 2.0

    testCase "DV addition" <| fun _ ->
      let x = [| 1.0 |] |> DV
      let y = [| 1.0 |] |> DV
      let s = x + y
      s.[0] |> Expect.dfloatClose "DV [ 1 ] + DV [ 1 ] failed" accuracy 2.0

    testCase "D exp" <| fun _ ->
      let x = 0.0 |> D
      // let s = exp x // fails
      let s = D.Exp x
      s |> Expect.dfloatClose "exp (D 0) = (D 1) failed" accuracy 1.0

    testCase "DV exp" <| fun _ ->
      let x = [| 0.0 |] |> DV
      // let s = exp x // fails
      let s = DV.Exp x
      s.[0] |> Expect.dfloatClose "exp DV [ 0 ] = [ 1 ] failed" accuracy 1.0
  ]
