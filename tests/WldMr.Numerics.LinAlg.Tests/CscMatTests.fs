module WldMr.Numerics.LinAlg.Tests.CscMat

open Expecto
open Expecto.Flip
open WldMr.Numerics.LinAlg
open WldMr.Numerics.LinAlg.CsrMat


[<Tests>]
let tests =
  testList "CSR Matrix" [
    testCase "Empty" <| fun _ ->
      let emptyCscMat = {Values=[||]; Columns=[||]; RowIndices=[|0;0;0|]; NCols=2}
      let t = emptyCscMat.Transpose()

      t.NRows |> Expect.equal "has 2 rows" 2
      t.NCols |> Expect.equal "has 2 cols" 2


      [ for i in 0 .. 1 do
          for j in 0 .. 1 do
            t.[i, j]
      ]
      |> Expect.allEqual "All zero" 0.

    testCase "One element" <| fun _ ->
      let cscMat = {Values=[| 1. |]; Columns=[|0|]; RowIndices=[|0;0;1|]; NCols=2}

      let csrMat2 = cscMat.Transpose()

      csrMat2.NRows |> Expect.equal "has 2 rows" 2
      csrMat2.NCols |> Expect.equal "has 2 cols" 2

      let expected = [| 0.; 1.; 0.; 0. |]
      let mat = Mat.init cscMat.NRows cscMat.NCols (fun i j -> cscMat.[i, j])
      mat.Data |> Expect.equal "original matrix" expected
      let expected2 = [| 0.; 0.; 1.; 0. |]
      let mat2 = Mat.init csrMat2.NRows csrMat2.NCols (fun i j -> csrMat2.[i, j])
      mat2.Data |> Expect.equal "transposed matrix" expected2

    testCase "Multiplication" <| fun _ ->
      let cscMat = {Values=[| 1.; 2. |]; Columns=[|0; 1|]; RowIndices=[|0;1;2|]; NCols=3}
      let x = [| 0.1; 1.; 10. |]
      let y = cscMat.MulV(x)

      let expected = [| 0.1; 2. |]
      y.Length |> Expect.equal "has 2 rows" 2
      y |> Expect.equal "correct values" expected
  ]
