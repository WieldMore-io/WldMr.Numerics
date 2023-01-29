module WldMr.Numerics.LinAlg.Tests.Mat

open Expecto
open WldMr.Numerics.LinAlg
open FsCheck


let genSquareMatrix: Gen<Mat> =
  let gm (s: int): Gen<Mat> =
    let sq = (s|>float|>sqrt|>int)
    let nf = Arb.generate<NormalFloat> |> Gen.map (fun (NormalFloat v) -> v)
    gen {
      let! v = Gen.arrayOfLength (sq*sq) nf
      return { Data= v; NRows= sq; NCols= sq }
    }
  Gen.sized gm

let genUpperTriangular: Gen<Mat> =
  let gm (s: int): Gen<Mat> =
    let sq = (s|>float|>sqrt|>int)
    let nf = Arb.generate<NormalFloat> |> Gen.map (fun (NormalFloat v) -> v)
    gen {
      let! v = Gen.arrayOfLength (sq*sq) nf
      for i = 0 to sq - 1 do
        for j = 0 to sq - 1 do
          if j < i then
            v.[i+j*sq] <- 0.
      return { Data= v; NRows= sq; NCols= sq }
    }
  Gen.sized gm

let genLowerTriangular: Gen<Mat> =
  let gm (s: int): Gen<Mat> =
    let sq = (s|>float|>sqrt|>int)
    let nf = Arb.generate<NormalFloat> |> Gen.map (fun (NormalFloat v) -> v)
    gen {
      let! v = Gen.arrayOfLength (sq*sq) nf
      for i = 0 to sq - 1 do
        for j = 0 to sq - 1 do
          if j > i then
            v.[i+j*sq] <- 0.
      return { Data= v; NRows= sq; NCols= sq }
    }
  Gen.sized gm

let genVector: Gen<float[]> =
  let gm (s: int): Gen<float[]> =
    let sq = s |> float |> sqrt |> int
    let nf = Arb.generate<NormalFloat> |> Gen.map (fun (NormalFloat v) -> v)
    Gen.arrayOfLength sq nf
  Gen.sized gm


type SquareMatrix = SquareMatrix of Mat
type UpperTriangular = UpperTriangular of Mat
type LowerTriangular = LowerTriangular of Mat
type Vector = Vector of double[]

type MatGenerator =
  static member Mat() =
    Arb.convert
      Mat.ofArray2Dtr
      Mat.toArray2Dtr
      Arb.from<float[,]>
  static member SquareMatrix() =
    {new Arbitrary<SquareMatrix>() with
      override x.Generator = genSquareMatrix |> Gen.map SquareMatrix
      override x.Shrinker t = Seq.empty }

  static member UpperTriangular() =
    {new Arbitrary<UpperTriangular>() with
      override x.Generator = genUpperTriangular |> Gen.map UpperTriangular
      override x.Shrinker t = Seq.empty }

  static member LowerTriangular() =
    {new Arbitrary<LowerTriangular>() with
      override x.Generator = genLowerTriangular |> Gen.map LowerTriangular
      override x.Shrinker t = Seq.empty }

  static member Vector() =
    {new Arbitrary<Vector>() with
      override x.Generator = genVector |> Gen.map Vector
      override x.Shrinker t = Seq.empty}

let config = {
  FsCheckConfig.defaultConfig
  with
    arbitrary = [ typeof<MatGenerator> ]
    endSize = 100
    maxTest = 200
}


let solveTests () =
  testList "Solve" [
    let f (SquareMatrix a) (Vector x) =
      let size = x.Length
      let y = Array.create size 0.
      Blas.dgemv('N', size, size, 1., a.Data, size, x, 1, 0., y, 1)
      let x2 = y |> Array.copy
      let ipiv = Array.create size 0
      Blas.dgesvUnblocked(
        size,
        1,
        WSpan.span a.Data,
        a.NRows,
        WSpan.span ipiv,
        WSpan.span x2,
        1
      ) |> ignore
      Util.(=~)(x2, x)
    let testSize = 16 * 16
    testPropertyWithConfig {config with endSize = testSize; maxTest = 200; startSize = 1} "Solve" f
  ]

let triangularTests () =
  testList "Triangular" [
    testPropertyWithConfig config "Solve Upper" <| fun (UpperTriangular a) (Vector x) ->
      let size = x.Length
      let y = Array.create size 0.
      Blas.dgemv('N', size, size, 1., a.Data, size, x, 1, 0., y, 1)
      let x2 = y |> Array.copy
      Blas.dtrsm('L', 'U', 'N', 'N',
                 size, 1, 1., WSpan.rospan a.Data, size, WSpan.span x2, 1
                 ) |> ignore
      Util.(=~)(x2, x)

    testPropertyWithConfig config "Solve Lower" <| fun (LowerTriangular a) (Vector x) ->
      let size = x.Length
      let y = Array.create size 0.
      Blas.dgemv('N', size, size, 1., a.Data, size, x, 1, 0., y, 1)
      let x2 = y |> Array.copy
      Blas.dtrsm('L', 'L', 'N', 'N',
                 size, 1, 1., WSpan.rospan a.Data, size, WSpan.span x2, 1
                 ) |> ignore
      Util.(=~)(x2, x)
  ]

let basicTests =
  testList "Basic" [
    testPropertyWithConfig config "nRows positive" <| fun (a: Mat) ->
      a.NRows > -1 && a.NCols > -1 && a.NRows * a.NCols = a.Length

    testPropertyWithConfig config "convert to Array2D" <| fun (m: Mat) ->
      let a = m |> Mat.toArray2D
      [
        m.NRows = (a |> Array2D.length1)
        m.NCols = (a |> Array2D.length2)
      ]
      |> List.reduce (&&)

    testPropertyWithConfig config "convert from Array2D" <| fun (a: float[,]) ->
      let m = a |> Mat.ofArray2D
      [
        m.NRows = (a |> Array2D.length1)
        m.NCols = (a |> Array2D.length2)
      ]
      |> List.reduce (&&)

//    let fscConfig =
//      { MaxTest = config.maxTest
//        MaxFail = 1000
//        Replay = Option.map Random.StdGen config.replay
//        Name = "name"
//        StartSize = config.startSize
//        EndSize = config.endSize
//        QuietOnSuccess = true
//        Every = fun _ _ -> String.Empty
//        EveryShrink = fun _ -> String.Empty
//        Arbitrary = config.arbitrary
//        Runner = failwith "" }
//
//    let a = Check.One(fscConfig, failwith "")

    testPropertyWithConfig config "Sum_M" <| fun (m: Mat) ->
      let mutable r = 0.
      for j = 0 to m.NCols - 1 do
        for i = 0 to m.NRows - 1 do
          r <- r + m.[i, j]
      let actual = Mat.sum m
      Util.(=~)(actual, r)

    testPropertyWithConfig config "SquareMatrix.Sum_M" <| fun (SquareMatrix m) ->
      let mutable r = 0.
      for j = 0 to m.NCols - 1 do
        for i = 0 to m.NRows - 1 do
          r <- r + m.[i, j]
      let actual = Mat.sum m
      Util.(=~)(actual, r)

    let multIsAssoc (SquareMatrix m1) (SquareMatrix m2) (SquareMatrix m3) =
      let r1 =
        Mat.mulM (Mat.mulM m1 m2) m3
      let r2 =
        Mat.mulM m1 (Mat.mulM m2 m3)
      Util.(=~)(r1.Data, r2.Data)

    testPropertyWithConfig config "SquareMatrix.MultIsAssociative" multIsAssoc

//    etestPropertyWithConfig (959243945, 296926832) config "SquareMatrix.MultIsAssociative2" multIsAssoc

    let multIsDist (SquareMatrix m1) (SquareMatrix m2) (SquareMatrix m3) =
      let r1 =
        Mat.mulM (Mat.addM m1 m2) m3
      let r2 =
        Mat.addM
          (Mat.mulM m1 m3)
          (Mat.mulM m2 m3)
      Util.(=~)(r1.Data, r2.Data)

    testPropertyWithConfig config "SquareMatrix.MultIsDistributive" multIsDist

    let traceOfMatrix (SquareMatrix m) =
      let expected =
        [|0 .. (max m.NRows m.NCols) - 1|]
        |> Array.map (fun i -> m.[i, i])
        |> Array.sum
      Util.(=~)(expected, Mat.trace m)

    testPropertyWithConfig config "SquareMatrix.MatrixTrace" traceOfMatrix


    let traceOfProduct (SquareMatrix m1) (SquareMatrix m2) =
      let r1 = Mat.mulM m1 m2 |> Mat.trace
      let r2 = Mat.mulM m2 m1 |> Mat.trace
      Util.(=~)(r1, r2)

    testPropertyWithConfig config "SquareMatrix.TraceOfProduct" traceOfProduct


    testPropertyWithConfig config "Mul_Dot_V_V" <| fun (v: float[]) ->
      let r = v |> Array.map (fun x -> x * x) |> Array.sum
      Util.(=~)(Blas.ddot(v.Length, v, 1, v, 1), r)
  ]

[<Tests>]
let tests =
  testList "Mat Blas" [solveTests(); basicTests; triangularTests ()]
