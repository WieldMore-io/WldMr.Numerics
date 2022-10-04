namespace WldMr.Analytics.LinAlg


[<AutoOpen>]
module GenMat =

  open CsrMat
  open WldMr.Analytics.LinAlg

  type GenMat =
    | ColMajor of Mat
    | TrColMajor of Mat
    | SparseDouble of CsrMat * CsrMat
    with

    member gm.NRows =
      match gm with
      | ColMajor m -> m.NRows
      | TrColMajor m -> m.NCols
      | SparseDouble (_, mt) -> mt.NCols

    member gm.NCols =
      match gm with
      | ColMajor m -> m.NCols
      | TrColMajor m -> m.NRows
      | SparseDouble (m, _) -> m.NCols

    member gm.Length =
      gm.NRows * gm.NCols

    member inline gm.Item
      with get (i, j) =
        match gm with
        | ColMajor m -> m.[i, j]
        | TrColMajor m -> m.[j, i]
        | SparseDouble (m, _) -> m.[i,j]

    member inline gm.ToMat() =
      match gm with
      | ColMajor m -> m
      | TrColMajor m -> m |> Mat.transpose
      | SparseDouble (_, mt) -> mt |> CsrMat.toMatT


    member gm.GetSlice(singleIdx: int, startIdx2, endIdx2): float[] =
      let mat = gm.ToMat()
      mat.GetSlice(singleIdx, startIdx2, endIdx2)

    member gm.GetSlice(startIdx, endIdx, singleIdx: int): float[] =
      let mat = gm.ToMat()
      mat.GetSlice(startIdx, endIdx, singleIdx)

    member gm.GetSlice(startIdx, endIdx, startIdx2, endIdx2): GenMat =
      let mat = gm.ToMat()
      mat.GetSlice(startIdx, endIdx, startIdx2, endIdx2) |> ColMajor



  [<RequireQualifiedAccess>]
  module GenMat =

    let map f gm =
      match gm with
      | ColMajor m -> Mat.map f m |> ColMajor
      | TrColMajor m -> Mat.map f m |> TrColMajor
      | SparseDouble (_m, _mt) -> failwith "todo"

    let toMat (gm: GenMat) = gm.ToMat()


    let map2 f gm1 gm2 =
      match gm1, gm2 with
      | ColMajor m1, ColMajor m2  ->
          Mat.map2 f m1 m2 |> ColMajor
      | _ ->
          let m1, m2 = gm1 |> toMat, gm2 |> toMat
          Mat.map2 f m1 m2 |> ColMajor

    let sum gm =
      match gm with
      | ColMajor m -> m |> Mat.sum
      | TrColMajor m -> m |> Mat.sum
      | SparseDouble (_m, _mt) -> failwith "todo"

    let empty =
      Mat.empty () |> ColMajor

    let copy gm =
      match gm with
      | ColMajor m -> m |> Mat.copy |> ColMajor
      | TrColMajor m -> m |> Mat.copy |> TrColMajor
      | SparseDouble (_m, _mt) -> failwith "todo"


    let inline transpose gm =
      match gm with
      | ColMajor m -> m |> TrColMajor
      | TrColMajor m -> m |> ColMajor
      | SparseDouble (m, mt) ->  (mt, m) |> SparseDouble

    let addM gm1 gm2 =
      match gm1, gm2 with
      | ColMajor m1, ColMajor m2  ->
          Mat.addM m1 m2 |> ColMajor
      | _ ->
          failwith "todo"

    let subM gm1 gm2 =
      match gm1, gm2 with
      | ColMajor m1, ColMajor m2  ->
          Mat.subM m1 m2 |> ColMajor
      | _ ->
          failwith "todo"

    let mulM gm1 gm2 =
      match gm1, gm2 with
      | ColMajor m1, ColMajor m2  ->
          Mat.mulM m1 m2 |> ColMajor
      | _ ->
          failwith "todo"

    let mulS gm a =
      match gm with
      | ColMajor m -> Mat.mulS m a |> ColMajor
      | TrColMajor _m -> failwith "TrColMajor.mulS"
      | SparseDouble (_m, _mt) -> failwith "todo"

    let addS gm a =
      match gm with
      | ColMajor m -> Mat.addS m a |> ColMajor
      | TrColMajor _m -> failwith "TrColMajor.addS"
      | SparseDouble (_m, _mt) -> failwith "todo"

    let inline mulV gm v =
      match gm with
      | ColMajor m -> Mat.mulV m v
      | TrColMajor m -> Mat.tMulV m v
      | SparseDouble (m, _) -> CsrMat.mulV m v

    #if !FABLE_COMPILER
    let ofArray2D (a: float[,]) =
      a |> Mat.ofArray2D |> ColMajor

    let ofArray2Dtr (a: float[,]) =
      a |> Mat.ofArray2Dtr |> ColMajor
    #endif


  module MatT =
    let mapF<'T> (f: 'T -> float) (mat: MatT<'T>): GenMat =
      {
        Data= Array.map f mat.Data
        NRows= mat.NRows
        NCols= mat.NCols
      } |> ColMajor
