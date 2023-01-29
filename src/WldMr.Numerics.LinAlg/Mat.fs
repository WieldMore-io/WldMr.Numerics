namespace WldMr.Numerics.LinAlg

open System.Diagnostics

[<AutoOpen>]
module private ArrayFast =
  module Array =
    let inline copyFast (array : float[]) =
      let l = array.Length
      let x = Array.zeroCreate l
      System.Array.Copy(array, x, l)
      x

[<DebuggerDisplay("{this.DebugDisplay,nq}")>]
type MatT<'T > =
  {
    Data: 'T[]
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    NRows: int
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    NCols: int
  }
  with

  [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
  member mat.DebugDisplay =
    let shortFmt f = f |> box :?> float |> sprintf "%.2f"
    let rows = [
      for i in 0..mat.NRows-1 do
        [
          for j in 0..mat.NCols-1 do
            mat.Data.[i + j*mat.NRows]
        ]
    ]
    let literal = rows |> List.map (List.map shortFmt >> String.concat ", ") |> String.concat " || "
    $"{mat.NRows}x{mat.NCols}: [{literal}]"

  [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
  member mat.Length = mat.Data.Length
  member mat.Item
    with get (i, j) =
      mat.Data.[i + j * mat.NRows]
    and set (i, j) v =
      mat.Data.[i + j * mat.NRows] <- v

  static member zeroCreate m n =
    {
      Data= Array.zeroCreate<'T> (m*n)
      NRows= m
      NCols= n
    }

  static member inline init m n (f: int -> int -> 'T) =
    let res = MatT.zeroCreate m n
    for j = 0 to n-1 do
      for i = 0 to m-1 do
        res.[i, j] <- f i j
    res

  // TODO: add tests
  member mat.GetSlice(startIdx, endIdx, startIdx2, endIdx2): MatT<'T> =
    let s = defaultArg startIdx 0
    let e = defaultArg endIdx (mat.NRows-1)
    let s2 = defaultArg startIdx2 0
    let e2 = defaultArg endIdx2 (mat.NCols-1)
    if e <= s || e2 <= s2 then
      MatT.zeroCreate 0 0
    else
      MatT.init (1+e-s) (1+e2-s2)
        (fun i j -> mat.[s+i, s2+j])

  // TODO: add tests
  member mat.GetSlice(singleIdx: int, startIdx2, endIdx2): 'T[] =
    let s2 = defaultArg startIdx2 0
    let e2 = defaultArg endIdx2 (mat.NCols-1)
    if e2 <= s2 then
      Array.init (1+e2-s2) (fun i -> mat.[singleIdx, i])
    else
      failwith "Mat.GetSlice out of bounds"

  // TODO: add tests
  member mat.GetSlice(startIdx, endIdx, singleIdx: int): 'T[] =
    let s = defaultArg startIdx 0
    let e = defaultArg endIdx (mat.NRows-1)
    if e <= s then
      Array.init (1+e-s) (fun i -> mat.[i, singleIdx])
    else
      failwith "Mat.GetSlice out of bounds"

  member mat.ToCsv(): string =
    let fullFmt f = f |> box :?> float |> sprintf "%.16g"
    let rows = [
      for i in 0..mat.NRows-1 do
        [
          for j in 0..mat.NCols-1 do
            mat.Data.[i + j*mat.NRows]
        ]
    ]
    let literal = rows |> List.map (List.map fullFmt >> String.concat ",") |> String.concat "\n"
    literal

  member mat.ToFSharp(): string =
    let fullFmt f = f |> box :?> float |> sprintf "%.16g"
    let data =
      mat.Data
      |> Array.chunkBySize mat.NRows
      |> Array.map (Array.map fullFmt >> Array.chunkBySize 4 >> Array.map (String.concat ";") >> String.concat "\n    ")
      |> String.concat "\n\n    "
    [
      yield  "{"
      yield  "  Data=[|"
      yield $"    {data}"
      yield  "  |]"
      yield $"  NRows={mat.NRows}"
      yield $"  NCols={mat.NCols}"
      yield  "}"
    ] |> String.concat "\n"


//[<DefaultAugmentation(false)>]
[<DebuggerDisplay("{this.DebugDisplay,nq}")>]
type Mat =
  {
    Data: float[]
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    NRows: int
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    NCols: int
  }
  with

  [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
  member mat.DebugDisplay =
    let shortFmt f = f |> box :?> float |> sprintf "%.2f"
    let rows = [
      for i in 0..mat.NRows-1 do
        [
          for j in 0..mat.NCols-1 do
            mat.Data.[i + j*mat.NRows]
        ]
    ]
    let literal = rows |> List.map (List.map shortFmt >> String.concat ", ") |> String.concat " || "
    $"{mat.NRows}x{mat.NCols}: [{literal}]"

  [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
  member mat.Length = mat.Data.Length
  member mat.Item
    with get (i, j) =
      mat.Data.[i + j * mat.NRows]
    and set (i, j) v =
      mat.Data.[i + j * mat.NRows] <- v

  static member zeroCreate m n =
    {
      Data= Array.zeroCreate (m*n)
      NRows= m
      NCols= n
    }

  static member inline init m n (f: int -> int -> float) =
    let res = Mat.zeroCreate m n
    for j = 0 to n-1 do
      for i = 0 to m-1 do
        res.[i, j] <- f i j
    res

  // TODO: add tests
  member mat.GetSlice(startIdx, endIdx, startIdx2, endIdx2): Mat =
    let s = defaultArg startIdx 0
    let e = defaultArg endIdx (mat.NRows-1)
    let s2 = defaultArg startIdx2 0
    let e2 = defaultArg endIdx2 (mat.NCols-1)
    if e <= s || e2 <= s2 then
      Mat.zeroCreate 0 0
    else
      Mat.init (1+e-s) (1+e2-s2)
        (fun i j -> mat.[s+i, s2+j])

  // TODO: add tests
  member mat.GetSlice(singleIdx: int, startIdx2, endIdx2): float[] =
    let s2 = defaultArg startIdx2 0
    let e2 = defaultArg endIdx2 (mat.NCols-1)
    if e2 <= s2 then
      Array.init (1+e2-s2) (fun i -> mat.[singleIdx, i])
    else
      failwith "Mat.GetSlice out of bounds"

  // TODO: add tests
  member mat.GetSlice(startIdx, endIdx, singleIdx: int): float[] =
    let s = defaultArg startIdx 0
    let e = defaultArg endIdx (mat.NRows-1)
    if e <= s then
      Array.init (1+e-s) (fun i -> mat.[i, singleIdx])
    else
      failwith "Mat.GetSlice out of bounds"

  member mat.ToCsv(): string =
    let fullFmt f = f |> box :?> float |> sprintf "%.16g"
    let rows = [
      for i in 0..mat.NRows-1 do
        [
          for j in 0..mat.NCols-1 do
            mat.Data.[i + j*mat.NRows]
        ]
    ]
    let literal = rows |> List.map (List.map fullFmt >> String.concat ",") |> String.concat "\n"
    literal

  member mat.ToFSharp(): string =
    let fullFmt f = f |> box :?> float |> sprintf "%.16g"
    let data =
      mat.Data
      |> Array.chunkBySize mat.NRows
      |> Array.map (Array.map fullFmt >> Array.chunkBySize 4 >> Array.map (String.concat ";") >> String.concat "\n    ")
      |> String.concat "\n\n    "
    [
      yield  "{"
      yield  "  Data=[|"
      yield $"    {data}"
      yield  "  |]"
      yield $"  NRows={mat.NRows}"
      yield $"  NCols={mat.NCols}"
      yield  "}"
    ] |> String.concat "\n"


module Mat =
  let copy (m: Mat) =
    {
      Data= Array.copyFast m.Data
      NRows= m.NRows
      NCols= m.NCols
    }
  let empty () =
    {
      Data= Array.empty
      NRows= 0
      NCols= 0
    }
  let zeroCreate m n =
    {
      Data= Array.zeroCreate (m*n)
      NRows= m
      NCols= n
    }
  let create m n v =
    {
      Data= Array.create (m*n) v
      NRows= m
      NCols= n
    }

  let init m n (f: int -> int -> float) =
    let res = zeroCreate m n
    for j = 0 to n-1 do
      for i = 0 to m-1 do
        res.[i, j] <- f i j
    res

  #if !FABLE_COMPILER
  let ofArray2D (a: float[,]) =
    init (Array2D.length1 a) (Array2D.length2 a)
      (fun i j -> a.[i, j])

  let ofArray2Dtr (a: float[,]) =
    init (Array2D.length2 a) (Array2D.length1 a)
      (fun i j -> a.[j, i])
  #endif

  let ofRowsArray (a: float[][]) =
    let nRows = a.Length
    let nCols = a.[0].Length
    Mat.init nRows nCols (fun i j -> a.[i].[j])

  let ofColsArray (a: float[][]) =
    let nRows = a.[0].Length
    let nCols = a.Length
    Mat.init nRows nCols (fun i j -> a.[j].[i])

  let rowVector (v: float[]) =
    {
      Data= Array.copyFast v
      NRows= 1
      NCols= v.Length
    }
  let colVector (v: float[]) =
    {
      Data= Array.copyFast v
      NRows= v.Length
      NCols= 1
    }
  let map f (mat: Mat) =
    {
      Data= Array.map f mat.Data
      NRows= mat.NRows
      NCols= mat.NCols
    }

  let map2 f (mat1: Mat) (mat2: Mat) =
    {
      Data= Array.map2 f mat1.Data mat2.Data
      NRows= mat1.NRows
      NCols= mat1.NCols
    }

  let sum (mat: Mat) = mat.Data |> Array.sum

  let trace (m: Mat) =
    let n = min m.NRows m.NCols
    let mutable r = 0.
    for i = 0 to n-1 do
      r <- r + m.[i, i]
    r

#if !FABLE_COMPILER
  let toArray2D (mat: Mat) =
    Array2D.init mat.NRows mat.NCols (fun i j -> mat.[i, j])

  let toArray2Dtr (mat: Mat) =
    Array2D.init mat.NCols mat.NRows (fun i j -> mat.[j, i])
#endif

  let inline mulV (mat: Mat)(x: float[]): float[] =
    let cols = mat.NCols
    let rows = mat.NRows
    if cols <> x.Length then failwith "mulMV: different size"
    let y = Array.zeroCreate rows
    Blas.dgemv('N', mat.NRows, mat.NCols, 1., mat.Data, mat.NRows, x, 1, 0., y, 1)
    y

  let inline tMulV (mat: Mat)(x: float[]): float[] =
    let cols = mat.NCols
    let rows = mat.NRows
    if rows <> x.Length then failwith "mulMV: different size"
    let y = Array.zeroCreate cols
    Blas.dgemv('T', mat.NRows, mat.NCols, 1., mat.Data, mat.NRows, x, 1, 0., y, 1)
    y


  let transpose (mat: Mat) =
    let nRows, nCols = mat.NRows, mat.NCols
    let xT = Mat.zeroCreate nCols nRows
    for i = 0 to nRows-1 do
        for j = 0 to nCols-1 do
            xT.Data.[ j + nCols * i ] <- mat.[ i, j ]
    xT

  let addM (x: Mat) (y: Mat) =
    let xl1 = x.NRows
    let xl2 = x.NCols
    let yl1 = y.NRows
    let yl2 = y.NCols
    if xl1 * xl2 = 0 then
      empty ()
    elif yl1 * yl2 = 0 then
      copy x
    elif (xl1 <> yl1) || (xl2 <> yl2) then
      failwith "inconsistent dimensions"
    else
      let y' = copy y
      Blas.daxpy( xl1 * xl2, 1., x.Data, 1, y'.Data, 1)
      y'

  let subM (x: Mat) (y: Mat) =
    let xl1 = x.NRows
    let xl2 = x.NCols
    let yl1 = y.NRows
    let yl2 = y.NCols
    if xl1 * xl2 = 0 then
      empty ()
    elif yl1 * yl2 = 0 then
      copy x
    elif (xl1 <> yl1) || (xl2 <> yl2) then
      failwith "inconsistent dimensions"
    else
      let y' = copy y
      Blas.daxpy( xl1 * xl2, -1., x.Data, 1, y'.Data, 1)
      y'

  let mulM (x: Mat) (y: Mat) =
    let xl1 = x.NRows
    let xl2 = x.NCols
    let yl1 = y.NRows
    let yl2 = y.NCols
    if xl1 * xl2 * yl1 * yl2 = 0 then
      empty ()
    elif xl2 <> yl1 then
      failwith "inconsistent dimensions"
    else
      let z = Mat.zeroCreate xl1 yl2
      Blas.dgemm('N', 'N', xl1, yl2, yl1, 1., x.Data, xl1, y.Data, yl1, 0., z.Data, xl1)
      z

  let mulS (m: Mat) (alpha: float) =
    let length = m.Data.Length
    if length = 0 then
      empty ()
    else
      let m' = copy m
      Blas.dscal(length, alpha, WSpan.span m'.Data, 1)
      m'

  let addS (m: Mat) (alpha: float) =
    let m' = copy m
    for i = 0 to m'.Data.Length-1 do
      m'.Data.[i] <- m'.Data.[i] + alpha
    m'

  let inv (m: Mat) =
    let size = m.NRows
    let res = Mat.init size size (fun i j -> if i = j then 1. else 0.)
    let iPiv = Array.create size 0
    let tempM = copy m
    Blas.dgesvUnblocked(
      size,
      size,
      WSpan.span tempM.Data,
      m.NRows,
      WSpan.span iPiv,
      WSpan.span res.Data,
      size
    ) |> ignore
    res

  let solveMV (m: Mat) (b: float array) =
    let xl1 = m.NRows
    let xl2 = m.NCols
    let yl = b.Length
    if xl1 * xl2 * yl = 0 then
      Error "At least one dimension is 0."
    elif xl1 <> yl then
      Error "Length of vector must match number of rows of matrix."
    else
      let size = m.NRows
      let iPiv = Array.create size 0
      let res = Array.copyFast b
      let tempM = copy m
      let info =
        Blas.dgesvUnblocked(
          size,
          1,
          WSpan.span tempM.Data,
          m.NRows,
          WSpan.span iPiv,
          WSpan.span res,
          1
        )
      if info <> 0 then
        Error $"solveMV error. Code: {info}"
      else
        Ok res


module MatT =
  let map<'T, 'U> (f: 'T -> 'U) (mat: MatT<'T>): MatT<'U> =
    {
      Data= Array.map f mat.Data
      NRows= mat.NRows
      NCols= mat.NCols
    }

  let create<'T> m n (v: 'T): MatT<'T> =
    {
      Data= Array.create<'T> (m*n) v
      NRows= m
      NCols= n
    }
