module WldMr.Numerics.LinAlg.CsrMat


[<Struct>]
type CsrMat =
  {
    Values: float[]
    Columns: int[]
    RowIndices: int[]
    NCols: int
  }
  member inline m.NRows = m.RowIndices.Length - 1

  member m.Item
    with get (i, j) =
      let rStart, rEnd = m.RowIndices.[i], m.RowIndices.[i+1]
      let mutable idx = rStart
      while idx < rEnd && m.Columns.[idx] <> j do
        idx <- idx + 1
      if idx = rEnd then
        0.
      else
        m.Values.[idx]

  member m.ColSizes () =
    let colSizes = Array.zeroCreate<int> m.NCols
    for col in m.Columns do
      colSizes.[col] <- colSizes.[col] + 1
    colSizes

  member m.Transpose () =
    {
      Values= m.Values
      Rows= m.Columns
      ColIndices= m.RowIndices
      NRows= m.NCols
    }

  member m.ToCscMat () =
    let colSizes = m.ColSizes ()
    let colIndices = Array.scan (+) 0 colSizes
    let newValues = Array.zeroCreate m.Values.Length
    let newRows = Array.zeroCreate<int> m.Values.Length
    let colTotal = colSizes
  #if !FABLE_COMPILER
    System.Array.Clear(colTotal, 0, colTotal.Length)
  #else
    for i in 0 .. colTotal.Length-1 do
      colTotal.[i] <- 0
  #endif
    let mutable rowIdx = 0
    for i in 0 .. m.Values.Length - 1 do
      while i >= m.RowIndices.[rowIdx+1] do rowIdx <- rowIdx + 1
      let colIdx = m.Columns.[i]
      let colCount = colTotal.[colIdx]
      let newIdx = colIndices.[colIdx] + colCount
      newValues.[newIdx] <- m.Values.[i]
      newRows.[newIdx] <- rowIdx
      colTotal.[colIdx] <- colCount + 1

    {
      Values= newValues
      Rows= newRows
      ColIndices= colIndices
      NRows= m.NRows
    }

  member m.MulV(x: float[]): float[] =
    let mnRows = m.NRows
    let y = Array.zeroCreate mnRows
    let mutable row = 0
    let mutable i = 0
    while row < mnRows do
      let rowEnd = m.RowIndices.[row+1]
      let mutable r = 0.
      while i < rowEnd do
        r <- r + x.[m.Columns.[i]] * m.Values.[i]
        i <- i + 1
      y.[row] <- r
      row <- row + 1
    y

and CscMat =
  {
    Values: float[]
    Rows: int[]
    ColIndices: int[]
    NRows: int
  }
  member inline m.NCols = m.ColIndices.Length - 1

  member m.Item
    with get (i, j) =
      let cStart, cEnd = m.ColIndices.[j], m.ColIndices.[j+1]
      let mutable idx = cStart
      while idx < cEnd && m.Rows.[idx] <> i do
        idx <- idx + 1
      if idx = cEnd then
        0.
      else
        m.Values.[idx]


  member m.Transpose () =
    {
      Values= m.Values
      Columns= m.Rows
      RowIndices= m.ColIndices
      NCols= m.NRows
    }



module CsrMat =
  // would it be faster to convert to a CsCMat and then toMat ?
  let toMat (m: CsrMat) =
    Mat.init m.NRows m.NCols (fun i j -> m.get_Item(i, j))

  let mulV (m: CsrMat) (x: float[]): float[] =
    let mnRows = m.NRows
    let y = Array.zeroCreate mnRows
    let mutable row = 0
    let mutable i = 0
    while row < mnRows do
      let rowEnd = m.RowIndices.[row+1]
      while i < rowEnd do
        y.[row] <- y.[row] + x.[m.Columns.[i]] * m.Values.[i]
        i <- i + 1
      row <- row + 1
    y

  let toMatT (m: CsrMat) =
    let mat = Mat.zeroCreate m.NRows m.NCols

    let mnCols = m.NCols
    let mutable col = 0
    let mutable i = 0
    while col < mnCols do
      let colEnd = m.RowIndices.[col+1]
      while i < colEnd do
        mat.[m.Columns.[i], col] <- m.Values.[i]
        i <- i + 1
      col <- col + 1
    mat


module CscMat =
  let toMat (m: CscMat) =
    let mat = Mat.zeroCreate m.NRows m.NCols

    let mnCols = m.NCols
    let mutable col = 0
    let mutable i = 0
    while col < mnCols do
      let colEnd = m.ColIndices.[col+1]
      while i < colEnd do
        mat.[m.Rows.[i], col] <- m.Values.[i]
        i <- i + 1
      col <- col + 1
    mat
