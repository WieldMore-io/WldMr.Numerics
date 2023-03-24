namespace WldMr.Numerics.LinAlg


module Vec =
  let addVV (a: float[]) (b: float[]) =
    if a.Length <> b.Length then failwith $"Arrays of different size {a.Length} {b.Length}"
    Array.init a.Length (fun i -> a.[i] + b.[i])

  let subVV (a: float[]) (b: float[]) =
    if a.Length <> b.Length then failwith $"Arrays of different size {a.Length} {b.Length}"
    Array.init a.Length (fun i -> a.[i] - b.[i])

  let absMax (v: float[]) =
    let mutable r = 0.
    for x in v do
      let a = x |> abs
      if a > r then r <- a
    r

  let inline dotProduct (x: float[]) (y: float[]) =
    let xl = x.Length
    let yl = y.Length
    if (xl = 0) || (yl = 0) then
        0.
    elif xl <> yl then
        failwith $"Arrays of different size {xl} {yl}"
    else
        Blas.ddot(xl, x, 1, y, 1)

  let inline mulVV (x: float[]) (y: float[]) =
    let xl = x.Length
    let yl = y.Length
    if (xl = 0) || (yl = 0) then
        0.
    elif xl <> yl then
        failwith $"Arrays of different size {xl} {yl}"
    else
        Blas.ddot(xl, x, 1, y, 1)

  let inline mulSV (x: float) (y: float[]) =
    let yl = y.Length
    if (yl = 0) then
        [||]
    else
      Array.init yl (fun i -> x * y.[i])
