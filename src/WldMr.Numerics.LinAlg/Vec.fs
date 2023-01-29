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
