// This file is part of DiffSharp: Differentiable Functional Programming - https://diffsharp.github.io
// Copyright (c) 2016-     University of Oxford (Atilim Gunes Baydin <gunes@robots.ox.ac.uk>)
// Copyright (c) 2017-     Microsoft Research, Cambridge, UK (Don Syme <dsyme@microsoft.com>)
// Copyright (c) 2014-     National University of Ireland Maynooth (Barak A. Pearlmutter <barak@pearlmutter.net>)
// Copyright (c) 2014-2016 National University of Ireland Maynooth (Atilim Gunes Baydin)
// This code is licensed under the BSD license (see LICENSE file for details)

module WldMr.Numerics.DiffSharp.Tests.AD.Float64

open FsCheck.NUnit
open WldMr.Numerics.DiffSharp.Tests
open WldMr.Numerics.DiffSharp.AD.Float64
open WldMr.Numerics.LinAlg
open WldMr.Numerics.LinAlg.CsrMat

[<Property>]
let ``FixedPoint forward``() =
    let g (a:D) (b:D) = (a + b / a) / (D 2.)
    let p, t = diff' (D.FixedPoint g (D 1.2)) (D 25.)
    Util.(=~)(p, D 5.) && Util.(=~)(t, D 0.1)

[<Property>]
let ``FixedPoint reverse``() =
    let g (a:D) (b:D) = (a + b / a) / (D 2.)
    let p, t = grad' (fun v -> D.FixedPoint g (D 1.2) v.[0]) (DV [|25.|])
    Util.(=~)(p, D 5.) && Util.(=~)(t.[0], D 0.1)

//[<Property>]
//let ``AD.32.R.D.FixedPoint``() =
//    let g (a:D) (b:D) = (a + b / a) / (D 2.f)
//    let p, t = jacobianTv' (D.FixedPoint g (D 1.2f)) (D 25.f) (D 1.f)
//    Util.(=~)(p, D 5.f) && Util.(=~)(t, D 0.1f)

// IMPROVE: re-enable this test once DiffSharp Lite backend is fully-functional
//[<Property>]
let ``Gradient descent``() =

    let minimize (f:DV->D) (x0:DV) =
        let eta = 1e-2
        let mutable W = x0
        for _ in [0..10] do
            let _L,g = grad' f W
            W <- W - eta*g

    let lossFunction (w:DV) =
        let x = toDM [[1.0; 0.0]]
        let Wg = w.[0..3] |> DM.ofDV 2
        let g = (x*Wg)
        cos g.[0,0]

    minimize lossFunction (DV.createOfFloat 5 1.0) //Smoke test


// IMPROVE: re-enable this test once DiffSharp Lite backend is fully-functional
//[<Property>]
let ``Gradient descent (with arrays)``() =

    let minimize (f:DV->D) (x0:DV) =
        let eta = 1e-2
        let mutable W = x0
        for _ in [0..10] do
            let _L,g = grad' f W
            W <- W - eta*g

    let n = 5
    let lossFunction (w:DV) =
        let x = DM.init n n (fun i j -> w.[n*i+j])
        let x' = x.GetSlice(None, None, None, None)
        cos x'.[0,0]

    minimize lossFunction (DV.createOfFloat (n*n) 1.0) //Smoke test


[<Property>]
let ``Gather reverse gradient matches finite differences``(v0: float[], w0: float[], ks0: int[]) =
    let v = v0 |> Array.map (fun x -> if Util.IsNice(x) then x % 10. else 1.0)
    if v.Length = 0 || ks0.Length = 0 then true
    else
        // abs after %, so Int32.MinValue cannot overflow
        let ks = ks0 |> Array.map (fun k -> abs (k % v.Length))
        let w = Array.init ks.Length (fun i -> if i < w0.Length && Util.IsNice(w0.[i]) then w0.[i] % 10. else 1.0)
        // Linear in v, so a weighted-sum probe covers the whole Jacobian and the
        // forward-difference reference is exact up to rounding.
        let gAD = grad (fun (u: DV) -> DV.Sum(DV.Gather(u, ks) .* DV w)) (DV v) |> DV.toFloats
        let fN (u: float[]) = Seq.init ks.Length (fun i -> u.[ks.[i]] * w.[i]) |> Seq.sum
        let gN = WldMr.Numerics.DiffSharp.Numerical.Float64.DiffOps.grad fN v
        Util.(=~)(gAD, gN)

[<Property>]
let ``Gather equals the CSR selection-matrix formulation exactly``(c0: float[], ks0: int[]) =
    // The in-repo canary for the InterpolateV rewrite (plans/ad-gather.md §7):
    // the gather formulation must produce bit-identical primals and reverse
    // gradients to the selection-CSR one it replaces. `=`, not `=~`, deliberately.
    let c = if c0.Length = 0 then [| 1.0 |] else c0 |> Array.map (fun x -> if Util.IsNice(x) then x % 10. else 1.0)
    let m = c.Length
    if ks0.Length = 0 then true
    else
        // sorted, because the hand-built CSR transpose requires non-decreasing ks;
        // gather itself does not care
        let ks = ks0 |> Array.map (fun k -> abs (k % m)) |> Array.sort
        let n = ks.Length
        // the selection matrix and its transpose, exactly as WldMr.Analytics'
        // InterpolateV builds them
        let csrS =
            { Values = Array.create n 1.; Columns = ks
              RowIndices = Array.init (n + 1) id; NCols = m }
        let csrST =
            let colIndices = Array.zeroCreate (m + 1)
            let mutable ksIdx = 0
            for i in 0 .. m - 1 do
                while ksIdx < n && i >= ks.[ksIdx] do ksIdx <- ksIdx + 1
                colIndices.[i + 1] <- ksIdx
            { Values = Array.create n 1.; Columns = Array.init n id
              RowIndices = colIndices; NCols = n }
        let seed = DV (Array.init n (fun i -> float (i % 7) - 3.0))

        let ca = DV c |> makeReverse WldMr.Numerics.DiffSharp.Util.GlobalTagger.Next
        let yA : DV = DM (SparseDouble (csrS, csrST)) * ca
        yA |> reverseProp seed
        let pA = yA |> primal |> DV.toFloats
        let gA = ca |> adjoint |> DV.toFloats

        let cb = DV c |> makeReverse WldMr.Numerics.DiffSharp.Util.GlobalTagger.Next
        let yB = DV.Gather(cb, ks)
        yB |> reverseProp seed
        let pB = yB |> primal |> DV.toFloats
        let gB = cb |> adjoint |> DV.toFloats

        pA = pB && gA = gB
