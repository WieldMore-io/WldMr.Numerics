// This file is part of DiffSharp: Differentiable Functional Programming - https://diffsharp.github.io
// Copyright (c) 2016-     University of Oxford (Atilim Gunes Baydin <gunes@robots.ox.ac.uk>)
// Copyright (c) 2017-     Microsoft Research, Cambridge, UK (Don Syme <dsyme@microsoft.com>)
// Copyright (c) 2014-     National University of Ireland Maynooth (Barak A. Pearlmutter <barak@pearlmutter.net>)
// Copyright (c) 2014-2016 National University of Ireland Maynooth (Atilim Gunes Baydin)
// This code is licensed under the BSD license (see LICENSE file for details)

module WldMr.Analytics.DiffSharp.Tests.AD.Float64

open FsCheck.NUnit
open WldMr.Analytics.DiffSharp.Tests
open WldMr.Analytics.DiffSharp.AD.Float64

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
