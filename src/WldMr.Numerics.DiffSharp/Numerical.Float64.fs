﻿// This file is part of DiffSharp: Differentiable Functional Programming - https://diffsharp.github.io
// Copyright (c) 2016-     University of Oxford (Atilim Gunes Baydin <gunes@robots.ox.ac.uk>)
// Copyright (c) 2017-     Microsoft Research, Cambridge, UK (Don Syme <dsyme@microsoft.com>)
// Copyright (c) 2014-     National University of Ireland Maynooth (Barak A. Pearlmutter <barak@pearlmutter.net>)
// Copyright (c) 2014-2016 National University of Ireland Maynooth (Atilim Gunes Baydin)
// This code is licensed under the BSD license (see LICENSE file for details)

/// Numerical differentiation module
module WldMr.Numerics.DiffSharp.Numerical.Float64

#if !FABLE_COMPILER

open WldMr.Numerics.DiffSharp
open WldMr.Numerics.DiffSharp.Util
open WldMr.Numerics.DiffSharp.Config
open WldMr.Numerics.LinAlg

/// Numerical differentiation operations module (automatically opened)
[<AutoOpen>]
module DiffOps =
    /// First derivative of a scalar-to-scalar function `f`, at point `x`
    let inline diff (f:float->float) x =
        ((f (x + GlobalConfig.Float64Epsilon)) - (f (x - GlobalConfig.Float64Epsilon))) * GlobalConfig.Float64EpsilonRec2

    /// Original value and first derivative of a scalar-to-scalar function `f`, at point `x`
    let inline diff' f x =
        (f x, diff f x)

    /// Gradient-vector product (directional derivative) of a vector-to-scalar function `f`, at point `x`, along vector `v`
    let inline gradv (f:float[]->float) (x:float[]) (v:float[]) =
        let veps = Lite.Backend.Mul_S_V(GlobalConfig.Float64Epsilon, v)
        ((f (Lite.Backend.Add_V_V(x, veps))) - (f (Lite.Backend.Sub_V_V(x, veps)))) * GlobalConfig.Float64EpsilonRec2

    /// Original value and gradient-vector product (directional derivative) of a vector-to-scalar function `f`, at point `x`, along vector `v`
    let inline gradv' f x v =
        (f x, gradv f x v)

    /// Second derivative of a scalar-to-scalar function `f`, at point `x`
    let inline diff2 f x =
        ((f (x + GlobalConfig.Float64Epsilon)) - 2. * (f x) + (f (x - GlobalConfig.Float64Epsilon))) / (GlobalConfig.Float64Epsilon * GlobalConfig.Float64Epsilon)

    /// Original value and second derivative of a scalar-to-scalar function `f`, at point `x`
    let inline diff2' f x =
        (f x, diff2 f x)

    /// Original value, first derivative, and second derivative of a scalar-to-scalar function `f`, at point `x`
    let inline diff2'' f x =
        (f x, diff f x, diff2 f x)

    /// Original value and gradient of a vector-to-scalar function `f`, at point `x`
    let inline grad' (f:float[]->float) x =
        let fx = f x
        let g = Array.create x.Length fx
        let gg = Array.init x.Length (fun i -> f (Lite.Backend.Add_V_V(x, standardBasisVal x.Length i GlobalConfig.Float64Epsilon)))
        (fx, Lite.Backend.Mul_S_V(GlobalConfig.Float64EpsilonRec, Lite.Backend.Sub_V_V(gg, g)))

    /// Gradient of a vector-to-scalar function `f`, at point `x`
    let grad f x =
        grad' f x |> snd

    /// Original value, gradient, and Hessian of a vector-to-scalar function `f`, at point `x`
    let inline gradhessian' f x =
        let (fx, g) = grad' f x
        let h = array2D (Array.create x.Length g) |> GenMat.ofArray2D
        let hh =
          array2D (Array.init x.Length (fun i -> grad f (Lite.Backend.Add_V_V(x, standardBasisVal x.Length i GlobalConfig.Float64Epsilon))))
          |> GenMat.ofArray2D
        (fx, g, Lite.Backend.Mul_S_M(GlobalConfig.Float64EpsilonRec, Lite.Backend.Sub_M_M(hh, h)))

    /// Gradient and Hessian of a vector-to-scalar function `f`, at point `x`
    let inline gradhessian f x =
        gradhessian' f x |> drop1Of3

    /// Original value and Hessian of a vector-to-scalar function `f`, at point `x`
    let inline hessian' f x =
        gradhessian' f x |> drop2Of3

    /// Hessian of a vector-to-scalar function `f`, at point `x`
    let inline hessian f x =
        gradhessian' f x |> p33

    /// Original value and Hessian-vector product of a vector-to-scalar function `f`, at point `x`, along vector `v`
    let inline hessianv' (f:float[]->float) (x:float[]) (v:float[]) =
        let veps = Lite.Backend.Mul_S_V(GlobalConfig.Float64Epsilon, v)
        let fx, gg1 = grad' f (Lite.Backend.Add_V_V(x, veps))
        let gg2 = grad f (Lite.Backend.Sub_V_V(x, veps))
        (fx, Lite.Backend.Mul_S_V(GlobalConfig.Float64EpsilonRec2, Lite.Backend.Sub_V_V(gg1, gg2)))

    /// Hessian-vector product of a vector-to-scalar function `f`, at point `x`, along vector `v`
    let inline hessianv (f:float[]->float) x v =
        hessianv' f x v |> snd

    /// Original value, gradient-vector product (directional derivative), and Hessian-vector product of a vector-to-scalar funtion `f`, at point `x`, along vector `v`
    let inline gradhessianv' (f:float[]->float) x v =
        let fx, gv = gradv' f x v
        let hv = hessianv f x v
        (fx, gv, hv)

    /// Gradient-vector product (directional derivative) and Hessian-vector product of a vector-to-scalar function `f`, at point `x`, along vector `v`
    let inline gradhessianv (f:float[]->float) x v =
        gradhessianv' f x v |> drop1Of3

    /// Original value and Laplacian of a vector-to-scalar function `f`, at point `x`
    let inline laplacian' f x =
        let (v, h) = hessian' f x in (v, Lite.Backend.Sum_V(Lite.Backend.Diagonal_M(h)))

    /// Laplacian of a vector-to-scalar function `f`, at point `x`
    let inline laplacian f x =
        laplacian' f x |> snd

    /// Original value and transposed Jacobian of a vector-to-vector function `f`, at point `x`
    let inline jacobianT' (f:float[]->float[]) x =
        let fx = f x
        let j = array2D (Array.create x.Length fx) |> GenMat.ofArray2D
        let jj =
          array2D (Array.init x.Length (fun i -> f (Lite.Backend.Add_V_V(x, standardBasisVal x.Length i GlobalConfig.Float64Epsilon))))
          |> GenMat.ofArray2D
        (fx, Lite.Backend.Mul_S_M(GlobalConfig.Float64EpsilonRec, Lite.Backend.Sub_M_M(jj, j)))

    /// Transposed Jacobian of a vector-to-vector function `f`, at point `x`
    let inline jacobianT f x =
        jacobianT' f x |> snd

    /// Original value and Jacobian of a vector-to-vector function `f`, at point `x`
    let inline jacobian' f x =
        jacobianT' f x |> fun (r, j) -> (r, Lite.Backend.Transpose_M(j))

    /// Jacobian of a vector-to-vector function `f`, at point `x`
    let inline jacobian f x =
        jacobian' f x |> snd

    /// Jacobian-vector product of a vector-to-vector function `f`, at point `x`, along vector `v`
    let inline jacobianv (f:float[]->float[]) x v =
        let veps = Lite.Backend.Mul_S_V(GlobalConfig.Float64Epsilon, v)
        Lite.Backend.Mul_S_V(GlobalConfig.Float64EpsilonRec2, Lite.Backend.Sub_V_V(f (Lite.Backend.Add_V_V(x, veps)), f (Lite.Backend.Sub_V_V(x, veps))))

    /// Original value and Jacobian-vector product of a vector-to-vector function `f`, at point `x`, along vector `v`
    let inline jacobianv' f x v =
        (f x, jacobianv f x v)

    /// Original value and curl of a vector-to-vector function `f`, at point `x`. Supported only for functions with a three-by-three Jacobian matrix.
    let inline curl' f x =
        let v, j = jacobianT' f x
        if (j.NRows, j.NCols) <> (3, 3) then ErrorMessages.InvalidArgCurl()
        v, [|j.[1, 2] - j.[2, 1]; j.[2, 0] - j.[0, 2]; j.[0, 1] - j.[1, 0]|]

    /// Curl of a vector-to-vector function `f`, at point `x`. Supported only for functions with a three-by-three Jacobian matrix.
    let inline curl f x =
        curl' f x |> snd

    /// Original value and divergence of a vector-to-vector function `f`, at point `x`. Defined only for functions with a square Jacobian matrix.
    let inline div' f x =
        let v, j = jacobianT' f x
        if j.NRows <> j.NCols then ErrorMessages.InvalidArgDiv()
        v, Lite.Backend.Sum_V(Lite.Backend.Diagonal_M(j))

    /// Divergence of a vector-to-vector function `f`, at point `x`. Defined only for functions with a square Jacobian matrix.
    let inline div f x =
        div' f x |> snd

    /// Original value, curl, and divergence of a vector-to-vector function `f`, at point `x`. Supported only for functions with a three-by-three Jacobian matrix.
    let inline curldiv' f x =
        let v, j = jacobianT' f x
        if (j.NRows, j.NCols) <> (3, 3) then ErrorMessages.InvalidArgCurlDiv()
        v, [|j.[1, 2] - j.[2, 1]; j.[2, 0] - j.[0, 2]; j.[0, 1] - j.[1, 0]|], j.[0, 0] + j.[1, 1] + j.[2, 2]

    /// Curl and divergence of a vector-to-vector function `f`, at point `x`. Supported only for functions with a three-by-three Jacobian matrix.
    let inline curldiv f x =
        curldiv' f x |> drop1Of3

#endif
