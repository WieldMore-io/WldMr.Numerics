// This file is part of DiffSharp: Differentiable Functional Programming - https://diffsharp.github.io
// Copyright (c) 2016-     University of Oxford (Atilim Gunes Baydin <gunes@robots.ox.ac.uk>)
// Copyright (c) 2017-     Microsoft Research, Cambridge, UK (Don Syme <dsyme@microsoft.com>)
// Copyright (c) 2014-     National University of Ireland Maynooth (Barak A. Pearlmutter <barak@pearlmutter.net>)
// Copyright (c) 2014-2016 National University of Ireland Maynooth (Atilim Gunes Baydin)
// This code is licensed under the BSD license (see LICENSE file for details)

namespace WldMr.Analytics.LinAlg.Tests


module Array2D =
    let empty<'T> = Array2D.zeroCreate<'T> 0 0
    let isEmpty (array : 'T[,]) = (array.Length = 0)
    let toArray (array : 'T[,]) = array |> Seq.cast<'T> |> Seq.toArray
    let find (predicate : 'T -> bool) (array : 'T[,]) = array |> toArray |> Array.find predicate
    let tryFind (predicate : 'T -> bool) (array : 'T[,]) = array |> toArray |> Array.tryFind predicate
    let map2 f (a1:_[,]) (a2:_[,]) =
        let m = min (Array2D.length1 a1) (Array2D.length1 a2)
        let n = min (Array2D.length2 a1) (Array2D.length2 a2)
        Array2D.init m n (fun i j -> f a1.[i, j] a2.[i, j])


type Util() =
    static let eps64 = 1e-4

    static member IsNice(a:float) = not (System.Double.IsInfinity(a) || System.Double.IsNaN(a) || (a = System.Double.MinValue) || (a = System.Double.MaxValue))
    static member IsNice(a:float[]) =
        match a |> Array.map Util.IsNice |> Array.tryFind not with
        | Some(_) -> false
        | _ -> true
    static member IsNice(a:float[,]) =
        match a |> Array2D.map Util.IsNice |> Array2D.tryFind not with
        | Some(_) -> false
        | _ -> true


    static member (=~) (a:float, b:float) =
        if   System.Double.IsNaN(a) then
             System.Double.IsNaN(b)
        elif System.Double.IsPositiveInfinity(a) || (a = System.Double.MaxValue) then
             System.Double.IsPositiveInfinity(b) || (b = System.Double.MaxValue)
        elif System.Double.IsNegativeInfinity(a) || (a = System.Double.MinValue) then
             System.Double.IsNegativeInfinity(b) || (b = System.Double.MinValue)
        else
             (abs (a - b) < eps64)
               || (abs a > 1. && abs b > 1. && abs ((a-b)/a) < eps64)


    static member (=~) (a:float[], b:float[]) =
        if (a.Length = 0) && (b.Length = 0) then
            true
        elif a.Length <> b.Length then
            false
        else
            match Array.map2 (fun (x:float) (y:float) -> (Util.(=~)(x, y))) a b |> Array.tryFindIndex not with
            | Some _ -> false
            | _ -> true


    static member (=~) (a:float[,], b:float[,]) =
        if (a.Length = 0) && (b.Length = 0) then
            true
        elif ((Array2D.length1 a) <> (Array2D.length1 b)) || ((Array2D.length2 a) <> (Array2D.length2 b))then
            false
        else
            match Array2D.map2 (fun (x:float) (y:float) -> (Util.(=~)(x, y))) a b |> Array2D.tryFind not with
            | Some(_) -> false
            | _ -> true
