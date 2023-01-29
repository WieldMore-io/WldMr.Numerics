// This file is part of DiffSharp: Differentiable Functional Programming - https://diffsharp.github.io
// Copyright (c) 2016-     University of Oxford (Atilim Gunes Baydin <gunes@robots.ox.ac.uk>)
// Copyright (c) 2017-     Microsoft Research, Cambridge, UK (Don Syme <dsyme@microsoft.com>)
// Copyright (c) 2014-     National University of Ireland Maynooth (Barak A. Pearlmutter <barak@pearlmutter.net>)
// Copyright (c) 2014-2016 National University of Ireland Maynooth (Atilim Gunes Baydin)
// Copyright (c) 2020-2021 Pierre-Yves Rivaille - Wieldmore IM Ltd
// This code is licensed under the BSD license (see LICENSE file for details)

module WldMr.Numerics.DiffSharp.Tests.Backend

open NUnit.Framework
open FsCheck.NUnit
open WldMr.Numerics.DiffSharp
open WldMr.Numerics.DiffSharp.Util
open WldMr.Numerics.DiffSharp.Tests
open WldMr.Numerics.LinAlg


//
// Random tests
//

// float64
[<Property>]
let ``Lite.Backend.Mul_Dot_V_V``(v:float[]) =
    let r = v |> Array.map (fun x -> x * x) |> Array.sum
    Util.(=~)(Lite.Backend.Mul_Dot_V_V(v, v), r)

[<Property>]
let ``Lite.Backend.L1Norm_V``(v:float[]) =
    let r = v |> Array.map abs |> Array.sum
    Util.(=~)(Lite.Backend.L1Norm_V(v), r)

[<Property>]
let ``Lite.Backend.L2Norm_V``(v:float[]) =
    let r = v |> Array.map (fun x -> x * x) |> Array.sum |> sqrt
    Util.(=~)(Lite.Backend.L2Norm_V(v), r)

[<Property>]
let ``Lite.Backend.Sum_V``(v:float[]) =
    let r = v |> Array.sum
    Util.(=~)(Lite.Backend.Sum_V(v), r)

[<Property>]
let ``Lite.Backend.Sum_M``(m:float[,]) =
    let mutable r = 0.
    for j = 0 to (Array2D.length2 m) - 1 do
      for i = 0 to (Array2D.length1 m) - 1 do
        r <- r + m.[i, j]
    let m' = m |> GenMat.ofArray2D
    Util.(=~)(Lite.Backend.Sum_M(m'), r)


[<Property>]
let ``Lite.Backend.Add_V_V``(v:float[]) =
    let r = Array.map2 (+) v v
    Util.(=~)(Lite.Backend.Add_V_V(v, v), r)

[<Property>]
let ``Lite.Backend.Add_S_V``(s:float, v:float[]) =
    let r = v |> Array.map ((+) s)
    Util.(=~)(Lite.Backend.Add_S_V(s, v), r)

[<Property>]
let ``Lite.Backend.Sub_V_V``(v:float[]) =
    let r = v |> Array.map (fun x -> x - x)
    Util.(=~)(Lite.Backend.Sub_V_V(v, v), r)

[<Property>]
let ``Lite.Backend.Sub_S_V``(s:float, v:float[]) =
    let r = v |> Array.map (fun x -> s - x)
    Util.(=~)(Lite.Backend.Sub_S_V(s, v), r)

[<Property>]
let ``Lite.Backend.Sub_V_S``(v:float[], s:float) =
    let r = v |> Array.map (fun x -> x - s)
    Util.(=~)(Lite.Backend.Sub_V_S(v, s), r)

[<Property>]
let ``Lite.Backend.Mul_S_V``(s:float, v:float[]) =
    if not (Util.IsNice(s)) then
        true
    else
        let r =
            if (s = 0.) then
                Array.zeroCreate v.Length
            else
                v |> Array.map (fun x -> s * x)
        Util.(=~)(Lite.Backend.Mul_S_V(s, v), r)

[<Property>]
let ``Lite.Backend.Diagonal_M``(m:float[,]) =
    let n = min (Array2D.length1 m) (Array2D.length2 m)
    let r = Array.init n (fun i -> m.[i, i])
    Util.(=~)(Lite.Backend.Diagonal_M(m |> GenMat.ofArray2D), r)

[<Property>]
let ``Lite.Backend.Map_F_V``(v:float[]) =
    let f (x:float) = sin (exp x)
    let r = v |> Array.map f
    Util.(=~)(Lite.Backend.Map_F_V(f, v), r)

[<Property>]
let ``Lite.Backend.Map2_F_V_V``(v:float[]) =
    let f (x1:float) (x2:float) = sin (exp x1) + cos x2
    let r = Array.map2 f v v
    Util.(=~)(Lite.Backend.Map2_F_V_V(f, v, v), r)

[<Ignore("Not implemented")>]
[<Property>]
let ``Lite.Backend.ReshapeCopy_MRows_V``(m:float[,]) =
    let r = Array.zeroCreate m.Length
    let mutable ri = 0
    for i = 0 to (Array2D.length1 m) - 1 do
        for j = 0 to (Array2D.length2 m) - 1 do
            r.[ri] <- m.[i, j]
            ri <- ri + 1
    Util.(=~)(Lite.Backend.ReshapeCopy_MRows_V(m), r)

[<Property>]
let ``Lite.Backend.Mul_Out_V_V``(v1:float[], v2:float[]) =
    if ((v1 |> Util.IsNice) && (v2 |> Util.IsNice)) |> not then true else
    let r =
        if (v1.Length = 0) || (v2.Length = 0) then
            GenMat.empty
        else
            let rr = Mat.zeroCreate v1.Length v2.Length
            for i = 0 to v1.Length - 1 do
                for j = 0 to v2.Length - 1 do
                    rr.[i, j] <- v1.[i] * v2.[j]
            rr |> ColMajor
    Util.(=~)(Lite.Backend.Mul_Out_V_V(v1, v2), r)

[<Property>]
let ``Lite.Backend.Add_M_M``(m:float[,]) =
    let m' = GenMat.ofArray2D m
    let r' =  m' |> GenMat.map (fun x -> 2. * x)
    Util.(=~)(Lite.Backend.Add_M_M(m', m'), r')

[<Property>]
let ``Lite.Backend.Add_S_M``(s:float, m_:float[,]) =
    let m = m_ |> GenMat.ofArray2D
    let r =
        if Array2D.isEmpty m_ then
            GenMat.empty
        else
            m |> GenMat.map (fun x -> s + x)
    Util.(=~)(Lite.Backend.Add_S_M'(s, m), r)

[<Property>]
let ``Lite.Backend.Add_V_MCols``(v:float[]) =
    let m = v.Length
    let n = 3
    let r =
        if m = 0 then
            GenMat.empty
        else
//        let rr =
          Mat.init v.Length n (fun i _ -> v.[i])
          |> ColMajor
//        let rr = Array2D.zeroCreate v.Length n
//        for i = 0 to m - 1 do
//            for j = 0 to n - 1 do
//                rr.[i, j] <- v.[i]
//        rr
    Util.(=~)(Lite.Backend.Add_V_MCols'(v, Mat.zeroCreate m n |> ColMajor), r)

[<Property>]
let ``Lite.Backend.Sub_M_M``(m:float[,]) =
    if not (Util.IsNice(m)) then
        true
    else
        let m' = GenMat.ofArray2D m
        let r =
            if m.Length = 0 then
                GenMat.empty
            else
                Mat.zeroCreate (Array2D.length1 m) (Array2D.length2 m)
                |> ColMajor
        Util.(=~)(Lite.Backend.Sub_M_M(m', m'), r)

[<Property>]
let ``Lite.Backend.Sub_M_S``(m:float[,], s:float) =
    let m' = GenMat.ofArray2D m
    let r =
        if m.Length = 0 then
            GenMat.empty
        else
            m' |> GenMat.map (fun x -> x - s)
    Util.(=~)(Lite.Backend.Sub_M_S(m', s), r)

[<Property>]
let ``Lite.Backend.Sub_S_M``(s:float, m:float[,]) =
    let m' = GenMat.ofArray2D m
    let r =
        if m.Length = 0 then
            GenMat.empty
        else
            m' |> GenMat.map (fun x -> s - x)
    Util.(=~)(Lite.Backend.Sub_S_M(s, m'), r)

[<Property>]
let ``Lite.Backend.Mul_S_M``(s:float, m:float[,]) =
    if not (Util.IsNice(s)) then
        true
    else
        let m' = m |> GenMat.ofArray2D
        let r =
            if (s = 0.) then
                Mat.zeroCreate (Array2D.length1 m) (Array2D.length2 m) |> ColMajor
            elif m.Length = 0 then
                GenMat.empty
            else
                m' |> GenMat.map (fun x -> s * x)
        Util.(=~)(Lite.Backend.Mul_S_M(s, m'), r)

[<Property>]
let ``Lite.Backend.Mul_Had_M_M``(m:float[,]) =
    let r = m |> Array2D.map (fun x -> x * x) |> GenMat.ofArray2D
    let m' = m |> GenMat.ofArray2D
    Util.(=~)(Lite.Backend.Mul_Had_M_M(m', m'), r)

[<Property>]
let ``Lite.Backend.Transpose_M``(mm:float[,]) =
    let mm' = GenMat.ofArray2D mm
    let m = (Array2D.length1 mm)
    let n = (Array2D.length2 mm)
    let r = Mat.zeroCreate n m
    for i = 0 to n - 1 do
        for j = 0 to m - 1 do
            r.[i, j] <- mm.[j, i]
    let r' = r |> ColMajor
    Util.(=~)(Lite.Backend.Transpose_M(mm'), r')

[<Property>]
let ``Lite.Backend.Map_F_M``(m:float[,]) =
    let f (x:float) = sin (exp x)
    let m' = GenMat.ofArray2D m
    let r = m |> Array2D.map f |> GenMat.ofArray2D
    Util.(=~)(Lite.Backend.Map_F_M(f, m'), r)

[<Property>]
let ``Lite.Backend.Map2_F_M_M``(m:float[,]) =
    let f (x1:float) (x2:float) = sin (exp x1) + cos x2
    let m' = GenMat.ofArray2D m
    let r = Array2D.map2 f m m |> GenMat.ofArray2D
    Util.(=~)(Lite.Backend.Map2_F_M_M(f, m', m'), r)

//
// Hard-coded tests
//

let m64_1 = array2D [[ 0.62406; 2.19092; 1.93734;-7.41726];
                     [ 0.66847; 7.18858; 9.21412; 1.83647];
                     [-9.13892; 3.36902; 4.14575; 3.64308]]
            |> GenMat.ofArray2D
let m64_2 = array2D [[ 0.62406; 2.19092; 1.93734;-7.41726];
                     [ 0.66847; 7.18858; 9.21412; 1.83647];
                     [-9.13892; 3.36902; 4.14575; 3.64308];
                     [-3.38312;-3.78691;-3.85926;-0.00381]]
            |> GenMat.ofArray2D
let m64_3 = array2D [[ 0.62406; 2.19092; 1.93734;-7.41726];
                     [ 2.19092; 7.18858; 9.21412; 1.83647];
                     [ 1.93734; 9.21412; 4.14575;-3.85926];
                     [-7.41726; 1.83647;-3.85926;-0.00381]]
            |> GenMat.ofArray2D
let m64_4 = array2D [[ 9.24230; 51.73230; 58.05327; 6.48088]
                     [-85.19758; 77.22825; 98.64351; 41.80417]
                     [-53.66380; 4.36692; 16.46500; 89.06226]]
            |> GenMat.ofArray2D
let m64_5 = array2D [[4.25136; 46.74136; 53.06233; 1.48994]
                     [-85.5446; 76.88123; 98.29649; 41.45715]
                     [-47.68089; 10.34983; 22.44791; 95.04517]
                     [24.47297; -53.78882; -63.5988; -2.08733]]
            |> GenMat.ofArray2D
let m64_6 = array2D [[-0.03792; 0.02867;-0.09172;-0.04912]
                     [ 0.02512;-0.68486; 0.39513;-1.19806]
                     [ 0.00872; 0.64692;-0.30733; 0.95965]
                     [-0.12831;-0.03091; 0.02872;-0.10736]]
            |> GenMat.ofArray2D
let m64_7 = array2D [[-4.99094;-0.34702]
                     [ 5.98291;-6.16668]]
            |> GenMat.ofArray2D
let v64_1 =         [|-4.99094;-0.34702; 5.98291;-6.16668|]
let v64_2 =         [|53.45586; 37.97145; 46.78062|]
let v64_3 =         [|-368.78194; 547.68320; 647.37647; -156.33702|]
let v64_4 =         [|-0.06652; 9.86439;-8.02472; 1.48504|]
let v64_5 =         [| 2.04706; 1.31825;-1.70990; 0.78788|]
let s64_1 = 556.04485

//let m32_1 = m64_1 |> Array2D.map float32
//let m32_2 = m64_2 |> Array2D.map float32
//let m32_3 = m64_3 |> Array2D.map float32
//let m32_4 = m64_4 |> Array2D.map float32
//let m32_5 = m64_5 |> Array2D.map float32
//let m32_6 = m64_6 |> Array2D.map float32
//let m32_7 = m64_7 |> Array2D.map float32
//let v32_1 = v64_1 |> Array.map float32
//let v32_2 = v64_2 |> Array.map float32
//let v32_3 = v64_3 |> Array.map float32
//let v32_4 = v64_4 |> Array.map float32
//let v32_5 = v64_5 |> Array.map float32
//let s32_1 = s64_1 |> float32


// float64
[<Property>]
let ``Lite.Backend.Mul_M_V``() =
    Util.(=~)(Lite.Backend.Mul_M_V(m64_1, v64_1), v64_2)

[<Property>]
let ``Lite.Backend.Mul_V_M``() =
    Util.(=~)(Lite.Backend.Mul_V_M(v64_2, m64_1), v64_3)

[<Property>]
let ``Lite.Backend.Solve_M_V``() =
    match Lite.Backend.Solve_M_V(m64_2, v64_1) with
    | Some(s) -> Util.(=~)(s, v64_4)
    | _ -> false

[<Ignore("Not implemented")>]
[<Property>]
let ``Lite.Backend.SolveSymmetric_M_V``() =
    match Lite.Backend.SolveSymmetric_M_V'(m64_3, v64_1) with
    | Some(s) -> Util.(=~)(s, v64_5)
    | _ -> false

[<Property>]
let ``Lite.Backend.Mul_M_M``() =
    Util.(=~)(Lite.Backend.Mul_M_M(m64_1, m64_2), m64_4)

[<Ignore("Not implemented")>]
[<Property>]
let ``Lite.Backend.Mul_M_M_Add_V_MCols``() =
    Util.(=~)(Lite.Backend.Mul_M_M_Add_V_MCols
                (m64_2 |> GenMat.toMat |> Mat.toArray2D,
                 m64_2 |> GenMat.toMat |> Mat.toArray2D,
                 v64_1)
                , m64_5 |> GenMat.toMat |> Mat.toArray2D)

[<Ignore("Not implemented")>]
[<Property>]
let ``Lite.Backend.Inverse_M``() =
    match Lite.Backend.Inverse_M'(m64_2) with
    | Some(s) -> Util.(=~)(s, m64_6)
    | _ -> false

[<Ignore("Not implemented")>]
[<Property>]
let ``Lite.Backend.Det_M``() =
    match Lite.Backend.Det_M'(m64_2) with
    | Some(s) -> Util.(=~)(s, s64_1)
    | _ -> false

[<Ignore("Not implemented")>]
[<Property>]
let ``Lite.Backend.ReshapeCopy_V_MRows``() =
    Util.(=~)(Lite.Backend.ReshapeCopy_V_MRows'(2, v64_1), m64_7)

[<Property>]
let ``Lite.Backend.RepeatReshapeCopy_V_MRows``() =
    let m = 2
    let n = v64_1.Length
    let r = Mat.zeroCreate m n
    for i = 0 to m - 1 do
        for j = 0 to n - 1 do
            r.[i, j] <- v64_1.[j]
    Util.(=~)(Lite.Backend.RepeatReshapeCopy_V_MRows'(m, v64_1), r |> ColMajor)

[<Property>]
let ``Lite.Backend.RepeatReshapeCopy_V_MCols``() =
    let m = v64_1.Length
    let n = 2
    let r = Mat.zeroCreate m n
    for i = 0 to m - 1 do
        for j = 0 to n - 1 do
            r.[i, j] <- v64_1.[i]
    Util.(=~)(Lite.Backend.RepeatReshapeCopy_V_MCols'(n, v64_1), r |> ColMajor)

[<Test>]
let ``Smoke test``() =
    ()
