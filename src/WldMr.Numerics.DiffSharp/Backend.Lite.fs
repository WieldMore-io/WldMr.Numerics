// This file is part of DiffSharp: Differentiable Functional Programming - https://diffsharp.github.io
// Copyright (c) 2016-     University of Oxford (Atilim Gunes Baydin <gunes@robots.ox.ac.uk>)
// Copyright (c) 2017-     Microsoft Research, Cambridge, UK (Don Syme <dsyme@microsoft.com>)
// Copyright (c) 2014-     National University of Ireland Maynooth (Barak A. Pearlmutter <barak@pearlmutter.net>)
// Copyright (c) 2014-2016 National University of Ireland Maynooth (Atilim Gunes Baydin)
// This code is licensed under the BSD license (see LICENSE file for details)

#nowarn "9"
#nowarn "51"
// fsharplint:disable ParameterNames PublicValuesNames

namespace WldMr.Analytics.DiffSharp

open System
open WldMr.Analytics.DiffSharp.Util
open WldMr.Analytics.LinAlg

module Lite =

  type Backend =
    static member inline Add_V_V(x: float[], y: float[]) =
        let xl = x.Length
        let yl = y.Length
        if xl = 0 then
            Array.copyFast y
        elif yl = 0 then
            Array.copyFast x
        elif xl <> yl then
            ErrorMessages.InvalidArgVV()
        else
            Array.init xl (fun i -> x.[i] + y.[i])

    // BLAS - in place addition
    static member inline Add_V_V_Inplace(x: float[], y: float[]) =
        let xl = x.Length
        if xl = 0 then ()
        elif xl = y.Length then
            Blas.daxpy(xl, 1.0, x, 1, y, 1)
        else
            ErrorMessages.InvalidArgVV()

    // BLAS
    static member inline Add_S_V(alpha, x: float[]) =
        let xLength = x.Length
        if xLength = 0 then
            Array.empty
        else
            let alpha' = Array.create xLength alpha
            Blas.daxpy(xLength, 1.0, x, 1, alpha', 1)
            alpha'

    // BLAS
    static member inline Mul_S_V(alpha, x: float[]) =
        if Array.isEmpty x then
            Array.empty
        else
            let xLength = x.Length
            let x' = Array.zeroCreate xLength
            for i = 0 to xLength - 1 do
              x'.[i] <- alpha * x.[i]
  //                    let x' = Array.copy x
  //                    Buffer.BlockCopy(x, 0, x', 0, xLength * sizeof<float>)
  //                    Blas.dscal(x.Length, alpha, WSpan.span x', 1)
            x'

    // BLAS
    static member inline Sub_V_V(x: float[], y: float[]) =
        let xl = x.Length
        let yl = y.Length
        if xl = 0 then
            Backend.Mul_S_V(-1.0, y)
        elif yl = 0 then
            Array.copyFast x
        elif xl <> yl then
            ErrorMessages.InvalidArgVV()
        else
            let x' = Array.copyFast x
            Blas.daxpy(xl, -1., y, 1, x', 1)
            x'

    // BLAS
    static member inline Mul_Dot_V_V(x: float[], y: float[]) =
        let xl = x.Length
        let yl = y.Length
        if (xl = 0) || (yl = 0) then
            0.
        elif xl <> yl then
            ErrorMessages.InvalidArgVV()
        else
            Blas.ddot(xl, x, 1, y, 1)


    static member inline Mul_Out_V_V(x: float[], y: float[]) =
        let xl = x.Length
        let yl = y.Length
        if (xl = 0) || (yl = 0) then
            GenMat.empty
        else
            let z = Mat.zeroCreate xl yl
            Blas.dger(xl, yl, 1., WSpan.rospan x, 1, WSpan.rospan y, 1, WSpan.span z.Data, xl)
            z |> ColMajor

    // BLAS
    static member inline L1Norm_V(x) =
        if Array.isEmpty x then
            0.
        else
           Blas.dasum(x.Length, x, 1)

    // BLAS
    static member inline L2Norm_V(x) =
        if Array.isEmpty x then
            0.
        else
            Blas.dnrm2(x.Length, x, 1)

    // BLAS
    static member inline SupNorm_V(x) =
        let mutable amax = 0.
        for xi in x do
            amax <- max (abs xi) amax
        amax

    static member inline Add_M_M(x: GenMat, y: GenMat): GenMat =
        GenMat.addM x y

    // BLAS
    static member inline AlphaAdd_M_M_Inplace'(alpha: float, xGM: GenMat, yGM: GenMat) =
      match yGM with
      | ColMajor y ->
          let x = xGM |> GenMat.toMat
          let xl1 = x.NRows
          let xl2 = x.NCols
          let yl1 = y.NRows
          let yl2 = y.NCols
          if xl1 * xl2 = 0 then ()
          elif (xl1 <> yl1) || (xl2 <> yl2) then
            ErrorMessages.InvalidArgMM()
          else
            Stats.InplaceOp(yl1 * yl2)
            Blas.daxpy(xl1*xl2, alpha, x.Data, 1, y.Data, 1)
      | _ -> failwith "Only ColMajor matrices can be updated in place"

    static member inline Add_S_M'(alpha: float, x: GenMat): GenMat =
        GenMat.addS x alpha

    static member inline RepeatReshapeCopy_V_MCols'(n, x) =
      if Array.isEmpty x then
          GenMat.empty
      else
          let m = x.Length
          let r = Mat.zeroCreate m n
          #if !FABLE_COMPILER
          let xbytes = m * sizeof<float>
          #endif
          for j = 0 to n-1 do
            #if FABLE_COMPILER
              // IMPROVE
              for i = 0 to m-1 do
                r.[i, j] <- x.[i]
            #else
              Buffer.BlockCopy(x, 0, r.Data, j * xbytes, xbytes)
            #endif
          r |> ColMajor

    static member inline Add_V_MCols'(x: float[], gm: GenMat): GenMat =
      match gm with
        | ColMajor y ->
            let xl = x.Length
            let yl1 = y.NRows
            let yl2 = y.NCols
            if yl1 * yl2 = 0 then
                GenMat.empty
            elif xl = 0 then
                y |> ColMajor
            elif xl <> yl1 then
                ErrorMessages.InvalidArgVMRows()
            else
                let gm' = Backend.RepeatReshapeCopy_V_MCols'(yl2, x)
                match gm' with
                | ColMajor x' ->
                    Blas.daxpy(yl1 * yl2, 1., y.Data, 1, x'.Data, 1)
                    x' |> ColMajor
                | _ -> failwith "impossible"
        | _ -> failwith "todo"

    static member inline Mul_S_M(alpha, x: GenMat) =
        GenMat.mulS x alpha

    static member inline Sub_M_M(x: GenMat, y: GenMat): GenMat =
        GenMat.subM x y

    static member inline Mul_M_M(x: GenMat, y: GenMat): GenMat =
      GenMat.mulM x y

    // BLAS - TODO
    static member inline Mul_M_M_Add_V_MCols(x, y, z) = failwith "deprecated"
  //                let xl1 = Array2D.length1 x
  //                let xl2 = Array2D.length2 x
  //                let yl1 = Array2D.length1 y
  //                let yl2 = Array2D.length2 y
  //                let zl = z.Length
  //                if zl = 0 then
  //                    Backend.Mul_M_M(x, y)
  //                elif xl1 * xl2 * yl1 * yl2 = 0 then
  //                    Array2D.empty
  //                elif xl2 <> yl1 then
  //                    ErrorMessages.InvalidArgMColsMRows()
  //                elif zl <> xl1 then
  //                    ErrorMessages.InvalidArgVMRows()
  //                else
  //                    let z' = Backend.RepeatReshapeCopy_V_MCols(yl2, z)
  //                    BLAS.dgemm'(1., x, y, 1., z')
  //                    z'

    static member inline Mul_M_V(x: GenMat, y) =
        GenMat.mulV x y

    static member inline Mul_M_V_Add_V'(gm: GenMat, y: float[], z: float[]) =
      match gm with
        | ColMajor x ->
          let xl1 = x.NCols
          let xl2 = x.NRows
          let yl = y.Length
          let zl = z.Length
          if zl = 0 then
              Backend.Mul_M_V(x |> ColMajor, y)
          elif xl1 * xl2 * yl = 0 then
              Array.empty
          elif yl <> xl2 then
              ErrorMessages.InvalidArgVMCols()
          elif zl <> xl1 then
              ErrorMessages.InvalidArgVMRows()
          else
              let z' = Array.copyFast z
              Blas.dgemv('N', x.NRows, x.NCols, 1., x.Data, x.NRows, y, 1, 1., z', 1)
              z'
        | _ -> failwith "todo"

    static member inline Mul_V_M(x: float[], gm: GenMat) =
      match gm with
        | ColMajor y ->
          let xl = x.Length
          let yl1 = y.NRows
          let yl2 = y.NCols
          if xl * yl1 * yl2 = 0 then
              Array.empty
          elif xl <> yl1 then
              ErrorMessages.InvalidArgVMRows()
          else
              let z = Array.zeroCreate yl2
              Blas.dgemv('T', y.NRows, y.NCols,  1., y.Data, y.NRows, x, 1, 1., z, 1)
              z
        | _ -> failwith "todo"

    static member inline Transpose_M(x: GenMat): GenMat =
        GenMat.transpose x

    // LAPACK
    static member inline Solve_M_V(x: GenMat, b: float[]) =
        let m = x.ToMat()
        let xl1 = m.NRows
        let xl2 = m.NCols
        let yl = b.Length
        if xl1 * xl2 * yl = 0 then
            None
        elif xl1 <> yl then
            ErrorMessages.InvalidArgVMRows()
        else
          let size = m.NRows
          let iPiv = Array.create size 0
          let res = Array.copyFast b
          let tempM = Mat.copy m
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
            None
          else
            Some res


    // LAPACK - TODO
    static member inline SolveSymmetric_M_V(x, y) = failwith "deprecated"
  //                let xl1 = Array2D.length1 x
  //                let xl2 = Array2D.length2 x
  //                let yl = y.Length
  //                if xl1 * xl2 * yl = 0 then
  //                    None
  //                elif xl1 <> yl then
  //                    ErrorMessages.InvalidArgVMRows()
  //                else
  //                    let x' = Array2D.copyFast x
  //                    let y' = Array.copyFast y
  //                    LAPACK.dsysv(x', y')

    static member inline SolveSymmetric_M_V'(x, y) = failwith "not implemented"

    // LAPACK - TODO
    static member inline Inverse_M(x) = failwith "deprecated"
  //                if Array2D.isEmpty x then
  //                    Some(Array2D.empty)
  //                else
  //                    let x' = Array2D.copyFast x
  //                    let ipiv = LAPACK.dgetrf(x')
  //                    match ipiv with
  //                    | Some(ipiv) ->
  //                        let inv = LAPACK.dgetri(x', ipiv)
  //                        match inv with
  //                        | Some(inv) -> Some(inv)
  //                        | _ -> None
  //                    | _ -> None

    static member inline Inverse_M'(x) = failwith "not implemented"


    // LAPACK - TODO
    static member inline Det_M(x) = failwith "deprecated"
  //                if Array2D.isEmpty x then
  //                    Some(0.)
  //                else
  //                    let x' = Array2D.copyFast x
  //                    let ipiv = LAPACK.dgetrf(x')
  //                    match ipiv with
  //                    | Some(ipiv) ->
  //                        let n = Array2D.length1 x
  //                        let mutable det = 1.
  //                        for i = 0 to n - 1 do
  //                            if ipiv.[i] <> (i + 1) then
  //                                det <- -det * x'.[i, i]
  //                            else
  //                                det <- det * x'.[i, i]
  //                        Some(det)
  //                    | _ -> None

    static member inline Det_M'(x) = failwith "not implemented"

    // Non-BLAS
    static member inline Map_F_V(f, x) =
        if Array.isEmpty x then
            Array.empty
        else
            Array.map f x

    // Non-BLAS
    static member inline Map_F_M(f: float -> float, x: GenMat) =
        GenMat.map f x

    // Non-BLAS
    static member inline Sub_S_V(alpha, x) =
        if alpha = 0. then
            Backend.Mul_S_V(-1., x)
        else
            Backend.Map_F_V((fun v -> alpha - v), x)

    // Non-BLAS
    static member inline Sub_V_S(x, alpha) =
        if alpha = 0. then
            x
        else
            Backend.Map_F_V((fun v -> v - alpha), x)

    static member inline Sub_S_M(alpha: float, x: GenMat) =
        if alpha = 0. then
            Backend.Mul_S_M(-1., x)
        else
            Backend.Map_F_M((fun v -> alpha - v), x)

    static member inline Sub_M_S(x: GenMat, alpha) =
        if alpha = 0. then
            x
        else
            Backend.Map_F_M((fun v -> v - alpha), x)

    // Non-BLAS
    static member inline Map2_F_V_V(f, x: float[], y: float[]) =
        let xl = x.Length
        let yl = y.Length
        if xl = yl then
            Array.map2 f x y
        elif xl = 0 then
            let x' = Array.zeroCreate yl
            Array.map2 f x' y
        elif yl = 0 then
            let y' = Array.zeroCreate xl
            Array.map2 f x y'
        else
            ErrorMessages.InvalidArgVV()

    static member inline Map2_Mul_V_V(x: float[], y: float[]) =
        let xl = x.Length
        let yl = y.Length
        if xl = yl then
            Array.init xl (fun i -> x.[i] * y.[i])
        elif xl = 0 then
            Array.zeroCreate yl
        elif yl = 0 then
            Array.zeroCreate xl
        else
            ErrorMessages.InvalidArgVV()



    // Non-BLAS
    static member inline Map2_F_M_M(f: float -> float -> float, x: GenMat, y: GenMat) =
        if (x.NRows <> y.NRows) || (x.NCols <> y.NCols) then
            ErrorMessages.InvalidArgMM()
        GenMat.map2 f x y


    // Non-BLAS
    static member inline Sum_V(x) =
        if Array.isEmpty x then
            0.
        else
            Array.sum x

    // Non-BLAS
    static member inline Mul_Had_M_M(x: GenMat, y: GenMat): GenMat =
        if (x.NRows <> y.NRows) || (x.NCols <> y.NCols) then
            ErrorMessages.InvalidArgMM()
        GenMat.map2 (*) x y

    // Non-BLAS
    static member inline Sum_M(x: GenMat) =
        x |> GenMat.sum

    // Non-BLAS
    static member inline Diagonal_M(x: GenMat) =
        let nRows= x.NRows
        let n = min nRows x.NCols
        Array.init n (fun i -> x.[i, i])


    // Non-BLAS - TODO
    static member inline ReshapeCopy_MRows_V(x) = failwith "deprecated"
  //                if Array2D.isEmpty x then
  //                    Array.empty<float>
  //                else
  //                    let r = Array.zeroCreate<float> x.Length
  //                    Buffer.BlockCopy(x, 0, r, 0, x.Length * sizeof<float>)
  //                    r

    static member inline ReshapeCopy_MRows_V' (x: GenMat): float[] =
        failwith "todo"

    // Non-BLAS - TODO
    static member inline ReshapeCopy_V_MRows(m: int, x: float[]): float[,] = failwith "deprecated"
  //                if Array.isEmpty x then
  //                    Array2D.empty<float>
  //                else
  //                    let n = x.Length / m
  //                    let r = Array2D.zeroCreate<float> m n
  //                    Buffer.BlockCopy(x, 0, r, 0, x.Length * sizeof<float>)
  //                    r

    static member inline ReshapeCopy_V_MRows'(m: int, x: float[]): GenMat =
        failwith "todo"


    static member inline RepeatReshapeCopy_V_MRows'(m, x) =
        if Array.isEmpty x then
            GenMat.empty
        else
            // IMPROVE: use dger with inc set to 0.
            let ones = Array.create m 1.
            Backend.Mul_Out_V_V(ones, x)
