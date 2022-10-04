// This file is part of DiffSharp: Differentiable Functional Programming - https://diffsharp.github.io
// Copyright (c) 2016-     University of Oxford (Atilim Gunes Baydin <gunes@robots.ox.ac.uk>)
// Copyright (c) 2017-     Microsoft Research, Cambridge, UK (Don Syme <dsyme@microsoft.com>)
// Copyright (c) 2014-     National University of Ireland Maynooth (Barak A. Pearlmutter <barak@pearlmutter.net>)
// Copyright (c) 2014-2016 National University of Ireland Maynooth (Atilim Gunes Baydin)
// This code is licensed under the BSD license (see LICENSE file for details)

namespace WldMr.Analytics.DiffSharp.Config


/// Record type holding configuration parameters
type Config =
    {
     Float64Epsilon : float
     Float64EpsilonRec : float
     Float64EpsilonRec2 : float
     Float64FixedPointEpsilon : float
     FixedPointMaxIterations : int
     Float64VisualizationContrast : float
     GrayscalePalette : string[]}

/// Global configuration
type GlobalConfig() =
    static let GrayscalePaletteUnicode = [|" "; "·"; "-"; "▴"; "▪"; "●"; "♦"; "■"; "█"|]
    static let GrayscalePaletteASCII = [|" "; "."; ":"; "x"; "T"; "Y"; "V"; "X"; "H"; "N"; "M"|]
    static let mutable C =
        let eps = 0.00001
        let fpeps = 0.01
        {

         Float64Epsilon = eps
         Float64EpsilonRec = 1. / eps
         Float64EpsilonRec2 = 0.5 / eps
         Float64FixedPointEpsilon = fpeps
         FixedPointMaxIterations = 100
         Float64VisualizationContrast = 1.2
         GrayscalePalette = GrayscalePaletteUnicode}

    static member Float64Epsilon = C.Float64Epsilon
    static member Float64EpsilonRec = C.Float64EpsilonRec
    static member Float64EpsilonRec2 = C.Float64EpsilonRec2
    static member Float64FixedPointEpsilon = C.Float64FixedPointEpsilon
    static member FixedPointMaxIterations = C.FixedPointMaxIterations
    static member Float64VisualizationContrast = C.Float64VisualizationContrast

    static member GrayscalePalette = C.GrayscalePalette

    static member SetEpsilon(e:float32) =
        C <- {C with
                Float64Epsilon = float e
                Float64EpsilonRec = 1. / (float e)
                Float64EpsilonRec2 = 0.5 / (float e)}

    static member SetEpsilon(e:float) =
        C <- {C with
                Float64Epsilon = e
                Float64EpsilonRec = 1. / e
                Float64EpsilonRec2 = 0.5 / e}

    static member SetFixedPointEpsilon(e:float32) =
        C <- {C with
                Float64FixedPointEpsilon = float e}

    static member SetFixedPointEpsilon(e:float) =
        C <- {C with
                Float64FixedPointEpsilon = e}

    static member SetFixedPointMaxIterations(i:int) =
        C <- {C with FixedPointMaxIterations = i}

    static member SetVisualizationContrast(c:float32) =
        C <- {C with
                Float64VisualizationContrast = float c}

    static member SetVisualizationContrast(c:float) =
        C <- {C with
                Float64VisualizationContrast = c}

    static member SetVisualizationPalette(palette:string) =
        match palette with
        | "ASCII" ->
            C <- {C with
                    GrayscalePalette = GrayscalePaletteASCII}
        | "Unicode" ->
            C <- {C with
                    GrayscalePalette = GrayscalePaletteUnicode}
        | _ -> invalidArg "" "Unsupported palette. Try: ASCII or Unicode"
