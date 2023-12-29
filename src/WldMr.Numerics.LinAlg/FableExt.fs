namespace WldMr.Numerics.Fable


module System =
  module Double =
    let IsPositiveInfinity x =
      #if FABLE_COMPILER_PYTHON
        System.Double.IsInfinity x && (not (System.Double.IsNegativeInfinity x))
      #else
        System.Double.IsPositiveInfinity x
      #endif

    let IsNegativeInfinity x =
      System.Double.IsNegativeInfinity x
