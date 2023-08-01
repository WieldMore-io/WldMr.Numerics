namespace WldMr.Numerics.Fable

open Fable.Core

module System =
  module Double =


    #if FABLE_COMPILER_JAVASCRIPT
    [<Emit("$0 === Number.NEGATIVE_INFINITY")>]
    let private isNegativeInfinityJS (x:double): bool = jsNative

    [<Emit("$0 === Number.POSITIVE_INFINITY")>]
    let private isPositiveInfinityJS (x:double): bool = jsNative
    #endif

    #if FABLE_COMPILER_PYTHON
    [<Import("inf", "math")>]
    let inf : float = nativeOnly

    let isNegativeInfinityPy (x:double): bool = (x = (-inf))

    let isPositiveInfinityPy (x:double): bool = (x = inf)
    #endif

    let IsPositiveInfinity x =
      #if !FABLE_COMPILER
      global.System.Double.IsPositiveInfinity x
      #else
        #if FABLE_COMPILER_PYTHON
        isPositiveInfinityPy x
        #else
        isPositiveInfinityJS x
        #endif
      #endif

    let IsNegativeInfinity x =
      #if !FABLE_COMPILER
      global.System.Double.IsNegativeInfinity x
      #else
        #if FABLE_COMPILER_PYTHON
        isNegativeInfinityPy x
        #else
        isNegativeInfinityJS x
        #endif
      #endif
