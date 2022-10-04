namespace WldMr.Fable
open Fable.Core

module System =
  module Double =


    [<Emit("$0 === Number.NEGATIVE_INFINITY")>]
    let private isNegativeInfinity (x:double): bool = jsNative

    [<Emit("$0 === Number.POSITIVE_INFINITY")>]
    let private isPositiveInfinity (x:double): bool = jsNative

    let IsPositiveInfinity x =
      #if !FABLE_COMPILER
        global.System.Double.IsPositiveInfinity x
      #else
        isPositiveInfinity x
      #endif

    let IsNegativeInfinity x =
      #if !FABLE_COMPILER
        global.System.Double.IsNegativeInfinity x
      #else
        isNegativeInfinity x
      #endif

