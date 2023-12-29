module MochaFlip

open WldMr.Numerics.DiffSharp.AD.Float64

#if !FABLE_COMPILER

open Expecto.Flip

#else

type Accuracy = { absolute: float; relative: float }

module Accuracy =
  let inline areCloseLhs a b = abs(a-b)
  let inline areCloseRhs m a b = m.absolute + m.relative * max (abs a) (abs b)
  let inline areClose m a b = areCloseLhs a b <= areCloseRhs m a b
  let low = {absolute=1e-6; relative=1e-3}
  let medium = {absolute=1e-8; relative=1e-5}
  let high = {absolute=1e-10; relative=1e-7}
  let veryHigh = {absolute=1e-12; relative=1e-9}

module Expect =
  let floatClose message accuracy expected actual =
    if System.Double.IsInfinity actual then
      failwithf "%s. Expected actual to not be infinity, but it was." message
    elif System.Double.IsInfinity expected then
      failwithf "%s. Expected expected to not be infinity, but it was." message
    elif Accuracy.areClose accuracy actual expected |> not then
      failwithf
        "%s. Expected difference to be less than %.20g for accuracy {absolute=%.20g; relative=%.20g}, but was %.20g. actual=%.20g expected=%.20g"
        message (Accuracy.areCloseRhs accuracy actual expected)
        accuracy.absolute accuracy.relative
        (Accuracy.areCloseLhs actual expected)
        actual expected

  let allEqual message v xs =
    if xs |> Seq.exists ( (<>) v) then
      failwithf "%s. Not all values are equal to %A" message v



#endif


#if FABLE_COMPILER_PYTHON
  let inline equal message x y =
    Fable.Pyxpecto.Expect.equal x y message
#endif
#if FABLE_COMPILER_JAVASCRIPT
  let isTrue message x =
    Fable.Mocha.Expect.isTrue x message

  let inline equal message x y =
    Fable.Mocha.Expect.equal x y message
#endif


[<AutoOpen>]
module WldMrExpect =

  [<RequireQualifiedAccess>]
  module Expect =
    let dfloatClose message accuracy expected actual =
      Expect.floatClose message accuracy expected (D.toFloat actual)

    let wantOk message x =
      match x with
      | Ok x -> x
      | Result.Error x ->
          failwithf "%s. Expected Ok, was Error(%A)." message x

    let wantSome message x =
      match x with
      | Some x -> x
      | None ->
          failwithf "%s. Expected Some, was None." message

    let isOk message x =
      match x with
      | Ok x -> ()
      | Result.Error x ->
          failwithf "%s. Expected Ok, was Error(%A)." message x
    let isSome message x =
      match x with
      | Some x -> ()
      | None ->
          failwithf "%s. Expected Some, was None." message
