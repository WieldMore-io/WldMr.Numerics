module ExpectoTests


#if FABLE_COMPILER_PYTHON
open Fable.Pyxpecto
#endif
#if FABLE_COMPILER_JAVASCRIPT
open Fable.Mocha
#endif
#if !FABLE_COMPILER
open Expecto
#endif


let all =
  testList "allTests"
    [
      BasicTests.tests
      CscMatTests.tests
    ]

[<EntryPoint>]
let main _args =
  #if FABLE_COMPILER_PYTHON
  Pyxpecto.runTests [||] all
  #endif
  #if FABLE_COMPILER_JAVASCRIPT
  Mocha.runTests all
  #endif
  #if !FABLE_COMPILER
  Tests.runTestsWithCLIArgs [] [||] all
  #endif
// #if FABLE_COMPILER
//   Mocha.runTests all
// #else
//   Tests.runTestsInAssemblyWithCLIArgs [] [||]
// #endif

