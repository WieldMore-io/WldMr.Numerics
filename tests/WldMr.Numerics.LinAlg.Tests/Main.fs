module ExpectoTests
open Expecto
open Expecto.Logging

[<EntryPoint>]
let main argv =
  let config =
    {
      defaultConfig
      with
        runInParallel = true
        verbosity = LogLevel.Verbose
//        fsCheckEndSize = Some 4096
//        fsCheckMaxTests = 10000
    }

  Tests.runTestsInAssembly config argv
