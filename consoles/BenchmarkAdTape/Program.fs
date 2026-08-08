module BenchmarkAdTape.Program

open BenchmarkDotNet.Running


/// Depths the `scale` mode sweeps. Doubling each step, so linearity in depth is
/// readable off the column without arithmetic.
let private scaleDepths = [ 1; 2; 4; 8; 16; 32; 64 ]

/// Pillar counts the `width` mode sweeps, at the default depth of 8. Also
/// doubling — this moves the width of every node instead of their number.
let private widthPillars = [ 5; 10; 20; 40; 80; 160; 320 ]

let private usage () =
  printfn "usage:"
  printfn "  dotnet run -c Release                       # census, then the BenchmarkDotNet suite"
  printfn "  dotnet run -c Release -- census [depth=8]   # graph shape and tape census"
  printfn "  dotnet run -c Release -- phases [depth=8] [reps=20]"
  printfn "                                              # forward / reset / push bytes, one tape"
  printfn "  dotnet run -c Release -- scale [reps=10]    # the same split across depths"
  printfn "  dotnet run -c Release -- width [reps=10]    # the same split across node widths"
  printfn "  dotnet run -c Release -- seeds [depth=8] [reps=20]"
  printfn "                                              # one forward, N reverse passes"
  printfn "  dotnet run -c Release -- profile [n=200] [depth=8]"
  printfn "                                              # bare loop, for an external profiler"
  printfn "  dotnet run -c Release -- gather [reps=20]   # InterpolateV shape: CSR vs gather,"
  printfn "                                              # bit-parity asserted"

/// Parse a positive int, or report which argument was wrong.
let private posInt (name: string) (s: string) =
  match System.Int32.TryParse s with
  | true, n when n > 0 -> Some n
  | _ ->
    eprintfn "%s must be a positive int, got %s" name s
    None

[<EntryPoint>]
let main argv =
  // Matched as a list so the variable-arity modes are patterns rather than
  // index arithmetic — same shape as WldMr.Analytics' BenchmarkMarketBuild.
  match List.ofArray argv with
  | [] ->
    Phases.census (Graph.spec 8)
    printfn ""
    BenchmarkRunner.Run<Benchmarks.AdTapeBenchmark>() |> ignore
    BenchmarkRunner.Run<Benchmarks.GatherBenchmark>() |> ignore
    0
  | [ "census" ] -> Phases.census (Graph.spec 8); 0
  | [ "census"; d ] ->
    match posInt "depth" d with
    | Some d -> Phases.census (Graph.spec d); 0
    | None -> 1
  | [ "phases" ] -> Phases.phases (Graph.spec 8) 20; 0
  | [ "phases"; d ] ->
    match posInt "depth" d with
    | Some d -> Phases.phases (Graph.spec d) 20; 0
    | None -> 1
  | [ "phases"; d; r ] ->
    match posInt "depth" d, posInt "reps" r with
    | Some d, Some r -> Phases.phases (Graph.spec d) r; 0
    | _ -> 1
  | [ "scale" ] -> Phases.scale scaleDepths 10; 0
  | [ "scale"; r ] ->
    match posInt "reps" r with
    | Some r -> Phases.scale scaleDepths r; 0
    | None -> 1
  | [ "width" ] -> Phases.width widthPillars 10; 0
  | [ "width"; r ] ->
    match posInt "reps" r with
    | Some r -> Phases.width widthPillars r; 0
    | None -> 1
  | [ "seeds" ] -> Phases.seedPasses (Graph.spec 8) 20; 0
  | [ "seeds"; d ] ->
    match posInt "depth" d with
    | Some d -> Phases.seedPasses (Graph.spec d) 20; 0
    | None -> 1
  | [ "seeds"; d; r ] ->
    match posInt "depth" d, posInt "reps" r with
    | Some d, Some r -> Phases.seedPasses (Graph.spec d) r; 0
    | _ -> 1
  | [ "gather" ] -> Phases.gather 20; 0
  | [ "gather"; r ] ->
    match posInt "reps" r with
    | Some r -> Phases.gather r; 0
    | None -> 1
  | [ "profile" ] -> Phases.profile (Graph.spec 8) 200; 0
  | [ "profile"; n ] ->
    match posInt "n" n with
    | Some n -> Phases.profile (Graph.spec 8) n; 0
    | None -> 1
  | [ "profile"; n; d ] ->
    match posInt "n" n, posInt "depth" d with
    | Some n, Some d -> Phases.profile (Graph.spec d) n; 0
    | _ -> 1
  | _ ->
    usage ()
    1
