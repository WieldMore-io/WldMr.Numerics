# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo is

Two NuGet packages of pure F# numerics that must compile **three ways**: .NET
(`netstandard2.0` libs, `net8.0` tests), Fable → JavaScript, and Fable → Python.

- `src/WldMr.Numerics.LinAlg` — BLAS-inspired dense/sparse linear algebra for small matrices, plus `Erf` special functions.
- `src/WldMr.Numerics.DiffSharp` — fork of DiffSharp 0.8.3 (functional automatic differentiation), re-backed onto `LinAlg` instead of native BLAS/LAPACK.

Both `.fsproj`s ship their `.fs` sources in the nupkg under `fable/`, so Fable consumers
compile from source. That makes the multi-target constraint non-negotiable: a change that
only builds on .NET breaks downstream Fable apps at *their* compile time, not here.

## Commands

```bash
dotnet tool restore && dotnet paket restore   # first time, and after paket.dependencies changes

dotnet test                                                          # all three test projects
dotnet test tests/WldMr.Numerics.LinAlg.Tests                        # one project
dotnet test tests/ExpectoTests --filter "FullyQualifiedName~D exp"   # one test, any project

npm install && npm test   # Fable → JS (`pretest` compiles into js-build/), or `npm run watch`
npx mocha js-build/tests/ExpectoTests/Main.fs.js -g "D exp"   # one test

dotnet fable tests/ExpectoTests --lang python -o py-build/tests/ExpectoTests \
  && python3 py-build/tests/ExpectoTests/main.py               # Fable → Python

dotnet pack -c Release -o <out>
```

`dotnet test --filter` is the reliable single-test path. Running an Expecto suite directly
(`dotnet run --project ... -- --filter-test-case X`) works for `WldMr.Numerics.LinAlg.Tests`
but **silently ignores args for `ExpectoTests`**, whose `Main.fs` calls
`runTestsWithCLIArgs [] [||] all` — the hardcoded `[||]` drops `argv`.

`TestPython.ps1` / `WatchPython.ps1` hold the Python commands but are Windows-flavoured
(`python`, backslash paths), and there is no `pwsh` on the Linux dev VMs. `Build.ps1` is
stale — it still cds into `tests/MochaTests`, which no longer exists.

## Multi-target conventions

Guard with `#if !FABLE_COMPILER` (.NET), `#if FABLE_COMPILER_JAVASCRIPT`, and
`#if FABLE_COMPILER_PYTHON`. Established patterns to follow rather than reinvent:

- **Spans** — `LiteBlas.fs` defines `VSpan<'a>`/`VRoSpan<'a>`/`WSpan`: aliases for `System.Span`/`ReadOnlySpan` on .NET, and a hand-rolled `ManualSpan` (array + offset) under Fable. Never use `System.Span` directly in shared code; go through `WSpan.span`/`slice`/`rospan`/`roslice`.
- **SIMD** — `System.Numerics.Vector` fast paths live behind `#if !FABLE_COMPILER` with a scalar loop as the `else` (see `Blas.daxpy`).
- **Parallelism** — `DiffSharp/Util.fs` shims `Parallel.For` and `Array.Parallel` to sequential under Fable.
- **BCL gaps** — `System.Array.Clear`, `Array2D`, reflection and quotations are unavailable or unsupported under Fable. `Array2D` helpers in `Util.fs` are entirely `#if !FABLE_COMPILER`; `Symbolic.Float64.fs` (quotation-based symbolic diff, needs `FSharp.Quotations.Evaluator`) becomes an empty `WldMr.Numerics.DiffSharp.NotAvailable.Float64` module under Fable. `LinAlg/FableExt.fs` is where per-runtime BCL patches go (e.g. `Double.IsPositiveInfinity` is broken in Fable/Python).
- **`Fable.Mocha` is not in `paket.references`.** It arrives via a conditional `PackageReference` in `WldMr.Numerics.LinAlg.fsproj` gated on `'$(FABLE_COMPILER_JAVASCRIPT)' == 'true'`. `Fable.Pyxpecto` comes from paket normally. Leave the `<!-- LocalWldMrDependencies -->` marker comment in that fsproj alone.
- Compile order is explicit in every `.fsproj` (`<Compile Include=...>`, no globs) — new files must be added there, in dependency order.

## Test layout

- `tests/ExpectoTests` — **the only suite Fable compiles**, so cross-runtime tests go here; register new lists in `all` in `Main.fs`, which dispatches to Expecto / `Fable.Mocha` / `Fable.Pyxpecto` per define. `MochaFlip.fs` re-implements the `Expecto.Flip.Expect` surface for the Fable targets (plus a dummy `TestsAttribute`), so a new assertion helper must be added to both of its branches. Tests are **Flip style** — message first, expected next, actual piped in: `actual |> Expect.floatClose "msg" accuracy expected`.
- `tests/WldMr.Numerics.LinAlg.Tests` — .NET only. Expecto + FsCheck, with custom `Gen`s (`genSquareMatrix`, `genUpperTriangular`, …) registered through a `MatGenerator` arbitrary.
- `tests/WldMr.Numerics.DiffSharp.Checks` — .NET only. FsCheck.NUnit `[<Property>]` comparing the Lite backend against reference implementations. Six are `[<Ignore("Not implemented")>]` (`Det_M`, `Inverse_M`, `SolveSymmetric_M_V`, reshape/mul variants) — expect them in the skipped count.

CI runs the JS and .NET tests; **the Python target is not in CI**.

## Architecture notes

### LinAlg

- `Mat` is **column-major**: `{ Data: float[]; NRows; NCols }` with `Item(i,j) = Data[i + j*NRows]`. `MatT<'T>` is the generic twin (same layout) that carries `D` values for the AD library. Both have `ToCsv()`/`ToFSharp()` for dumping literals into tests.
- `CsrMat` / `CscMat` are a mutually-recursive `[<Struct>]` pair; `Transpose()` on one produces the other by reinterpreting the same arrays.
- `GenMat` unifies the representations — `ColMajor | TrColMajor of Mat | SparseDouble of CsrMat * CsrMat` — so transpose is free and sparse mat-vec avoids densifying. Many `GenMat` ops are still `failwith "todo"` for the non-`ColMajor` cases; check before relying on one.
- `Blas` in `LiteBlas.fs` keeps original BLAS/LAPACK names and argument order (`daxpy`, `dscal`, `ddot`, `dger`, `dgemv`, `dgemm`, `dtrsm`, `idamax`, and `dlaswp`/`dgetrs` for the LU solve path).
- `Erf.fs` is generated — its coefficient tables were transcribed from MathNet.Numerics' `SpecialFunctions` by `scripts/MathNetParse.fsx`. Edit the script, not the tables, for bulk changes.

### DiffSharp fork

- `AD.Lite.fs` (~4200 lines) is the core. `D`/`DV`/`DM` each have three cases: primal (`D`), forward dual (`DF(primal, tangent, tag)`), and reverse node (`DR(primal, NodeState, TraceOp, tag)`, where `NodeState` is one mutable object holding the accumulated adjoint and the fan-out counter); `uint32` tags from `GlobalTagger` prevent perturbation confusion when nesting. `DOps` and `DiffOps` are `[<AutoOpen>]`, so `diff`, `grad`, `jacobian*`, `hessian*`, `laplacian`, `curl`, `div` and their `'`-primed value-returning variants come in with the module.
- `Backend.Lite.fs` (`WldMr.Numerics.DiffSharp.Lite.Backend`) is the array/matrix layer the AD types call into, delegating to `LinAlg`. Numeric behaviour changes belong here, not in `AD.Lite.fs`.
- `Numerical.Float64.fs` is finite differences, stepping by `GlobalConfig.Float64Epsilon`; `Config.fs` holds the mutable global epsilons.
- Forked files keep their license headers, `#nowarn "77" "1182"` and `// fsharplint:disable` lines — those are load-bearing for licensing and for the build, not stylistic. The 4-space indentation is upstream's and is worth keeping for consistency *within* those files, but it is only a convention: **diffability against upstream DiffSharp is no longer a constraint** — 0.8.3 was rewritten upstream shortly after the fork, so there is no merge to take and never will be. Restructuring a forked file is allowed when a change earns it. (The repo's own `.editorconfig` style applies everywhere else.)

## Build settings

- `src/Directory.Build.props` sets `<WarningsAsErrors>FS0025</WarningsAsErrors>` — an incomplete pattern match fails the build in `src/`. `tests/Directory.Build.props` sets `GenerateProgramFile=false`; every test project supplies its own `[<EntryPoint>]`.
- Package version comes from CI only (`azure-pipelines.yml` composes `major.minor` variables with a counter); the checked-in `<Version>` is a placeholder.
