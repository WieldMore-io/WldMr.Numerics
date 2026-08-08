module AdjointLifetimeTests

#if FABLE_COMPILER_PYTHON
open Fable.Pyxpecto
#endif
#if FABLE_COMPILER_JAVASCRIPT
open Fable.Mocha
#endif
#if !FABLE_COMPILER
open Expecto
open Expecto.Flip
#else
type TestsAttribute() =
  inherit System.Attribute()
#endif

open WldMr.Numerics.LinAlg
open WldMr.Numerics.DiffSharp.AD.Float64

open MochaFlip

let accuracy = { absolute = 1e-9; relative = 0. }

let nextTag () = WldMr.Numerics.DiffSharp.Util.GlobalTagger.Next

/// `reverseReset` reuses a node's adjoint buffer instead of replacing it, so the
/// value a caller reads out is only safe because `adjoint` hands back a copy.
/// These pin that contract — assert on values, never on reference identity: the
/// two ways it can break look different (a stale row reads zero while push still
/// allocates, and aliases the last row once push accumulates in place) but both
/// are silent. See plans/ad-tape-allocation.md, constraint 3.
[<Tests>]
let tests =
  testList "adjoint lifetime" [

    testCase "DV adjoint survives a later reverse pass over the same node" <| fun _ ->
      // The shape WldMr.Analytics' CurveSolver.ff' uses: one makeReverse hoisted out
      // of the loop, one reverseProp per output over the shared tape, the adjoint
      // read every pass and the rows consumed only after the loop has finished.
      let xa = DV [| 3.0; 5.0 |] |> makeReverse (nextTag ())
      let rows =
        [| (fun (v: DV) -> v.[0] * v.[1]); (fun (v: DV) -> v.[0] + v.[1]) |]
        |> Array.map (fun f ->
            let z = f xa
            z |> reverseProp D.One
            xa |> adjoint |> DV.toFloats
          )
      rows.[0].[0] |> Expect.floatClose "d(x*y)/dx, read after the second pass ran" accuracy 5.0
      rows.[0].[1] |> Expect.floatClose "d(x*y)/dy, read after the second pass ran" accuracy 3.0
      rows.[1].[0] |> Expect.floatClose "d(x+y)/dx" accuracy 1.0
      rows.[1].[1] |> Expect.floatClose "d(x+y)/dy" accuracy 1.0

    testCase "DM adjoint survives a later reverse pass over the same node" <| fun _ ->
      let m = DM (Mat.ofRowsArray [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] |> ColMajor)
      let ma = m |> makeReverse (nextTag ())
      // A different seed per pass: with an in-place push, a same-seed rerun refills
      // an aliased buffer with identical values and the aliasing reads as green.
      let pass (seed: D) =
        let z = DM.Sum(ma .* ma)
        z |> reverseProp seed
        ma |> adjoint
      let g1 = pass D.One
      let g2 = pass (D 3.0)
      // d(s * sum(M .* M))/dM = 2sM
      g1.[0, 0] |> Expect.dfloatClose "first pass [0,0], read after the second ran" accuracy 2.0
      g1.[1, 1] |> Expect.dfloatClose "first pass [1,1], read after the second ran" accuracy 8.0
      g2.[0, 0] |> Expect.dfloatClose "second pass [0,0]" accuracy 6.0
      g2.[1, 1] |> Expect.dfloatClose "second pass [1,1]" accuracy 24.0

    testCase "jacobianTv'' reverse evaluator keeps earlier seeds intact" <| fun _ ->
      // A strict special case of the first test, through the public DiffOps surface.
      let f (v: DV) = DV.ofSeqD [| v.[0] * v.[1]; v.[0] + v.[1] |]
      let _, r2 = jacobianTv'' f (DV [| 3.0; 5.0 |])
      let row0 = r2 (DV [| 1.0; 0.0 |]) |> DV.toFloats
      let row1 = r2 (DV [| 0.0; 1.0 |]) |> DV.toFloats
      row0.[0] |> Expect.floatClose "seed 0 [0], read after seed 1 ran" accuracy 5.0
      row0.[1] |> Expect.floatClose "seed 0 [1], read after seed 1 ran" accuracy 3.0
      row1.[0] |> Expect.floatClose "seed 1 [0]" accuracy 1.0
      row1.[1] |> Expect.floatClose "seed 1 [1]" accuracy 1.0

    testCase "a node's adjoint is lazy — empty until a reverse pass materialises it" <| fun _ ->
      // `DV.R`/`DM.R` seed the node's adjoint with the shared empty sentinel; the
      // full-length buffer only exists once `reverseReset`'s shape-mismatch arm
      // has run. The passing push below is the invariant that keeps
      // `Add_V_V_Inplace`'s empty-destination error path unreachable: reset runs
      // before every push, so buffers are full-length by push time.
      let xa = DV [| 3.0; 5.0 |] |> makeReverse (nextTag ())
      let z = xa.[0] * xa.[1]
      (xa |> adjoint |> DV.toFloats).Length |> Expect.equal "adjoint before any pass is the empty sentinel" 0
      z |> reverseProp D.One
      let g = xa |> adjoint |> DV.toFloats
      g.Length |> Expect.equal "materialised to primal length by the pass" 2
      g.[0] |> Expect.floatClose "d(x*y)/dx" accuracy 5.0
      g.[1] |> Expect.floatClose "d(x*y)/dy" accuracy 3.0

    testCase "nested forward-over-reverse works over lazy adjoints" <| fun _ ->
      // Forward-on-reverse — the shape that puts DVF duals into node adjoints
      // (constraint 5) — built as Hessian-vector products via the forward
      // directional derivative of the reverse-mode gradient. Deliberately not
      // `gradhessian`, whose DM assembly is `failwith` under Fable; this form
      // runs on all three targets.
      let f (v: DV) = v.[0] * v.[1] * v.[1]
      let x = DV [| 3.0; 5.0 |]
      let hcol0 = jacobianv (grad f) x (DV [| 1.0; 0.0 |]) |> DV.toFloats
      let hcol1 = jacobianv (grad f) x (DV [| 0.0; 1.0 |]) |> DV.toFloats
      hcol0.[0] |> Expect.floatClose "H[0,0] = 0" accuracy 0.0
      hcol0.[1] |> Expect.floatClose "H[1,0] = 2y" accuracy 10.0
      hcol1.[0] |> Expect.floatClose "H[0,1] = 2y" accuracy 10.0
      hcol1.[1] |> Expect.floatClose "H[1,1] = 2x" accuracy 6.0

    testCase "an extra reverseReset disarms the push that follows (known trap)" <| fun _ ->
      // resetRec bumps each node's fan-out counter and recurses only when it hits
      // exactly 1, so a second reset with no push in between walks the root alone and
      // leaves every child armed; the push then stops at the root. Pinned deliberately
      // — this is a trap for anyone writing a benchmark or a test, not a contract. If
      // it is ever fixed, this test is what will tell you. Constraint 2 in the plan.
      let xa = DV [| 3.0; 5.0 |] |> makeReverse (nextTag ())
      let z = xa.[0] * xa.[1]
      reverseReset z
      z |> reverseProp D.One
      let g = xa |> adjoint |> DV.toFloats
      g.[0] |> Expect.floatClose "extra reset silently zeroes the gradient" accuracy 0.0
      g.[1] |> Expect.floatClose "extra reset silently zeroes the gradient" accuracy 0.0
  ]
