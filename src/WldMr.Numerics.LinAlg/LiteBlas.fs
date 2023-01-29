namespace WldMr.Numerics.LinAlg


#if !FABLE_COMPILER
open System.Numerics
type VSpan<'a> = System.Span<'a>
type VRoSpan<'a> = System.ReadOnlySpan<'a>
type WSpan() =
  static member span(a) = VSpan(a)
  static member slice(a, i: int) = System.Span(a, i, a.Length - i)
  static member slice(vs: VSpan<'a>, i: int) = vs.Slice(i)
  static member rospan(a) = System.ReadOnlySpan(a)
  static member rospan(a: VSpan<'a>): VRoSpan<'a> =
    let r: System.ReadOnlySpan<'a> = VSpan<'a>.op_Implicit a
    r
  static member roslice(a, i: int) = System.ReadOnlySpan(a, i, a.Length - i)
  static member roslice(vs: VSpan<'a>, i: int): VRoSpan<'a> = VSpan<'a>.op_Implicit(vs.Slice(i))
#else

type ManualSpan<'a> =
  class
    val Arr: 'a []
    val Offset: int

    new(a) = {Arr= a; Offset= 0}
    new(a, i) = {Arr= a; Offset= i}
    member ms.slice(i: int) = ManualSpan(ms.Arr, ms.Offset + i)

    member ms.Item
      with get i =
        ms.Arr.[ms.Offset + i]
      and set i v =
        ms.Arr.[ms.Offset + i] <- v
  end


type VSpan<'a> = ManualSpan<'a>
type VRoSpan<'a> = ManualSpan<'a>
type WSpan() =
  static member span(a) = VSpan(a)
  static member slice(a:VSpan<'a>, i: int) = a.slice(i)
  static member slice(a, i: int) = VSpan(a, i)
  static member rospan(a: VSpan<'a>) = a
  static member rospan(a) = VRoSpan(a)
  static member rospan(a, i: int) = VRoSpan(a, i)
  static member roslice(a, i: int) = VRoSpan(a, i)
  static member roslice(a:VSpan<'a>, i: int) = a.slice(i)
#endif


module Blas =
  let inline sub_V_V(a: float[], b:float[]) =
    let length = a.Length
    if length = b.Length then
      let z = Array.zeroCreate length
      for i = 0 to length - 1 do
        z.[i] <- a.[i] - b.[i]
      z
    else
      failwith $"Blas.sub_V_V Arrays have different length {a.Length} {b.Length}."

  let inline add_V_V(a: float[]) (b:float[]) =
    let length = a.Length
    if length = b.Length then
      let z = Array.zeroCreate length
      for i = 0 to length - 1 do
        z.[i] <- a.[i] + b.[i]
      z
    else
      failwith $"Blas.add_V_V Arrays have different length {a.Length} {b.Length}."


  #if !FABLE_COMPILER
  let simdOffset = Vector<double>.Count
  #endif

  /// <summary>
  /// Y = a * X + Y
  /// </summary>
  let inline daxpy(n:int, a: float, x: float[], incx: int, y:float[], incy:int): unit =
    #if !FABLE_COMPILER
    if incx = 1 && incy = 1 then
      let offset = simdOffset
      let mutable i = 0
      while i < n - offset do
        let v1 = Vector(x, i)
        let v2 = Vector(y, i)
        let r = a * v1 + v2
        r.CopyTo(y, i)
        i <- i + offset
      for i in i .. n-1 do
        y.[i * incy] <- a * x.[i * incx] + y.[i * incy]
    else
    #endif
      for i in 0..n-1 do
        y.[i * incy] <- a * x.[i * incx] + y.[i * incy]


  /// <summary>
  /// IDAMAX finds the index of the first element having maximum absolute value.
  /// </summary>
  /// <returns>
  ///   -1 if the input is empty,
  /// <br/>
  ///   otherwise, the index of the first element having maximum absolute value
  /// </returns>
  let idamax(n: int, x: VRoSpan<float>, incx: int): int =
    if n < 0 || incx <= 0 then
      -1
    elif n = 1 then
      0
    elif incx = 1 then
      let mutable idamax = 0
      let mutable dmax = abs x.[0]
      for i = 1 to n-1 do
        if abs x.[i] > dmax then
          idamax <- i
          dmax <- abs x.[i]
      idamax
    else
      let mutable idamax = 0
      let mutable ix = 0
      let mutable dmax = abs x.[0]
      ix <- ix + incx
      for i = 1 to n-1 do
        if abs x.[ix] > dmax then
          idamax <- i
          dmax <- abs x.[ix]
        ix <- ix + incx
      idamax


  /// <summary>
  /// DSCAL scales a vector by a constant.
  /// </summary>
  let inline dscal(n: int, a: float, x:VSpan<float>, incx: int) =
    for i in 0..n-1 do
      x.[i * incx] <- a * x.[i * incx]


  /// <summary>
  /// DSWAP interchanges two vectors.
  /// </summary>
  let dswap(n: int, x: VSpan<float>, incx: int, y: VSpan<float>, incy: int) =
    if incx = 1 && incy = 1 then
      for i = 0 to n-1 do
        let temp = x.[i]
        x.[i] <- y.[i]
        y.[i] <- temp
    else
      let mutable ix = if incx >= 0 then 0 else (-n+1)*incx
      let mutable iy = if incy >= 0 then 0 else (-n+1)*incy
      for i = 0 to n-1 do
        let temp = x.[ix]
        x.[ix] <- y.[iy]
        y.[iy] <- temp
        ix <- ix + incx
        iy <- iy + incy


  /// <summary>
  /// DLASWP interchanges two vectors.
  /// </summary>
  /// <param name="n">The number of columns of `a`.</param>
  /// <param name="a">The input/output matrix.</param>
  /// <param name="lda">The leading dimension of the array A.</param>
  /// <param name="k1">The first element of IPIV for which a row interchange will
  ///   be done</param>
  /// <param name="k2"> (K2-K1+1) is the number of elements of IPIV for which a row
  ///   interchange will be done. </param>
  /// <param name="iPiv"> </param>
  /// <param name="incx">The increment between successive values of IPIV. If INCX
  ///   is negative, the pivots are applied in reverse order.</param>
  let dlaswp(n: int, a: VSpan<float>, lda: int, k1: int, k2: int, iPiv: VRoSpan<int>, incx: int) =
    let ix0, i1, i2, inc =
      if incx > 0 then
        k1, k1, k2, 1
      elif incx < 0 then
        k1 + ( k1-k2 )*incx, k2, k1, -1
      else
        0, 0, 0, 0

    if inc <> 0 then
      let mutable n32 = (n / 32) * 32
      if n32 <> 0 then
        for j in 0 .. 32 .. n32-1 do
          let mutable ix = ix0
          for i in i1 .. inc .. i2 do
            let ip = iPiv.[ix]
            if ip <> i then
              for k in j .. j+31 do
                let temp = a.[i + k*lda]
                a.[i + k*lda] <- a.[ip + k*lda]
                a.[ip + k*lda] <- temp
            ix <- ix + incx
      if n32 <> n then
        let mutable ix = ix0
        for i in i1 .. inc .. i2 do
          let ip = iPiv.[ix]
          if ip <> i then
            for k in n32 .. n-1 do
              let temp = a.[i + k*lda]
              a.[i + k*lda] <- a.[ip + k*lda]
              a.[ip + k*lda] <- temp
          ix <- ix + incx


  /// <summary>
  /// DGER   performs the rank 1 operation
  /// A := alpha*x*y**T + A,
  /// where alpha is a scalar, x is an m element vector, y is an n element
  /// vector and A is an m by n matrix.
  /// </summary>
  /// <param name="m">The number of rows of `A`.</param>
  /// <param name="n">The number of columns of `A`.</param>
  /// <param name="alpha"></param>
  /// <param name="x">at least `m` elements</param>
  /// <param name="incx"></param>
  /// <param name="y">at least `n` element</param>
  /// <param name="incy"></param>
  /// <param name="a">The input matrix `A`.</param>
  /// <param name="lda">The leading dimension of the array `a`.</param>
  let dger(m: int, n: int, alpha: float, x: VRoSpan<float>, incx: int, y:VRoSpan<float>, incy: int, a: VSpan<float>, lda: int) =
    let mutable jy = if incy >= 0 then 0 else -(n-1)*incy
    let kx = if incx >= 0 then 0 else -(m-1)*incx
    for j = 0 to n-1 do
      if y.[jy] <> 0. then
        let temp = alpha * y.[jy]
        let mutable ix = kx
        for i = 0 to m-1 do
          a.[i+lda*j] <- a.[i+lda*j] + x.[ix] * temp
          ix <- ix + incx
      jy <- jy + incy

  /// <summary>
  /// DGETF2 computes an LU factorization of a general m-by-n matrix A
  /// using partial pivoting with row interchanges.
  /// <br/>
  /// The factorization has the form
  /// <c>A = P * L * U</c>
  /// where
  /// <list type="bullet">
  /// <item>P is a permutation matrix, </item>
  /// <item>L is lower triangular with unit diagonal elements (lower trapezoidal if m &gt; n), </item>
  /// <item>and U is upper triangular (upper trapezoidal if m &lt; n). </item>
  /// </list>
  /// This is the right-looking Level 2 BLAS version of the algorithm.
  /// </summary>
  /// <returns>
  /// <list type="bullet">
  /// <item>
  /// <term> = 0, </term>
  /// <description>
  /// successful exit
  /// </description>
  /// </item>
  /// <item>
  /// <term> =-k &lt; 0, </term>
  /// <description>
  ///   the `k`-th argument had an illegal value
  /// </description>
  /// </item>
  /// <item>
  /// <term> =k &gt; 0, </term>
  /// <description>
  ///  U(k,k) is exactly zero. The factorization
  ///          has been completed, but the factor U is exactly
  ///          singular, and division by zero will occur if it is used
  ///          to solve a system of equations.
  /// </description>
  /// </item>
  /// </list>
  /// </returns>
  let dgetf2(m: int, n: int, a: VSpan<float>, lda: int, iPiv: VSpan<int>): int =
    let tiny = 0.2225073858507202e-307
    let huge = 0.17976931e+309
    let epsilon = 0.222446049e-15
    let small = 1. / huge
    let sfmin =
      if small >= tiny then
        small * ( 1. + epsilon)
      else
        tiny
    let mutable info = 0

    for j = 0 to (min m n)-1 do
      // Find pivot and test for singularity.
      let jp =
        let x_ = WSpan.roslice(a, j + j*lda)
        j + idamax( m-j, x_, 1)
      iPiv.[j] <- jp
      if a.[jp + j*lda] <> 0. then
        // Apply the interchange to columns 0:n-1
        if jp <> j then
          let x_ = WSpan.slice(a, j)
          let y_ = WSpan.slice(a, jp)
          dswap(n, x_ ,lda, y_, lda)
        // Compute elements J+1:M of J-th column.
        if j < m - 1 then
          if abs a.[j+j*lda] >= sfmin then
            let x_ = WSpan.slice(a, j+1+j*lda)
            dscal(m-j-1, 1./a.[j+j*lda], x_, 1)
          else
            for i = 1 to m - j - 1 do
              a.[j+i+j*lda] <- a.[j+i+j*lda] / a.[j+j*lda]
//        failwith "todo"
      elif info = 0 then
        info <- j
      if j < (min m n)-1 then
        let x_ = WSpan.roslice(a, j+1+j*lda)
        let y_ = WSpan.roslice(a, j+(j+1)*lda)
        let a_ = WSpan.slice(a, j+1+(j+1)*lda)
        dger(m-j-1, n-j-1, -1., x_, 1, y_, lda,
             a_, lda)

    info


  // IMPROVE
  let ddot(n: int, x:float[], incx:int , y:float[], incy: int) =
    // let arg_n = min x.Length y.Length
    let mutable res = 0.
    for i in 0..n-1 do
      res <- res + x.[i * incx] * y.[i * incy]
    res




  let dasum(n: int, x:float[], xinc: int) =
    let mutable res = 0.
    for i = 0 to n - 1 do
      res <- res + (abs x.[i * xinc])
    res

  /// <summary>
  /// DNRM2 returns the euclidean norm of a vector via the function
  /// name, so that
  /// <br/>
  ///    DNRM2 := sqrt( x'*x )
  /// </summary>
  let dnrm2(n: int, x:float[], xinc: int) =
      // IMPROVE: see netlib LAPACK, sum big/medium/small value separately
    let mutable sqSum = 0.
    for i = 0 to n - 1 do
      sqSum <- sqSum + x.[i * xinc] * x.[i * xinc]
    System.Math.Sqrt sqSum


  // C <- alpha * A * B + beta * C
  ///<summary>
  /// DGEMM  performs one of the matrix-matrix operations
  /// <br/>
  ///    C := alpha*op( A )*op( B ) + beta*C,
  /// <br/>
  /// where  op( X ) is one of
  /// <br/>
  ///    op( X ) = X   or   op( X ) = X**T,
  /// <br/>
  /// alpha and beta are scalars, and A, B and C are matrices, with op( A )
  /// an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
  ///</summary>
  let dgemm(ta: char, tb: char, m: int, n: int, k: int, alpha:float,
            a:float[], lda:int, b:float[], ldb:int, beta:float, c:float[], ldc: int) =
    // Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
    // transposed and set  NROWA and NROWB  as the number of rows of  A
    // and  B  respectively.
    let nota = ta = 'N'
    let notb = tb = 'N'
    let nrowa = if nota then m else k
    nrowa |> ignore
    let nrowb = if notb then k else n
    nrowb |> ignore

    // quick return
    if m = 0 || n = 0 || (( alpha = 0. || k = 0) && beta = 1.) then
      ()
    elif notb then
      if nota then
        // Form  C := alpha*A*B + beta*C
        for j = 0 to n-1 do
          if beta = 0. then
            for i = 0 to m-1 do
              c.[i + j*ldc] <- 0.
          elif beta <> 1. then
            for i = 0 to m-1 do
              c.[i + j*ldc] <- beta * c.[i + j*ldc]
          for l = 0 to k-1 do
            let temp = alpha * b.[l + j*ldb]
            for i = 0 to m-1 do
              c.[i + j*ldc] <- c.[i + j*ldc] + temp * a.[i + lda*l]
      else
        // Form  C := alpha*A**T*B + beta*C
        for j = 0 to n-1 do
          for i = 0 to m-1 do
            let mutable temp = 0.
            for l = 0 to k do
              temp <- temp + a.[l + i*lda] * b.[l + j*ldb]
            if beta = 0. then
              c.[i + j*ldc] <- alpha * temp
            else
              c.[i + j*ldc] <- alpha * temp + beta * c.[i + j*ldc]
    else // "if not notb"
      if nota then
        // Form  C := alpha*A*B**T + beta*C
        for j = 0 to n-1 do
          if beta = 0. then
            for i = 0 to m-1 do
              c.[i + j*ldc] <- 0.
          elif beta <> 1. then
            for i = 0 to m-1 do
              c.[i + j*ldc] <- beta * c.[i + j*ldc]
          else
            for l = 0 to k-1 do
              let temp = alpha * b.[j + l*ldb]
              for i = 0 to m-1 do
                c.[i + j*ldc] <- c.[i + j*ldc] + temp * a.[i + lda*l]
      else
        // Form  C := alpha*A**T*B**T + beta*C
        for j = 0 to n-1 do
          for i = 0 to m-1 do
            let mutable temp = 0.
            for l = 0 to k do
              temp <- temp + a.[l + i*lda] * b.[l + j*ldb]
            if beta = 0. then
              c.[i + j*ldc] <- alpha * temp
            else
              c.[i + j*ldc] <- alpha * temp + beta * c.[i + j*ldc]

  ///<summary>
  /// DGEMV  performs one of the matrix-vector operations
  /// <br/>
  ///    y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,
  /// <br/>
  /// where alpha and beta are scalars, x and y are vectors and A is an
  /// m by n matrix.
  ///</summary>
  /// <param name="t">'N'o Transpose, 'T'ranspose, 'C'</param>
  /// <param name="m">The number of rows of matrix 'A'.</param>
  /// <param name="n">The number of columns of 'A'.</param>
  /// <param name="alpha">scala 'alpha'.</param>
  /// <param name="a">The input matrix 'A'.</param>
  /// <param name="lda">The leading dimension of the array 'A'.</param>
  /// <param name="x">The input vector 'X'</param>
  /// <param name="incx">The increment between successive values of 'X'</param>
  /// <param name="beta">scalar beta</param>
  /// <param name="y">The input/output vector 'Y' </param>
  /// <param name="incy">The increment between successive values of 'Y'
  ///   is negative, the pivots are applied in reverse order.</param>
  let dgemv(t: char, m: int, n:int, alpha:float, a:float[], lda:int, x:float[], incx: int, beta:float, y:float[], incy: int) =
    let lenx, leny = if t = 'N' then n, m else m, n
    let kx = if incx >= 0 then 0 else (1-lenx)*incx
    let ky = if incy >= 0 then 0 else (1-leny)*incy
    if beta <> 1. then
      let mutable iy = ky
      if beta = 0. then
        for i = 0 to leny-1 do
          y.[iy] <- 0.
          iy <- iy + incy
      else
        for i = 0 to leny-1 do
          y.[iy] <- beta * y.[iy]
          iy <- iy + incy

    // IMPROVE: special case for alpha= 0.
    // general form
    if t = 'N' then
      let mutable jx = kx
      for j = 0 to n-1 do
        let temp = alpha * x.[jx]
        let mutable iy = ky
        for i = 0 to m-1 do
          y.[iy] <- y.[iy] + temp * a.[i + j * lda]
          iy <- iy + incy
        jx <- jx + incx
    else
      let mutable jy = ky
      for j = 0 to n-1 do
        let mutable temp = 0.
        let mutable ix = kx
        for i = 0 to m-1 do
          temp <- temp + a.[i + j * lda] * x.[ix]
          ix <- ix + incx
        y.[jy] <- y.[jy] + alpha * temp
        jy <- jy + incy



  /// <summary>
  /// DTRSM  solves one of the matrix equations
  /// <br/>
  ///    <c>op( A )*X = alpha*B</c>,   or   X*op( A ) = alpha*B,
  /// <br/>
  /// where alpha is a scalar, X and B are m by n matrices, A is a unit, or
  /// non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
  /// <br/>
  ///    op( A ) = A   or   op( A ) = A**T.
  /// <br/>
  /// The matrix X is overwritten on B.
  /// </summary>
  /// <param name="side">'L'eft, 'R'ight
  /// <br/>
  ///    SIDE = 'L' or 'l'   op( A )*X = alpha*B.
  /// <br/>
  ///    SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
  /// </param>
  /// <param name="uplo">'U'pper, 'L'ower</param>
  /// <param name="transa">'N'o Transpose, 'T'ranspose, 'C'</param>
  /// <param name="diag">'N'o Unit, 'U'nit, diag specifies whether or not 'A' is unit triangular</param>
  /// <param name="m">The number of rows of matrix 'A'.</param>
  /// <param name="n">The number of columns of 'A'.</param>
  /// <param name="alpha">scala 'alpha'.</param>
  /// <param name="a">The input matrix 'A'.</param>
  /// <param name="lda">The leading dimension of the array 'A'.</param>
  /// <param name="b">The input/output matrix 'B'.</param>
  /// <param name="ldb">The leading dimension of the array 'B'.</param>
  let dtrsm(side: char, uplo: char, transa: char, diag: char, m: int, n: int, alpha: float,
            a: VRoSpan<float>, lda: int, b: VSpan<float>, ldb: int
            ): int =
    let lside = side = 'L'
    let nounit = diag = 'N'
    let upper = uplo = 'U'

    let info = 0
//    if a = 0. then
    if lside then
      // Form  B := alpha*inv( A )*B.
      if transa = 'N' then
        if upper then
          for j = 0 to n-1 do
            if alpha <> 1. then
              for i = 1 to m - 1 do
                b.[i+j*ldb] <- alpha * b.[i+j*ldb]
            for k in m-1 .. -1 .. 0 do
              if b.[k+j*ldb] <> 0. then
                if nounit then
                  b.[k+j*ldb] <- b.[k+j*ldb] / a.[k+k*lda]
                for i = 0 to k-1 do
                  b.[i+j*ldb] <- b.[i+j*ldb] - b.[k+j*ldb] * a.[i+k*lda]
        else
          for j = 0 to n-1 do
            if alpha <> 1. then
              for i = 0 to m-1 do
                b.[i+j*ldb] <- alpha * b.[i+j*ldb]
            for k = 0 to m-1 do
              if b.[k+j*ldb] <> 0. then
                if nounit then
                  b.[k+j*ldb] <- b.[k+j*ldb] / a.[k+k*lda]
                for i = k + 1 to m-1 do
                  b.[i+j*ldb] <- b.[i+j*ldb] - b.[k+j*ldb] * a.[i+k*lda]
      else
        failwith "todo"
        // Form  B := alpha*inv( A**T )*B.
        if upper then
          ()
        else
          ()
    else
      failwith "Not currently implemented (not necessary for dgetrs)"
      if transa = 'N' then
        // Form  B := alpha*B*inv( A ).
        if upper then
          ()
        else
          ()
      else
        // Form  B := alpha*B*inv( A**T ).
        if upper then
          ()
        else
          ()

    info



  /// <summary>
  /// DGETRS solves a system of linear equations
  /// <br/>
  ///    A * X = B  or  A**T * X = B
  /// <br/>
  /// with a general N-by-N matrix A using the LU factorization computed
  /// by DGETRF (or DGETF2, or .
  ///</summary>
  /// <param name="trans">
  /// if = 'N':  A * X = B  (No transpose),
  /// <br/>
  /// if = 'T':  A**T* X = B  (Transpose),
  /// <br/>
  /// if = 'C':  A**T* X = B  (Conjugate transpose = Transpose)
  /// </param>
  /// <param name="n">The order of matrix 'A'</param>
  /// <param name="nrhs">The number of right-hand sides</param>
  /// <param name="a">The input matrix 'A'.</param>
  /// <param name="lda">The leading dimension of the array 'A'.</param>
  /// <param name="b">The input/output matrix 'B'. On exit the solution matrix 'X'</param>
  /// <param name="ipiv">The pivot indices from <see>dgetrf</see>; for 1 &lt;= i &gt;= N, row i of the
  ///      matrix was interchanged with row IPIV(i). </param>
  /// <param name="ldb">The leading dimension of the array 'B'.</param>
  let dgetrs(trans: char, n: int, nrhs: int, a:VRoSpan<float>, lda:int, ipiv: VRoSpan<int>, b: VSpan<float>, ldb: int): int =
    let notran = trans = 'N'

    if notran then
      // solve A * X = B
      dlaswp(nrhs, b, ldb, 0, n-1, ipiv, 1)
      dtrsm('L', 'L', 'N', 'U', n, nrhs, 1., a, lda, b, ldb ) |> ignore
      dtrsm('L', 'U', 'N', 'N', n, nrhs, 1., a, lda, b, ldb ) |> ignore
    else
      failwith "todo"

    0



  /// <summary>
  /// DGESV computes the solution to a real system of linear equations
  /// <br/>
  ///    A * X = B,
  /// <br/>
  /// where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
  /// <br/>
  /// The LU decomposition with partial pivoting and row interchanges is
  /// used to factor A as
  /// <br/>
  ///    A = P * L * U,
  /// <br/>
  /// where P is a permutation matrix, L is unit lower triangular, and U is
  /// upper triangular.  The factored form of A is then used to solve the
  /// system of equations A * X = B.
  /// </summary>
  let dgesvUnblocked(n: int, nhrs: int, a:VSpan<float>, lda:int, ipiv: VSpan<int>, b: VSpan<float>, ldb: int): int =
    let info = dgetf2( n, n, a, lda, ipiv)
    if info <> 0 then
      info
    else
      let a_: VRoSpan<float> = WSpan.rospan(a)
      dgetrs( 'N', n, nhrs, a_, lda, WSpan.rospan ipiv, b, ldb )
