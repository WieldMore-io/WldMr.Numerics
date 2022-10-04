﻿let input = """
    /// <summary>
    /// **************************************
    /// COEFFICIENTS FOR METHOD ErfImp       *
    /// **************************************
    /// </summary>
    /// <summary> Polynomial coefficients for a numerator of ErfImp
    /// calculation for Erf(x) in the interval [1e-10, 0.5].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpAn">`SpecialFunctions.ErfImpAn` on google.com</a></footer>
    private static readonly double[] ErfImpAn = new double[8]
    {
      0.00337916709551257,
      -0.00073695653048168,
      -0.37473233739292,
      0.0817442448733587,
      -0.0421089319936549,
      0.00701657095120958,
      -0.00495091255982435,
      0.000871646599037922
    };
    /// <summary> Polynomial coefficients for  a denominator of ErfImp
    /// calculation for Erf(x) in the interval [1e-10, 0.5].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpAd">`SpecialFunctions.ErfImpAd` on google.com</a></footer>
    private static readonly double[] ErfImpAd = new double[8]
    {
      1.0,
      -0.218088218087925,
      0.412542972725442,
      -0.0841891147873107,
      0.0655338856400242,
      -0.0120019604454942,
      0.00408165558926174,
      -0.00061590072155777
    };
    /// <summary> Polynomial coefficients for a numerator in ErfImp
    /// calculation for Erfc(x) in the interval [0.5, 0.75].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpBn">`SpecialFunctions.ErfImpBn` on google.com</a></footer>
    private static readonly double[] ErfImpBn = new double[6]
    {
      -0.0361790390718262,
      0.292251883444883,
      0.281447041797604,
      0.125610208862767,
      0.0274135028268931,
      0.00250839672168066
    };
    /// <summary> Polynomial coefficients for a denominator in ErfImp
    /// calculation for Erfc(x) in the interval [0.5, 0.75].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpBd">`SpecialFunctions.ErfImpBd` on google.com</a></footer>
    private static readonly double[] ErfImpBd = new double[6]
    {
      1.0,
      1.85450058979035,
      1.43575803037831,
      0.582827658753037,
      0.12481047693295,
      0.0113724176546353
    };
    /// <summary> Polynomial coefficients for a numerator in ErfImp
    /// calculation for Erfc(x) in the interval [0.75, 1.25].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpCn">`SpecialFunctions.ErfImpCn` on google.com</a></footer>
    private static readonly double[] ErfImpCn = new double[7]
    {
      -0.0397876892611137,
      0.153165212467878,
      0.191260295600936,
      0.102763270619893,
      0.0296370906157388,
      0.00460934867802755,
      0.00030760782034868
    };
    /// <summary> Polynomial coefficients for a denominator in ErfImp
    /// calculation for Erfc(x) in the interval [0.75, 1.25].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpCd">`SpecialFunctions.ErfImpCd` on google.com</a></footer>
    private static readonly double[] ErfImpCd = new double[7]
    {
      1.0,
      1.95520072987628,
      1.64762317199385,
      0.768238607022126,
      0.20979318593651,
      0.0319569316899913,
      0.00213363160895785
    };
    /// <summary> Polynomial coefficients for a numerator in ErfImp
    /// calculation for Erfc(x) in the interval [1.25, 2.25].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpDn">`SpecialFunctions.ErfImpDn` on google.com</a></footer>
    private static readonly double[] ErfImpDn = new double[7]
    {
      -0.030083856055795,
      0.0538578829844455,
      0.0726211541651914,
      0.0367628469888049,
      0.00964629015572527,
      0.00133453480075291,
      7.78087599782504E-05
    };
    /// <summary> Polynomial coefficients for a denominator in ErfImp
    /// calculation for Erfc(x) in the interval [1.25, 2.25].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpDd">`SpecialFunctions.ErfImpDd` on google.com</a></footer>
    private static readonly double[] ErfImpDd = new double[8]
    {
      1.0,
      1.75967098147168,
      1.32883571437961,
      0.552528596508758,
      0.133793056941333,
      0.0179509645176281,
      0.00104712440019937,
      -1.06640381820357E-08
    };
    /// <summary> Polynomial coefficients for a numerator in ErfImp
    /// calculation for Erfc(x) in the interval [2.25, 3.5].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpEn">`SpecialFunctions.ErfImpEn` on google.com</a></footer>
    private static readonly double[] ErfImpEn = new double[7]
    {
      -0.0117907570137228,
      0.0142621320905388,
      0.0202234435902961,
      0.00930668299990432,
      0.00213357802422066,
      0.000250229873864601,
      1.20534912219588E-05
    };
    /// <summary> Polynomial coefficients for a denominator in ErfImp
    /// calculation for Erfc(x) in the interval [2.25, 3.5].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpEd">`SpecialFunctions.ErfImpEd` on google.com</a></footer>
    private static readonly double[] ErfImpEd = new double[7]
    {
      1.0,
      1.5037622520362,
      0.965397786204463,
      0.339265230476797,
      0.068974064954157,
      0.00771060262491768,
      0.000371421101531069
    };
    /// <summary> Polynomial coefficients for a numerator in ErfImp
    /// calculation for Erfc(x) in the interval [3.5, 5.25].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpFn">`SpecialFunctions.ErfImpFn` on google.com</a></footer>
    private static readonly double[] ErfImpFn = new double[7]
    {
      -0.00546954795538729,
      0.00404190278731707,
      0.00549633695531612,
      0.00212616472603945,
      0.000394984014495084,
      3.65565477064442E-05,
      1.35485897109932E-06
    };
    /// <summary> Polynomial coefficients for a denominator in ErfImp
    /// calculation for Erfc(x) in the interval [3.5, 5.25].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpFd">`SpecialFunctions.ErfImpFd` on google.com</a></footer>
    private static readonly double[] ErfImpFd = new double[8]
    {
      1.0,
      1.21019697773631,
      0.620914668221144,
      0.173038430661143,
      0.0276550813773432,
      0.0024062597442431,
      8.91811817251337E-05,
      -4.65528836283383E-12
    };
    /// <summary> Polynomial coefficients for a numerator in ErfImp
    /// calculation for Erfc(x) in the interval [5.25, 8].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpGn">`SpecialFunctions.ErfImpGn` on google.com</a></footer>
    private static readonly double[] ErfImpGn = new double[6]
    {
      -0.00270722535905778,
      0.00131875634250294,
      0.00119925933261002,
      0.000278496198113447,
      2.67822988218332E-05,
      9.23043672315028E-07
    };
    /// <summary> Polynomial coefficients for a denominator in ErfImp
    /// calculation for Erfc(x) in the interval [5.25, 8].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpGd">`SpecialFunctions.ErfImpGd` on google.com</a></footer>
    private static readonly double[] ErfImpGd = new double[7]
    {
      1.0,
      0.814632808543142,
      0.2689016658563,
      0.0449877216103041,
      0.00381759663320248,
      0.000131571897888597,
      4.04815359675764E-12
    };
    /// <summary> Polynomial coefficients for a numerator in ErfImp
    /// calculation for Erfc(x) in the interval [8, 11.5].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpHn">`SpecialFunctions.ErfImpHn` on google.com</a></footer>
    private static readonly double[] ErfImpHn = new double[6]
    {
      -0.00109946720691742,
      0.000406425442750423,
      0.000274499489416901,
      4.65293770646659E-05,
      3.20955425395767E-06,
      7.78286018145021E-08
    };
    /// <summary> Polynomial coefficients for a denominator in ErfImp
    /// calculation for Erfc(x) in the interval [8, 11.5].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpHd">`SpecialFunctions.ErfImpHd` on google.com</a></footer>
    private static readonly double[] ErfImpHd = new double[6]
    {
      1.0,
      0.588173710611846,
      0.13936333128941,
      0.0166329340417084,
      0.00100023921310235,
      2.42548375215872E-05
    };
    /// <summary> Polynomial coefficients for a numerator in ErfImp
    /// calculation for Erfc(x) in the interval [11.5, 17].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpIn">`SpecialFunctions.ErfImpIn` on google.com</a></footer>
    private static readonly double[] ErfImpIn = new double[5]
    {
      -0.00056907993601095,
      0.000169498540373762,
      5.18472354581101E-05,
      3.82819312231929E-06,
      8.24989931281894E-08
    };
    /// <summary> Polynomial coefficients for a denominator in ErfImp
    /// calculation for Erfc(x) in the interval [11.5, 17].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpId">`SpecialFunctions.ErfImpId` on google.com</a></footer>
    private static readonly double[] ErfImpId = new double[6]
    {
      1.0,
      0.339637250051139,
      0.0434726478703107,
      0.00248549335224637,
      5.35633305337153E-05,
      -1.1749094440546E-13
    };
    /// <summary> Polynomial coefficients for a numerator in ErfImp
    /// calculation for Erfc(x) in the interval [17, 24].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpJn">`SpecialFunctions.ErfImpJn` on google.com</a></footer>
    private static readonly double[] ErfImpJn = new double[5]
    {
      -0.000241313599483991,
      5.74224975202501E-05,
      1.15998962927384E-05,
      5.81762134402594E-07,
      8.53971555085674E-09
    };
    /// <summary> Polynomial coefficients for a denominator in ErfImp
    /// calculation for Erfc(x) in the interval [17, 24].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpJd">`SpecialFunctions.ErfImpJd` on google.com</a></footer>
    private static readonly double[] ErfImpJd = new double[5]
    {
      1.0,
      0.233044138299688,
      0.020418694054644,
      0.000797185647564398,
      1.17019281670172E-05
    };
    /// <summary> Polynomial coefficients for a numerator in ErfImp
    /// calculation for Erfc(x) in the interval [24, 38].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpKn">`SpecialFunctions.ErfImpKn` on google.com</a></footer>
    private static readonly double[] ErfImpKn = new double[5]
    {
      -0.00014667469927776,
      1.62666552112281E-05,
      2.69116248509165E-06,
      9.79584479468092E-08,
      1.01994647625723E-09
    };
    /// <summary> Polynomial coefficients for a denominator in ErfImp
    /// calculation for Erfc(x) in the interval [24, 38].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpKd">`SpecialFunctions.ErfImpKd` on google.com</a></footer>
    private static readonly double[] ErfImpKd = new double[5]
    {
      1.0,
      0.165907812944847,
      0.0103361716191506,
      0.000286593026373868,
      2.984015708409E-06
    };
    /// <summary> Polynomial coefficients for a numerator in ErfImp
    /// calculation for Erfc(x) in the interval [38, 60].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpLn">`SpecialFunctions.ErfImpLn` on google.com</a></footer>
    private static readonly double[] ErfImpLn = new double[5]
    {
      -5.83905797629772E-05,
      4.12510325105496E-06,
      4.31790922420251E-07,
      9.93365155590013E-09,
      6.53480510020105E-11
    };
    /// <summary> Polynomial coefficients for a denominator in ErfImp
    /// calculation for Erfc(x) in the interval [38, 60].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpLd">`SpecialFunctions.ErfImpLd` on google.com</a></footer>
    private static readonly double[] ErfImpLd = new double[5]
    {
      1.0,
      0.10507708607204,
      0.00414278428675476,
      7.26338754644524E-05,
      4.77818471047399E-07
    };
    /// <summary> Polynomial coefficients for a numerator in ErfImp
    /// calculation for Erfc(x) in the interval [60, 85].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpMn">`SpecialFunctions.ErfImpMn` on google.com</a></footer>
    private static readonly double[] ErfImpMn = new double[4]
    {
      -1.9645779760923E-05,
      1.57243887666801E-06,
      5.43902511192701E-08,
      3.17472492369118E-10
    };
    /// <summary> Polynomial coefficients for a denominator in ErfImp
    /// calculation for Erfc(x) in the interval [60, 85].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpMd">`SpecialFunctions.ErfImpMd` on google.com</a></footer>
    private static readonly double[] ErfImpMd = new double[5]
    {
      1.0,
      0.0528039892409576,
      0.000926876069151753,
      5.4101172322663E-06,
      5.35093845803642E-16
    };
    /// <summary> Polynomial coefficients for a numerator in ErfImp
    /// calculation for Erfc(x) in the interval [85, 110].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpNn">`SpecialFunctions.ErfImpNn` on google.com</a></footer>
    private static readonly double[] ErfImpNn = new double[4]
    {
      -7.89224703978723E-06,
      6.22088451660987E-07,
      1.45728445676882E-08,
      6.03715505542715E-11
    };
    /// <summary> Polynomial coefficients for a denominator in ErfImp
    /// calculation for Erfc(x) in the interval [85, 110].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErfImpNd">`SpecialFunctions.ErfImpNd` on google.com</a></footer>
    private static readonly double[] ErfImpNd = new double[4]
    {
      1.0,
      0.0375328846356294,
      0.000467919535974625,
      1.93847039275846E-06
    };
    /// <summary>
    /// **************************************
    /// COEFFICIENTS FOR METHOD ErfInvImp    *
    /// **************************************
    /// </summary>
    /// <summary> Polynomial coefficients for a numerator of ErfInvImp
    /// calculation for Erf^-1(z) in the interval [0, 0.5].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErvInvImpAn">`SpecialFunctions.ErvInvImpAn` on google.com</a></footer>
    private static readonly double[] ErvInvImpAn = new double[8]
    {
      -0.000508781949658281,
      -0.00836874819741737,
      0.0334806625409745,
      -0.0126926147662974,
      -0.0365637971411763,
      0.0219878681111169,
      0.00822687874676916,
      -0.00538772965071243
    };
    /// <summary> Polynomial coefficients for a denominator of ErfInvImp
    /// calculation for Erf^-1(z) in the interval [0, 0.5].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErvInvImpAd">`SpecialFunctions.ErvInvImpAd` on google.com</a></footer>
    private static readonly double[] ErvInvImpAd = new double[10]
    {
      1.0,
      -0.970005043303291,
      -1.56574558234176,
      1.56221558398423,
      0.662328840472003,
      -0.712289023415428,
      -0.05273963823401,
      0.0795283687341572,
      -0.0023339375937419,
      0.000886216390456425
    };
    /// <summary> Polynomial coefficients for a numerator of ErfInvImp
    /// calculation for Erf^-1(z) in the interval [0.5, 0.75].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErvInvImpBn">`SpecialFunctions.ErvInvImpBn` on google.com</a></footer>
    private static readonly double[] ErvInvImpBn = new double[9]
    {
      -0.202433508355939,
      0.105264680699392,
      8.3705032834312,
      17.6447298408374,
      -18.8510648058714,
      -44.6382324441787,
      17.4453859855709,
      21.1294655448341,
      -3.67192254707729
    };
    /// <summary> Polynomial coefficients for a denominator of ErfInvImp
    /// calculation for Erf^-1(z) in the interval [0.5, 0.75].
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErvInvImpBd">`SpecialFunctions.ErvInvImpBd` on google.com</a></footer>
    private static readonly double[] ErvInvImpBd = new double[9]
    {
      1.0,
      6.24264124854248,
      3.97134379533439,
      -28.66081804998,
      -20.1432634680485,
      48.560921310874,
      10.826866735546,
      -22.643693341314,
      1.721147657612
    };
    /// <summary> Polynomial coefficients for a numerator of ErfInvImp
    /// calculation for Erf^-1(z) in the interval [0.75, 1] with x less than 3.
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErvInvImpCn">`SpecialFunctions.ErvInvImpCn` on google.com</a></footer>
    private static readonly double[] ErvInvImpCn = new double[11]
    {
      -0.131102781679952,
      -0.163794047193317,
      0.117030156341995,
      0.387079738972604,
      0.337785538912036,
      0.142869534408157,
      0.0290157910005329,
      0.00214558995388805,
      -6.79465575181126E-07,
      2.85225331782217E-08,
      -6.81149956853777E-10
    };
    /// <summary> Polynomial coefficients for a denominator of ErfInvImp
    /// calculation for Erf^-1(z) in the interval [0.75, 1] with x less than 3.
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErvInvImpCd">`SpecialFunctions.ErvInvImpCd` on google.com</a></footer>
    private static readonly double[] ErvInvImpCd = new double[8]
    {
      1.0,
      3.46625407242567,
      5.38168345707007,
      4.77846592945844,
      2.5930192162362,
      0.848854343457902,
      0.152264338295332,
      0.0110592422934649
    };
    /// <summary> Polynomial coefficients for a numerator of ErfInvImp
    /// calculation for Erf^-1(z) in the interval [0.75, 1] with x between 3 and 6.
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErvInvImpDn">`SpecialFunctions.ErvInvImpDn` on google.com</a></footer>
    private static readonly double[] ErvInvImpDn = new double[9]
    {
      -0.0350353787183178,
      -0.00222426529213448,
      0.0185573306514231,
      0.0095080470132592,
      0.00187123492819559,
      0.000157544617424961,
      4.60469890584318E-06,
      -2.30404776911883E-10,
      2.66339227425782E-12
    };
    /// <summary> Polynomial coefficients for a denominator of ErfInvImp
    /// calculation for Erf^-1(z) in the interval [0.75, 1] with x between 3 and 6.
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErvInvImpDd">`SpecialFunctions.ErvInvImpDd` on google.com</a></footer>
    private static readonly double[] ErvInvImpDd = new double[7]
    {
      1.0,
      1.36533498175541,
      0.762059164553623,
      0.220091105764131,
      0.0341589143670948,
      0.00263861676657016,
      7.64675292302794E-05
    };
    /// <summary> Polynomial coefficients for a numerator of ErfInvImp
    /// calculation for Erf^-1(z) in the interval [0.75, 1] with x between 6 and 18.
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErvInvImpEn">`SpecialFunctions.ErvInvImpEn` on google.com</a></footer>
    private static readonly double[] ErvInvImpEn = new double[9]
    {
      -0.0167431005076634,
      -0.0011295143874558,
      0.00105628862152493,
      0.000209386317487588,
      1.49624783758342E-05,
      4.49696789927706E-07,
      4.62596163522879E-09,
      -2.81128735628832E-14,
      9.90557099733103E-17
    };
    /// <summary> Polynomial coefficients for a denominator of ErfInvImp
    /// calculation for Erf^-1(z) in the interval [0.75, 1] with x between 6 and 18.
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErvInvImpEd">`SpecialFunctions.ErvInvImpEd` on google.com</a></footer>
    private static readonly double[] ErvInvImpEd = new double[7]
    {
      1.0,
      0.591429344886418,
      0.138151865749083,
      0.0160746087093677,
      0.000964011807005166,
      2.75335474764726E-05,
      2.82243172016108E-07
    };
    /// <summary> Polynomial coefficients for a numerator of ErfInvImp
    /// calculation for Erf^-1(z) in the interval [0.75, 1] with x between 18 and 44.
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErvInvImpFn">`SpecialFunctions.ErvInvImpFn` on google.com</a></footer>
    private static readonly double[] ErvInvImpFn = new double[8]
    {
      -0.00249782127918981,
      -7.79190719229054E-06,
      2.54723037413027E-05,
      1.62397777342511E-06,
      3.96341011304801E-08,
      4.11632831190944E-10,
      1.45596286718675E-12,
      -1.16765012397184E-18
    };
    /// <summary> Polynomial coefficients for a denominator of ErfInvImp
    /// calculation for Erf^-1(z) in the interval [0.75, 1] with x between 18 and 44.
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErvInvImpFd">`SpecialFunctions.ErvInvImpFd` on google.com</a></footer>
    private static readonly double[] ErvInvImpFd = new double[7]
    {
      1.0,
      0.207123112214423,
      0.0169410838120976,
      0.000690538265622685,
      1.45007359818233E-05,
      1.44437756628144E-07,
      5.09761276599778E-10
    };
    /// <summary> Polynomial coefficients for a numerator of ErfInvImp
    /// calculation for Erf^-1(z) in the interval [0.75, 1] with x greater than 44.
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErvInvImpGn">`SpecialFunctions.ErvInvImpGn` on google.com</a></footer>
    private static readonly double[] ErvInvImpGn = new double[8]
    {
      -0.000539042911019079,
      -2.83987590047277E-07,
      8.99465114892291E-07,
      2.29345859265921E-08,
      2.255614448635E-10,
      9.47846627503023E-13,
      1.35880130108925E-15,
      -3.48890393399949E-22
    };
    /// <summary> Polynomial coefficients for a denominator of ErfInvImp
    /// calculation for Erf^-1(z) in the interval [0.75, 1] with x greater than 44.
    /// </summary>
    /// <footer><a href="https://www.google.com/search?q=MathNet.Numerics.SpecialFunctions.ErvInvImpGd">`SpecialFunctions.ErvInvImpGd` on google.com</a></footer>
    private static readonly double[] ErvInvImpGd = new double[7]
    {
      1.0,
      0.0845746234001899,
      0.00282092984726265,
      4.68292921940894E-05,
      3.99968812193862E-07,
      1.61809290887904E-09,
      2.3155860831026E-12
    };
"""

open System.Text.RegularExpressions
let declRegex =
    Regex "private static readonly double\[\] (Er..*Imp..).*$"

input.Split "\n"
|> Array.map (fun s -> s.Trim([|' '; '\t'|]))
|> Array.filter (fun s -> s.StartsWith "//" |> not)
|> Array.map (
    fun s ->
        match s with
        | "{" -> "  [|"
        | "};" -> "  |]"
        | _ when s.EndsWith "," -> "    " + s.TrimEnd(',')
        | _ when declRegex.IsMatch(s) ->
            let mr = declRegex.Match(s)
            let name = mr.Groups.[1].Value
            $"let tab{name} = "
        | _ -> "    " + s        
    )
|> String.concat "\n"
|> printf "%s"
