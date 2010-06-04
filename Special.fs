module Special

open System
open NUnit.Framework
open FsUnit

let rec factorial x = if x <= 1 then 1 else x * factorial (x - 1)
let combination n k = n
let permutation n r = n
let gamma x = x
let beta x y = x
let si x = x
let ci x = x
let laguerre x deg = x
let hermite x deg = x
let chebyshev x deg = x
let legendre x deg = x
let bessel x a = x

let erfcCheb x =
    let coef = [| -1.3026537197817094; 6.4196979235649026e-1; 1.9476473204185836e-2;
        -9.561514786808631e-3; -9.46595344482036e-4; 3.66839497852761e-4; 4.2523324806907e-5;
        -2.0278578112534e-5; -1.624290004647e-6; 1.303655835580e-6; 1.5626441722e-8;
        -8.5238095915e-8; 6.529054439e-9; 5.059343495e-9; -9.91364156e-10; -2.27365122e-10;
        9.6467911e-11; 2.394038e-12; -6.886027e-12; 8.94487e-13; 3.13092e-13; -1.12708e-13; 3.81e-16;
        7.106e-15; -1.523e-15; -9.4e-17; 1.21e-16; -2.8e-17 |]
    let t = 2. / (2. + x)
    let ty = 4. * t - 2.
    let (d, dd) = Seq.fold (fun (d, dd) c -> (ty * d - dd + c, d)) (0., 0.) (Seq.skip 1 coef |> Seq.toList |> List.rev)
    t * Math.Exp(-x * x + 0.5 * (coef.[0] + ty * d) - dd);

let erf x = if x >= 0. then 1. - erfcCheb x else (erfcCheb -x) - 1.

[<TestFixture>]
type SpecialTests ()= 
    [<Test>] member x.erf1 ()= erf 0.05 |> withThreeDecimals |> should equal 0.056
    [<Test>] member x.erf2 ()= erf 0.30 |> withThreeDecimals |> should equal 0.328
    [<Test>] member x.erf3 ()= erf 1.80 |> withThreeDecimals |> should equal 0.989
    [<Test>] member x.erf4 ()= erf 3.10 |> withThreeDecimals |> should equal 0.999