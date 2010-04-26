module Trig

open System
open NUnit.Framework
open FsUnit

(* deg rad grad converisons *)
let degPerRad =  (180. / Math.PI)
let degPerGrad = (9. /10.)

let degToRad x = x / degPerRad
let gradToDeg x = degPerGrad * x
let gradToRad x = gradToDeg x |> degToRad
let radToDeg x = degPerRad * x
let degToGrad x = x / degPerGrad
let radToGrad x = radToDeg x |> degToGrad

(* deg representations *)
let dmsToDd (d:int) (m:int) s = float(d) + float(m) / 60. + s / 3600.
let ddToDms d =
    let unroll (x, (y, z)) = (x, y, z)
    let loop f x =
        let i = Math.Floor (x:float)
        (i, f((x - i) * 60.))
    loop (loop Math.Round) d |> unroll

(* trigs *)
let sec x = 1.0 / Math.Cos x
let csc x = 1.0 / Math.Sin x
let cot x = Math.Cos x / Math.Sin x

let asec x = Math.Acos(1. / x)
let acsc x = Math.Asin(1. / x)
let acot x = Math.Atan(1. / x)

let sech x = 1. / Math.Cosh x
let csch x = 1. / Math.Sinh x
let coth x = Math.Cosh x / Math.Sinh x

let asinh x = Math.Log(x + Math.Sqrt(x * x + 1.))
let acosh x = Math.Log(x + Math.Sqrt(x * x - 1.))
let atanh x = Math.Log((1. + x) / (1. - x)) / 2.

let acoth x = atanh(1. / x)
let asech x = acosh(1. / x)
let acsch x = asinh(1. / x)

[<TestFixture>]
type TrigTests ()= 
    [<Test>] member x.degToRad ()= degToRad 60. |> withThreeDecimals |> should equal 1.047
    [<Test>] member x.gradToRad ()= gradToRad 222. |> withThreeDecimals |> should equal 3.487
    [<Test>] member x.radToDeg ()= radToDeg 3.2 |> withThreeDecimals |> should equal 183.346
    [<Test>] member x.gradToDeg ()= gradToDeg 212. |> withThreeDecimals |> should equal 190.8
    [<Test>] member x.degToGrad ()= degToGrad 212. |> withThreeDecimals |> should equal 235.555
    [<Test>] member x.radToGrad ()= radToGrad 3.14 |> withThreeDecimals |> should equal 199.898
    
    [<Test>] member x.dmsToDd ()= dmsToDd 2 3 4. |> withDecimals 2 |> should equal 2.05
    [<Test>] member x.ddToDms ()= ddToDms 33.1234 |> should equal (33., 7., 24.)

    [<Test>] member x.sec ()= sec 1. |> withThreeDecimals |> should equal 1.850
    [<Test>] member x.csc ()= csc 1. |> withThreeDecimals |> should equal 1.188
    [<Test>] member x.cot ()= cot 1. |> withThreeDecimals |> should equal 0.642
    
    [<Test>] member x.asec ()= asec 2. |> withThreeDecimals |> should equal 1.047
    [<Test>] member x.acsc ()= acsc 1. |> withThreeDecimals |> should equal 1.570
    [<Test>] member x.acot ()= acot 1. |> withThreeDecimals |> should equal 0.785

    [<Test>] member x.sech ()= sech 2. |> withThreeDecimals |> should equal 0.265
    [<Test>] member x.csch ()= csch 1. |> withThreeDecimals |> should equal 0.850
    [<Test>] member x.coth ()= coth 1. |> withThreeDecimals |> should equal 1.313

    [<Test>] member x.asinh ()= asinh 1. |> withThreeDecimals |> should equal 0.881
    [<Test>] member x.acosh ()= acosh 2. |> withThreeDecimals |> should equal 1.316
    [<Test>] member x.atanh ()= atanh 0.5 |> withThreeDecimals |> should equal 0.549

    [<Test>] member x.asech ()= asech 0.5 |> withThreeDecimals |> should equal 1.316
    [<Test>] member x.acsch ()= acsch 0.5 |> withThreeDecimals |> should equal 1.443
    [<Test>] member x.acoth ()= acoth 2.5 |> withThreeDecimals |> should equal 0.423