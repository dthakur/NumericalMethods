module Nonlinear

open System
open NUnit.Framework
open FsUnit

let linearIncrement f xstart (xdelta:float) max =
    let ((x1, x2), (y1, y2)) =
        seq { xstart .. xdelta .. max }
        |> Seq.pairwise
        |> Seq.map (fun (a, b) -> ((a, b), (f(a), f(b))))
        |> Seq.find (fun (_, (y1, y2)) -> y1 * y2 < 0.)
    x2 - (xdelta * y2) / (y2 - f(x2 - xdelta))
    
let rec bisection f (x1:float) x2 epsilon =
    match Math.Abs(x2 - x1) > epsilon with
    | false -> x2 - (x2 - x1) * f(x2) / (f(x2) - f(x1))
    | true ->
        let mid = (x2 + x1) * 0.5
        match f(x2) * f(mid) > 0. with
        | true -> bisection f x1 mid epsilon
        | false -> bisection f mid x2 epsilon

let rec secant f x1 (x2:float) epsilon = 
    let current = float(f(x2))
    match Math.Abs(current) > epsilon with
    | false -> x2
    | true -> secant f x2 (x2 - (x2 - x1) * f(x2) / (f(x2) - f(x1))) epsilon

let rec falsePositioning f (x1:float) x2 epsilon =
    let interp = x2 - (x2 - x1) * f(x2) / (f(x2) - f(x1))
    match Math.Abs(f(interp)) > epsilon with
    | false -> interp
    | true -> 
        match Math.Abs(x2 - x1) > epsilon with
        | false -> interp
        | true ->
            match f(x2) * f(interp) > 0. with
            | true -> falsePositioning f x1 interp epsilon
            | false -> falsePositioning f interp x2 epsilon

(* needs the function to be in fixed point form *)
let rec fixedPoint f x epsilon =
    let y = float(f(x))
    let currentEpsilon = Math.Abs(y - x)
    match currentEpsilon < epsilon with
    | false -> fixedPoint f y epsilon
    | true -> x

(* needs the prime *)
let rec newtonRaphson f fp x epsilon =
    match Math.Abs(float(f(x))) > epsilon with
    | false -> x
    | true -> newtonRaphson f fp (x - f(x) / fp(x)) epsilon

[<TestFixture>]
type NonlinearTests ()=
    let f1 x = x*x*x - 5.*x + 3.
    let f2 x = Math.Sqrt(35.0-2.*x)
    let f3 x = Math.Sin(x) - x * x * x * x
    let f3p x = Math.Cos(x) - 4. * x * x * x

    [<Test>] member x.linearIncrement ()= linearIncrement f1 -4. 0.01 500. |> withThreeDecimals |> should equal -2.490
    [<Test>] member x.bisection ()= bisection f1 1. 2. 0.0001 |> withDecimals 4 |> should equal 1.8342
    [<Test>] member x.secant ()= secant f1 1. 2. 0.0001 |> withDecimals 4 |> should equal 1.8342
    [<Test>] member x.falsePositioning ()= falsePositioning f1 1. 2. 0.0001 |> withDecimals 4 |> should equal 1.8342
    [<Test>] member x.fixedPoint ()= fixedPoint f2 1.6 0.0001 |> withDecimals 4 |> should equal 5.0000
    [<Test>] member x.newtonRaphson ()= newtonRaphson f3 f3p 1. 0.0001 |> withDecimals 4 |> should equal 0.9496
