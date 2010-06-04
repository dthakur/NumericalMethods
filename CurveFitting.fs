module CurveFitting

open System
open NUnit.Framework
open FsUnit

let leastSquaresStraightLine (xs:array<float>) (ys:array<float>) =
    let xave = Seq.average xs
    let yave = Seq.average ys
    let pairs = Seq.zip xs ys
    let num = Seq.fold (fun s (x, y) -> s + (y * (x - xave))) 0. pairs
    let den = Seq.fold (fun s x -> s + (x * (x - xave))) 0. xs
    let slope = num / den
    let intercept = yave - xave * slope
    let sumOfResiduals = Seq.fold (fun s (x, y) -> s + (y - intercept - slope * x) ** 2.) 0. pairs
    let sigma = Math.Sqrt(sumOfResiduals / (float(Seq.length xs)-2.))
    [intercept; slope; sigma] |> Array.ofSeq

[<TestFixture>]
type CurveFittingTests ()=
    let xs = [| 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; |]
    let ys = [| 2.9; 4.7; 6.3; 8.4; 10.5; 12.5; |]
    
    [<Test>]
    member x.leastSquaresStraightLine ()=
        leastSquaresStraightLine xs ys
            |> Seq.map (fun x -> withThreeDecimals x)
            |> Seq.toList
            |> should equal [| -1.128; 1.928; 0.190 |]