module Statistics

open System
open NUnit.Framework
open FsUnit

(* needs test methods for every function *)

let arithmeticMean x = Seq.average x

let geometricMean x =
    let product = Seq.fold (fun p c -> p * c) 1. x
    Math.Pow(product, (1. / float(Seq.length x)))

let harmonicMean x = float(Seq.length x) / (Seq.map (fun c -> 1. / c) x |> Seq.sum)

let truncatedMean (x:seq<float>) t = 
    let rem = int(Math.Floor(float(Seq.length x) * t / 2.))
    match rem with 
    | 0 -> Seq.average x
    | _ ->
        let frontTrunc = Seq.sort x |> Seq.skip rem
        let endTrunc = Seq.take ((Seq.length frontTrunc) - rem) frontTrunc
        Seq.average endTrunc

let rms x =
    let product = Seq.fold (fun p c -> p + c * c) 0. x
    Math.Sqrt(product / float(Seq.length x))

let weightedArithmeticMean x w =
    let num = Seq.zip x w |> Seq.map (fun (xi, wi) -> xi * wi) |> Seq.sum
    let den = Seq.sum w
    num / den

let weightedGeometricMean x w =
    let num = Seq.zip x w |> Seq.map (fun (xi, wi) -> Math.Log(xi) * wi) |> Seq.sum
    let den = Seq.sum w
    Math.Exp(num / den)

let weightedHarmonicMean x w = 
    let num = Seq.sum w
    let den = Seq.zip x w |> Seq.map (fun (xi, wi) -> wi / xi) |> Seq.sum
    num / den

let sortedMeasure x f = 
    f (Seq.sort x)

let statRange x = (Seq.nth ((Seq.length x) - 1) x) - (Seq.nth 0 x)

let midrange x = ((Seq.nth ((Seq.length x) - 1) x) + (Seq.nth 0 x)) / 2.0

let median x =
    match (Seq.length x) % 2 = 0 with
    | false -> Seq.nth ((Seq.length x) + 1 / 2) x
    | true -> float(Seq.nth ((Seq.length x)/ 2) x + Seq.nth (((Seq.length x)/ 2) + 1) x) / 2.0

let mode x =
    Seq.groupBy (fun c -> c) x
    |> Seq.maxBy (fun (_, l) -> Seq.length l)
    |> fst

let meanDeviation (x:seq<float>) = 
    let mean = Seq.average x
    (Seq.fold (fun d c -> d + Math.Abs(c - mean)) 0. x) / float(Seq.length x)

let medianDeviationOfMean (x:seq<float>) = 
    let mean = Seq.average x
    Seq.map (fun c -> Math.Abs(c - mean)) x |> median

let medianDeviationOfMedian (x:seq<float>) = 
    let m = median x
    Seq.map (fun c -> Math.Abs(c - m)) x |> median

let centralmoment (x:seq<float>) ub n =
    let mean = Seq.average x
    let length = if ub then (Seq.length x) - 1 else Seq.length x
    let product =
        Seq.map (fun c -> c - mean) x
        |> Seq.fold (fun p c -> p + Math.Pow(c, n)) 0.
    product / float(length)

let variance (x:seq<float>) ub = centralmoment x ub 2.

let standardDeviation (x:seq<float>) ub = Math.Sqrt(variance x ub)

let skewness (x:seq<float>) ub =
    let num = centralmoment x ub 3.
    let den = Math.Pow(centralmoment x ub 2., 3./2.)
    num / den

let kurtosis (x:seq<float>) ub =
    let num = centralmoment x ub 4.
    let den = Math.Pow(centralmoment x ub 2., 2.)
    (num / den) - 3.

let covariance x y ub =
    let xmean = Seq.average x
    let ymean = Seq.average y
    let num =
        Seq.zip x y
        |> Seq.fold (fun s (xi, yi) -> s + (xi - xmean) * (yi - ymean)) 0.
    let den = if ub then (Seq.length x) - 1 else Seq.length x
    num / float(den)

let correlation x y ub =
    let cov = covariance x y ub
    let xstd = standardDeviation x ub
    let ystd = standardDeviation y ub
    cov / (xstd * ystd)

let percentile x n =
    match n with
    | 0. -> Seq.nth 0 x
    | 1. -> Seq.nth ((Seq.length x) - 1) x
    | _ ->
        let pos = n * float((Seq.length x) - 1)
        let low = int(Math.Floor(pos))
        let high = int(Math.Ceiling(pos))
        match low = high with
        | true -> Seq.nth low x
        | false -> (Seq.nth low x) + (pos - float(low)) * ((Seq.nth high x) - (Seq.nth low x))

let percentileRank x v =
    match v < (Seq.nth 0 x) with
    | true -> 0.
    | false ->
        match v > (Seq.nth ((Seq.length x) - 1) x) with
        | true -> 1.
        | false ->
            let ((i1, x1), (i2, x2)) =
                Seq.mapi (fun i c -> (i, c)) x
                |> Seq.pairwise
                |> Seq.find (fun ((i1, x1), (i2, x2)) -> x1 <= v && x2 >= v)
            match x1 = v with
            | true -> float(i1) / float((Seq.length x) - 1)
            | false ->
                match x2 = v with
                | true -> float(i2) / float((Seq.length x) - 1)
                | false -> (float(i1) + (v - x1) / (x2 - x1)) / float((Seq.length x) - 1)

[<TestFixture>]
type StatisticsTests ()=
    let n = [5.64; 5.49; 4.82; 8.1; 6.01; 4.84; 2.15; 9.99; 1.13; 8.63;
        1.79; 0.1; 6.65; 3.68; 3.62; 9.7; 9.68; 6.27; 3.43; 4.04]
    let w = { 1. .. 20. }

    [<Test>] member x.arithmeticMean ()= arithmeticMean n |> withTwoDecimals |> should equal 5.28
    [<Test>] member x.weightedArithmeticMean ()= weightedArithmeticMean n w |> withTwoDecimals |> should equal 5.28