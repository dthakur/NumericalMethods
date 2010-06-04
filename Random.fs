module Random

open System
open NUnit.Framework
open FsUnit
open Special

let uniqueRandom n =
    let r = Random()
    let rec unique x =
        let current = r.NextDouble()
        match Seq.exists (fun c -> c = current) x with
        | false -> current::x
        | true -> unique x
    Seq.fold (fun x c -> unique x) [] { 0 .. (n - 1)}

let bernoulli (r:Random) p = if r.NextDouble() <= p then 1. else 0.
let bernoulliPmf x p = Math.Pow(p, float(x)) * Math.Pow(1. - p, 1. - float(x))

let binomial (r:Random) n p = Seq.fold (fun s c -> s + bernoulli r p) 0. { 0 .. n }
let binomialPmf x n p =
    let num = gamma (n+1.) * Math.Pow(p, float(x)) * Math.Pow(1. - p, n - float(x))
    let den = gamma (x+1.) * gamma (n - float(x) + 1.)
    num / den
    
let geometric (r:Random) p = Math.Ceiling(Math.Log(r.NextDouble()) / Math.Log(1. - p))
let geometricPmf x p = p * Math.Pow(1. - p, float(x) - 1.)

let negativeBinomial r p = p (* finish this! *)
let negativeBinomialPmf r x p =
    let num = gamma (float(r + x)) * Math.Pow(p, float(r)) * Math.Pow(1. - p, float(x))
    let den = gamma (float(r)) + gamma(float(x + 1))
    num / den

let triangular (r:Random) a b c = a
let triangularPmf x a b c = x

let poisson (r:Random) lambda = lambda
let poissonPmf x lambda = x

let uniform (r:Random) a b = a
let uniformPmf x a b = x

let beta (r:Random) a b = a
let betaPdf x a b = x

let betaPrime (r:Random) a b  = a
let letPrimePdf x a b = x

let cauchy (r:Random) a b = a
let cauchyPdf x a b = x

let chi (r:Random) n sigma = n
let chiPdf x n sigma = x

let chiSquare (r:Random) n sigma = n
let chiSquarePdf x n sigma = x

let erlang (r:Random) a b = a
let erlangPdf x a b = x

let exponential (r:Random) a = a
let exponentialPdf x a = x

let extremeValue (r:Random) mu sigma = mu
let extremeValuePdf x mu sigma = x

let laplace (r:Random) sigma = sigma
let laplacePdf x sigma = x

let logistic (r:Random) a b = a
let logisticPdf x a b = x

let lognomal (r:Random) mu sigma = mu
let lognomalPdf x mu sigma = x

let normal (r:Random) mu sigma = mu
let normalPdf x mu sigma = x
let normalCdf x = 0.5 * (1. + erf (x / Math.Sqrt(2.)))

let pareto (r:Random) a b = a
let paretoPdf x a b = x

let rayleigh (r:Random) sigma = sigma
let rayleighPdf x sigma = x

let studentT (r:Random) n = n
let studentTPdf x n = x

let uniformContinuous (r:Random) a b = a
let uniformContinuousPdf x a b = a

let weibull (r:Random) a b = a
let weibullPdf x a b = x

[<TestFixture>]
type RandomTests ()= 
    [<Test>] member x.normalCdf1 ()= normalCdf 0.5 |> withThreeDecimals |> should equal 0.691
    [<Test>] member x.normalCdf2 ()= normalCdf 1.1 |> withThreeDecimals |> should equal 0.864