module Complex

open System
open NUnit.Framework
open FsUnit

let arg a b = Math.Atan2(b, a)
let norm a b = a*a + b*b |> Math.Sqrt
let ofPolar r theta = (r * Math.Cos(theta), r * Math.Sin(theta))
let toPolar a b = (norm a b, arg a b)
let real a b : float = a
let imaginary a b : float = b

type NumericField =
    | Complex of float * float

    member m.run f = match m with Complex(r, i) -> f r i
    member m.re = m.run (fun r i -> real r i)
    member m.im = m.run (fun r i -> imaginary r i)
    member m.conjugate = m.run (fun r i -> (r, -i)) |> Complex
    member m.arg = m.run (fun r i -> arg r i)
    member m.norm = m.run (fun r i -> norm r i)
    member m.ofPolar r theta = ofPolar r theta |> Complex
    member m.toPolar = m.run (fun r i -> toPolar r i)
    member m.transform f = m.run (fun r i -> (f(r), f(i))) |> Complex

    static member Zero = Complex(0., 0.)
    static member One = Complex(0., 0.)
    static member (+) ((l:NumericField), (r:NumericField)) = Complex(l.re+r.re, l.im+r.im)
    static member (-) ((l:NumericField), (r:NumericField)) = Complex(l.re-r.re, l.im-r.im)
    static member (*) ((l:NumericField), (r:NumericField)) =
        let x = l.re * r.re - l.im * r.im
        let y = l.re * r.im + l.im * r.re
        Complex(x, y)
    static member (/) ((l:NumericField), (r:NumericField)) = 
        let d = Math.Pow(r.re, 2.) + Math.Pow(r.im, 2.)
        let x = (l.re * r.re + l.im * r.im) / d
        let y = (l.im * r.re - l.re * r.im) / d
        Complex(x, y)
    static member ofReal x = Complex(float x, 0.)
    static member i = Complex(0., 1.)

    override m.ToString() =
        match m with
        | Complex(a,b) -> a.ToString() + " " + b.ToString() 

(* some converters *)
let inf (x:int) = Complex(float x, 0.)
let fnf (x:float) = Complex(x, 0.)
let ia2nf (x:int[,]) = Array2D.map (fun z -> inf z) x
let itnf (x, y) = Complex(float x, float y)
let itanf x = Seq.map (fun z -> itnf z) x
let ianf (x:seq<int>) = Seq.map (fun z -> inf z) x
let fanf (x:seq<float>) = Seq.map (fun z -> fnf z) x
let fa2nf (x:float[,]) = Array2D.map (fun z -> fnf z) x

[<TestFixture>]
type ComplexTests ()=
    [<Test>] member x.arg ()= Complex(2., 1.).arg |> withThreeDecimals |> should equal 0.463