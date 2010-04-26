module Interpolation

open System
open NUnit.Framework
open FsUnit

let interpol ip f x i = Seq.map (fun z -> ip f x z) i
let twoVarInterpol ip f x y i = Seq.map (fun z -> ip f x y z) i

let toFloat x = Seq.map (fun c -> float(c)) x

let locateInDiscreet x i =
        Seq.pairwise x
        |> Seq.find (fun p -> (fst p <= i) && (snd p >= i))

let linear f x i =
    let (x1, x2) = locateInDiscreet x i
    let y2, y1 = f(x2), f(x1)
    let slope = (y2 - y1) / (x2 - x1)
    y1 + slope * (i - x1)

let bilinear f x y i =
    let (ix, iy) = i
    let (x1, x2) = locateInDiscreet x ix
    let (y1, y2) = locateInDiscreet y iy
    let z11 = f(x1, y1)
    let z21 = f(x2, y1)
    let z12 = f(x1, y2)
    let z22 = f(x2, y2)
    let ip1 = z11 * (x2-ix) * (y2-iy) / (x2 - x1) / (y2 - y1)
    let ip2 = z21 * (ix-x1) * (y2-iy) / (x2 - x1) / (y2 - y1)
    let ip3 = z12 * (x2-ix) * (iy-y1) / (x2 - x1) / (y2 - y1)
    let ip4 = z22 * (ix-x1) * (iy-y1) / (x2 - x1) / (y2 - y1)
    ip1 + ip2 + ip3 + ip4

let lagrange f (x:_[]) ip =
    let length = Seq.length x
    let y = Array.map (fun c -> f(c)) x
    seq { 0 .. length - 1 }
    |> Seq.fold (fun sum i ->
        seq { for j in 0 .. length - 1 do if i <> j then yield j }
        |> Seq.fold (fun l j -> l * (ip - x.[j]) / (x.[i] - x.[j])) 1.
        |> fun l -> sum + l * y.[i]) 0.

let barycentric f x ip =
    let length = Seq.length x
    let y = Array.map (fun c -> f(c)) x
    let weights =
        seq { 0 .. length - 1 }
        |> Seq.fold (fun weights i ->
            seq { for j in 0 .. length - 1 do if i <> j then yield j }
            |> Seq.fold (fun product j -> product * (x.[i] - x.[j])) 1.
            |> fun product -> product::weights) []
        |> List.rev
        |> Seq.map (fun c -> 1. / c)
        |> Seq.toArray
    let (bc1, bc2) =
        seq { 0 .. length - 1 }
        |> Seq.fold (fun (bc1, bc2) i ->
            let deltaX = weights.[i] / (ip - x.[i])
            (bc1 + y.[i] * deltaX, bc2 + deltaX)) (0., 0.)
    bc1 / bc2
    
let newtonDividedDifferece f x ip =
    let length = Seq.length x
    let y = Array.map (fun c -> f(c)) x
    let rec dividedDifference s e =
        match s with
        | _ when s = e -> y.[s]
        | _ -> ((dividedDifference (s+1) e) - (dividedDifference s (e-1))) / (x.[e] - x.[s])
    let basisPoly n = { 0 .. n - 1} |> Seq.fold (fun p i -> p * (ip - x.[i])) 1.
    { 0 .. length - 1} |> Seq.fold (fun r i -> r + (dividedDifference 0 i) * (basisPoly i)) 0.

let naturalSpline f x ip = ip
let clampedSpline fp_0 fp_n f x ip = ip

[<TestFixture>]
type InterpolationTests ()= 
    let toFourPlaces x = Seq.map (fun (c:float) -> Math.Round(c, 4)) x

    let x1 = [1; 3; 5; 7; 9] |> toFloat
    let f1(x) = x * 2.
    let i1 = [2; 4; 6; 8] |> toFloat
    let r1 = [4; 8; 12; 16] |> toFloat

    let x2 = [0; 2;] |> toFloat
    let y2 = [0; 2;] |> toFloat
    let z2 = [[0.; 20.]; [20.; 10.]]
    let f2 x2 y2 ((x:float), (y:float)) =
        let xOffset = Seq.findIndex (fun c -> c = x) x2
        let yOffset = Seq.findIndex (fun c -> c = y) y2
        Seq.nth xOffset z2 |> Seq.nth yOffset
    let i2 = [(0.1, 0.1); (0.1, 0.15); (0.1, 0.25); (0.15, 0.1)]
    let r2 = [1.925; 2.3875; 3.3125; 2.3875]

    let x3 = [2; 4; 6; 8; 10;] |> toFloat |> Seq.toArray
    let y3 = [2; 8; 18; 32; 50] |> toFloat
    let listLookup xs ys x = Seq.zip xs ys |> Seq.find (fun c -> (fst c) = x) |> snd
    let f3 = listLookup x3 y3
    let i3 = [5; 7; 3] |> toFloat
    let r3 = [12.5; 24.5; 4.5]

    let x4 = [0; 4; 8; 12; 16] |> toFloat |> Seq.toArray
    let y4 = [0; 8; 32; 72; 128] |> toFloat
    let f4 = listLookup x4 y4
    let i4 = [2; 6; 10; 14] |> toFloat
    let r4 = [2.; 18.; 50.; 98.]

    let x5 = [50; 60; 70; 80; 90] |> toFloat |> Seq.toArray
    let y5 = [75; 150; 200; 225; 250] |> toFloat
    let f5 = listLookup x5 y5
    let i5 = [55; 65; 75; 85] |> toFloat
    let r5 = [114.6484375; 178.7109375; 214.6484375; 234.9609375] |> toFourPlaces

    let x6 = [0.1; 0.2; 0.3; 0.4]
    let y6 = [-0.62049958; -0.28398668; 0.00660095; 0.24842440]
    let f6 = listLookup x6 y6
    let i6 = [0.25]
    let r6 = [-0.129559271569149]

    let x7 = [0; 1; 2; 3] |> toFloat
    let y7 = [1.; 2.718281828; 7.389056099; 20.08553692]
    let f7 = listLookup x7 y7
    let fp_0 = 1.0;
    let fp_n = 20.0855369;
    let i7 = [2.0]
    let r7 = [7.389056099]

    [<Test>] member x.linear ()= interpol linear f1 x1 i1 |> should equal r1
    [<Test>] member x.bilinear ()= twoVarInterpol bilinear (f2 x2 y2) x2 y2 i2 |> toFourPlaces |> should equal r2
    [<Test>] member x.lagrange ()= interpol lagrange f3 x3 i3 |> Seq.toList |> should equal r3
    [<Test>] member x.barycentric ()= interpol barycentric f4 x4 i4 |> toFourPlaces |> should equal r4
    [<Test>] member x.newton ()= interpol newtonDividedDifferece f5 x5 i5 |> toFourPlaces |> should equal r5
    [<Test>] member x.spline ()= interpol naturalSpline f6 x6 i6 |> toFourPlaces |> should equal r6
    [<Test>] member x.clampedSpline ()= interpol (clampedSpline fp_0 fp_n) f7 x7 i7 |> toFourPlaces |> should equal r7