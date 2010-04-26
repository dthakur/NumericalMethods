module Vector

open System
open NUnit.Framework
open FsUnit
open Complex

let euclideanDotProduct l r =
    Seq.zip l r
    |> Seq.map (fun ((a:NumericField), (b:NumericField)) -> a * b.conjugate)
    |> Seq.sum

let euclideanNorm (a:seq<NumericField>) =
    let (sum:NumericField) = Seq.map (fun (x:NumericField) -> x * x.conjugate) a |> Seq.sum
    sum.re |> Math.Sqrt

let normalize a =
    let norm = euclideanNorm a |> fnf
    Seq.map (fun x -> x / norm) a

let crossProduct a b =
    let ll = a |> List.ofSeq
    let rl = b |> List.ofSeq
    let d1 = ll.Item(1) * rl.Item(2) - ll.Item(2) * rl.Item(1)
    let d2 = ll.Item(2) * rl.Item(0) - ll.Item(0) * rl.Item(2)
    let d3 = ll.Item(0) * rl.Item(1) - ll.Item(1) * rl.Item(0)
    [d1; d2; d3] |> Seq.ofList

[<Struct>]
type Vector(s : seq<NumericField>) =
    member m.v = s

    static member ofSeq l = Vector(l)
    static member (.*) (l:Vector, r:Vector) = euclideanDotProduct l.v r.v
    static member (^*) (l:Vector, r:Vector) = crossProduct l.v r.v |> Vector.ofSeq

[<TestFixture>]
type VectorTests ()= 
    let v1 = Vector(ianf [1; 2; 3;])
    let v2 = Vector(ianf [5; 6; 7;])
    let v3 = Vector(itanf [(2, 1); (0, 0); (4, -5)])
    let v4 = Vector(itanf [(1, 1); (2, 1); (0, 0)])
    [<Test>] member x.dotProduct ()= v1 .* v2 |> should equal (inf 38)
    [<Test>] member x.norm ()= euclideanNorm v1.v |> withThreeDecimals |> should equal 3.741
    
    member x.compare (c:seq<NumericField>) (s:seq<NumericField>) =
        let cc = Seq.map (fun (n:NumericField) -> n.transform withThreeDecimals) c
        let sc = Seq.map (fun (n:NumericField) -> n.transform withThreeDecimals) s
        cc |> should equal sc

    [<Test>] member x.normalize ()= normalize v1.v |> x.compare (fanf [0.267; 0.534; 0.801])
    [<Test>] member x.cross ()= (v1 ^* v2).v |> x.compare (fanf [-4.; 8.; -4.])

    [<Test>] member x.complexDot ()= v3 .* v4 |> should equal (NumericField.Complex(3., -1.))
    [<Test>] member x.complexNormalize ()=  euclideanNorm v3.v |> withThreeDecimals |> should equal 6.782