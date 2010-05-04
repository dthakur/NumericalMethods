module Linear

open System;
open NUnit.Framework;
open FsUnit;
open Complex;
open Vector;
open Matrix;

let gaussJordan (a:_[,]) b =
    let reducedRowEchelon a b =
        let r = augment a b
        let pivot x i =
            // printfn "%A %A" x i
            { 0 .. Array2D.length2(x) - 1 } |> Seq.iter (fun j -> x.[i, j] <- x.[i, j] / x.[i, i]) 
            seq { for j in 0 .. Array2D.length1(x) - 1 do if i <> j then yield j } 
            |> Seq.iter (fun j ->
                // printfn "%A %A" x i
                { 0 .. Array2D.length2(x) - 1 } |> Seq.iter (fun k ->
                    // printfn "%A %A" x i
                    x.[j, k] <- x.[j, k] - x.[j, k] * x.[i, k]))
        { 0 .. Array2D.length1(r) - 1 } |> Seq.iter (fun i -> pivot r i)
        unaugment r
    let (ap, bp) = reducedRowEchelon a b
    { 0 .. Array2D.length1(ap) - 1 }
        |> Seq.map (fun i -> ap.[i,i])
        |> Seq.zip bp
        |> Seq.map (fun (j, k) -> j / k)

let luCrout a b = b
let gaussJacobi a b = b
let gaussSeidel a b = b

[<TestFixture>]
type LinearTests ()=
    let m1 = Matrix(array2D [ [2; 4; -6]; [6; -4; 2]; [4; 2; 6] ] |> ia2nf)
    let b1 = Vector([6; -2; 4] |> ianf)
    let r1 = Vector([0.476190476190476; 1.19047619047619; -0.0476190476190476] |> fanf)

    let m2 = Matrix(array2D [[2; 4; -6]; [6; -4; 2]; [4; 2; 6]] |> ia2nf)
    let b2 = Vector([4; 4; 8] |> ianf)
    let r2 = Vector([1.14285714285714; 0.857142857142857; 0.285714285714286] |> fanf)

    let m3 = Matrix(array2D [ [4; 0; 1]; [0; 3; 2]; [1; 2; 4] ] |> ia2nf)
    let b3 = Vector([2; 1; 3] |> ianf)
    let r3 = Vector([0.310397736820174; -0.17227270181287; 0.751248669722443] |> fanf)

    let m4 = Matrix(array2D [ [4; 0; 1]; [0; 3; 2]; [1; 2; 4] ] |> ia2nf)
    let b4 = Vector([2; 1; 3] |> ianf)
    let r4 = Vector([0.310390073145789; -0.172293138277897; 0.758549050852501] |> fanf)

    [<Test>] member x.gaussJordan ()= gaussJordan m1.v b1.v |> should equal r1
    [<Test>] member x.luCrout () = luCrout m2 b2 |> should equal r2
    [<Test>] member x.luCroutInverse () = luCrout m2 b2 |> should equal r2
    [<Test>] member x.gaussJacobi () = gaussJacobi m3 b3 |> should equal r3
    [<Test>] member x.gaussSeidel () = gaussSeidel m4 b4 |> should equal r4
