module Linear

open NUnit.Framework;
open FsUnit;
open Complex;
open Vector;
open Matrix;

let gaussJordan a b = b
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

    [<Test>] member x.gaussJordan ()= gaussJordan m1 b1 |> should equal r1
    [<Test>] member x.luCrout () = luCrout m2 b2 |> should equal r2
    [<Test>] member x.luCroutInverse () = luCrout m2 b2 |> should equal r2
    [<Test>] member x.gaussJacobi () = gaussJacobi m3 b3 |> should equal r3
    [<Test>] member x.gaussSeidel () = gaussSeidel m4 b4 |> should equal r4
