module Matrix

open System
open NUnit.Framework
open FsUnit
open Vector
open Complex

let flatten (A:'a[,]) = A |> Seq.cast<'a>
let getColumn c (A:_[,]) = flatten A.[*,c..c]
let getRow r (A:_[,]) = flatten A.[r..r,*]
let rows x = [0 .. (Array2D.length1(x) - 1)] |> Seq.map (fun i -> getRow i x)
let cols x = [0 .. (Array2D.length2(x) - 1)] |> Seq.map (fun i -> getColumn i x)
let rowVectors m = rows m |> Seq.map (fun x -> Vector.Vector x)
let colVectors m = cols m |> Seq.map (fun x -> Vector.Vector x)
let transpose x = cols x |> array2D
let unroll (A:_[,]) = [0 .. (Array2D.length1(A) - 1)] |> Seq.map (fun x -> getRow x A)
let skipRow r (A:_[,]) =
    unroll A
    |> Seq.mapi (fun i x -> (i, x))
    |> Seq.filter (fun (i,x) -> i <> r)
    |> Seq.map (fun (i,x) -> x) |> array2D
let minor r c (x:_[,]) =
    skipRow r x
    |> transpose
    |> skipRow c
    |> transpose
let rec det (x:NumericField[,]) =
    match Array2D.length1(x) with
    | 1 -> x.[0,0]
    | _ ->
        rows x
        |> Seq.mapi (fun i z ->
            (fnf(Math.Pow(-1., float i))) * x.[0,i] * det (minor 0 i x))
        |> Seq.sum
let cofactor x =
    let r = rows x
    let c = cols x
    r |> Seq.mapi (fun i y -> c |> Seq.mapi (fun j z -> fnf(Math.Pow(-1., float (i+j))) * det(minor i j x)))
      |> array2D
let adjoint x = x |> cofactor |> transpose
let scale x s = rows x |> Seq.map (fun i -> Seq.map (fun j -> j / s) i) |> array2D
let inverse x =
    let a = adjoint x
    let d = det x
    scale a d

type Matrix(p: NumericField[,]) =
    member m.v = p

    member m.colVector c = getColumn c m.v |> Vector.ofSeq
    member m.rowVector r = getRow r m.v |> Vector.ofSeq
    member m.minor r c = minor r c m.v |> Matrix.ofSeq

    static member ofSeq x = Matrix(x)
    static member toCol (x:Vector) = x.v |> Seq.map (fun c -> [c]) |> array2D |> Matrix.ofSeq
    static member toRow (x:Vector) = [x.v] |> array2D |> Matrix.ofSeq

    static member (*) ((l:Matrix), (r:Matrix)) =
        let rows = rowVectors l.v
        let columns = colVectors r.v
        rows |> Seq.map (fun r -> columns |> Seq.map (fun c -> c .* r)) |> array2D |> Matrix.ofSeq
        
let identity x y = 
    Matrix(Array2D.init x y (fun _ _ -> inf 1))

[<TestFixture>]
type MatrixTests ()= 
    let a1 = array2D [ [1.; 2.; 3.]; [3.; 4.; 6.] ] |> fa2nf
    let a2 = array2D [ [7.; 10.;]; [8.; 11.;]; [9.; 12.;] ] |> fa2nf
    let a3 = array2D [ [-1.; 2.; 0.]; [3.; -2.; 1.]; [-2.; 9.; -2.] ] |> fa2nf
    let a4 = array2D [ [1.; 2.; 3.]; [0.; 1.; 4.]; [5.; 6.; 0.] ] |> fa2nf
    let m1 = Matrix(a1)
    let m2 = Matrix(a2)

    [<Test>]
    member x.matrixProduct ()=
        let ans = [[50.0; 68.0]; [107.0; 146.0]] |> array2D |> fa2nf |> Matrix.ofSeq
        (m1 * m2).v |> should equal ans.v

    [<Test>] member x.det ()= det a3 |> should equal (inf 13)
    [<Test>] member x.inverse ()= inverse a4 |> should equal (array2D [[-24.;18.;5.];[20.;-15.;-4.];[-5.;4.;1.]] |> fa2nf)