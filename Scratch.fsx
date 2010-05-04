#r "nunit.framework.dll"
#load "FsUnit.fs"
#load "Complex.fs"
#load "Vector.fs"
#load "Matrix.fs"
#load "Sort.fs"
#load "Search.fs"
#load "Bits.fs"
#load "Interpolation.fs"
#load "Linear.fs"
#load "Nonlinear.fs"
#load "Random.fs"

open System
open FsUnit
open Complex
open Vector
open Matrix
open Sort
open Search
open Bits
open Interpolation
open Linear
open Nonlinear
open Random

let r = Random();
let a1 = array2D [ [1.; 2.; 3.]; [3.; 4.; 6.] ] |> fa2nf
let a2 = array2D [ [7.; 10.;]; [8.; 11.;]; [9.; 12.;] ] |> fa2nf
let a3 = array2D [ [-1.; 2.; 0.]; [3.; -2.; 1.]; [-2.; 9.; -2.] ] |> fa2nf
let m1 = Matrix(a1)
let m2 = Matrix(a2)
let m3 = Matrix(a3)
let v3 = Vector(itanf [(1, 0); (0, 2); (0, 3)])
let v4 = Vector(itanf [(0, 2); (-3, 0); (1, 0)])
let s1 = [1; 3; 5; 9; 4; 2; 5; 2; 4;]
let s2 = List.init 10000 (fun i -> r.Next(0, 10000))

#time

insertion s1;;
quick s1;;
bubble s1;;
merge s1;;

binary (quick s1) 9;;

toBase10 "0" 2
toBase10 "1" 2
toBase10 "10" 2
toBase10 "11" 2
toBase10 "100" 2