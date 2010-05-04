module Differentiation

open System
open NUnit.Framework
open FsUnit

let fdForward1 f x h = f
let fdForward2 f x h = f
let fdForward3 f x h = f
let fdForward4 f x h = f

let fdBackward1 f x h = f
let fdBackward2 f x h = f
let fdBackward3 f x h = f
let fdBackward4 f x h = f

let fdCentral1 f x h = f
let fdCentral2 f x h = f
let fdCentral3 f x h = f
let fdCentral4 f x h = f

let fdOh4Central1 f x h = f
let fdOh4Central2 f x h = f
let fdOh4Central3 f x h = f
let fdOh4Central4 f x h = f

let fdRichardson1 f x h t = f
let fdRichardson2 f x h t = f
let fdRichardson3 f x h t = f
let fdRichardson4 f x h t = f

(* use interpolation to interpolate and test functions *)