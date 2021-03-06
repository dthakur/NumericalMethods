﻿module FsUnit
open NUnit.Framework
open NUnit.Framework.Constraints
open System

let should (f : 'a -> #Constraint) x (y : obj) =
    let c = f x
    let y =
        match y with
        | :? (unit -> unit) -> box (new TestDelegate(y :?> unit -> unit))
        | _                 -> y
    Assert.That(y, c)

let equal x = new EqualConstraint(x)

let not x = new NotConstraint(x)

let contain x = new ContainsConstraint(x)

let haveLength n = Has.Length.EqualTo(n)

let haveCount n = Has.Count.EqualTo(n)

let be = id

let Null = new NullConstraint()

let Empty = new EmptyConstraint()

let EmptyString = new EmptyStringConstraint()

let NullOrEmptyString = new NullOrEmptyStringConstraint()

let True = new TrueConstraint()

let False = new FalseConstraint()

let sameAs x = new SameAsConstraint(x)

let throw = Throws.TypeOf

let withDecimals (y:int) x =
    let scale = Math.Pow(10., float y)
    Math.Truncate(x * scale) / scale

let withTwoDecimals x =
    withDecimals 2 x

let withThreeDecimals x =
    withDecimals 3 x