module Bits

open System
open System.Linq
open NUnit.Framework
open FsUnit

let toNum x =
    match Char.IsNumber x with
    | true -> Int32.Parse(x.ToString())
    | false -> (Convert.ToInt32(x.ToString().ToLower()) - 55)

let toChar x =
    match x < 10 with
    | true -> x.ToString().First();
    | false -> Convert.ToChar(x + 55)

let toBase10 (x:string) currentBase =
    let chars = x.ToCharArray()
    let calc x s =
        let (position, sum) = s
        let currentMult = Math.Pow(float(currentBase), float(position))
        let newSum = sum + float(toNum x) * currentMult
        (position+1, newSum)
    snd (Array.foldBack calc chars (0, 0.))

let fromBase10 x newBase =
    let rec calc x s =
        let (q, acc:list<Char>) = s
        let r = toChar (q % newBase)
        let newQ = q / newBase
        match newQ >= newBase with
        | true -> calc newQ (newQ, r::acc)
        | false -> String.Join("", ((toChar newQ)::r::acc))
    calc x (x, [])

[<TestFixture>]
type BitTests ()= 
    [<Test>] member x.toBase10 ()= toBase10 "101" 2 |> should equal 5
    [<Test>] member x.fromBase10 ()= fromBase10 8 2 |> should equal "1000"