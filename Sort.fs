module Sort

let compareSwap a b = if a > b then (b, a) else (a, b)

let bubble x =
    let rec bubbleTraverse source acc swaps = 
        match source with
        | x::y::tl -> 
            let (a, b) = compareSwap x y
            bubbleTraverse (b::tl) (a::acc) (swaps || (a, b) <> (x, y))
        | hd::tl ->
            let (a, b) = compareSwap acc.Head hd
            bubbleTraverse [] (b::a::acc.Tail) (swaps || (a, b) <> (acc.Head, hd))
        | [] -> (List.rev acc, swaps)
    let rec sort x =
        let s = bubbleTraverse x [] false
        match s with
        | (interim, true) -> sort interim
        | (final, false) -> final
    sort x

let rec quick x =
    match x with
    | [] -> []
    | [z] -> [z]
    | h::t -> quick (List.filter ((>=) h) t) @ [h] @ quick (List.filter ((<) h) t)
    
let insertion x =
    let rec insert x lst =
        match lst with
        | hd::tl when x <= hd -> x::lst
        | hd::tl when x > hd -> hd :: insert x tl
        | _ -> [x]
    List.foldBack insert x []

let rec merge x =
    let rec combine l1 l2 =
        match l1, l2 with
        | _, [] -> l1
        | [], _ -> l2
        | h1::t1, h2::t2 ->
            if h1 <= h2 then h1 :: combine t1 l2
            else h2 :: combine l1 t2
    match x with
    | [] -> []
    | [z] -> [z]
    | _ ->
        let size = x.Length/2
        let half1 = Seq.take size x |> List.ofSeq
        let half2 = Seq.skip size x |> List.ofSeq
        combine half1 half2