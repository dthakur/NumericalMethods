module Search

let linear x y =  List.findIndex ((=) y) x
let binary x y = 
    let rec loop l (low, high) =
        match l with
        | [] -> -1
        | [z] -> if z = y then 0 else -1
        | _ ->
            let m = (low + high) / 2
            let middle = l.Item(m)
            match middle with
            | _ when middle = y -> m
            | _ when middle < y -> loop l (m + 1, high)
            | _ -> loop l (low, m - 1)
    loop x (0, x.Length)
