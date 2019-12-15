let f a =
    match a with
    | 0 -> None
    | _ -> Some <| 10 / a

let h b =
    if b < 0 then None else Some b



let result = Some 5 |> f |> h

let result1 = Some -1 |> Option.bind f |> Option.bind h 
let result2 = Some 0 |> Option.bind f |> Option.bind h 
let result3 = Some 2 |> Option.bind f |> Option.bind h 

let (>>=) a b = Option.bind b a

let result1 = Some -1 >>= f >>= h 
let result2 = Some 0 >>= f >>=  h 
let result3 = Some 2 >>= f >>=  h 