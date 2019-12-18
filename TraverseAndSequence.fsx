module Option =
    // The apply function for Options
    let apply fOpt xOpt = 
        match fOpt,xOpt with
        | Some f, Some x -> Some (f x)
        | _ -> None


type Result<'a> =
    | Success of 'a
    | Failure of string list

module Result =
    let retn x =
        Success x

    let apply fResult xResult =
        match fResult,xResult with
        | Success f, Success x ->
            Success (f x)
        | Failure errs, Success x ->
            Failure errs
        | Success f, Failure errs ->
            Failure errs
        | Failure errs1, Failure errs2 ->
            // concat both lists of errors
            Failure (List.concat [errs1; errs2])


//CODE
let (<*>) = Option.apply
let retn = Some


let rec mapOption f list =
    let cons head tail = head :: tail
    match list with
    | [] -> 
        retn []
    | head::tail ->
        retn cons <*> (f head) <*> (mapOption f tail)

let parseInt str =
    match (System.Int32.TryParse str) with
    | true,i -> Some i
    | false,_ -> None
    
let good = ["1";"2";"3"] |> mapOption parseInt
let bad = ["1";"x";"y"] |> mapOption parseInt





let (<*>) = Result.apply
let retn = Success

let rec mapResult f list =
    let cons head tail = head :: tail
    match list with
    | [] -> 
        retn []
    | head::tail ->
        retn cons <*> (f head) <*> (mapResult f tail)

let parseInt str =
    match (System.Int32.TryParse str) with
    | true,i -> Success i
    | false,_ -> Failure [str + " is not an int"]

let good = ["1";"2";"3"] |> mapResult parseInt

let bad = ["1";"x";"y"] |> mapResult parseInt



module List =   
    let sequenceResult x =mapResult id x

let goodSequenceA = 
    ["1"; "2"; "3"] 
    |> List.map parseInt
    |> List.sequenceResult

let badSequenceA = 
    ["1"; "x"; "y"] 
    |> List.map parseInt
    |> List.sequenceResult
