module Option =

    // The apply function for Options
    let apply fOpt xOpt = 
        match fOpt,xOpt with
        | Some f, Some x -> Some (f x)
        | _ -> None

 
module List =

    // The apply function for lists
    // [f;g] apply [x;y] becomes [f x; f y; g x; g y]
    let apply (fList: ('a->'b) list) (xList: 'a list)  = 
        [ for f in fList do
          for x in xList do
              yield f x ]


type Human = 
|Man of string * int
|Woman of string


let isAlowedToBeInClub (folk:Human) = 
    match folk with
    | Man (name, age) -> age >=21
    | Woman _ -> true  
    | _ -> false

let queue = [Man ("Adam", 25); Man ("Witek", 18); Woman ("Alicja");]


let selectionProcess = isAlowedToBeInClub |> List.map 
let inClub = queue |> selectionProcess

let lifted = [isAlowedToBeInClub]
let selectionProcess2 = lifted |> List.apply 
let inClub2 = queue|> selectionProcess2

let add x y = x+y

let resultList =  
    let (<*>) = List.apply
    [add] <*> [1;2] <*> [10;20]

