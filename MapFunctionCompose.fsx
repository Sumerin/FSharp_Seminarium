let f x =
    match x with
    | x when x%2=0 -> x
    | _ -> 0

let g x =
    match x with
    | x when x%3=0 -> x
    | _ -> 0

let h = f >> g

let liftedH = List.map h


let f' = List.map f
let g' = List.map g
let h' = f'>> g'


let x = [1;2;3;4;5;6;7;8;9]

let result = liftedH x
let result2 = h' x
