type Human = 
|Man of string * int
|Woman of string


let isAlowedToBeInClub (folk:Human) = 
    match folk with
    | Man (name, age) -> age >=21
    | Woman _ -> true  
    | _ -> false


let firstPerson =  Man ("Adam", 25)
let secondPerson =  Man ("Witek", 18)
let thirdPerson =  Woman ("Alicja")

let AdamIsInClub = isAlowedToBeInClub firstPerson

let queue = [Man ("Adam", 25); Man ("Witek", 18); Woman ("Alicja");]


let selectionProcess = List.map isAlowedToBeInClub
let inClub = queue |> selectionProcess
