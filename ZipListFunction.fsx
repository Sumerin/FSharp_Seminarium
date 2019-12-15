module List =

    let rec zipList fList xList  = 
        match fList,xList with
        | [],_ 
        | _,[] -> 
        // either side empty, then done
            []  
        | (f::fTail),(x::xTail) -> 
        // new head + new tail
            (f x) :: (zipList fTail xTail)

    // The apply function for lists
    // [f;g] apply [x;y] becomes [f x; f y; g x; g y]
    let apply (fList: ('a->'b) list) (xList: 'a list)  = 
        [ for f in fList do
          for x in xList do
              yield f x ]


type Human = 
|Man of string * int
|Woman of string


let isAlowedToBeInClub (folk:Human) restrict = 
    match folk with
    | Man (name, age) -> age >=restrict
    | Woman _ -> true  
    | _ -> false


let firstPerson =  Man ("Adam", 25)
let secondPerson =  Man ("Witek", 18)
let thirdPerson =  Woman ("Alicja")

let queue = [Man ("Adam", 25); Man ("Witek", 18); Woman ("Alicja");]
let yearRestriction = [30;19;16]



let (<!>) = List.map
let (<^>) = List.apply
let (<*>) = List.zipList

let result1 = isAlowedToBeInClub <!> queue <^> yearRestriction
let result2 = isAlowedToBeInClub <!> queue <*> yearRestriction