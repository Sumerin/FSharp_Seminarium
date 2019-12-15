type Human = 
|Man of string * int
|Woman of string

let isAlowedToBeInClub (folk:Human) restrict = 
    match folk with
    | Man (name, age) -> age >=restrict
    | Woman _ -> true  
    | _ -> false

let queue = [Man ("Adam", 25); Man ("Witek", 18); Woman ("Alicja");]
let yearRestriction = [30;19;16]



module List =

    // The apply function for lists
    // [f;g] apply [x;y] becomes [f x; f y; g x; g y]
    let apply (fList: ('a->'b) list) (xList: 'a list)  = 
        [ for f in fList do
          for x in xList do
              yield f x ]


    let (<!>) = List.map
    let (<*>) = apply

    let lift2 f x y = 
        f <!> x <*> y
        
    let lift3 f x y z = 
        f <!> x <*> y <*> z
        
    let lift4 f x y z w = 
        f <!> x <*> y <*> z <*> w

let (<!>) = List.map
let (<*>) = List.apply




let result1 = isAlowedToBeInClub <!> queue <*> yearRestriction

let result2 = List.lift2 isAlowedToBeInClub queue yearRestriction

