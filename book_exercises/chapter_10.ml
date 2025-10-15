type colour  = Red | Green | Blue | Yellow | RGB of int * int * int;;
let components c = match c with 
|Red -> (255,0,0)
|RGB (r,g,b) -> (r,g,b)
|_ -> (0,0,0);;


type 'a option = None | Some of 'a;;
let number = Some 50;;