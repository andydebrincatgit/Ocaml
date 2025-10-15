
 let rec map f l = match l with 
  | [] -> []
  | h::t -> (f h)::(map f t);;

let rec member x l = match l with 
| [] -> false
| h::t -> if x=h then true else member x t;;

let rec member_all x ls =  let temp  = member x in match ls with 
| [] -> false
| l1::t -> if temp l1 then member_all x t else false;;

let rec member_all x ls =  map (member x) ls;;

let div a b = b/a;;

let halve l = map (div 2) l;;

halve [1;2;3;4;5];;


let mapll f l = map (map (map f)) l;;

let halve lll = mapll (div 2) lll;;

let rec map_n f l n  = if n= 0 then map f l else map_n (map f) l (n-1);;  