type colour  = Red | Green | Blue | Yellow | RGB of int * int * int;;
let components c = match c with 
|Red -> (255,0,0)
|Green -> (0,255,0)
|Blue -> (0,0,255)
|RGB (r,g,b) -> (r,g,b)
|Yellow-> (255,255,0);;


type 'a option = None | Some of 'a;;
let number = Some 50;;

let rec look_up k l = match l with
| [] -> None
| (x,y)::t -> if x = k then Some y else look_up x t;;

type 'a sequence = Nil | Cons of 'a * 'a sequence;;

(*append a to b*)
let rec append a l = match l with 
| Nil -> a
| Cons(h,t) -> Cons (h, append a t);;


let rec len l = match l with 
|Nil -> 0
|Cons(_,t) -> 1+ len t;;

type expr = Num of int | Add of expr*expr | Sub of expr*expr | Mult of  expr*expr | Div of expr*expr ;;

let rec eval e = match e with 
| Num a -> a
| Add (e1,e2) -> (eval e1) + (eval e2)
| Sub(e1,e2) -> (eval e1) - eval(e2)
| Mult (e1,e2) -> (eval e1) * (eval e2)
| Div(e1,e2) -> if (eval(e2) <> 0) then ((eval e1) / eval(e2)) else raise Division_by_zero;;

let rec list_to_sequence l = match l with 
| [] -> Nil
| h::t -> Cons (h, list_to_sequence(t));;

type rectangle = Width_Height of int*int;;

let area r = match r with 
| Width_Height (a,b) -> a*b;;

let rot_tall r = match r with 
| Width_Height (a, b) -> if a>b then Width_Height(b,a) else Width_Height(a,b);;

 let rec map f l = match l with 
  | [] -> []
  | h::t -> (f h)::(map f t);;

  let make_width_small l = map rot_tall l;;
let rec len_list l = match l with 
|[] -> 0
|_::t -> 1+ len_list t;;

  let rec merge cmp x y acc = match x , y with
| [], t -> acc@t
| t, [] -> acc@t
|h::t , q::s -> if cmp h q then merge cmp t y (acc@[h]) else merge cmp x s (acc@[q]);;

let rec drop l n = if n = 0 then l else match l with 
|[] -> []
|h::t -> drop t (n-1);;

let rec take l n acc = if n = 0 then acc else match l with 
| [] -> acc
| h::t -> take t (n-1) (acc@[h]);;

let rec merge_sort x cmp= match x with
| [] -> []
| [a] -> [a]
| _ -> let n = ((len_list x )/2) in 
  let l = take x n [] in
  let s = drop x n in
  (merge cmp (merge_sort l cmp) (merge_sort s cmp) []);;

let compare_widths r1 r2 = match r1,r2 with 
| Width_Height(a1,b1), Width_Height(a2,b2)-> if a1>=a2 then true else false;;
let sort_widest_first l = let s = make_width_small l in 
merge_sort s compare_widths;;

sort_widest_first [Width_Height (20 ,12) ; Width_Height (1 ,2); Width_Height (3 ,2); Width_Height(1 ,1)];;

let rec take_n_seq n s  =if n = 0 then Nil else match s with
|Nil -> Nil
|Cons(h,t) -> Cons(h,take_n_seq (n-1) t);;

let rec drop_n_seq n s = if n= 0 then s else match s with 
| Nil -> Nil
|Cons(h,t)-> drop_n_seq (n-1) t;;

let rec map_seq f s = match s with 
| Nil -> Nil
| Cons(h,t) -> Cons (f h ,map_seq f t);;

type expr = Num of int | Add of expr*expr | Sub of expr*expr | Mult of  expr*expr | Div of expr*expr | Pow of expr*expr;;

exception InvalidType ;;
let rec power x y = if y<=0 then 1 else (x*(power x (y-1)));;
let rec eval e = match e with 
| Num a -> a
| Add (e1,e2) -> (eval e1) + (eval e2)
| Sub(e1,e2) -> (eval e1) - eval(e2)
| Mult (e1,e2) -> (eval e1) * (eval e2)
| Div(e1,e2) -> if (eval(e2) <> 0) then ((eval e1) / eval(e2)) else raise Division_by_zero
| Pow(e1,e2) -> power (eval e1) (eval e2);;

eval (Add((Num(2)), (Mult((Num(4)),(Pow(Num(3), Num(4)))))));;



