
let rec merge cmp x y acc = match x , y with
| [], t -> acc@t
| t, [] -> acc@t
|h::t , q::s -> if cmp h q then merge cmp t y (acc@[h]) else merge cmp x s (acc@[q]);;

let rec length x acc = match x with 
|[] -> acc
|h::t -> length t (acc+1);;

let rec drop l n = if n = 0 then l else match l with 
|[] -> []
|h::t -> drop t (n-1);;

let rec take l n acc = if n = 0 then acc else match l with 
| [] -> acc
| h::t -> take t (n-1) (acc@[h]);;

let rec merge_sort x cmp= match x with
| [] -> []
| [a] -> [a]
| _ -> let n = ((length x 0)/2) in 
  let l = take x n [] in
  let s = drop x n in
  (merge cmp (merge_sort l cmp) (merge_sort s cmp) []);;

  let rec replace_char c1 c2 l = match l with 
  | [] ->[]
  | h::t ->  if c1=h then (c2::(replace_char c1 c2 t)) else (h::(replace_char c1 c2 t));;

  let clip a b x = if x<=a then a else (if x>= b then b else x);;

  let clip_1_10 = clip 1 10;;

  let rec map f l = match l with 
  | [] -> []
  | h::t -> (f h)::(map f t);;
  
  let clip_list = map clip_1_10;;
  
  let rec apply f n l = match n with 
  | 0 -> l
  | _-> apply f (n-1) (f l);;

  apply (fun n -> n+1) 6 4;;

  let rec convert f acc l = match l with
  |[] -> acc
  |h::t -> convert f (acc@[f h]) t;;

  let rec mapl f l acc = match l with 
  | [] -> acc
  |h::t -> mapl f t acc@[convert f [] h];;

  let a_to_b n = if n = 1 then 'a' else 'b';;
  mapl a_to_b [[1;32;4;1]; [1;2;1;1;1]; [1;2;321;321]] [];;
  
  