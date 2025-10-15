
let pair_first (x,_) = x;;
let pair_second (_,y) = y;;

pair_first (1,'a');;

let dic = [(1,'c'); (2, 'x'); (3,'f')];;

let rec lookup x l = match l with
| [] -> (raise Not_found)
| (w,z)::t -> if (x=w) then z else lookup x t;;

lookup 2 dic;;


let rec add k v l = match l with
| []-> [(k,v)]
| (x,y)::t -> if x = k then (x , v)::t else (x,y)::(add k v t);;

add 4 'w' dic;;
add 2 'm' dic;;

let rec remove k l = match l with 
| [] -> []
| (x,y)::t -> if x = k then t else (x,y)::(remove k t);;

remove 2 dic;;


let rec key_exists k f = try let _ = lookup k f in true with Not_found->false;;

key_exists 1 dic;;

let rec no_of_keys l acc= match l with 
|[] -> 0
|h::t -> no_of_keys t (1+acc);;

let rec replace k v l = match l with 
| [] -> raise Not_found
| (x,y)::t -> if x=k then ((x,v)::t) else (x,y)::(replace k v t) ;;

let rec build l1 l2 = match l1, l2 with 
| [],[] -> []
| h::t, p::q -> (h,p)::(build t q)
| _ -> raise (Invalid_argument "lists not equal");;



let rec rev_p l acc = match l with 
| [] -> acc
| h::t -> rev_p t (h::acc);;

let rev l = rev_p l [];;

let split d = let rec split_r q (l1,l2) = match q with 
| [] ->  (rev(l1),rev(l2))
| (x,y)::t -> split_r t ((x::l1), (y::l2))
in split_r d ([],[]);;

let rec convert_into_dic_w l dic= match l with
| [] -> rev dic
| (x,y)::t -> if (key_exists x dic) then convert_into_dic_w t dic else convert_into_dic_w t ((x,y)::dic);;

let convert_into_dic l = convert_into_dic_w l [];;

convert_into_dic [(1,'a'); (1, 'b'); (4, 'c'); (5, 'd'); (5, 'd')];;

let rec union a b dic= match a, b with
| [], [] -> dic
| (x,y)::t, [] -> if key_exists x dic then union t [] dic else union t [] ((x,y)::dic)
| [], (x,y)::t -> if key_exists x dic then union [] t dic else union [] t((x,y)::dic)
| (x1, y1)::t1, (x2, y2)::t2 -> let flag_1 = key_exists x1 dic in if (x1 = x2 && flag_1<>true) then union t1 t2 ((x1,y1)::dic) else
  let flag_2 = key_exists x2 dic in
  if ((x1 = x2 && flag_1)||(flag_1&&flag_2)) then  union t1 t2 dic else if ((flag_1<>true) && flag_2) then  union t1 t2 ((x1,y1)::dic) else
     if (flag_1&& (flag_2<>true)) then  union t1 t2 ((x2,y2)::dic) else union t1 t2 ((x1,y1)::((x2,y2)::dic));;

let union_wrap a b = union a b [];;

union_wrap [(1,'a');  (4, 'c'); (5, 'd')] [ (5, 'd'); (1, 'c'); (3,'a')];;

