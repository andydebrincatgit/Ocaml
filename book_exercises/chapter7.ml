let rec take n l = match l with 
| [] -> if n = 0 then [] else raise (Invalid_argument "take")
| h::t -> if n <0 then raise (Invalid_argument "take") else if n = 0 then [] else h::(take (n-1) t);;

exception NotPrime of int;;
exception Problem;;

let add_2 p l = if p <> 2 then raise (NotPrime p) else p::l;;

add_2 3 [12];;

let safe_divide x y = 
  try x/y with Division_by_zero->0;;

safe_divide 2 0;;

let rec last l = match l with 
| [] -> raise Not_found
|[x] -> x
|_::t -> last t;;

last [1;2;34];;
last [];;

let smallest_pos l = let rec smallest_r min flag s = match s with 
|[] -> if (flag = false) then (raise Not_found) else min
|h::t -> if ((flag = false)&&(h>=0)) then smallest_r h true t else if ((flag = false)&&(h<0)) then smallest_r min false t else
   if ((flag = true)&&(h<0)) then smallest_r min true t else if ((flag = true)&&(h>=0)&&(min<=h)) then smallest_r min true t
   else  smallest_r h true t 
  in smallest_r 0 false l;;

smallest_pos [(-21321);32;333;4;1];;
smallest_pos [(-21321);(-1);(-2)];;

let smallest_or_zero l = try smallest_pos l with Not_found -> 0;;

smallest_or_zero [(-21321);(-1);(-2)];;


exception Imaginary of int;; 

let rec sqrt_int_lb_a a x = if x <0 then (raise (Imaginary x)) else if (a*a)<=x then sqrt_int_lb_a (a+1) x else (a-1);;

let sqrt_int_lb = sqrt_int_lb_a 0;;


let sqrt_int_lb_handled x  = try sqrt_int_lb x with Imaginary _ -> 0;;



