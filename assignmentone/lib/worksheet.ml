(** Question 1.
    [is_even x] returns true if [x] is even, otherwise false.
*)
let is_even (x : int): bool = if (x mod 2) = 0 then true else false;;

(** Question 2.
  [remove_int l n] removes every occurrence of [n] from [l].
*)
let rec remove_int (l: int list) (n: int) = match l with 
| [] -> []
| h::t -> if h=n then remove_int t n else h::(remove_int t n);;
  

(** Question 3.
  [sum l] returns the summation of the elements of [l].
*)
let rec sum (l : int list): int = match l with 
| [] -> 0
| h::t -> h + (sum t);;
  
  
(** Question 4.
  [rev l] reverses the elements of [l].
*)
let rev (l : 'a list) : 'a list =
  let rec reverse s acc = match s with 
  | [] -> acc
  | h::t -> reverse t (h::acc)
in ( reverse l []);;
  

(** Question 5.
  [even_sized_palindrome l] returns the even-sized palindrome of [l].

  Length of result is 2 * [l].
*)
let even_sized_palindrome (l: 'a list): 'a list  =
let l_rev = rev l in 
let rec concat b a_rev = match a_rev with 
| [] -> b
| h::t -> concat (h::b) t 
in (concat l_rev l_rev);;
 
(** Question 6.
  [is_palindrome l] returns true if [l] is a palindrome, otherwise false.
*)
let is_palindrome (l: 'a list): bool =
  let l_rev = rev l in 
  let rec comp a b = match a, b with
  | [] ,[] -> true
  | t, [] -> false
  | [], t -> false
  |h::t , j::s -> if h = j  then comp t s else false
in ( comp l_rev l);;

