(* ----------------------------- Type definitions ----------------------------*)

(** Alphabet set of characters. *)
type alpha = char

(** 'Not in alphabet' exception. *)
exception Not_in_alphabet of alpha


(* ----------------------------- Supporting code ---------------------------- *)
(** [map f s] 
    Maps every element in [s] using the function [f]. 
*)
let rec map (f: 'a -> 'b) (s: 'a list): 'b list =
  match s with 
  | []   -> []
  | h::t -> (f h) :: (map f t)


(* ------------------------------ Implementation -----------------------------*)

(** Question 1.
    [num_of_c c s] 
    Returns the number of occurrences of [c] in list of characters [s].
*)
let rec num_of_c (c : alpha) (s : alpha list): int =
  match s with 
  |[] -> 0
  |h::t -> if h = c then (1 + (num_of_c c t)) else num_of_c c t;;
    
(** Question 2.
    [has_c c s] 
    Return [true] if [c] is contained in [s], otherwise [false].
*)
let rec has_c (c : alpha) (s : alpha list): bool =
  match s with 
  | [] -> false
  | h::t -> if h =c then true else has_c c t;;

(** Question 3.
    [flt_c c s] 
    Filters out all occurrences of [c] in [s].
*)
let rec flt_c (c : alpha) (s : alpha list): alpha list =
  match s with 
  | [] ->  []
  | h::t -> if h = c then flt_c c t else h::(flt_c c t);;

(** Question 4.
    [invert s] 
    Returns an inverted list, switching 'a' -> 'b' and 'b' -> 'a'.
    Raises [Not_in_alphabet].
*)
let rec invert (s : alpha list): alpha list =
  match s with
  | [] -> []
  | h::t -> if h = 'a' then 'b'::(invert t) else if h = 'b' then 'a'::(invert t) else (raise (Not_in_alphabet h));;

(** Question 5.
    [len_acc s n]
    Returns the length of the string [s] added to [n].
*)
let rec len_acc  (s : alpha list) (n : int): int =
  match s with 
  | [] -> n
  | _::t -> len_acc t (n+1) 

(** Question 5 (wrapper).
    [len s]
    Returns the length of [s].
*)
let len (s : alpha list): int = len_acc s 0

(** Question 6.
    [inv_map s] 
    Returns an inverted list, switching 'a' -> 'b' and 'b' -> 'a'.
    Raises [Not_in_alphabet].
*)

let inv_map (s : alpha list): alpha list = let auxiliary_func x = 
  if x = 'a' then 'b' else if x = 'b' then 'a' else raise (Not_in_alphabet x) in
 map auxiliary_func s;;
 

(** Question 7. 
    [concat s t] 
    Returns the concatenation of [s] and [t].
*)
let concat (s : alpha list) (t : alpha list): alpha list =
  let rec rev acc q = match q with 
| [] -> acc
| h::t -> rev (h::acc) t
in
  let rec aux_func acc l r = match l, r with
  | [], [] -> acc
  | h::p, _ ->  aux_func (h::acc) p r
  | [],h::p -> aux_func (h::acc) [] p
in rev [] (aux_func [] s t);;