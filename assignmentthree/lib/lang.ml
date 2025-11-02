(* ----------------------------- Type definitions ----------------------------*)

(** Alphabet set [A], [B] in sigma. *)
type alpha = A | B

(** String as a sequence of alphabet in sigma. *)
type astring = alpha list

(** Language as a set (here, as a list) of strings build from sigma*. *)
type alang = astring list

(** 'Not in alphabet' exception. *)
exception Not_in_alphabet


(* ----------------------------- Supporting code ---------------------------- *)
(** [map f s] 
    Maps every element in [s] using the function [f].
*)
let rec map (f: 'a -> 'b)  (s: 'a list): 'b list =
  match s with 
  | []   -> []
  | h::t -> (f h) :: (map f t)

(** [reduce f acc s]
    Reduces the list [s] to the accumulator [acc] using [f].
*)
let rec reduce (f: 'a -> 'b -> 'a) (acc : 'a) (s: 'b list): 'a = 
  match s with
  | []   -> acc
  | b::t -> reduce f (f acc b) t


(* ------------------------------ Implementation -----------------------------*)

(** Question 1.
    [a_eq a b]
    Returns [true] if [a] = [b], otherwise [false].
*)
let a_eq (a: alpha) (b: alpha): bool = (a=b);;

(** Question 2.
    [even_a a s]
    Returns [true] if the number of occurrences of [a] is odd/even in [l], 
    otherwise [false].
*)
let rec even_a (a: alpha) (s: astring): bool = match s with 
|[]-> true
|h::t-> if a_eq h a then odd_a a t else even_a a t
  
and odd_a (a: alpha) (s: astring): bool = match s with
  |[]-> false
  |h::t-> if a_eq h a then even_a a t else odd_a a t;;
  

(** Question 3.
  [has_A s]
  Returns [true] if the letter [A] is contained in [s], otherwise [false].
*)

  (** or_list was deemed to be necessary to take a list of booleans representing if the entry in the list is the charachter c or not and
    check if at least one entry is true to output the desired has_A and has_B functionality. Its neccesity was derived from this logic:
      We were asked to implement has_A and has_B using only previously defined functions which means we must use a_eq
      reduce, map, odd_a or even_a. However, it is impossible to define the function has_A and has_B using even_A and odd_A, since even_A
      counts 0 occurences of a charachter as being even, so it will not relent any information on whether a string has a charachter or not.
      That left reduce and map, but in question 5 we were asked to implemenet has_A with the reduce function, so that was also eleminated,
      leaving the map function. The map function must always return a list which means that the only way to output a boolean was to make some 
      other helper function, hence the need for or_list.*)
let rec or_list (l: bool list) :bool  = match l with 
|[] -> false
|h::t -> if h then true else or_list t;;

let has_A (s: astring): bool = or_list (map (a_eq A) s);;

(** Question 3.
  [has_B s]
  Returns [true] if the letter [B] is contained in [s], otherwise [false].
*)
let has_B (s: astring): bool =  or_list (map (a_eq B) s);;
(** Question 3.
    [has_odd_B s]
    Returns [true] if [s] has an odd number of the letter [B], otherwise
    [false].
*)
let has_odd_B (s: astring): bool = odd_a B s;;
 

(** Question 4. 
    [is_ABstar s]
    Returns [true] if [s] is in the language [(AB)*], [false] otherwise.
*)
let rec is_ABstar (s: astring): bool = match s with
| [] -> true
| [_] -> false
| h::t::r-> if (h=A && t=B ) then is_ABstar r else false
  

(** Question 4.
  [is_ABplus s]
  Returns [true] if [s] is in the language [(AB)+], [false] otherwise.
*)
let is_ABplus (s: astring): bool = match s with
| [] -> false
|_ -> is_ABstar s;;
  

(** Question 5. 
    [len_red s]
    Returns the length of [s].
*)
let len_red (s: astring): int = reduce (fun (acc:int) _ :int-> (acc + 1)) 0 s;;

(** Question 5.
    [has_A_red s]
    Returns [true] if the letter [A] is contained in [s], otherwise [false].
*)
let has_A_red (s: astring): bool = 
  reduce (fun (acc:bool) (letter:alpha):bool -> if ((a_eq A letter)||(acc=true)) then true else false) false s;;

(** Question 6.
    [is_subset l p]
    Returns [true] if the language [l] is contained in the language defined by 
    the predicate [p], otherwise [false].

    This means that every string in [l] satisfies [p], including the empty 
    language [[]].
*)
let is_subset (l: alang) (p: astring -> bool): bool = let helper_func (acc:bool) (bol:bool) : bool =
   if (bol=true && acc = true) then true else false in
    reduce (helper_func) true (map p l);;