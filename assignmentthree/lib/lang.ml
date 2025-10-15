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
let a_eq (a: alpha) (b: alpha): bool =
  failwith "TODO Implementation"

(** Question 2.
    [even_a a s]
    Returns [true] if the number of occurrences of [a] is odd/even in [l], 
    otherwise [false].
*)
let rec even_a (a: alpha) (s: astring): bool =
  failwith "TODO Implementation"
and odd_a (a: alpha) (s: astring): bool =
  failwith "TODO Implementation"

(** Question 3.
  [has_A s]
  Returns [true] if the letter [A] is contained in [s], otherwise [false].
*)
let has_A (s: astring): bool =
  failwith "TODO Implementation"

(** Question 3.
  [has_B s]
  Returns [true] if the letter [B] is contained in [s], otherwise [false].
*)
let has_B (s: astring): bool =
  failwith "TODO Implementation"

(** Question 3.
    [has_odd_B s]
    Returns [true] if [s] has an odd number of the letter [B], otherwise
    [false].
*)
let has_odd_B (s: astring): bool =
  failwith "TODO Implementation"

(** Question 4. 
    [is_ABstar s]
    Returns [true] if [s] is in the language [(AB)*], [false] otherwise.
*)
let rec is_ABstar (s: astring): bool =
  failwith "TODO Implementation"

(** Question 4.
  [is_ABplus s]
  Returns [true] if [s] is in the language [(AB)+], [false] otherwise.
*)
let is_ABplus (s: astring): bool =
  failwith "TODO Implementation"

(** Question 5. 
    [len_red s]
    Returns the length of [s].
*)
let len_red (s: astring): int =
  failwith "TODO Implementation"

(** Question 5.
    [has_A_red s]
    Returns [true] if the letter [A] is contained in [s], otherwise [false].
*)
let has_A_red (s: astring): bool =
  failwith "TODO Implementation"

(** Question 6.
    [is_subset l p]
    Returns [true] if the language [l] is contained in the language defined by 
    the predicate [p], otherwise [false].

    This means that every string in [l] satisfies [p], including the empty 
    language [[]].
*)
let is_subset (l: alang) (p: astring -> bool): bool =
  failwith "TODO Implementation"