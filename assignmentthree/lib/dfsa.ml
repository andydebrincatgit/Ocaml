open Lang

(* ----------------------------- Type definitions ----------------------------*)

(** DFSA state. *)
type state = int

(** Set represented as a list. *)
type 'a set = 'a list

(** DFSA represented as a record. *)
type dfsa = { 
  start: state;
  (** Start state. *)
  
  final: state set;
  (** Set of final states. *)
  
  delta: ((state * alpha) * state) set 
  (** Transition function. *)
}


(* ------------------------------ Implementation -----------------------------*)

(** [create_dfsa fin tra]
    Creates a new {!dfsa} with transition function [tra] and final state set 
    [fin].
*)
let create_dfsa (fin : state set) (tra : ((state * alpha) * state) set): dfsa =
{
  delta = tra;
  start = 0;  (* Always assumed to be 0 *)
  final = fin
}

(** Question 7.
    [transition status tr_fun]
    Returns the next {!dfsa} state [Some q''] if the current configuration 
    [config] [(q, a)] is defined in the transition function [delta], otherwise 
    [None].
*) 
let rec transition (config: (state * alpha)) (delta : ((state * alpha) * state) set) : state option =
  failwith "TODO Implementation"

(** Question 8.
    [delta_star s q dfsa]
    Returns the {!dfsa} state [Some q'] after following the transition function 
    of [dfsa], starting from state [q] and consuming the input string [s].

    If no such [q'] exists, [None] is returned.
*)
let rec delta_star (s: astring) (q: state) (dfsa: dfsa) : state option =
  failwith "TODO Implementation"

(** Question 9.
    [is_accepted s dfsa]
    Returns [true] if the input string [s] is accepted by the DFSA [dfsa], 
    otherwise [false].
*)
let is_accepted (s: astring) (dfsa: dfsa): bool = 
  failwith "TODO Implementation"

(** Question 10.
    [has_cycle dfsa]
    Returns [true] if [dfsa] contains at least one cycle, otherwise [false].
*)
let has_cycle (dfsa: dfsa): bool =
  failwith "TODO Implementation"