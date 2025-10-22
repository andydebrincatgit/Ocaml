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
type 'a option = Some of 'a | None
let rec transition (config: (state * alpha)) (delta : ((state * alpha) * state) set) : state option =match config with (st,alph) ->
  match delta with 
  | [] -> None
  | ((a,b), c)::t -> if ((st = a) && (a_eq alph b)) then Some c else transition config t;;
  

(** Question 8.
    [delta_star s q dfsa]
    Returns the {!dfsa} state [Some q'] after following the transition function 
    of [dfsa], starting from state [q] and consuming the input string [s].

    If no such [q'] exists, [None] is returned.
*)
let rec delta_star (s: astring) (q: state) (dfsa: dfsa) : state option = 
  match dfsa with {delta;_} ->
    match s with 
    | [] -> Some q
    | h::t -> let x = transition (q,h) delta in
     (match x with 
     |None -> None 
      | Some y -> ( delta_star t y dfsa ));;


(** Question 9.
    [is_accepted s dfsa]
    Returns [true] if the input string [s] is accepted by the DFSA [dfsa], 
    otherwise [false].
*)

let rec is_elem e l = match l with 
|[] -> false
|h::t -> if e = h then true else is_elem e t;;
let is_accepted (s: astring) (dfsa: dfsa): bool = match dfsa with {start;final;_} ->
  match delta_star s start dfsa with
  |None -> false
  |Some c -> if is_elem c final then true else false;;
  
(** Question 10.
    [has_cycle dfsa]
    Returns [true] if [dfsa] contains at least one cycle, otherwise [false].
*)

  let extract_all_possible_states dfsa =  match dfsa with {start;final;delta} ->
    let rec rec_helper d acc = match d with
    |[]-> acc
    |((a,_),c)::t-> rec_helper t (a::(c::acc))
  in ([start]@final@(rec_helper delta []));;

  let rec get_next_set_of_reachable_states q  delta acc= match delta with 
  |[] -> acc
  |((a,_),c)::t-> if a=q then get_next_set_of_reachable_states q t (c::acc) else get_next_set_of_reachable_states q t acc

  let rec or_bool_list l = match l with 
  |[] -> false
  |h::t -> if h = true then true else or_bool_list t;;

  let rec is_intersection_not_zero a b = match a with 
  | []-> false
  | h::t ->  if is_elem h b then true else is_intersection_not_zero t b;;
  
  let rec check_for_cycles_from_state already_visited q p delta = let next_set = get_next_set_of_reachable_states p delta [] in
  if is_intersection_not_zero already_visited next_set then true
  else let already_visited = already_visited@next_set in
  match next_set with
  | [] -> false 
  |_ -> or_bool_list (map (fun v ->check_for_cycles_from_state already_visited q v delta) next_set);;

let has_cycle (dfsa: dfsa): bool = match dfsa with {delta;_} ->
   let states = extract_all_possible_states dfsa in
   let rec check_from_each_state l d = match l with 
   |[]-> false
   |h::t -> if check_for_cycles_from_state [h] h h d then true else check_from_each_state t d
in (check_from_each_state states delta);;

  