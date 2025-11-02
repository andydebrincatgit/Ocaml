

type alpha = A | B |C;;

type astring = alpha list;;

type alang = astring list;;

exception Not_in_alphabet;;

type state = int;;


type 'a set = 'a list;;

type dfsa = { 
  start: state;

  
  final: state set;
 
  
  delta: ((state * alpha) * state) set 
  
};;
let rec map (f: 'a -> 'b)  (s: 'a list): 'b list =
  match s with 
  | []   -> []
  | h::t -> (f h) :: (map f t);;

let rec reduce (f: 'a -> 'b -> 'a) (acc : 'a) (s: 'b list): 'a = 
  match s with
  | []   -> acc
  | b::t -> reduce f (f acc b) t;;


let a_eq (a: alpha) (b: alpha): bool = match a,b with 
|A,A -> true
|B,B -> true
|C,C -> true
|_,_ -> false;;



let rec even_a (a: alpha) (s: astring): bool = match s with 
|[]-> true
|h::t-> if a_eq h a then odd_a a t else even_a a t
  
and odd_a (a: alpha) (s: astring): bool = match s with
  |[]-> false
  |h::t-> if a_eq h a then even_a a t else odd_a a t;;
  


let has_A (s: astring): bool = let rec helper t = match t with 
|[] -> false
|h::r -> if a_eq h A then true else helper r
in helper s;;

let has_B (s: astring): bool = let rec helper t = match t with 
|[] -> false
|h::r -> if a_eq h B then true else helper r
in helper s;;

let has_odd_B (s: astring): bool = odd_a B s;;
 


let rec is_ABstar (s: astring): bool = match s with
| [] -> true
| [_] -> false
| h::t::r-> if (h=A && t=B ) then is_ABstar r else false;;
  


let is_ABplus (s: astring): bool = match s with
| [] -> false
|_ -> is_ABstar s;;
  


let len_red (s: astring): int = reduce (fun acc _ -> (acc + 1)) 0 s;;


let has_A_red (s: astring): bool = reduce (fun acc letter -> if ((a_eq A letter)||(acc=true)) then true else false) false s;;


let is_subset (l: alang) (p: astring -> bool): bool =let helper_func acc bol =
   if (bol=true && acc = true) then true else false in reduce helper_func true (map p l);;


let create_dfsa (fin : state set) (tra : ((state * alpha) * state) set): dfsa =
{
  delta = tra;
  start = 0;  
  final = fin
};;

type 'a option = Some of 'a | None
let rec transition (config: (state * alpha)) (delta : ((state * alpha) * state) set) : state option =match config with (st,alph) ->
  match delta with 
  | [] -> None
  | ((a,b), c)::t -> if ((st = a) && (a_eq alph b)) then Some c else transition config t;;
  


let rec delta_star (s: astring) (q: state) (dfsa: dfsa) : state option = 
  match dfsa with {delta;_} ->
    match s with 
    | [] -> Some q
    | h::t -> let x = transition (q,h) delta in
     (match x with 
     |None -> None 
      | Some y -> ( delta_star t y dfsa ));;




let rec is_elem e l = match l with 
|[] -> false
|h::t -> if e = h then true else is_elem e t;;
let is_accepted (s: astring) (dfsa: dfsa): bool = match dfsa with {start;final;_} ->
  match delta_star s start dfsa with
  |None -> false
  |Some c -> if is_elem c final then true else false;;
  

  
  let extract_all_possible_states dfsa =  match dfsa with {start;final;delta} ->
    let rec rec_helper d acc = match d with
    |[]-> acc
    |((a,_),c)::t-> rec_helper t (a::(c::acc))
  in ([start]@final@(rec_helper delta []));;

  let rec get_next_set_of_reachable_states q  delta acc= match delta with 
  |[] -> acc
  |((a,_),c)::t-> if a=q then get_next_set_of_reachable_states q t (c::acc) else get_next_set_of_reachable_states q t acc;;

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


let rec make_set l acc = match l with 
|[] -> acc
|h::t -> if is_elem h acc then make_set t acc else make_set t (h::acc);;
let dfsa_empty = create_dfsa [] [];;
let dfsa_single_no_loop = create_dfsa [0] [ ];;

let dfsa_self_loop = create_dfsa [0] [ ((0, A), 0) ];;

let dfsa_linear = create_dfsa [1] [ ((0, A), 1) ];;

let dfsa_two_cycle = create_dfsa [0] [ ((0, A), 1); ((1, B), 0) ];;

let dfsa_branch_cycle = create_dfsa [2]
  [ ((0, A), 1);
    ((1, B), 2);
    ((2, C), 1) ];;

    let dfsa_disconnected_cycle = create_dfsa [3]
  [ ((0, A), 1);
    ((2, B), 3);
    ((3, C), 2) ];;

   
let dfsa_multi_edge_no_cycle =
  create_dfsa [1]
    [ ((0, A), 1);
      ((0, B), 1) ];;


let dfsa_multi_edge_cycle =
  create_dfsa [0]
    [ ((0, A), 1);
      ((1, B), 0);
      ((0, A), 1) ];;


let dfsa_duplicate_self_loop =
  create_dfsa [0]
    [ ((0, A), 0);
      ((0, B), 0);
      ((0, A), 0) ];;


    has_cycle dfsa_empty;;
    has_cycle dfsa_single_no_loop;;
    has_cycle dfsa_self_loop;;
    has_cycle dfsa_linear;;
    has_cycle dfsa_two_cycle;;
    has_cycle dfsa_branch_cycle;;
    has_cycle dfsa_disconnected_cycle;;
     has_cycle dfsa_multi_edge_no_cycle;;
    has_cycle dfsa_multi_edge_cycle;;
    has_cycle dfsa_duplicate_self_loop;;
















  