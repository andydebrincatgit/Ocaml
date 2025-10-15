open OUnit2
open Assignmentthree.Lang
open Assignmentthree.Dfsa

(* Helpers. *)
let tr_total = [((0, A), 1); ((0, B), 0); ((1, A), 0); ((1, B), 1)]

let d_non_accepting = create_dfsa [1] []

(** DFSA M_1. *)
let d_total = create_dfsa [1] tr_total


(** [transition status tr_fun] tests. *)
let transition_match_alpha _ =
  assert_equal (Some 1) (transition (0, A) [((0, A), 1)])

let transition_nomatch_alpha _ =
  assert_equal None (transition (0, B) [((0, A), 1)])

(** [delta_star s q d] tests. *)
let delta_star_non_accepting_d_state_exists _ =
  assert_equal (Some 0) (delta_star [] 0 d_non_accepting)

let delta_star_empty_alpha _ =  
  assert_equal (Some 0) (delta_star [] 0 d_total)

let delta_star_match_alpha _ =
  assert_equal (Some 1) (delta_star [A] 0 d_total)

(** [is_accepted s d] tests. *)
let is_accepted_no_match _ =
  assert_equal false (is_accepted [B] d_total)

let is_accepted_match _ =
  assert_equal true (is_accepted [A; B; A; B; A] d_total)

(* [has_cycle dfsa] tests. *)
let has_cycle_empty _ =
  assert_equal false (has_cycle (create_dfsa [1] []))

let has_cycle_total_cycle _ =
  assert_equal true (has_cycle (create_dfsa [1] [((0, A), 1); ((0, B), 0); ((1, A), 0); ((1, B), 1)]))

let has_cycle_partial_cycle_no_start_state _ =
  assert_equal true (has_cycle (
    create_dfsa [] [((0, A), 1); ((1, B), 2); ((1, B), 3); ((2, A), 4); ((4, B), 5); ((5, B), 2)]
  ))

(** Main test suite. *)
let suite = "Test suite for Assignment Three" >::: [
  "Test transition match" >:: transition_match_alpha;
  "Test transition no match alpha"  >:: transition_nomatch_alpha;
  "Test delta_star non accepting d state exists" >:: delta_star_non_accepting_d_state_exists;
  "Test delta_star empty alpha" >:: delta_star_empty_alpha;
  "Test delta_star match alpha" >:: delta_star_match_alpha;
  "Test is_accepted no match" >:: is_accepted_no_match;
  "Test is_accepted match" >:: is_accepted_match;
  "Test has_cycle empty" >:: has_cycle_empty;
  "Test has_cycle total cycle" >:: has_cycle_total_cycle;
  "Test has_cycle partial cycle no start state" >:: has_cycle_partial_cycle_no_start_state
]

(** Test launcher. *)
let () = run_test_tt_main suite