open OUnit2
open Assignmenttwo.Langbasic

(** [num_of_c c s] tests. *)
let num_of_c_singleton_match _ =
  assert_equal 1 (num_of_c 'a' ['a'])

let num_of_c_list_match _ =
  assert_equal 3 (num_of_c 'a' ['a'; 'b'; 'b'; 'c'; 'a'; 'g'; 'a'])

let num_of_c_list_no_match _ =
  assert_equal 0 (num_of_c 'a' ['b'; 'b'; 'c'; 'g'])

(** [has_c c s] tests. *)
let has_c_singleton_found _ =
  assert_equal true (has_c 'a' ['a'])

  let has_c_list_found _ =
  assert_equal true (has_c 'a' ['a'; 'b'; 'b'; 'c'; 'a'; 'g'; 'a'])

let has_c_list_no_found _ =
  assert_equal false (has_c 'a' ['b'; 'b'; 'c'; 'g'])

(** [flt_c c s] tests. *)
let flt_c_singleton_match _ =
  assert_equal [] (flt_c 'a' ['a'])

let flt_c_list_match _ =
  assert_equal ['b'; 'b'; 'c'; 'g'] (flt_c 'a' ['a'; 'b'; 'b'; 'c'; 'a'; 'g'; 'a'])

let flt_c_list_no_match _ =
  assert_equal ['b'; 'b'; 'c'; 'g'] (flt_c 'a' ['b'; 'b'; 'c'; 'g'])

(** [invert s] tests. *)
let invert_singleton_match_a _ =
  assert_equal ['b'] (invert ['a'])

let invert_singleton_match_b _ =
  assert_equal ['a'] (invert ['b'])

let invert_list_match _ =
  assert_equal ['a'; 'b'; 'b'; 'a'] (invert ['b'; 'a'; 'a'; 'b'])

(** [len_acc s n] tests. *)
let len_acc_empty _ =
  assert_equal 0 (len_acc [] 0) 

let len_acc_singleton _ =
  assert_equal 1 (len_acc ['a'] 0)

let len_acc_list _ =
  assert_equal 4 (len_acc ['a'; 'b'; 'b'; 'a'] 0)

(** [inv_map s] tests. *)
let inv_map_singleton_match_a _ =
  assert_equal ['b'] (inv_map ['a'])

let inv_map_singleton_match_b _ =
  assert_equal ['a'] (inv_map ['b'])

let inv_map_list_match _ =
  assert_equal ['a'; 'b'; 'b'; 'a'] (inv_map ['b'; 'a'; 'a'; 'b'])

(** [concat s t] tests. *)
let concat_empty_l _ =
  assert_equal ['a'] (concat ['a'] [])

let concat_singleton _ =
  assert_equal ['a'; 'b'] (concat ['a'] ['b'])

let concat_list_l _ =
  assert_equal ['a'; 'b'; 'b'; 'a'] (concat ['a'; 'b'; 'b'; 'a'] [])

let concat_list _ =
  assert_equal ['a'; 'd'; 'r'; 'i'; 'a'; 'n'] (concat ['a'; 'd'; 'r'] ['i'; 'a'; 'n'])

(** Main test suite. *)
let suite = "Test suite for Assignment Two" >::: [
  "Test num_of_c singleton match" >:: num_of_c_singleton_match;
  "Test num_of_c list match" >:: num_of_c_list_match;
  "Test num_of_c list no match" >:: num_of_c_list_no_match;
  "Test has_c singleton found" >:: has_c_singleton_found;
  "Test has_c list found" >:: has_c_list_found;
  "Test has_c list no found" >:: has_c_list_no_found;
  "Test flt_c singleton match" >:: flt_c_singleton_match;
  "Test flt_c list match" >:: flt_c_list_match;
  "Test flt_c list no match" >:: flt_c_list_no_match;
  "Test invert singleton match a" >:: invert_singleton_match_a;
  "Test invert singleton match b" >:: invert_singleton_match_b;
  "Test invert list match" >:: invert_list_match;
  "Test len_acc empty" >:: len_acc_empty;
  "Test len_acc singleton" >:: len_acc_singleton;
  "Test len_acc list" >:: len_acc_list;
  "Test inv_map singleton match a" >:: inv_map_singleton_match_a;
  "Test inv_map singleton match b" >:: inv_map_singleton_match_b;
  "Test inv_map list match" >:: inv_map_list_match;
  "Test concat empty left" >:: concat_empty_l;
  "Test concat singleton" >:: concat_singleton;
  "Test concat list left" >:: concat_list_l;
  "Test concat list" >:: concat_list;
]

(** Test launcher. *)
let () = run_test_tt_main suite