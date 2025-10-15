open OUnit2
open Assignmentthree.Lang

(** [a_eq a b] tests. *)
let a_eq_match _ =
  assert_equal true (a_eq A A)

let a_eq_no_match _ =
  assert_equal false (a_eq A B)

(** [even_a a s] tests. *)
let even_a_singleton_A _ =
  assert_equal false (even_a A [A])

let even_a_list_even_A _ =
  assert_equal true (even_a A [A; A])

let even_a_list_odd_not_A _ = 
  assert_equal false (even_a B [A; B])

let even_a_list_even_not_A _ =
  assert_equal true (even_a B [A; A])

let odd_a_singleton_A _ =
  assert_equal true (odd_a A [A])

let odd_a_list_even_A _ =
  assert_equal false (odd_a A [A; A])

let odd_a_list_odd_not_A _ =
  assert_equal true (odd_a B [A; B])

let odd_a_list_even_not_A _ =
  assert_equal false (odd_a B [A; A])

(** [has_A s] tests. *)
let has_A_list_found _ =
  assert_equal true (has_A [B; A])

(** [has_B s] tests. *)
let has_B_list_found _ =
  assert_equal true (has_B [A; B])

(** [has_odd_B s] tests. *)
let has_odd_B_list_odd _ =
  assert_equal true (has_odd_B [A; B])
      
let has_odd_B_list_even _ =
  assert_equal false (has_odd_B [A; A])
  
(** [is_ABstar s] tests. *)
let is_ABstar_match _ =
  assert_equal true (is_ABstar [A; B])

let is_ABstar_no_match _ =
  assert_equal false (is_ABstar [A; B; A])

(** [is_ABplus s] tests. *)
let is_ABplus_match _ =
  assert_equal true (is_ABplus [A; B])

  let is_ABplus_no_match _ =
  assert_equal false (is_ABplus [A; B; A])

(** [len_red s] tests. *)
let len_red_empty _ =
  assert_equal 0 (len_red [])

let len_red_singleton _ =
  assert_equal 1 (len_red [A])

let len_red_list _ =
  assert_equal 3 (len_red [A; B; A])

(** [has_A_red s] tests. *)
let has_A_red_list_found _ =
  assert_equal true (has_A_red [B; A])

(** [is_subset l p] tests. *)
let is_subset_match _ =
  assert_equal true (is_subset [[]; [A; B]; [A; B; A; B]] is_ABstar)

let is_subset_no_match _ =
  assert_equal false (is_subset [[]; [B; B]; [A; B; A; B]] is_ABstar)

(** Main test suite. *)
let suite = "Test suite for Assignment Two" >::: [
  "Test a_eq match" >:: a_eq_match;
  "Test a_eq no match" >:: a_eq_no_match;
  "Test even_a singleton with A" >:: even_a_singleton_A;
  "Test even_a list even with A" >:: even_a_list_even_A;
  "Test even_a list odd with not A" >:: even_a_list_odd_not_A;
  "Test even_a list even with not A" >:: even_a_list_even_not_A;
  "Test odd_a singleton with A" >:: odd_a_singleton_A;
  "Test odd_a list event with A" >:: odd_a_list_even_A;
  "Test odd_a list odd with not A" >:: odd_a_list_odd_not_A;
  "Test odd_a list even with not A" >:: odd_a_list_even_not_A;
  "Test has_a list found" >:: has_A_list_found;
  "Test has_b list found" >:: has_B_list_found;
  "Test has_odd_B list odd" >:: has_odd_B_list_odd;
  "Test has_odd_B list even" >:: has_odd_B_list_even;
  "Test is_ABstar match" >:: is_ABstar_match;
  "Test is_ABstar no match" >:: is_ABstar_no_match;
  "Test is_ABplus match" >:: is_ABplus_match;
  "Test is_ABplus no match" >:: is_ABplus_no_match;
  "Test len_red empty" >:: len_red_empty;
  "Test len_red singleton" >:: len_red_singleton;
  "Test len_red list" >:: len_red_list;
  "Test has_A_red list found" >:: has_A_red_list_found;
  "Test is_subset match" >:: is_subset_match;
  "Test is_subset no match" >:: is_subset_no_match;
]

(** Test launcher. *)
let () = run_test_tt_main suite