open OUnit2
open Assignmentone.Worksheet

(* is_even tests *)
let is_even_even_case _ =
  assert_equal true (is_even 2)

let is_even_odd_case _ =
  assert_equal false (is_even 1) 

(* remove_int tests *)
let remove_int_empty _ =
  assert_equal [] (remove_int [] 1)

let remove_int_singleton_match _ =
  assert_equal [] (remove_int [1] 1)

let remove_int_singleton_unmatch _ =
  assert_equal [1] (remove_int [1] 2)

let remove_int_list_match _ = 
  assert_equal [2;3;4] (remove_int [1;2;3;1;4;1] 1)

let remove_int_list_unmatch _ =
  assert_equal [1;2;3;1;4;1] (remove_int [1;2;3;1;4;1] 6)

(* sum tests *)
let sum_empty _ = 
  assert_equal 0 (sum [])

let sum_singleton _ =
  assert_equal 5 (sum [5])

let sum_list _ =
  assert_equal 10 (sum [5;2;3])

(* rev tests *)
let rev_empty _ =
  assert_equal [] (rev [])

let rev_singleton _ =
  assert_equal [5] (rev [5])

let rev_list _ =
  assert_equal [4;1;2;1] (rev [1;2;1;4])

(* even_sized_palindrome test *)
let even_sized_palindrome_empty _ =
  assert_equal [] (even_sized_palindrome [])

let even_sized_palindrome_singleton _ =
  assert_equal [2;2] (even_sized_palindrome [2])

let even_sized_palindrome_list _ =
  assert_equal ['a';'b';'b';'a'] (even_sized_palindrome ['a';'b'])

let even_sized_palindrome_size _ =
  let l = [1;2;3;4] in
    assert_equal (2 * List.length(l)) (List.length (even_sized_palindrome l))

(* is_palindrome test *)
let is_palindrome_empty _ = 
  assert_equal true (is_palindrome [])

let is_palindrome_singleton _ =
  assert_equal true (is_palindrome ['a'])

let is_palindrome_list_even _ =
  assert_equal true (is_palindrome ['a'; 'b'; 'b'; 'a'])

let is_palindrome_list_odd _ =
  assert_equal true (is_palindrome ['a'; 'b'; 'a'])

let suite = "Worksheet suite" >::: [
  "Test is_even: even case" >:: is_even_even_case;
  "Test is_even: odd case" >:: is_even_odd_case;
  "Test remove_int: empty" >:: remove_int_empty;
  "Test remove_int: singleton match" >:: remove_int_singleton_match;
  "Test remove_int: singleton unmatch" >:: remove_int_singleton_unmatch;
  "Test remove_int: list match" >:: remove_int_list_match;
  "Test remove_int: list unmatch" >:: remove_int_list_unmatch;
  "Test sum empty" >:: sum_empty;
  "Test sum_singleton" >:: sum_singleton;
  "Test sum_list" >:: sum_list;
  "Test rev empty" >:: rev_empty;
  "Test rev singleton" >:: rev_singleton;
  "Test rev list" >:: rev_list;
  "Test even_sized_palindrome empty" >:: even_sized_palindrome_empty;
  "Test even_sized_palindrome singleton" >:: even_sized_palindrome_singleton;
  "Test even_sized_palindrome list" >:: even_sized_palindrome_list;
  "Test even_sized_palindrome size" >:: even_sized_palindrome_size;
  "Test is_palindrome empty" >:: is_palindrome_empty;
  "Test is_palindrome singleton" >:: is_palindrome_singleton;
  "Test is_palindrome list even" >:: is_palindrome_list_even;
  "Test is_palindrome list odd" >:: is_palindrome_list_odd
]

let () = run_test_tt_main suite
