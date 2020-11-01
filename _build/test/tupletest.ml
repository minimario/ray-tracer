open OUnit2
open Tuple

let p = point 3 4 5

let tests = "Test Suite for Tuple" >::: [
    "tupletest" >:: 
    (fun _ -> assert_equal 0 0);
]