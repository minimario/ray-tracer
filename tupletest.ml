open OUnit2
open Tuple

let p = point 3. 4. 5.

let tests = "Test Suite for Tuple" >::: [
    "tupletest" >:: 
    (fun _ -> 
        let a = (tuple 4.3 (-4.2) 3.1 1.0) in
        assert_equal a.x 4.3;
        assert_equal a.y (-4.2);
        assert_equal a.z 3.1;
        assert_equal a.w 1.0;
        assert_bool "a should be a point" (isPoint a);
        assert_bool "a should not be a vector" (not (isVector a))
    )
]

let _ = run_test_tt_main tests