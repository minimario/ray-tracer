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
    );
    "addtest" >::
    (fun _ -> 
        let a = (tuple 3. (-2.) 5. 1.) in
        let b = (tuple (-2.) 3. 1. 0.) in
        let c = addTuple a b in
        assert_equal c.x 1.;
        assert_equal c.y 1.;
        assert_equal c.z 6.;
        assert_equal c.w 1.
    )
]

let _ = run_test_tt_main tests