open OUnit2
open Tuple
open Operation

let tests = "Test Suite for Tuple Operations" >::: [
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
