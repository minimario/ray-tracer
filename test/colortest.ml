open OUnit2
open Color

let tests = "Test Suite for Colors" >::: [
    "Color constructor" >::
    (fun _ ->
        let c = color (-0.5) 0.4 1.7 in
	assert_equal c.red (-0.5);
	assert_equal c.green 0.4;
	assert_equal c.blue 1.7;

    );
    "Adding colors" >::
    (fun _ ->
        let c1 = color 0.9 0.6 0.75 in
	let c2 = color 0.7 0.1 0.25 in
	let expected_sum = color 1.6 0.7 1.0 in
	assert_bool "color addition incorrect"
	    (equalColor (addColor c1 c2) expected_sum);
    );
    "Subtracting colors" >::
    (fun _ ->
        let c1 = color 0.9 0.6 0.75 in
	let c2 = color 0.7 0.1 0.25 in
	let expected_diff = color 0.2 0.5 0.5 in
	assert_bool "color subtraction incorrect"
	    (equalColor (subtractColor c1 c2) expected_diff);
    );
    "Multiplying colors" >::
    (fun _ ->
        let c1 = color 1. 0.2 0.4 in
	let c2 = color 0.9 1. 0.1 in
	let expected_product = color 0.9 0.2 0.04 in
	assert_bool "color multiplication incorrect"
	    (equalColor (multiplyColor c1 c2) expected_product);
    );
    "Scalar color multiplication" >::
    (fun _ ->
        let c1 = color 0.2 0.3 0.4 in
	let expected_product = color 0.4 0.6 0.8 in
	assert_bool "color scalar multiplication incorrect"
	    (equalColor (multiplyColorScalar c1 2.) expected_product);
    );
]

let _ = run_test_tt_main tests
