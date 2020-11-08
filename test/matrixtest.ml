open OUnit2
open Tuple
open Matrix

let tests = "Test Suite for Matrices" >::: [
    "Constructing 4x4 matrix" >::
    (fun _ ->
	let m = [|
		[| 1.; 2.; 3.; 4. |];
		[| 5.5; 6.5; 7.5; 8.5 |];
		[| 9.; 10.; 11.; 12. |];
		[| 13.5; 14.5; 15.5; 16.5 |]
	|] in
	assert_bool "position 0 0 incorrect" (equalFloat (matrix_get m 0 0) 1.);
	assert_bool "position 0 3 incorrect" (equalFloat (matrix_get m 0 3) 4.);
	assert_bool "position 1 0 incorrect" (equalFloat (matrix_get m 1 0) 5.5);
	assert_bool "position 1 2 incorrect" (equalFloat (matrix_get m 1 2) 7.5);
	assert_bool "position 2 2 incorrect" (equalFloat (matrix_get m 2 2) 11.);
	assert_bool "position 3 0 incorrect" (equalFloat (matrix_get m 3 0) 13.5);
	assert_bool "position 3 2 incorrect" (equalFloat (matrix_get m 3 2) 15.5);
    );
    "Constructing 2x2 matrix" >::
    (fun _ ->
	let m = [|
		[| (-3.); 5. |];
		[| 1.; (-2.) |]
	|] in
	assert_bool "position 0 0 incorrect" (equalFloat (matrix_get m 0 0) (-3.));
	assert_bool "position 0 1 incorrect" (equalFloat (matrix_get m 0 1) 5.);
	assert_bool "position 1 0 incorrect" (equalFloat (matrix_get m 1 0) 1.);
	assert_bool "position 1 1 incorrect" (equalFloat (matrix_get m 1 1) (-2.));
    );
    "Constructing 3x3 matrix" >::
    (fun _ ->
	let m = [|
		[| (-3.); 5.; 0.; |];
		[| 1.; (-2.); (-7.); |];
		[| 0.; 1.; 1.; |]
	|] in
	assert_bool "position 0 0 incorrect" (equalFloat (matrix_get m 0 0) (-3.));
	assert_bool "position 1 1 incorrect" (equalFloat (matrix_get m 1 1) (-2.));
	assert_bool "position 2 2 incorrect" (equalFloat (matrix_get m 2 2) 1.);
    );
]

let _ = run_test_tt_main tests
