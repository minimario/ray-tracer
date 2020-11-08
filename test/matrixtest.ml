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
    "Comparing equal matrices" >::
    (fun _ ->
	let a = [|
		[| 1.; 2.; 3.; 4. |];
		[| 5.; 6.; 7.; 8. |];
		[| 9.; 8.; 7.; 6. |];
		[| 5.; 4.; 3.; 2. |]
	|] in
	let b = [|
		[| 1.; 2.; 3.; 4. |];
		[| 5.; 6.; 7.; 8. |];
		[| 9.; 8.; 7.; 6. |];
		[| 5.; 4.; 3.; 2. |]
	|] in
	assert_bool "equality of equal matrices incorrect" (equalMatrix a b);
    );
    "Comparing unequal matrices" >::
    (fun _ ->
	let a = [|
		[| 1.; 2.; 3.; 4. |];
		[| 5.; 6.; 7.; 8. |];
		[| 9.; 8.; 7.; 6. |];
		[| 5.; 4.; 3.; 2. |]
	|] in
	let b = [|
		[| 2.; 3.; 4.; 5. |];
		[| 6.; 7.; 8.; 9. |];
		[| 8.; 7.; 6.; 5. |];
		[| 4.; 3.; 2.; 1. |]
	|] in
	assert_bool "equality of different matrices incorrect" (not (equalMatrix a b));
    );
    "Multiplying square matrices" >::
    (fun _ ->
	let a = [|
		[| 1.; 2.; 3.; 4. |];
		[| 5.; 6.; 7.; 8. |];
		[| 9.; 8.; 7.; 6. |];
		[| 5.; 4.; 3.; 2. |]
	|] in
	let b = [|
		[| (-2.); 1.; 2.; 3. |];
		[| 3.; 2.; 1.; (-1.) |];
		[| 4.; 3.; 6.; 5. |];
		[| 1.; 2.; 7.; 8. |]
	|] in
	let c = [|
		[| 20.; 22.; 50.; 48. |];
		[| 44.; 54.; 114.; 108. |];
		[| 40.; 58.; 110.; 102. |];
		[| 16.; 26.; 46.; 42. |]
	|] in
	assert_bool "matrix multiplication incorrect" (equalMatrix (multiplyMatrix a b) c);
    );
]

let _ = run_test_tt_main tests
