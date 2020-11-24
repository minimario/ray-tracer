open OUnit2
open Tuple
open Matrix

let equalTuple = Tuple.equals
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

    "A matrix multiplied by a tuple" >::
    (fun _ ->
	let a = [|
		[| 1.; 2.; 3.; 4. |];
		[| 2.; 4.; 4.; 2. |];
		[| 8.; 6.; 4.; 1. |];
		[| 0.; 0.; 0.; 1. |]
	|] in
	let b = tuple 1. 2. 3. 1. in
	let c = tuple 18. 24. 33. 1. in
	assert_bool "matrix multiplication incorrect" 
		(equalTuple (multiplyMatrixTuple a b) c)
    );

    "Identity matrix sanity check" >::
    (fun _ ->
	let a = [| 
		[|1.; 0.; 0.; 0.|];
		[|0.; 1.; 0.; 0.|];
   		[|0.; 0.; 1.; 0.|];
   		[|0.; 0.; 0.; 1.|] 
	|] in
	assert_bool "identity matrix failed" 
		(equalMatrix identity_matrix a);
    );

    "Multiplying a matrix by the identity matrix" >::
    (fun _ ->
	let a = [| 
		[|0.; 1.; 2.; 4.|];
		[|1.; 2.; 4.; 8.|];
   		[|2.; 4.; 8.; 16.|];
   		[|4.; 8.; 16.; 32.|] 
	|] in
	let b = identity_matrix in
	assert_bool "multiplication by identity failed" 
		(equalMatrix (multiplyMatrix a b) a);
    );

    "Multiplying the identity matrix by a tuple" >::
    (fun _ ->
	let a = tuple 1. 2. 3. 4. in
	let id = identity_matrix in 
	assert_bool "identity tuple multiplication incorrect" 
		(equalTuple (multiplyMatrixTuple id a) a)
    );
	
	"Transposing a matrix" >::
	(fun _ ->
	let a = [|
		[|0.; 9.; 3.; 0.|];
		[|9.; 8.; 0.; 8.|];
   		[|1.; 8.; 5.; 3.|];
   		[|0.; 0.; 5.; 8.|] 
	|] in 
	let b = [|
		[|0.; 9.; 1.; 0.|];
		[|9.; 8.; 8.; 0.|];
   		[|3.; 0.; 5.; 5.|];
   		[|0.; 8.; 3.; 8.|]
	|] in
	assert_bool "matrix transpose incorrect" (equalMatrix (transpose a) b);
	assert_bool "identity transposition incorrect"
		(equalMatrix identity_matrix (transpose identity_matrix))
	);	

	"Determinant of 2d matrix" >::
	(fun _ ->
	let a = [|
		[| 1.; 5. |];
		[| (-3.); 2. |] 
	|] in
	assert_bool "2d determinant incorrect" (equalFloat (determinant_2d a) 17.)
	);

	"Submatrix of 3x3" >::
	(fun _ ->
	let a = [| 
		[|    1.; 5.;    0. |]; 
		[| (-3.); 2.;    7. |];
		[|    0.; 6.; (-3.) |]	
	|] in
	let b = [|
		[| (-3.); 2. |];
		[| 	  0.; 6. |]
	|] in
	assert_bool "3x3 submatrix incorrect" (equalMatrix (submatrix a 0 2) b)
	);

	"Submatrix of 4x4" >::
	(fun _ ->
	let a = [| 
		[| (-6.); 1.;    1.; 6. |]; 
		[| (-8.); 5.;    8.; 6. |];
		[| (-1.); 0.; 	 8.; 2. |];
		[| (-7.); 1.; (-1.); 1. |]
	|] in
	let b = [|
		[| (-6.);    1.; 6. |]; 
		[| (-8.);    8.; 6. |];
		[| (-7.); (-1.); 1. |]
	|] in
	assert_bool "4x4 submatrix incorrect" (equalMatrix (submatrix a 2 1) b)
	);

	"Minor of 3x3 matrix" >::
	(fun _ ->
	let a = [|
		[| 3.;    5.;    0. |]; 
		[| 2.; (-1.); (-7.) |];
		[| 6.; (-1.);    5. |]
	|] in
	let b = submatrix a 1 0 in 
	assert_bool "minor incorrect" (equalFloat (minor a 1 0) 25.);
	assert_bool "determinant of submatrix incorrect" (equalFloat (determinant b) 25.)
	);

	"Cofactor of 3x3 matrix" >::
	(fun _ ->
	let a = [|
		[| 3.;  5.;  0. |]; 
		[| 2.; -1.; -7. |];
		[| 6.; -1.;  5. |]
	|] in
	assert_bool "minor 0 incorrect" (equalFloat (minor a 0 0) (-12.));
	assert_bool "minor 1 incorrect" (equalFloat (minor a 1 0) 25.);
	assert_bool "cofactor 0 incorrect" (equalFloat (cofactor a 0 0) (-12.));
	assert_bool "cofactor 1 incorrect" (equalFloat (cofactor a 1 0) (-25.))
	);

    "Determinant of a 3x3 matrix" >::
    (fun _ ->
    let a = [| 
		[|  1.; 2.;  6. |];
        [| -5.; 8.; -4. |];
        [|  2.; 6.;  4. |] 
	|] in
	assert_bool "cofactor 0 incorrect" (equalFloat (cofactor a 0 0) (56.));
    assert_bool "cofactor 1 incorrect" (equalFloat (cofactor a 0 1) (12.));
    assert_bool "cofactor 2 incorrect" (equalFloat (cofactor a 0 2) (-46.));
    assert_bool "determinant incorrect" (equalFloat (determinant a) (-196.))
	);

    "Determinant of a 4x4 matrix" >::
    (fun _ ->
    let a = [| 
		[| -2.; -8.;  3.; 5. |];
        [| -3.;  1.;  7.; 3. |];
		[|  1.;  2.; -9.; 6. |];
		[| -6.;  7.;  7.; -9. |]
	|] in
	assert_bool "cofactor 0 incorrect" (equalFloat (cofactor a 0 0) (690.));
    assert_bool "cofactor 1 incorrect" (equalFloat (cofactor a 0 1) (447.));
    assert_bool "cofactor 2 incorrect" (equalFloat (cofactor a 0 2) (210.));
    assert_bool "cofactor 3 incorrect" (equalFloat (cofactor a 0 3) (51.));
	assert_bool "determinant incorrect" (equalFloat (determinant a) (-4071.))
	);

    "Testing an invertible matrix for invertibility" >::
    (fun _ ->
	let a = [| 
		[| 6.;  4.; 4.;  4. |];
        [| 5.;  5.; 7.;  6. |];
        [| 4.; -9.; 3.; -7. |];
        [| 9.;  1.; 7.; -6. |]
	|] in
		assert_bool "determinant incorrect" (equalFloat (determinant a) (-2120.));
		assert_bool "invertibility wrong" (invertible a)
	);

    "Testing a noninvertible matrix for invertibility" >::
    (fun _ ->
	let a = [| 
		[| -4.;  2.; -2.; -3. |];
		[|  9.;  6.;  2.;  6. |];
		[|  0.; -5.;  1.; -5. |];
		[|  0.;  0.;  0.;  0. |]
	|] in
		assert_bool "determinant incorrect" (equalFloat (determinant a) (0.));
		assert_bool "invertibility wrong" (not (invertible a))
	);

    "Calculating the inverse of a matrix" >::
    (fun _ ->
    let a = [|
		[| -5.;  2.;  6.; -8. |];
		[|  1.; -5.;  1.;  8. |];
		[|  7.;  7.; -6.; -7. |];
		[|  1.; -3.;  7.;  4. |] 
	|] in
    let b = [| 
		[|  0.21805;  0.45113;  0.24060; -0.04511 |];
    	[| -0.80827; -1.45677; -0.44361;  0.52068 |];
    	[| -0.07895; -0.22368; -0.05263;  0.19737 |];
    	[| -0.52256; -0.81391; -0.30075;  0.30639 |]
	|] in
		assert_bool "inverse wrong" (equalMatrix (inverse a) b)
	);

    "Calculating the inverse of a matrix 2" >::
    (fun _ ->
    let a = [|
		[|  8.; -5.;  9.;  2. |];
		[|  7.;  5.;  6.;  1. |];
		[| -6.;  0.;  9.;  6. |];
		[| -3.;  0.; -9.; -4. |] 
	|] in
	let b = [|
		[| -0.15385; -0.15385; -0.28205; -0.53846 |];
		[| -0.07692;  0.12308;  0.02564;  0.03077 |];
		[|  0.35897;  0.35897;  0.43590;  0.92308 |];
		[| -0.69231; -0.69231; -0.76923; -1.92308 |]
	|] in
		assert_bool "inverse wrong" (equalMatrix (inverse a) b)
	);

    "Calculating the inverse of a matrix 3" >::
    (fun _ ->
    let a = [|
		[|  9.;  3.;  0.;  9. |];
        [| -5.; -2.; -6.; -3. |];
        [| -4.;  9.;  6.;  4. |];
        [| -7.;  6.;  6.;  2. |]
	|] in
    let b = [| 
		[| -0.04074; -0.07778;  0.14444; -0.22222 |];
		[| -0.07778;  0.03333;  0.36667; -0.33333 |];
		[| -0.02901; -0.14630; -0.10926;  0.12963 |];
		[|  0.17778;  0.06667; -0.26667;  0.33333 |]
	|] in
		assert_bool "inverse wrong" (equalMatrix (inverse a) b)
	);

	"Multiplying a product by its inverse" >::
    (fun _ ->
    let a = [| 
	  	[| 3.; -9.;  7.;  3. |];
        [| 3.; -8.;  2.; -9. |];
        [| -4.; 4.;  4.;  1. |];
        [| -6.; 5.; -1.;  1. |]
	|] in
    let b = [|
		[| 8.;  2.; 2.; 2. |];
		[| 3.; -1.; 7.; 0. |];
		[| 7.;  0.; 5.; 4. |];
		[| 6.; -2.; 0.; 5. |]
	|] in
    let c = multiplyMatrix a b in
	assert_bool "multiplying product by inverse wrong"
		(equalMatrix (multiplyMatrix c (inverse b)) a)
	);
]

let _ = run_test_tt_main tests
