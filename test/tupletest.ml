open OUnit2
open Tuple

let tests = "Test Suite for Tuple" >::: [
    "A tuple with w=1.0 is a point" >:: 
    (fun _ -> 
        let a = (tuple 4.3 (-4.2) 3.1 1.0) in
        assert_equal a.x 4.3;
        assert_equal a.y (-4.2);
        assert_equal a.z 3.1;
        assert_equal a.w 1.0;
        assert_bool "a should be a point" (isPoint a);
        assert_bool "a should not be a vector" (not (isVector a))
    );
    "A tuple with w=0.0 is a point" >:: 
    (fun _ -> 
        let a = (tuple 4.3 (-4.2) 3.1 0.0) in
        assert_equal a.x 4.3;
        assert_equal a.y (-4.2);
        assert_equal a.z 3.1;
        assert_equal a.w 0.0;
        assert_bool "a should not be a point" (not (isPoint a));
        assert_bool "a should be a vector"  (isVector a)
    );
    "point() creates tuples with w=1" >::
    (fun _ ->
        let p = point 4. (-4.) 3. in
        let pTuple = tuple 4. (-4.) 3. 1. in 
        assert_bool "point() should create tuple with w=1"
            (equalTuple p pTuple)
    );
    "vector() creates tuples with w=0" >::
    (fun _ ->
        let p = vector 4. (-4.) 3. in
        let pTuple = tuple 4. (-4.) 3. 0. in 
        assert_bool "vector() should create tuple with w=1"
            (equalTuple p pTuple)
    );
    "Adding two tuples" >::
    (fun _ -> 
        let a1 = (tuple 3. (-2.) 5. 1.) in
        let a2 = (tuple (-2.) 3. 1. 0.) in
        let sum = (tuple 1. 1. 6. 1.) in
        assert_bool "tuple addition incorrect" 
            (equalTuple (addTuple a1 a2) sum)
    );
    "Subtracting two points" >::
    (fun _ -> 
        let p1 = point 3. 2. 1. in
        let p2 = point 5. 6. 7. in 
        let diff = vector (-2.) (-4.) (-6.) in
        assert_bool "point point subtraction incorrect" 
            (equalTuple (subtractTuple p1 p2) diff) 
    );
    "Subtracting a vector from a point" >::
    (fun _ -> 
        let p = point 3. 2. 1. in
        let v = vector 5. 6. 7. in 
        let diff = point (-2.) (-4.) (-6.) in
        assert_bool "vector from point subtraction incorrect" 
            (equalTuple (subtractTuple p v) diff) 
    );
    "Subtracting two vectors" >::
    (fun _ -> 
        let v1 = vector 3. 2. 1. in
        let v2 = vector 5. 6. 7. in 
        let diff = vector (-2.) (-4.) (-6.) in
        assert_bool "vector vector subtraction incorrect" 
            (equalTuple (subtractTuple v1 v2) diff) 
    );
    "Subtracting a vector from the zero vector" >::
    (fun _ -> 
        let zero = vector 0. 0. 0. in
        let v = vector 1. (-2.) 3. in 
        let diff = vector (-1.) 2. (-3.) in
        assert_bool "zero vector subtraction incorrect" 
            (equalTuple (subtractTuple zero v) diff) 
    );
    "Negating a tuple" >::
    (fun _ -> 
        let a = tuple 1. (-2.) 3. (-4.) in
        let neg_a = tuple (-1.) 2. (-3.) 4. in
        assert_bool "negation incorrect" 
            (equalTuple (negateTuple a) neg_a)
    );
    "Multiplying a tuple by a scalar" >::
    (fun _ -> 
        let a = tuple 1. (-2.) 3. (-4.) in
        let k = 3.5 in
        let res_a = tuple (3.5) (-7.) (10.5) (-14.) in
        assert_bool "tuple scalar multiplication incorrect" 
            (equalTuple (multiplyTupleScalar a k) res_a)
    );
    "Multiplying a tuple by a fraction" >::
    (fun _ -> 
        let a = tuple 1. (-2.) 3. (-4.) in
        let k = 0.5 in
        let res_a = tuple (0.5) (-1.) (1.5) (-2.) in
        assert_bool "tuple fraction multiplication incorrect" 
            (equalTuple (multiplyTupleScalar a k) res_a)
    );
    "Dividing a tuple by a fraction" >::
    (fun _ -> 
        let a = tuple 1. (-2.) 3. (-4.) in
        let k = 2. in
        let res_a = tuple (0.5) (-1.) (1.5) (-2.) in
        assert_bool "tuple fraction division incorrect" 
            (equalTuple (divideTupleScalar a k) res_a)
    );
    "Computing vector magnitudes" >::
    (fun _ -> 
        let v1 = vector 1. 0. 0. in
        let v2 = vector 0. 1. 0. in
        let v3 = vector 0. 0. 1. in
        let v4 = vector 1. 2. 3. in
        let v5 = vector (-1.) (-2.) (-3.) in
        let m1 = 1. in
        let m2 = 1. in
        let m3 = 1. in
        let m4 = sqrt 14. in
        let m5 = sqrt 14. in
        assert_bool "magnitude v1 failed" 
            (equalFloat (magnitude v1) m1);
        assert_bool "magnitude v2 failed" 
            (equalFloat (magnitude v2) m2);
        assert_bool "magnitude v3 failed" 
            (equalFloat (magnitude v3) m3);
        assert_bool "magnitude v4 failed" 
            (equalFloat (magnitude v4) m4);
        assert_bool "magnitude v5 failed" 
            (equalFloat (magnitude v5) m5);
    );
    "Normalizing vectors" >::
    (fun _ -> 
        let v1 = vector 4. 0. 0. in
        let v2 = vector 1. 2. 3. in
        let v1_norm = vector 1. 0. 0. in
        let v2_norm = vector (1./.sqrt 14.) (2./.sqrt 14.) (3./.sqrt 14.) in
        assert_bool "normalize v1 failed" 
            (equalTuple (normalize v1) v1_norm);
        assert_bool "normalize v2 failed" 
            (equalTuple (normalize v2) v2_norm);
        assert_bool "magnitude of normalized v1 failed" 
            (equalFloat (magnitude (normalize v1)) 1.0);
        assert_bool "magnitude of normalized v2 failed" 
            (equalFloat (magnitude (normalize v2)) 1.0);
    );
    "Dot product of two vectors" >::
    (fun _ -> 
        let v1 = vector 2. 3. 4. in
        let v2 = vector 1. 2. 3. in
        assert_bool "dot product failed" 
            (equalFloat (dot v1 v2) 20.);
    );
    "Cross product of two vectors" >::
    (fun _ -> 
        let v1 = vector 1. 2. 3. in
        let v2 = vector 2. 3. 4. in
        let v1_cross_v2 = vector (-1.) 2. (-1.) in
        let v2_cross_v1 = vector (1.) (-2.) (1.) in
        assert_bool "v1 x v2 failed" 
            (equalTuple (cross v1 v2) v1_cross_v2);
        assert_bool "v2 x v1 failed" 
            (equalTuple (cross v2 v1) v2_cross_v1);
    );
<<<<<<< HEAD
=======
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
    "Scalar color multiplication" >::
    (fun _ ->
        let c1 = color 0.2 0.3 0.4 in
	let expected_product = color 0.4 0.6 0.8 in
	assert_bool "color scalar multiplication incorrect"
	    (equalColor (multiplyColorScalar c1 2.) expected_product);
    );
    "Multiplying colors" >::
    (fun _ ->
        let c1 = color 1. 0.2 0.4 in
	let c2 = color 0.9 1. 0.1 in
	let expected_product = color 0.9 0.2 0.04 in
	assert_bool "color multiplication incorrect"
	    (equalColor (multiplyColor c1 c2) expected_product);
    );
>>>>>>> 095fdf60103d5f597557472f1ba214254b1b3eb4
]

let _ = run_test_tt_main tests
