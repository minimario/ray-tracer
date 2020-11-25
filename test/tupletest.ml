open OUnit2

let tests = "Test Suite for Tuple" >::: [
    "A tuple with w=1.0 is a point" >:: 
    (fun _ -> 
        let a = (Tuple.tuple 4.3 (-4.2) 3.1 1.0) in
        assert_equal a.x 4.3;
        assert_equal a.y (-4.2);
        assert_equal a.z 3.1;
        assert_equal a.w 1.0;
        assert_bool "a should be a point" (Tuple.is_point a);
        assert_bool "a should not be a vector" (not (Tuple.is_vector a))
    );
    "A tuple with w=0.0 is a point" >:: 
    (fun _ -> 
        let a = (Tuple.tuple 4.3 (-4.2) 3.1 0.0) in
        assert_equal a.x 4.3;
        assert_equal a.y (-4.2);
        assert_equal a.z 3.1;
        assert_equal a.w 0.0;
        assert_bool "a should not be a point" (not (Tuple.is_point a));
        assert_bool "a should be a vector"  (Tuple.is_vector a)
    );
    "point() creates tuples with w=1" >::
    (fun _ ->
        let p = Tuple.point 4. (-4.) 3. in
        let pTuple = Tuple.tuple 4. (-4.) 3. 1. in 
        assert_bool "point() should create tuple with w=1"
            (Tuple.equals p pTuple)
    );
    "vector() creates tuples with w=0" >::
    (fun _ ->
        let p = Tuple.vector 4. (-4.) 3. in
        let pTuple = Tuple.tuple 4. (-4.) 3. 0. in 
        assert_bool "vector() should create tuple with w=1"
            (Tuple.equals p pTuple)
    );
    "Adding two tuples" >::
    (fun _ -> 
        let a1 = (Tuple.tuple 3. (-2.) 5. 1.) in
        let a2 = (Tuple.tuple (-2.) 3. 1. 0.) in
        let sum = (Tuple.tuple 1. 1. 6. 1.) in
        assert_bool "tuple addition incorrect" 
            (Tuple.equals (Tuple.add a1 a2) sum)
    );
    "Subtracting two points" >::
    (fun _ -> 
        let p1 = Tuple.point 3. 2. 1. in
        let p2 = Tuple.point 5. 6. 7. in 
        let diff = Tuple.vector (-2.) (-4.) (-6.) in
        assert_bool "point point subtraction incorrect" 
            (Tuple.equals (Tuple.subtract p1 p2) diff) 
    );
    "Subtracting a vector from a point" >::
    (fun _ -> 
        let p = Tuple.point 3. 2. 1. in
        let v = Tuple.vector 5. 6. 7. in 
        let diff = Tuple.point (-2.) (-4.) (-6.) in
        assert_bool "vector from point subtraction incorrect" 
            (Tuple.equals (Tuple.subtract p v) diff) 
    );
    "Subtracting two vectors" >::
    (fun _ -> 
        let v1 = Tuple.vector 3. 2. 1. in
        let v2 = Tuple.vector 5. 6. 7. in 
        let diff = Tuple.vector (-2.) (-4.) (-6.) in
        assert_bool "vector vector subtraction incorrect" 
            (Tuple.equals (Tuple.subtract v1 v2) diff) 
    );
    "Subtracting a vector from the zero vector" >::
    (fun _ -> 
        let zero = Tuple.vector 0. 0. 0. in
        let v = Tuple.vector 1. (-2.) 3. in 
        let diff = Tuple.vector (-1.) 2. (-3.) in
        assert_bool "zero vector subtraction incorrect" 
            (Tuple.equals (Tuple.subtract zero v) diff) 
    );
    "Negating a tuple" >::
    (fun _ -> 
        let a = Tuple.tuple 1. (-2.) 3. (-4.) in
        let neg_a = Tuple.tuple (-1.) 2. (-3.) 4. in
        assert_bool "negation incorrect" 
            (Tuple.equals (Tuple.negate a) neg_a)
    );
    "Multiplying a tuple by a scalar" >::
    (fun _ -> 
        let a = Tuple.tuple 1. (-2.) 3. (-4.) in
        let k = 3.5 in
        let res_a = Tuple.tuple (3.5) (-7.) (10.5) (-14.) in
        assert_bool "tuple scalar multiplication incorrect" 
            (Tuple.equals (Tuple.multiply_scalar a k) res_a)
    );
    "Multiplying a tuple by a fraction" >::
    (fun _ -> 
        let a = Tuple.tuple 1. (-2.) 3. (-4.) in
        let k = 0.5 in
        let res_a = Tuple.tuple (0.5) (-1.) (1.5) (-2.) in
        assert_bool "tuple fraction multiplication incorrect" 
            (Tuple.equals (Tuple.multiply_scalar a k) res_a)
    );
    "Dividing a tuple by a fraction" >::
    (fun _ -> 
        let a = Tuple.tuple 1. (-2.) 3. (-4.) in
        let k = 2. in
        let res_a = Tuple.tuple (0.5) (-1.) (1.5) (-2.) in
        assert_bool "tuple fraction division incorrect" 
            (Tuple.equals (Tuple.divide_scalar a k) res_a)
    );
    "Computing vector magnitudes" >::
    (fun _ -> 
        let v1 = Tuple.vector 1. 0. 0. in
        let v2 = Tuple.vector 0. 1. 0. in
        let v3 = Tuple.vector 0. 0. 1. in
        let v4 = Tuple.vector 1. 2. 3. in
        let v5 = Tuple.vector (-1.) (-2.) (-3.) in
        let m1 = 1. in
        let m2 = 1. in
        let m3 = 1. in
        let m4 = sqrt 14. in
        let m5 = sqrt 14. in
        assert_bool "magnitude v1 failed" 
            (Tuple.float_equals (Tuple.magnitude v1) m1);
        assert_bool "magnitude v2 failed" 
            (Tuple.float_equals (Tuple.magnitude v2) m2);
        assert_bool "magnitude v3 failed" 
            (Tuple.float_equals (Tuple.magnitude v3) m3);
        assert_bool "magnitude v4 failed" 
            (Tuple.float_equals (Tuple.magnitude v4) m4);
        assert_bool "magnitude v5 failed" 
            (Tuple.float_equals (Tuple.magnitude v5) m5);
    );
    "Normalizing vectors" >::
    (fun _ -> 
        let v1 = Tuple.vector 4. 0. 0. in
        let v2 = Tuple.vector 1. 2. 3. in
        let v1_norm = Tuple.vector 1. 0. 0. in
        let v2_norm = Tuple.vector (1./.sqrt 14.) (2./.sqrt 14.) (3./.sqrt 14.) in
        assert_bool "normalize v1 failed" 
            (Tuple.equals (Tuple.normalize v1) v1_norm);
        assert_bool "normalize v2 failed" 
            (Tuple.equals (Tuple.normalize v2) v2_norm);
        assert_bool "magnitude of normalized v1 failed" 
            (Tuple.float_equals (Tuple.magnitude (Tuple.normalize v1)) 1.0);
        assert_bool "magnitude of normalized v2 failed" 
            (Tuple.float_equals (Tuple.magnitude (Tuple.normalize v2)) 1.0);
    );
    "Dot product of two vectors" >::
    (fun _ -> 
        let v1 = Tuple.vector 2. 3. 4. in
        let v2 = Tuple.vector 1. 2. 3. in
        assert_bool "dot product failed" 
            (Tuple.float_equals (Tuple.dot v1 v2) 20.);
    );
    "Cross product of two vectors" >::
    (fun _ -> 
        let v1 = Tuple.vector 1. 2. 3. in
        let v2 = Tuple.vector 2. 3. 4. in
        let v1_cross_v2 = Tuple.vector (-1.) 2. (-1.) in
        let v2_cross_v1 = Tuple.vector (1.) (-2.) (1.) in
        assert_bool "v1 x v2 failed" 
            (Tuple.equals (Tuple.cross v1 v2) v1_cross_v2);
        assert_bool "v2 x v1 failed" 
            (Tuple.equals (Tuple.cross v2 v1) v2_cross_v1);
    );
]

let _ = run_test_tt_main tests
