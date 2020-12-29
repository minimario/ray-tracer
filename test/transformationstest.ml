open OUnit2
open Matrix
open Transformations

let equalTuple = Tuple.equals
let tests = "Test Suite for Transformations" >::: [
    "Multiplying by a translation matrix" >::
    (fun _ ->
    let p = Tuple.point (-3.) 4. 5. in
    let transform = translation 5. (-3.) 2. in
    let q = Tuple.point 2. 1. 7. in
    assert_bool "multiplication by translation matrix failed"
        (equalTuple (Matrix.multiply_tuple transform p) q)
    );

    "Multiplying by the inverse of a translation matrix" >::
    (fun _ ->
    let p = Tuple.point (-3.) 4. 5. in
    let transform = translation 5. (-3.) 2. in
    let inv = inverse transform in
    let q = Tuple.point (-8.) 7. 3. in
    assert_bool "multiplication by inverse translation matrix failed"
        (equalTuple (Matrix.multiply_tuple inv p) q)
    );

    "Translation does not affect vectors" >::
    (fun _ ->
    let v = Tuple.vector (-3.) 4. 5. in
    let transform = translation 5. (-3.) 2. in
    assert_bool "translation of vector not constant"
        (equalTuple (Matrix.multiply_tuple transform v) v)
    );

    "A scaling matrix applied to a Tuple.point" >::
    (fun _ ->
    let p = Tuple.point (-4.) 6. 8. in
    let transform = scaling 2. 3. 4. in
    let q = Tuple.point (-8.) 18. 32. in
    assert_bool "scaling matrix applied to Tuple.point failed"
        (equalTuple (Matrix.multiply_tuple transform p) q)
    );

    "A scaling matrix applied to a vector" >::
    (fun _ ->
    let v = Tuple.vector (-4.) 6. 8. in
    let transform = scaling 2. 3. 4. in
    let v2 = Tuple.vector (-8.) 18. 32. in
    assert_bool "scaling matrix applied to vector failed"
        (equalTuple (Matrix.multiply_tuple transform v) v2)
    );

    "Multiplying by the inverse of a scaling matrix" >::
    (fun _ ->
    let v = Tuple.vector (-4.) 6. 8. in
    let transform = scaling 2. 3. 4. in
    let inv = inverse transform in
    let v2 = Tuple.vector (-2.) 2. 2. in
    assert_bool "multiplication by inverse scaling matrix failed"
        (equalTuple (Matrix.multiply_tuple inv v) v2)
    );

    "Reflection is scaling by a negative value" >::
    (fun _ ->
    let p = Tuple.point 2. 3. 4. in
    let transform = scaling (-1.) 1. 1. in
    let q = Tuple.point (-2.) 3. 4. in
    assert_bool "reflection failed"
        (equalTuple (Matrix.multiply_tuple transform p) q)
    );

    "Rotating a Tuple.point aUtil.round the x axis" >::
    (fun _ ->
    let p = Tuple.point 0. 1. 0. in 
    let half_quarter = rotation_x (Float.pi /. 4.) in
    let full_quarter = rotation_x (Float.pi /. 2.) in
    let hq_p = Tuple.point 0. ((sqrt 2.) /. 2.) ((sqrt 2.) /. 2.) in
    let fq_p = Tuple.point 0. 0. 1. in
    assert_bool "half quarter failed" 
        (equalTuple (Matrix.multiply_tuple half_quarter p) hq_p);
    assert_bool "full quarter failed" 
        (equalTuple (Matrix.multiply_tuple full_quarter p) fq_p)
    );

    "Inverse of an x-rotation" >::
    (fun _ ->
    let p = Tuple.point 0. 1. 0. in 
    let half_quarter = rotation_x (Float.pi /. 4.) in
    let inv = inverse half_quarter in
    let inv_hq_p = Tuple.point 0. ((sqrt 2.) /. 2.) (-.(sqrt 2.) /. 2.) in
    assert_bool "inverse half quarter failed" 
        (equalTuple (Matrix.multiply_tuple inv p) inv_hq_p)
    );

    "Rotating a Tuple.point aUtil.round the y axis" >::
    (fun _ ->
    let p = Tuple.point 0. 0. 1. in 
    let half_quarter = rotation_y (Float.pi /. 4.) in
    let full_quarter = rotation_y (Float.pi /. 2.) in
    let hq_p = Tuple.point ((sqrt 2.) /. 2.) 0. ((sqrt 2.) /. 2.) in
    let fq_p = Tuple.point 1. 0. 0. in
    assert_bool "half quarter failed" 
        (equalTuple (Matrix.multiply_tuple half_quarter p) hq_p);
    assert_bool "full quarter failed" 
        (equalTuple (Matrix.multiply_tuple full_quarter p) fq_p)
    );

    "Rotating a Tuple.point aUtil.round the z axis" >::
    (fun _ ->
    let p = Tuple.point 0. 1. 0. in 
    let half_quarter = rotation_z (Float.pi /. 4.) in
    let full_quarter = rotation_z (Float.pi /. 2.) in
    let hq_p = Tuple.point (-.(sqrt 2.) /. 2.) ((sqrt 2.) /. 2.) 0. in
    let fq_p = Tuple.point (-1.) 0. 0. in
    assert_bool "half quarter failed" 
        (equalTuple (Matrix.multiply_tuple half_quarter p) hq_p);
    assert_bool "full quarter failed" 
        (equalTuple (Matrix.multiply_tuple full_quarter p) fq_p)
    );

    "A shearing transformation moves x in proportion to y" >::
    (fun _ ->
    let transform = shearing 1. 0. 0. 0. 0. 0. in
    let p = Tuple.point 2. 3. 4. in
    let q = Tuple.point 5. 3. 4. in
    assert_bool "shearing x in proportion to y failed"
        (equalTuple (Matrix.multiply_tuple transform p) q)
    );

    "A shearing transformation moves x in proportion to z" >::
    (fun _ ->
    let transform = shearing 0. 1. 0. 0. 0. 0. in
    let p = Tuple.point 2. 3. 4. in
    let q = Tuple.point 6. 3. 4. in
    assert_bool "shearing x in proportion to z failed"
        (equalTuple (Matrix.multiply_tuple transform p) q)
    );

    "A shearing transformation moves y in proportion to x" >::
    (fun _ ->
    let transform = shearing 0. 0. 1. 0. 0. 0. in
    let p = Tuple.point 2. 3. 4. in
    let q = Tuple.point 2. 5. 4. in
    assert_bool "shearing x in proportion to y failed"
        (equalTuple (Matrix.multiply_tuple transform p) q)
    );

    "A shearing transformation moves y in proportion to z" >::
    (fun _ ->
    let transform = shearing 0. 0. 0. 1. 0. 0. in
    let p = Tuple.point 2. 3. 4. in
    let q = Tuple.point 2. 7. 4. in
    assert_bool "shearing y in proportion to z failed"
        (equalTuple (Matrix.multiply_tuple transform p) q)
    );

    "A shearing transformation moves z in proportion to x" >::
    (fun _ ->
    let transform = shearing 0. 0. 0. 0. 1. 0. in
    let p = Tuple.point 2. 3. 4. in
    let q = Tuple.point 2. 3. 6. in
    assert_bool "shearing z in proportion to x failed"
        (equalTuple (Matrix.multiply_tuple transform p) q)
    );

    "A shearing transformation moves z in proportion to y" >::
    (fun _ ->
    let transform = shearing 0. 0. 0. 0. 0. 1. in
    let p = Tuple.point 2. 3. 4. in
    let q = Tuple.point 2. 3. 7. in
    assert_bool "shearing z in proportion to y failed"
        (equalTuple (Matrix.multiply_tuple transform p) q)
    );

    "Individual transformations are applied in sequence" >::
    (fun _ ->
    let p = Tuple.point 1. 0. 1. in
    let a = rotation_x (Float.pi /. 2.) in
    let b = scaling 5. 5. 5. in
    let c = translation 10. 5. 7. in
    let p2 = Matrix.multiply_tuple a p in
    let p3 = Matrix.multiply_tuple b p2 in
    let p4 = Matrix.multiply_tuple c p3 in
    let p2_correct = Tuple.point 1. (-1.) 0. in 
    let p3_correct = Tuple.point 5. (-5.) 0. in 
    let p4_correct = Tuple.point 15. 0. 7. in
    assert_bool "p2 failed" (equalTuple p2 p2_correct);
    assert_bool "p3 failed" (equalTuple p3 p3_correct);
    assert_bool "p4 failed" (equalTuple p4 p4_correct)
    );

    "Individual transformations are applied in sequence" >::
    (fun _ ->
    let p = Tuple.point 1. 0. 1. in
    let a = rotation_x (Float.pi /. 2.) in
    let b = scaling 5. 5. 5. in
    let c = translation 10. 5. 7. in
    let t = Matrix.multiply c (Matrix.multiply b a) in
    let tp = Matrix.multiply_tuple t p in
    let tp_correct = Tuple.point 15. 0. 7. in 
    assert_bool "chained transformations failed" (equalTuple tp tp_correct);
    );

    "Transformation matrix for default orientation" >::
    (fun _ ->
    let from_pt = Tuple.point 0. 0. 0. in
    let to_pt = Tuple.point 0. 0. (-1.) in
    let up = Tuple.vector 0. 1. 0. in
    let t = Transformations.view_transform from_pt to_pt up in
    assert (Matrix.equals identity_matrix t)
    );

    "Transformation matrix looking in positive z direction" >::
    (fun _ ->
    let from_pt = Tuple.point 0. 0. 0. in
    let to_pt = Tuple.point 0. 0. 1. in
    let up = Tuple.vector 0. 1. 0. in
    let t = Transformations.view_transform from_pt to_pt up in
    assert (Matrix.equals (scaling (-1.) 1. (-1.)) t)
    );

    "The view transformation moves the world via translation" >::
    (fun _ ->
    let from_pt = Tuple.point 0. 0. 8. in
    let to_pt = Tuple.point 0. 0. 0. in
    let up = Tuple.vector 0. 1. 0. in
    let t = Transformations.view_transform from_pt to_pt up in
    assert (Matrix.equals (translation 0. 0. (-8.)) t)
    );

    "An arbitrary view transformation" >::
    (fun _ ->
    let from_pt = Tuple.point 1. 3. 2. in
    let to_pt = Tuple.point 4. (-2.) 8. in
    let up = Tuple.vector 1. 1. 0. in
    let t = Transformations.view_transform from_pt to_pt up in
    let correct_transform = [| 
        [| -0.50709; 0.50709;  0.67612; -2.36643 |];
        [|  0.76772; 0.60609;  0.12122; -2.82843 |];
        [| -0.35857; 0.59761; -0.71714;  0.00000 |];
        [|  0.00000; 0.00000;  0.00000;  1.00000 |] 
        |] in
    assert (Matrix.equals correct_transform t)
    );
]

let _ = run_test_tt_main tests
