open OUnit2

let equalTuple = Tuple.equals
let tests = "Test Suite for Shapes" >::: [
    "The default transformation" >::
    (fun _ ->
        let s = Shape.shape_create TestShape in
        assert (s.transform = Matrix.identity_matrix)
    );

    "Assigning a transformation" >::
    (fun _ ->
        let s = Shape.shape_create TestShape in
        let s' = Shape.set_transform s (Transformations.translation 2. 3. 4.) in
        assert (s'.transform = Transformations.translation 2. 3. 4.)
    );

    "The default material" >::
    (fun _ ->
        let s = Shape.shape_create TestShape in
        assert (s.material = Reflection.default_material)
    );

    "Assigning a material" >::
    (fun _ ->
        let s = Shape.shape_create TestShape in
        let m = {Reflection.default_material with ambient=1.} in 
        let s' = Shape.set_material s m in
        assert (s'.material = m)
    );

    "Intersecting a scaled shape with a ray" >::
    (fun _ ->
        let s = Shape.shape_create TestShape in
        let r = Rays.ray (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
        let s' = Shape.set_transform s (Transformations.scaling 2. 2. 2.) in
        match Shape.intersect s' r with
        | [i1; i2; i3; i4; i5; i6; i7; i8] -> (
            let saved_ray_origin = Tuple.tuple i1.t i2.t i3.t i4.t in
            let saved_ray_direction = Tuple.tuple i5.t i6.t i7.t i8.t in
            assert (Tuple.equals saved_ray_origin (Tuple.point 0. 0. (-2.5)));
            assert (Tuple.equals saved_ray_direction (Tuple.vector 0. 0. 0.5)))
        | _ -> assert false
    );

    "Intersecting a translated shape with a ray" >::
    (fun _ ->
        let s = Shape.shape_create TestShape in
        let r = Rays.ray (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
        let s' = Shape.set_transform s (Transformations.translation 5. 0. 0.) in
        match Shape.intersect s' r with
        | [i1; i2; i3; i4; i5; i6; i7; i8] -> (
            let saved_ray_origin = Tuple.tuple i1.t i2.t i3.t i4.t in
            let saved_ray_direction = Tuple.tuple i5.t i6.t i7.t i8.t in
            assert (Tuple.equals saved_ray_origin (Tuple.point (-5.) 0. (-5.)));
            assert (Tuple.equals saved_ray_direction (Tuple.vector 0. 0. 1.)))
        | _ -> assert false
    );

    "Computing the normal on a translated shape" >::
    (fun _ ->
        let s = Shape.shape_create TestShape in
        let s' = Shape.set_transform s (Transformations.translation 0. 1. 0.) in
        let n = Shape.normal_at s' (Tuple.point 0. 1.70711 (-0.70711)) in
        assert (Tuple.equals n (Tuple.vector 0. 0.70711 (-0.70711)))
    );

    "Computing the normal on a transformed shape" >::
    (fun _ ->
        let s = Shape.shape_create TestShape in
        let m = Matrix.multiply (Transformations.scaling 1. 0.5 1.) 
                                (Transformations.rotation_z (Float.pi/.5.)) in
        let s' = Shape.set_transform s m in
        let n = Shape.normal_at s' (Tuple.point 0. (sqrt 2./.2.) (-.(sqrt 2./.2.))) in
        assert (Tuple.equals n (Tuple.vector 0. 0.97014 (-0.24254)))
    );
]

let _ = run_test_tt_main tests
