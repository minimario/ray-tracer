open OUnit2

let equalTuple = Tuple.equals
let tests = "Test Suite for Planes" >::: [
    "The normal of a plane is constant everywhere" >::
    (fun _ ->
        let p = Shape.plane in
        let n1 = Plane.local_normal_at p (Tuple.point 0. 0. 0.) in
        let n2 = Plane.local_normal_at p (Tuple.point 10. 0. (-10.)) in
        let n3 = Plane.local_normal_at p (Tuple.point (-5.) 0. 150.) in
        assert (Tuple.equals n1 (Tuple.vector 0. 1. 0.));
        assert (Tuple.equals n2 (Tuple.vector 0. 1. 0.));
        assert (Tuple.equals n3 (Tuple.vector 0. 1. 0.))
    );

    "Intersect with a ray parallel to the plane" >::
    (fun _ ->
        let p = Shape.plane in
        let r = Rays.ray (Tuple.point 0. 10. 0.) (Tuple.vector 0. 0. 1.) in
        let xs = Plane.local_intersect p r in
        assert (List.length xs = 0)
    );

    "Intersect with a coplanar ray" >::
    (fun _ ->
        let p = Shape.plane in
        let r = Rays.ray (Tuple.point 0. 0. 0.) (Tuple.vector 0. 0. 1.) in
        let xs = Plane.local_intersect p r in
        assert (List.length xs = 0)
    );

    "A ray intersecting a plane from above" >::
    (fun _ ->
        let p = Shape.plane in
        let r = Rays.ray (Tuple.point 0. 1. 0.) (Tuple.vector 0. (-1.) 0.) in
        let xs = Plane.local_intersect p r in
        assert (List.length xs = 1);
        assert ((List.hd xs).t = 1.);
        assert ((List.hd xs).intersection_object = p);
    );

    "A ray intersecting a plane from below" >::
    (fun _ ->
        let p = Shape.plane in
        let r = Rays.ray (Tuple.point 0. (-1.) 0.) (Tuple.vector 0. 1. 0.) in
        let xs = Plane.local_intersect p r in
        assert (List.length xs = 1);
        assert ((List.hd xs).t = 1.);
        assert ((List.hd xs).intersection_object = p);
    );
]

let _ = run_test_tt_main tests
