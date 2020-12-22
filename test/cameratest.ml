open OUnit2

let tests = "Test Suite for Camera" >::: [
    "Multiplying by a translation matrix" >::
    (fun _ ->
    let hsize = 160
    and vsize = 120
    and field_of_view = Float.pi /. 2. in
    let c = Camera.create_camera hsize vsize field_of_view in
    assert (c.hsize = 160 && 
            c.vsize = 120 && 
            Tuple.float_equals c.field_of_view (Float.pi/.2.) &&
            c.transform = Matrix.identity_matrix)
    );

    "The pixel size for a horizontal canvas" >::
    (fun _ ->
        let half_pi = Float.pi /. 2. in
        let c = Camera.create_camera 200 125 half_pi in
        assert (Tuple.float_equals c.pixel_size 0.01)
    );

    "The pixel size for a vertical canvas" >::
    (fun _ ->
        let half_pi = Float.pi /. 2. in
        let c = Camera.create_camera 125 200 half_pi in
        assert (Tuple.float_equals c.pixel_size 0.01)
    );

    "Constructing a ray through the center of the canvas" >::
    (fun _ ->
        let half_pi = Float.pi /. 2. in
        let c = Camera.create_camera 201 101 half_pi in
        let r = Camera.ray_for_pixel c 100. 50. in
        assert (
            Tuple.equals r.origin (Tuple.point 0. 0. 0.) && 
            Tuple.equals r.direction (Tuple.vector 0. 0. (-1.))
        )
    );

    "Constructing a ray through the center of the canvas" >::
    (fun _ ->
        let half_pi = Float.pi /. 2. in
        let c = Camera.create_camera 201 101 half_pi in
        let r = Camera.ray_for_pixel c 0. 0. in
        assert (
            Tuple.equals r.origin (Tuple.point 0. 0. 0.) && 
            Tuple.equals r.direction (Tuple.vector 0.66519 0.33259 (-0.66851))
        )
    );

    "Constructing a ray through the center of the canvas" >::
    (fun _ ->
        let half_pi = Float.pi /. 2. in
        let quarter_pi = half_pi /. 2. in
        let c = Camera.create_camera 201 101 half_pi in
        let transform = Transformations.(Matrix.multiply 
                            (rotation_y quarter_pi) 
                            (translation 0. (-2.) 5.)) in 
        let c' = Camera.set_transform c transform in
        let r = Camera.ray_for_pixel c' 100. 50. in
        assert (
            Tuple.equals r.origin (Tuple.point 0. 2. (-5.)) && 
            Tuple.equals r.direction (Tuple.vector (sqrt 2./.2.) 0. (-.sqrt 2./.2.))
        )
    );

    "Rendering a world with a camera" >::
    (fun _ ->
        let w = World.default_world in
        let half_pi = Float.pi /. 2. in
        let c = Camera.create_camera 11 11 half_pi in
        let from_pt = Tuple.point 0. 0. (-5.) in
        let to_pt = Tuple.point 0. 0. 0. in
        let up = Tuple.vector 0. 1. 0. in
        let c' = Camera.set_transform c (Transformations.view_transform from_pt to_pt up) in 
        let image = Camera.render c' w in
        assert (Color.equals
                    (Canvas.pixel_at image 5 5) 
                    (Color.color 0.38066 0.47583 0.2855)
        )
    );
]

let _ = run_test_tt_main tests
