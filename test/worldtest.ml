open OUnit2

let tests = "Test Suite for World" >::: [
    "Creating a world" >::
    (fun _ ->
        let w = World.world_create in
	assert_bool "should have no objects" ((List.length w.objects) == 0);
	assert_bool "should have no light source" (Option.is_none w.light);
    );
    "The default world" >::
    (fun _ ->
    	let light = Reflection.point_light (Tuple.point ~-.10. 10. ~-.10.) (Color.color 1. 1. 1.) 
        and w = World.default_world in
	assert_bool "incorrect light source" (Reflection.point_light_equals (Option.get w.light) light);

	let s1 = Sphere.sphere
    	and (material: Reflection.material) = {color=Color.color 1. 1. 1.; ambient=0.1; diffuse=0.9;
                   specular=0.9; shininess=200.0} in
    	Sphere.set_material s1 material;
	assert_bool "outer sphere not found" (World.contains w s1);

    	let s2 = Sphere.sphere
    	and new_transform = Transformations.scaling 0.5 0.5 0.5 in
    	Sphere.set_transform s2 new_transform;
	assert_bool "inner sphere not found" (World.contains w s2);
    );
    "Precomputing the state of an intersection" >::
    (fun _ ->
	let shape = Sphere.sphere in
	let i = Intersections.intersection 4. shape
        and r = Rays.ray (Tuple.point 0. 0. ~-.5.) (Tuple.vector 0. 0. 1.) in
        let comps = World.prepare_computations i r in
	assert_bool "object property incorrect"
	    (Sphere.shape_equals comps.comps_object i.intersection_object);
        assert_bool "computed point incorrect"
	    (Tuple.equals comps.point (Tuple.point 0. 0. ~-.1.));
	assert_bool "computed eyev incorrect"
	    (Tuple.equals comps.eyev (Tuple.vector 0. 0. ~-.1.));
	assert_bool "precomputed normalv incorrect"
	    (Tuple.equals comps.normalv (Tuple.vector 0. 0. ~-.1.));
    );
]

let _ = run_test_tt_main tests
