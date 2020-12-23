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
    	and (material: Reflection.material) = {color=Color.color 0.8 1.0 0.6; ambient=0.1; diffuse=0.7;
                   specular=0.2; shininess=200.0} in
    	let s1' = Sphere.set_material s1 material in
	assert_bool "outer sphere not found" (World.contains w s1');

    	let s2 = Sphere.sphere
    	and new_transform = Transformations.scaling 0.5 0.5 0.5 in
    	let s2' = Sphere.set_transform s2 new_transform in
	assert_bool "inner sphere not found" (World.contains w s2');
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

	"Intersect a world with a ray" >::
	(fun _ ->
		let w = World.default_world in
		let r = Rays.ray (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in 
		let xs = World.intersect_world w r in 
		match xs with 
		| i1 :: i2 :: i3 :: i4 :: [] -> 
			assert ((Tuple.float_equals i1.t 4.) && (Tuple.float_equals i2.t 4.5) &&
				   (Tuple.float_equals i3.t 5.5) && (Tuple.float_equals i4.t 6.))
		| _ -> assert false
	);

	"Hit when intersection is on outside" >::
	(fun _ ->
		let r = Rays.ray (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
		let shape = Sphere.sphere in
		let i = Intersections.intersection 4. shape in
		let comps = World.prepare_computations i r in
		assert (not comps.inside)
	);

	"Hit when intersection is on inside" >::
	(fun _ ->
		let r = Rays.ray (Tuple.point 0. 0. 0.) (Tuple.vector 0. 0. 1.) in
		let shape = Sphere.sphere in
		let i = Intersections.intersection 1. shape in
		let comps = World.prepare_computations i r in
		assert (Tuple.equals comps.point (Tuple.point 0. 0. 1.));
		assert (Tuple.equals comps.eyev (Tuple.vector 0. 0. (-1.)));
		assert comps.inside;
		assert (Tuple.equals comps.normalv (Tuple.vector 0. 0. (-1.)));
	);

	"Shading an intersection" >::
	(fun _ ->
		let w = World.default_world in
		let r = Rays.ray (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
		let shape = List.nth w.objects 0 in
		let i = Intersections.intersection 4. shape in 
		let comps = World.prepare_computations i r in
		let c = World.shade_hit w comps in
		assert (Color.equals c (Color.color 0.38066 0.47583 0.2855))
	);

	"Shading an intersection from the inside" >::
	(fun _ ->
		let world_light = Reflection.point_light (Tuple.point 0. 0.25 0.) (Color.color 1. 1. 1.) in
		let w = {World.default_world with light= Some world_light} in
		let r = Rays.ray (Tuple.point 0. 0. 0.) (Tuple.vector 0. 0. 1.) in
		let shape = List.nth w.objects 1 in
		let i = Intersections.intersection 0.5 shape in 
		let comps = World.prepare_computations i r in
		let c = World.shade_hit w comps in
		assert (Color.equals c (Color.color 0.90498 0.90498 0.90498))
	);

	"The colour when a ray misses" >::
	(fun _ ->
		let w = World.default_world in
		let r = Rays.ray (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 1. 0.) in
		let c = World.color_at w r in
		assert (Color.equals c Color.black)
	);

	"The colour when a ray hits" >::
	(fun _ ->
		let w = World.default_world in
		let r = Rays.ray (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
		let c = World.color_at w r in
		assert (Color.equals c (Color.color 0.38066 0.47583 0.2855))
	);

	"The colour when a ray hits" >::
	(fun _ ->
		let w = World.default_world in
		let r = Rays.ray (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
		let c = World.color_at w r in
		assert (Color.equals c (Color.color 0.38066 0.47583 0.2855))
	);

	"The colour with an intersection behind the ray" >::
	(fun _ ->
		let w = World.default_world in
		let outer = List.nth w.objects 0 in
		let new_outer = {outer with material={outer.material with ambient=1.}} in 
		let inner = List.nth w.objects 1 in
		let new_inner = {inner with material={inner.material with ambient=1.}} in 
		let r = Rays.ray (Tuple.point 0. 0. 0.75) (Tuple.vector 0. 0. (-1.)) in
		let c = World.color_at {w with objects=[new_outer; new_inner]} r in
		assert (Color.equals c inner.material.color)
	);

	"There is no shadow when nothing is collinear with point and light" >::
	(fun _ ->
		let w = World.default_world in
		let p = Tuple.point 0. 10. 0. in
		assert (not (World.is_shadowed w p))
	);

	"The shadow when an object is between the point and the light" >::
	(fun _ ->
		let w = World.default_world in
		let p = Tuple.point 10. (-10.) 10. in
		assert (World.is_shadowed w p)
	);

	"There is no shadow when an object is behind the light" >::
	(fun _ ->
		let w = World.default_world in
		let p = Tuple.point (-20.) 20. (-20.) in
		assert (not (World.is_shadowed w p))
	);

	"There is no shadow when an object is behind the point" >::
	(fun _ ->
		let w = World.default_world in
		let p = Tuple.point (-2.) 2. (-2.) in
		assert (not (World.is_shadowed w p))
	);

	"shade_hit() is given an intersection in shadow" >::
	(fun _ ->
		let light = Reflection.point_light (Tuple.point 0. 0. (-10.)) (Color.color 1. 1. 1.) in
		let s1 = Sphere.sphere in
		let s2 = Sphere.sphere in 
		let s2' = Sphere.set_transform s2 (Transformations.translation 0. 0. 10.) in
		let (w:World.world) = {objects=[s1; s2']; light=Some light} in 
		let r = Rays.ray (Tuple.point 0. 0. 5.) (Tuple.vector 0. 0. 1.) in
		let i = Intersections.intersection 4. s2' in
		let comps = World.prepare_computations i r in
		let c = World.shade_hit w comps in
		assert (Color.equals c (Color.color 0.1 0.1 0.1))		 
	);

	"The hit should offset the point" >::
	(fun _ ->
		let r = Rays.ray (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
		let shape = Sphere.sphere in
		let shape' = Sphere.set_transform shape (Transformations.translation 0. 0. 1.) in
		let i = Intersections.intersection 5. shape' in
		let comps = World.prepare_computations i r in
		let op = comps.over_point in
		assert (comps.over_point.z < -.Tuple.epsilon/.2.);
		assert (comps.point.z > comps.over_point.z)
	);
]	

let _ = run_test_tt_main tests
