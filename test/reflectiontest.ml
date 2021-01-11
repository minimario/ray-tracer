open Reflection
open Tuple
open OUnit2
open Color
open Sphere
open Shape

let equalTuple = Tuple.equals
let tests = "Test Suite for Reflections" >::: [
    "Reflecting a vector approaching at 45 degrees" >::
    (fun _ ->
    let v = vector 1. (-1.) 0. in 
    let n = vector 0. 1. 0. in
    let r = reflect v n in
    let r' = vector 1. 1. 0. in
    assert_bool "reflection wrong" 
        (equalTuple r r')
    );

    "Reflecting a vector off a slanted surface" >::
    (fun _ ->
    let v = vector 0. (-1.) 0. in 
    let n = vector ((sqrt 2.)/.2.) ((sqrt 2.)/.2.) 0. in
    let r = reflect v n in
    let r' = vector 1. 0. 0. in
    assert_bool "reflection wrong" 
        (equalTuple r r')
    );

    "A point light has a position and intensity" >::
    (fun _ ->
    let intensity = color 1. 1. 1. in
    let position = point 0. 0. 0. in
    let light = point_light position intensity in
    assert (light.position = position);
    assert (light.intensity = intensity)
    );

    "The default material" >::
    (fun _ ->
    let m = default_material in
    assert (m.color = color 1. 1. 1.);
    assert (m.ambient = 0.1);
    assert (m.diffuse = 0.9);
    assert (m.specular = 0.9); 
    assert (m.shininess = 200.0);
    assert (m.reflective = 0.0)
    );

    "A sphere has a default material" >::
    (fun _ ->
    let s = sphere in 
    let m = s.material in
    assert (m = default_material)
    );

    "A sphere may be assigned a material" >::
    (fun _ ->
    let s = sphere in 
    let m = {default_material with ambient = 1.} in
    let s' = set_material s m in
    assert (s'.material = m)
    );

    "Lighting with the eye between the light and the surface" >::
    (fun _ ->
    let m = default_material in
    let position = point 0. 0. 0. in
    let eyev = vector 0. 0. (-1.) in
    let normalv = vector 0. 0. (-1.) in
    let light = point_light (point 0. 0. (-10.)) (color 1. 1. 1.) in
    let in_shadow = false in 
    let sphere = sphere in 
    let result = lighting m sphere light position eyev normalv in_shadow in
    assert (Color.equals result (color 1.9 1.9 1.9))
    );

    "Lighting with the eye between the light and the surface, eye offset 45deg" >::
    (fun _ ->
    let m = default_material in
    let position = point 0. 0. 0. in
    let eyev = vector 0. (sqrt 2./.2.) (-.sqrt 2./.2.) in
    let normalv = vector 0. 0. (-1.) in
    let light = point_light (point 0. 0. (-10.)) (color 1. 1. 1.) in
    let in_shadow = false in 
    let sphere = sphere in 
    let result = lighting m sphere light position eyev normalv in_shadow in
    assert (Color.equals result (color 1. 1. 1.))
    );

    "Lighting with eye opposite surface, light offset 45deg" >::
    (fun _ ->
    let m = default_material in
    let position = point 0. 0. 0. in
    let eyev = vector 0. 0. (-1.) in
    let normalv = vector 0. 0. (-1.) in
    let light = point_light (point 0. 10. (-10.)) (color 1. 1. 1.) in
    let in_shadow = false in 
    let sphere = sphere in 
    let result = lighting m sphere light position eyev normalv in_shadow in
    assert (Color.equals result (color 0.7364 0.7364 0.7364))
    );

    "Lighting with eye in the path of the reflection vector" >::
    (fun _ ->
    let m = default_material in
    let position = point 0. 0. 0. in
    let eyev = vector 0. (-.sqrt 2./.2.) (-.sqrt 2./.2.) in
    let normalv = vector 0. 0. (-1.) in
    let light = point_light (point 0. 10. (-10.)) (color 1. 1. 1.) in
    let in_shadow = false in 
    let sphere = sphere in 
    let result = lighting m sphere light position eyev normalv in_shadow in
    assert (Color.equals result (color 1.6364 1.6364 1.6364))
    );

    "Lighting with the light behind the surface" >::
    (fun _ ->
    let m = default_material in
    let position = point 0. 0. 0. in
    let eyev = vector 0. 0. (-1.) in
    let normalv = vector 0. 0. (-1.) in
    let light = point_light (point 0. 0. 10.) (color 1. 1. 1.) in
    let in_shadow = false in 
    let sphere = sphere in 
    let result = lighting m sphere light position eyev normalv in_shadow in
    assert (Color.equals result (color 0.1 0.1 0.1))
    );

    "Lighting with the surface in shadow" >::
    (fun _ ->
    let m = default_material in
    let position = point 0. 0. 0. in
    let eyev = vector 0. 0. (-1.) in
    let normalv = vector 0. 0. (-1.) in
    let light = point_light (point 0. 0. (-10.)) (color 1. 1. 1.) in
    let in_shadow = true in
    let sphere = sphere in 
    let result = lighting m sphere light position eyev normalv in_shadow in
    assert (Color.equals result (color 0.1 0.1 0.1))
    );
]

let _ = run_test_tt_main tests
