open OUnit2
open Rays
open Color
open Tuple
open Transformations
open Matrix
open Intersections
open Shape

let equalTuple = Tuple.equals
let tests = "Test Suite for Spheres" >::: [
    "Computing a normal on a sphere at a point on the x axis" >::
    (fun _ ->
    let s = sphere in
    let n = normal_at s (point 1. 0. 0.) in 
    assert_bool "normal wrong" 
        (equalTuple n (vector 1. 0. 0.));
    );
    "Computing a normal on a sphere at a point on the y axis" >::
    (fun _ ->
    let s = sphere in
    let n = normal_at s (point 0. 1. 0.) in
    assert_bool "normal wrong" 
        (equalTuple n (vector 0. 1. 0.));
    );
    "Computing a normal on a sphere at a point on the z axis" >::
    (fun _ ->
    let s = sphere in
    let n = normal_at s (point 0. 0. 1.) in 
    assert_bool "normal wrong" 
        (equalTuple n (vector 0. 0. 1.));
    );
    "Computing a normal on a sphere at a nonaxial point" >::
    (fun _ ->
    let sqrt_third = (Float.sqrt 3.) /. 3. in
    let s = sphere in
    let n = normal_at s (point sqrt_third sqrt_third sqrt_third) in 
    assert_bool "normal wrong" 
        (equalTuple n (vector sqrt_third sqrt_third sqrt_third));
    );
    "Computing a normal on a translated sphere" >::
    (fun _ ->
    let s = sphere in
    let s' = set_transform s (translation 0. 1. 0.) in
    let n = normal_at s' (point 0. 1.70711 (-0.70711)) in 
    assert_bool "normal wrong"
        (equalTuple n (vector 0. 0.70711 (-0.70711)));
    );
    "Computing a normal on a transformed sphere" >::
    (fun _ ->
    let s = sphere in
    let m = Matrix.multiply (scaling 1. 0.5 1.) (rotation_z (Float.pi /. 5.)) in
    let s' = set_transform s m in
    let n = normal_at s' (point 0. (Float.sqrt 2. /. 2.) ~-.(Float.sqrt 2. /. 2.)) in 
    assert_bool "normal wrong"
        (equalTuple n (vector 0. 0.97014 (-0.24254)));
    );
]

let _ = run_test_tt_main tests
