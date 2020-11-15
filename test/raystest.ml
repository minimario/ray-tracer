open OUnit2
open Rays
open Color
open Tuple
open Transformations
open Matrix
open Intersections
open Sphere
let tests = "Test Suite for Rays" >::: [
    "Creating and querying a ray" >::
    (fun _ ->
    let o = point 1. 2. 3. in 
    let d = vector 4. 5. 6. in 
    let r = {origin=o; direction=d} in 
    assert_bool "origin wrong" 
        (equalTuple (origin r) o);
    assert_bool "direction wrong"
        (equalTuple (direction r) d);
    );

    "Computing a point from a distance" >::
    (fun _ ->
    let r = {origin=point 2. 3. 4.; direction=vector 1. 0. 0.} in 
    assert_bool "position 0 wrong" 
        (equalTuple (position r 0.) (point 2. 3. 4.));
    assert_bool "position 1 wrong" 
        (equalTuple (position r 1.) (point 3. 3. 4.));
    assert_bool "position -1 wrong" 
        (equalTuple (position r (-1.)) (point 1. 3. 4.));
    assert_bool "position 2.5 wrong" 
        (equalTuple (position r 2.5) (point 4.5 3. 4.))
    );

    "A ray intersects a sphere at two points" >::
    (fun _ ->
    let r = {origin=point 0. 0. (-5.); direction=vector 0. 0. 1.} in
    let s = sphere in 
    let xs = intersect s r in
    assert_equal (List.length xs) 2;
    assert_bool "xs[0] shape wrong"
        ((List.hd xs).intersectionObject == s);
    assert_bool "xs[1] shape wrong"
        ((List.hd (List.tl xs)).intersectionObject == s);
    assert_bool "xs[0] wrong" 
        (equalFloat (List.hd xs).t 4.);
    assert_bool "xs[1] wrong" 
        (equalFloat (List.hd (List.tl xs)).t 6.)
    );

    "A ray intersects a sphere at a tangent" >::
    (fun _ ->
    let r = {origin=point 0. 1. (-5.); direction=vector 0. 0. 1.} in
    let s = sphere in 
    let xs = intersect s r in
    assert_equal (List.length xs) 2;
    assert_bool "xs[0] shape wrong"
        ((List.hd xs).intersectionObject == s);
    assert_bool "xs[1] shape wrong"
        ((List.hd (List.tl xs)).intersectionObject == s);
    assert_bool "xs[0] wrong" 
        (equalFloat (List.hd xs).t 5.);
    assert_bool "xs[1] wrong" 
        (equalFloat (List.hd (List.tl xs)).t 5.)
    );

    "A ray misses a sphere" >::
    (fun _ ->
    let r = {origin=point 0. 2. (-5.); direction=vector 0. 0. 1.} in
    let s = sphere in 
    let xs = intersect s r in
    assert_equal (List.length xs) 0
    );

    "A ray originates inside a sphere" >::
    (fun _ ->
    let r = {origin=point 0. 0. 0.; direction=vector 0. 0. 1.} in
    let s = sphere in 
    let xs = intersect s r in
    assert_equal (List.length xs) 2;
    assert_bool "xs[0] shape wrong"
        ((List.hd xs).intersectionObject == s);
    assert_bool "xs[1] shape wrong"
        ((List.hd (List.tl xs)).intersectionObject == s);
    assert_bool "xs[0] wrong" 
        (equalFloat (List.hd xs).t (-1.));
    assert_bool "xs[1] wrong" 
        (equalFloat (List.hd (List.tl xs)).t 1.)
    );

    "A sphere is behind a ray" >::
    (fun _ ->
    let r = {origin=point 0. 0. 5.; direction=vector 0. 0. 1.} in
    let s = sphere in 
    let xs = intersect s r in
    assert_equal (List.length xs) 2;
    assert_bool "xs[0] shape wrong"
        ((List.hd xs).intersectionObject == s);
    assert_bool "xs[1] shape wrong"
        ((List.hd (List.tl xs)).intersectionObject == s);
    assert_bool "xs[0] wrong" 
        (equalFloat (List.hd xs).t (-6.));
    assert_bool "xs[1] wrong" 
        (equalFloat (List.hd (List.tl xs)).t (-4.))
    );

    "An intersection encapsulates t and object" >::
    (fun _ ->
    let s = sphere in 
    let i = intersection 3.5 s in
    assert_bool "i.t wrong" 
        (equalFloat i.t (3.5));
    assert_bool "i.intersectionObject wrong"
        (i.intersectionObject == s)
    );

    "Aggregating intersections" >::
    (fun _ ->
    let s = sphere in 
    let i1 = intersection 1. s in 
    let i2 = intersection 2. s in
    let xs = [i1; i2] in 
    assert_equal (List.length xs) 2;
    assert_bool "xs[0].t wrong" 
        (equalFloat (List.hd xs).t (1.));
    assert_bool "xs[1].t wrong" 
        (equalFloat (List.hd (List.tl xs)).t (2.))
    );

    "Hit test 1" >::
    (fun _ ->
    let s = sphere in 
    let i1 = intersection 1. s in 
    let i2 = intersection 2. s in 
    let xs = [i2; i1] in 
    let i = hit xs in 
    match i with 
    | Some i' -> assert_bool "hit wrong" (i' == i1)
    | _ -> assert_bool "hit wrong" false
    );

    "Hit test 2" >::
    (fun _ ->
    let s = sphere in 
    let i1 = intersection (-1.) s in 
    let i2 = intersection 1. s in 
    let xs = [i2; i1] in 
    let i = hit xs in 
    match i with 
    | Some i' -> assert_bool "hit wrong" (i' == i2)
    | _ -> assert_bool "hit wrong" false
    );

    "Hit test 3" >::
    (fun _ ->
    let s = sphere in 
    let i1 = intersection (-2.) s in 
    let i2 = intersection (-1.) s in 
    let xs = [i2; i1] in 
    let i = hit xs in 
    assert_bool "hit wrong" 
        (i == None)
    );

    "Hit test 4" >::
    (fun _ ->
    let s = sphere in 
    let i1 = intersection 5. s in 
    let i2 = intersection 7. s in 
    let i3 = intersection (-3.) s in 
    let i4 = intersection 2. s in
    let xs = [i1; i2; i3; i4] in 
    let i = hit xs in 
    let i = hit xs in 
    match i with 
    | Some i' -> assert_bool "hit wrong" (i' == i4)
    | _ -> assert_bool "hit wrong" false
    );

    "Translating a ray" >::
    (fun _ ->
    let r = {origin=point 1. 2. 3.; direction=vector 0. 1. 0.} in 
    let m = translation 3. 4. 5. in 
    let r2 = transform r m in 
    assert_bool "origin incorrect" 
        (equalTuple (r2.origin) (point 4. 6. 8.));
    assert_bool "direction incorrect"
        (equalTuple (r2.direction) (vector 0. 1. 0.))
    );

    "Scaling a ray" >::
    (fun _ ->
    let r = {origin=point 1. 2. 3.; direction=vector 0. 1. 0.} in 
    let m = scaling 2. 3. 4. in 
    let r2 = transform r m in 
    assert_bool "origin incorrect" 
        (equalTuple (r2.origin) (point 2. 6. 12.));
    assert_bool "direction incorrect"
        (equalTuple (r2.direction) (vector 0. 3. 0.))
    );

    "A sphere's default transform" >::
    (fun _ ->
    let s = sphere in 
    assert_equal (s.transform) (identity_matrix)
    );

    "Changing a sphere's transformation" >::
    (fun _ ->
    let s = sphere in 
    let t = translation 2. 3. 4. in
    let _ = set_transform s t in 
    assert_equal (s.transform) (t)
    );

    "Intersecting a scaled sphere with a ray" >::
    (fun _ ->
    let r = {origin=point 0. 0. (-5.); direction=vector 0. 0. 1.} in 
    let s = sphere in 
    let _ = set_transform s (scaling 2. 2. 2.) in 
    let xs = intersect s r in 
    assert_equal (List.length xs) 2;
    assert_equal (List.hd xs).t 3.;
    assert_equal (List.hd (List.tl xs)).t 7.;
    assert ((List.hd (List.tl xs)).intersectionObject == s)
    );

    "Intersecting a translated sphere with a ray" >::
    (fun _ ->
    let r = {origin=point 0. 0. (-5.); direction=vector 0. 0. 1.} in 
    let s = sphere in 
    let _ = set_transform s (translation 5. 0. 0.) in 
    let xs = intersect s r in 
    assert_equal (List.length xs) 0;
    );
]

let _ = run_test_tt_main tests
