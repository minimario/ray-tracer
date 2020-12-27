open Shapetype
let local_normal_at (sphere:shape) local_point =
    Tuple.(subtract local_point (point 0. 0. 0.))

let local_intersect (sphere:shape) {Rays.origin; direction} =
    let sphere_to_ray = Tuple.subtract origin (Tuple.point 0. 0. 0.) in 
    let a = Tuple.dot direction direction in 
    let b = 2. *. Tuple.dot direction sphere_to_ray in 
    let c = Tuple.dot sphere_to_ray sphere_to_ray -. 1. in 
    let discriminant = b*.b-.4.*.a*.c in
    if discriminant < 0. then [] 
    else 
        let t1 = (-.b -. sqrt discriminant) /. (2.*.a) in
        let t2 = (-.b +. sqrt discriminant) /. (2.*.a) in
        [{Intersections.t=t1; intersection_object=sphere}; {t=t2; intersection_object=sphere}]