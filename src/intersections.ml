type intersection = {t: float; intersection_object: Sphere.shape}
let intersection t intersection_object = {t; intersection_object}

let hit intersection_list  = 
    let cmp x y = 
        if (Tuple.float_equals x.t y.t) then 0
        else if (x.t < y.t) then (-1)
        else 1 in
    let positive_intersections = (List.filter (fun x->x.t >= 0.) intersection_list) in
    let sorted_intersections = 
        List.sort cmp positive_intersections
    in 
    match sorted_intersections with 
    | [] -> None
    | _ -> Some (List.hd sorted_intersections)

let intersect (sphere:Sphere.shape) (ray:Rays.ray) = 
    let {origin; direction}:Rays.ray = Rays.transform ray (Matrix.inverse sphere.transform) in
    let sphere_to_ray = Tuple.subtract origin (Tuple.point 0. 0. 0.) in 
    let a = Tuple.dot direction direction in 
    let b = 2. *. Tuple.dot direction sphere_to_ray in 
    let c = Tuple.dot sphere_to_ray sphere_to_ray -. 1. in 
    let discriminant = b*.b-.4.*.a*.c in
    if discriminant < 0. then [] 
    else 
        let t1 = (-.b -. sqrt discriminant) /. (2.*.a) in
        let t2 = (-.b +. sqrt discriminant) /. (2.*.a) in
        [{t=t1; intersection_object=sphere}; {t=t2; intersection_object=sphere}]
