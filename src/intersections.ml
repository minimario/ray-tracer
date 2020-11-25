type intersection = {t: float; intersectionObject: Sphere.shape}
let intersection t intersectionObject = {t; intersectionObject}

let hit intersectionList  = 
    let cmp x y = 
        if (Tuple.float_equals x.t y.t) then 0
        else if (x.t < y.t) then (-1)
        else 1 in
    let positiveIntersections = (List.filter (fun x->x.t >= 0.) intersectionList) in
    let sortedIntersections = 
        List.sort cmp positiveIntersections
    in 
    match sortedIntersections with 
    | [] -> None
    | _ -> Some (List.hd sortedIntersections)

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
        [{t=t1; intersectionObject=sphere}; {t=t2; intersectionObject=sphere}]

