open ShapeType
let local_normal_at plane world_point =
    Tuple.vector 0. 1. 0.

let local_intersect plane (ray:Rays.ray) = 
    if (Float.abs (ray.direction.y) < Util.epsilon) then []
    else
        let t = -.ray.origin.y /. ray.direction.y in
        [Intersections.intersection t plane]