open Shapetype

(* constructor for a shape *)
let shape_create shape_type =
    {shape_type;
     material = Reflection.default_material;
     transform = Matrix.identity_matrix}

(* are two shapes equal? *)
let shape_equals shape other_shape =
    shape.shape_type = other_shape.shape_type
    && shape.material = other_shape.material
    && shape.transform = other_shape.transform

(* shape setters *)
let set_transform shape new_transform = {shape with transform = new_transform}
let set_material shape new_material = {shape with material = new_material}

let intersect (shape:shape) ray =
    let local_ray = Rays.transform ray (Matrix.inverse shape.transform) in
    match shape.shape_type with
    | Sphere -> Sphere.local_intersect shape local_ray
    | Plane -> Plane.local_intersect shape local_ray
    | TestShape -> TestShape.local_intersect shape local_ray

let normal_at (shape:shape) world_point =
    let local_point = Matrix.(multiply_tuple (inverse shape.transform) world_point) in
    let local_normal =
        match shape.shape_type with
        | Sphere -> Sphere.local_normal_at shape local_point
        | Plane -> Plane.local_normal_at shape local_point
        | TestShape -> TestShape.local_normal_at shape local_point
    in 
    let world_normal = Matrix.(multiply_tuple (transpose (inverse shape.transform)) local_normal) in
    Tuple.normalize (Tuple.vector world_normal.x world_normal.y world_normal.z)

let sphere = shape_create Sphere