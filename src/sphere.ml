type object_type = Sphere (* add other types later *)
type shape = {shapeType: object_type; mutable material: Reflection.material; mutable transform: Matrix.matrix}
let shape_equals shape other_shape =
    shape.shapeType = other_shape.shapeType
    && shape.material = other_shape.material
    && shape.transform = other_shape.transform
let sphere = {shapeType = Sphere; material = Reflection.default_material; transform = Matrix.identity_matrix}
let set_transform sphere new_transform = 
    sphere.transform <- new_transform

let set_material sphere new_material = 
    sphere.material <- new_material
    
let normal_at sphere world_point =
    let object_point = Matrix.(multiply_tuple (inverse sphere.transform) world_point) in
    let object_normal = Tuple.(subtract object_point (point 0. 0. 0.)) in
    let world_normal = Matrix.(multiply_tuple (transpose (inverse sphere.transform)) object_normal) in
    Tuple.normalize (Tuple.vector world_normal.x world_normal.y world_normal.z)
