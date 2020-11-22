open Transformations
open Matrix
open Tuple

type object_type = Sphere (* add other types later *)
type shape = {shapeType: object_type; mutable transform: matrix}
let sphere = {shapeType = Sphere; transform = identity_matrix}
let set_transform sphere new_transform = 
    sphere.transform <- new_transform
let normal_at sphere world_point =
    let object_point = multiplyMatrixTuple (inverse sphere.transform) world_point in
    let object_normal = subtractTuple object_point (point 0. 0. 0.) in
    let world_normal = multiplyMatrixTuple (transpose (inverse sphere.transform)) object_normal in
    normalize (vector world_normal.x world_normal.y world_normal.z)
