open Transformations
open Matrix
type object_type = Sphere (* add other types later *)
type shape = {shapeType: object_type; mutable transform: matrix}
let sphere = {shapeType = Sphere; transform = identity_matrix}
let set_transform sphere new_transform = 
    sphere.transform <- new_transform