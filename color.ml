open Tuple

(* color can be treated as vector *)
type color = {red:float; green:float; blue:float}

let color r g b = {red=r; green=g; blue=b}
let tuple_of_color color = vector color.red color.green color.blue
let color_of_tuple tuple = {red=tuple.x; green=tuple.y; blue=tuple.z}

(* equality *)
let equalColor a b = equalTuple (tuple_of_color a) (tuple_of_color b)

(* operations *)
let addColor a b = color_of_tuple (addTuple (tuple_of_color a) (tuple_of_color b))
let subtractColor a b = color_of_tuple (subtractTuple (tuple_of_color a) (tuple_of_color b))
let hadamard_product a b = {red=a.red*.b.red; green=a.green*.b.green; blue=a.blue*.b.blue}
let multiplyColor = hadamard_product
