(* color can be treated as vector *)
type color = {red:float; green:float; blue:float}

let color r g b = {red=r; green=g; blue=b}
let tuple_of_color color = Tuple.vector color.red color.green color.blue
let color_of_tuple tuple = {red=tuple.Tuple.x; green=tuple.Tuple.y; blue=tuple.Tuple.z}

(* equality *)
let equalColor a b = Tuple.equals (tuple_of_color a) (tuple_of_color b)

(* operations *)
let addColor a b = color_of_tuple (Tuple.add (tuple_of_color a) (tuple_of_color b))
let subtractColor a b = color_of_tuple (Tuple.subtract (tuple_of_color a) (tuple_of_color b))
let multiplyColorScalar a k = color_of_tuple (Tuple.multiplyScalar (tuple_of_color a) k)
let hadamard_product a b = {red=a.red*.b.red; green=a.green*.b.green; blue=a.blue*.b.blue}
let multiplyColor = hadamard_product
