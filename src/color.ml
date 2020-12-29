(* color can be treated as vector *)
type color = {red:float; green:float; blue:float}

let color r g b = {red=r; green=g; blue=b}
let white = color 1. 1. 1.
let black = color 0. 0. 0.
let red = color 1. 0. 0.

let tuple_of_color color = Tuple.vector color.red color.green color.blue
let color_of_tuple (tuple:Tuple.tuple) = {red=tuple.x; green=tuple.y; blue=tuple.z}

(* equality *)
let equals a b = Tuple.equals (tuple_of_color a) (tuple_of_color b)

(* operations *)
let add a b = color_of_tuple (Tuple.add (tuple_of_color a) (tuple_of_color b))
let subtract a b = color_of_tuple (Tuple.subtract (tuple_of_color a) (tuple_of_color b))
let multiply_scalar a k = color_of_tuple (Tuple.multiply_scalar (tuple_of_color a) k)
let hadamard_product a b = {red=a.red*.b.red; green=a.green*.b.green; blue=a.blue*.b.blue}
let multiply = hadamard_product

let round f = int_of_float (floor (f +. 0.5))
let int_of_color c = round (min 255. (max 0. (255.*.c)))
let to_string {red; green; blue} = String.concat " " 
                                        (List.map (fun color -> string_of_int (int_of_color color))
                                            [red; green; blue])
