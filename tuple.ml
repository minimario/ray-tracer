type tuple = {x:float; y:float; z:float; w:float}

(* basic constructors *)
let tuple x y z w = {x; y; z; w}
let point x y z = {x=x; y=y; z=z; w=1.0}
let vector x y z = {x=x; y=y; z=z; w=0.0}

(* checking type of a tuple *)
let isVector tuple = (tuple.w = 0.0)
let isPoint tuple = (tuple.w = 1.0)

(* checking for equality *)
let epsilon = 0.000001
let equalFloat a b = ((Float.abs (a-.b)) < epsilon)
let equalTuple a b =
    equalFloat a.x b.x && equalFloat a.y b.y &&
    equalFloat a.z b.z && equalFloat a.w b.w

(* basic arithmetic *)
let addTuple a b = {x=a.x+.b.x; y=a.y+.b.y; z=a.z+.b.z; w=a.w+.b.w}
let subtractTuple a b = {x=a.x-.b.x; y=a.y-.b.y; z=a.z-.b.z; w=a.w-.b.w}
let negateTuple a = {x=(-.a.x); y=(-.a.y); z=(-.a.z); w=(-.a.w)}
let multiplyTupleScalar a k = {x=a.x*.k; y=a.y*.k; z=a.z*.k; w=a.w*.k}
let divideTupleScalar a k = {x=a.x/.k; y=a.y/.k; z=a.z/.k; w=a.w/.k}

(* magnitude and normalization *)
let magnitude v = sqrt (v.x*.v.x +. v.y*.v.y +. v.z*.v.z)
let normalize v = divideTupleScalar v (magnitude v)

(* dot and cross products *)
let dot v1 v2 = v1.x*.v2.x+.v1.y*.v2.y+.v1.z*.v2.z+.v1.w*.v2.w
let cross a b = vector (a.y*.b.z-.a.z*.b.y) (a.z*.b.x-.a.x*.b.z) (a.x*.b.y-.a.y*.b.x)

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
let multiplyColorScalar a k = color_of_tuple (multiplyTupleScalar (tuple_of_color a) k)
let hadamard_product a b = {red=a.red*.b.red; green=a.green*.b.green; blue=a.blue*.b.blue}
let multiplyColor = hadamard_product
