type tuple = {x:float; y:float; z:float; w:float}

(* basic constructors *)
let tuple x y z w = {x; y; z; w}
let point x y z = {x=x; y=y; z=z; w=1.0}
let vector x y z = {x=x; y=y; z=z; w=0.0}

(* checking type of a tuple *)
let is_vector tuple = (tuple.w = 0.0)
let is_point tuple = (tuple.w = 1.0)

(* checking for equality *)
let float_equals a b = ((Float.abs (a-.b)) < Util.epsilon)
let equals a b =
    float_equals a.x b.x && float_equals a.y b.y &&
    float_equals a.z b.z && float_equals a.w b.w

(* basic arithmetic *)
let add a b = {x=a.x+.b.x; y=a.y+.b.y; z=a.z+.b.z; w=a.w+.b.w}
let subtract a b = {x=a.x-.b.x; y=a.y-.b.y; z=a.z-.b.z; w=a.w-.b.w}
let negate a = {x=(-.a.x); y=(-.a.y); z=(-.a.z); w=(-.a.w)}
let multiply_scalar a k = {x=a.x*.k; y=a.y*.k; z=a.z*.k; w=a.w*.k}
let divide_scalar a k = {x=a.x/.k; y=a.y/.k; z=a.z/.k; w=a.w/.k}

(* magnitude and normalization *)
let magnitude v = sqrt (v.x*.v.x +. v.y*.v.y +. v.z*.v.z)
let normalize v = divide_scalar v (magnitude v)

(* dot and cross products *)
let dot v1 v2 = v1.x*.v2.x+.v1.y*.v2.y+.v1.z*.v2.z+.v1.w*.v2.w
let cross a b = vector (a.y*.b.z-.a.z*.b.y) (a.z*.b.x-.a.x*.b.z) (a.x*.b.y-.a.y*.b.x)
