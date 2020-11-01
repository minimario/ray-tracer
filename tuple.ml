type tuple = {x:float; y:float; z:float; w:float}

let tuple x y z w = {x; y; z; w}
let point x y z = {x=x; y=y; z=z; w=1.0}
let vector x y z = {x=x; y=y; z=z; w=0.0}

let isVector tuple = (tuple.w = 0.0)
let isPoint tuple = (tuple.w = 1.0)
