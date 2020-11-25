type ray = {origin: Tuple.tuple; direction: Tuple.tuple}

let origin {origin; _} = origin
let direction {direction; _} = direction

let position {origin; direction} t = 
    Tuple.add origin (Tuple.multiply_scalar direction t)
let transform {origin; direction} matrix = 
    {origin = Matrix.multiply_tuple matrix origin;
     direction = Matrix.multiply_tuple matrix direction}
