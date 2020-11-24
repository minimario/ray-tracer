open Matrix

type ray = {origin: Tuple.tuple; direction: Tuple.tuple}

let origin {origin; _} = origin
let direction {direction; _} = direction

let position {origin; direction} t = 
    Tuple.add origin (Tuple.multiplyScalar direction t)
let transform {origin; direction} matrix = 
    {origin = multiplyMatrixTuple matrix origin;
     direction = multiplyMatrixTuple matrix direction}
