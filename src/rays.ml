open Tuple
open Matrix

type ray = {origin: tuple; direction: tuple}

let origin {origin; _} = origin
let direction {direction; _} = direction

let position {origin; direction} t = 
    addTuple origin (multiplyTupleScalar direction t)
let transform {origin; direction} matrix = 
    {origin = multiplyMatrixTuple matrix origin;
     direction = multiplyMatrixTuple matrix direction}
