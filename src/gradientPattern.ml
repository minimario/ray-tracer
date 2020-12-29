open PatternType
let local_stripe_at a b (point:Tuple.tuple) =
    Color.(
        let distance = subtract b a in
        let fraction = point.x -. Float.floor point.x in
        add a (multiply_scalar distance fraction)
    )