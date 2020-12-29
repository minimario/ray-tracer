open PatternType

exception BadPattern of string
let local_stripe_at a b (point:Tuple.tuple) =
    Util.(
        let sum_dims = floor point.x + floor point.y + floor point.z in
        if mod_positive sum_dims 2 = 0 then a else b
    )