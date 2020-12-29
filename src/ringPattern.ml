open PatternType
let local_stripe_at a b (point:Tuple.tuple) =
    Util.(
        let r = Util.floor (sqrt (point.x *. point.x +. point.z *. point.z)) in
        if mod_positive r 2 = 0 then a else b
    )