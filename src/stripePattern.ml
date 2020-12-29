open PatternType

exception BadPattern of string
let local_stripe_at a b (point:Tuple.tuple) =
    if Util.mod_positive (Util.floor point.x) 2 == 0 then a else b