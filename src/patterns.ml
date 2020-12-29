open PatternType


let pattern pattern_type =
    {pattern_type;
     transform = Matrix.identity_matrix}

let set_transform pattern new_transform = {pattern with transform = new_transform}

let stripe_at pattern (point:Tuple.tuple) =
    match pattern.pattern_type with
    | StripePattern (a, b) -> StripePattern.local_stripe_at a b point
    | GradientPattern (a, b) -> GradientPattern.local_stripe_at a b point
    | RingPattern (a, b) -> RingPattern.local_stripe_at a b point
    | CheckersPattern (a, b) -> CheckersPattern.local_stripe_at a b point
    | TestPattern -> Color.color point.x point.y point.z

let stripe_at_object pattern (obj:ShapeType.shape) world_point =
    let object_point = Matrix.multiply_tuple (Matrix.inverse obj.transform) world_point in
    let pattern_point = Matrix.multiply_tuple (Matrix.inverse pattern.transform) object_point in
    stripe_at pattern pattern_point