open OUnit2

let tests = "Test Suite for Patterns" >::: [
    "Creating a stripe pattern" >::
    (fun _ ->
        let pattern = Patterns.pattern (StripePattern (Color.white, Color.black)) in
        match pattern.pattern_type with
        | StripePattern (a, b) -> assert (a = Color.white && b = Color.black)
        | _ -> assert false
    );

    "Stripe pattern colors" >::
    (fun _ ->
        let pattern = Patterns.pattern (StripePattern (Color.white, Color.black)) in
        match pattern.pattern_type with 
        | StripePattern (a, b) ->
            assert (StripePattern.local_stripe_at a b (Tuple.point 0. 0. 0.) = Color.white);
            assert (StripePattern.local_stripe_at a b (Tuple.point 0. 1. 0.) = Color.white);
            assert (StripePattern.local_stripe_at a b (Tuple.point 0. 2. 0.) = Color.white);
            assert (StripePattern.local_stripe_at a b (Tuple.point 0. 0. 1.) = Color.white);
            assert (StripePattern.local_stripe_at a b (Tuple.point 0. 0. 2.) = Color.white);
            assert (StripePattern.local_stripe_at a b (Tuple.point 0.9 0. 0.) = Color.white);
            assert (StripePattern.local_stripe_at a b (Tuple.point 1. 0. 0.) = Color.black);
            assert (StripePattern.local_stripe_at a b (Tuple.point (-0.1) 0. 0.) = Color.black);
            assert (StripePattern.local_stripe_at a b (Tuple.point (-1.) 0. 0.) = Color.black);
            assert (StripePattern.local_stripe_at a b (Tuple.point (-1.1) 0. 0.) = Color.white)
        | _ -> assert false
    );

    "Lighting with a pattern applied" >::
    (fun _ ->
        let p = Patterns.pattern (StripePattern (Color.white, Color.black)) in
        let m = {Reflection.default_material with pattern=Some p; ambient = 1.; diffuse = 0.; specular = 0.} in
        let eyev = Tuple.vector 0. 0. (-1.) in
        let normalv = Tuple.vector 0. 0. (-1.) in
        let light = Reflection.point_light (Tuple.point 0. 0. (-10.)) Color.white in
        let s = Shape.sphere in
        let c1 = Reflection.lighting m s light (Tuple.point 0.9 0. 0.) eyev normalv false in
        let c2 = Reflection.lighting m s light (Tuple.point 1.1 0. 0.) eyev normalv false in
        assert (Color.equals c1 Color.white);
        assert (Color.equals c2 Color.black)
    );

    "Stripes with an object transformation" >::
    (fun _ ->
        let s = Shape.sphere in
        let s' = Shape.set_transform s (Transformations.scaling 2. 2. 2.) in
        let pattern = Patterns.pattern (StripePattern (Color.white, Color.black)) in
        let c = Patterns.stripe_at_object pattern s' (Tuple.point 1.5 0. 0.) in
        assert (Color.equals c Color.white)
    );

    "Stripes with an pattern transformation" >::
    (fun _ ->
        let s = Shape.sphere in
        let pattern = Patterns.pattern (StripePattern (Color.white, Color.black)) in
        let pattern' = Patterns.set_transform pattern (Transformations.scaling 2. 2. 2.) in
        let c = Patterns.stripe_at_object pattern' s (Tuple.point 1.5 0. 0.) in
        assert (Color.equals c Color.white)
    );

    "Stripes with both an object and a pattern transformation" >::
    (fun _ ->
        let s = Shape.sphere in
        let s' = Shape.set_transform s (Transformations.scaling 2. 2. 2.) in
        let pattern = Patterns.pattern (StripePattern (Color.white, Color.black)) in
        let pattern' = Patterns.set_transform pattern (Transformations.translation 0.5 0. 0.) in
        let c = Patterns.stripe_at_object pattern' s' (Tuple.point 2.5 0. 0.) in
        assert (Color.equals c Color.white)
    );

    "The default pattern transformation" >::
    (fun _ ->
        let pattern = Patterns.pattern TestPattern in
        assert (pattern.transform = Matrix.identity_matrix)
    );

    "Assigning a transformation" >::
    (fun _ ->
        let pattern = Patterns.pattern TestPattern in
        let pattern' = Patterns.set_transform pattern (Transformations.translation 1. 2. 3.) in
        assert (pattern'.transform = Transformations.translation 1. 2. 3.)
    );

    "A pattern with an object transformation" >::
    (fun _ ->
        let s = Shape.sphere in
        let s' = Shape.set_transform s (Transformations.scaling 2. 2. 2.) in
        let pattern = Patterns.pattern TestPattern in
        let c = Patterns.stripe_at_object pattern s' (Tuple.point 2. 3. 4.) in
        assert (Color.equals c (Color.color 1. 1.5 2.))
    );

    "Stripes with an pattern transformation" >::
    (fun _ ->
        let s = Shape.sphere in
        let pattern = Patterns.pattern TestPattern in
        let pattern' = Patterns.set_transform pattern (Transformations.scaling 2. 2. 2.) in
        let c = Patterns.stripe_at_object pattern' s (Tuple.point 2. 3. 4.) in
        assert (Color.equals c (Color.color 1. 1.5 2.))
    );

    "Stripes with both an object and a pattern transformation" >::
    (fun _ ->
        let s = Shape.sphere in
        let s' = Shape.set_transform s (Transformations.scaling 2. 2. 2.) in
        let pattern = Patterns.pattern TestPattern in
        let pattern' = Patterns.set_transform pattern (Transformations.translation 0.5 1. 1.5) in
        let c = Patterns.stripe_at_object pattern' s' (Tuple.point 2.5 3. 3.5) in
        assert (Color.equals c (Color.color 0.75 0.5 0.25))
    );

    "Gradient pattern colors" >::
    (fun _ ->
        let pattern = Patterns.pattern (GradientPattern (Color.white, Color.black)) in
        match pattern.pattern_type with 
        | GradientPattern (a, b) ->
            assert (GradientPattern.local_stripe_at a b (Tuple.point 0. 0. 0.) = Color.white);
            assert (GradientPattern.local_stripe_at a b (Tuple.point 0.25 0. 0.) = Color.color 0.75 0.75 0.75);
            assert (GradientPattern.local_stripe_at a b (Tuple.point 0.5 0. 0.) = Color.color 0.5 0.5 0.5);
            assert (GradientPattern.local_stripe_at a b (Tuple.point 0.75 0. 0.) = Color.color 0.25 0.25 0.25)
        | _ -> assert false
    );

    "Ring pattern colors" >::
    (fun _ ->
        let pattern = Patterns.pattern (RingPattern (Color.white, Color.black)) in
        match pattern.pattern_type with 
        | RingPattern (a, b) ->
            assert (RingPattern.local_stripe_at a b (Tuple.point 0. 0. 0.) = Color.white);
            assert (RingPattern.local_stripe_at a b (Tuple.point 1. 0. 0.) = Color.black);
            assert (RingPattern.local_stripe_at a b (Tuple.point 0. 0. 1.) = Color.black);
            assert (RingPattern.local_stripe_at a b (Tuple.point 0.708 0. 0.708) = Color.black)
        | _ -> assert false
    );

    "Checkers pattern colors" >::
    (fun _ ->
        let pattern = Patterns.pattern (CheckersPattern (Color.white, Color.black)) in
        match pattern.pattern_type with 
        | CheckersPattern (a, b) ->
            assert (CheckersPattern.local_stripe_at a b (Tuple.point 0. 0. 0.) = Color.white);
            assert (CheckersPattern.local_stripe_at a b (Tuple.point 0.99 0. 0.) = Color.white);
            assert (CheckersPattern.local_stripe_at a b (Tuple.point 1.01 0. 0.) = Color.black);
            assert (CheckersPattern.local_stripe_at a b (Tuple.point 0. 0.99 0.) = Color.white);
            assert (CheckersPattern.local_stripe_at a b (Tuple.point 0. 1.01 0.) = Color.black);
            assert (CheckersPattern.local_stripe_at a b (Tuple.point 0. 0. 0.99) = Color.white);
            assert (CheckersPattern.local_stripe_at a b (Tuple.point 0. 0. 1.01) = Color.black);
        | _ -> assert false
    );
]

let _ = run_test_tt_main tests
