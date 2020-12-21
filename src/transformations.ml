let translation x y z = [| 
    [| 1.; 0.; 0.; x  |];
    [| 0.; 1.; 0.; y  |];
    [| 0.; 0.; 1.; z  |];
    [| 0.; 0.; 0.; 1. |] 
    |]

let scaling x y z = [|
    [| x;  0.; 0.; 0. |];
    [| 0.; y;  0.; 0. |];
    [| 0.; 0.; z;  0. |];
    [| 0.; 0.; 0.; 1. |] 
    |]

let rotation_x theta = [| 
    [| 1.; 0.; 0.; 0. |];
    [| 0.; cos(theta); -.sin(theta); 0. |];
    [| 0.; sin(theta); cos(theta); 0. |];
    [| 0.; 0.; 0.; 1. |] 
    |]
                
let rotation_y theta = [| 
    [| cos(theta); 0.; sin(theta); 0. |];
    [| 0.; 1.; 0.; 0. |];
    [| -.sin(theta); 0.; cos(theta); 0. |];
    [| 0.; 0.; 0.; 1. |] 
    |]

let rotation_z theta = [|
    [| cos(theta); -.sin(theta); 0.; 0. |];
    [| sin(theta); cos(theta); 0.; 0. |];
    [| 0.; 0.; 1.; 0. |];
    [| 0.; 0.; 0.; 1. |]
    |]

let shearing xy xz yx yz zx zy = [|
    [| 1.; xy; xz; 0. |];
    [| yx; 1.; yz; 0. |];
    [| zx; zy; 1.; 0. |];
    [| 0.; 0.; 0.; 1. |]
|]

let view_transform from_pt to_pt up_vec =
    Tuple.(
    let neg_from = negate from_pt in
    let forward = normalize (subtract to_pt from_pt) in 
    let neg_forward = negate forward in
    let upn = normalize up_vec in 
    let left = cross forward upn in
    let true_up = cross left forward in
    let orientation = [|
        [| left.x; left.y; left.z; 0. |];
        [| true_up.x; true_up.y; true_up.z; 0. |];
        [| neg_forward.x; neg_forward.y; neg_forward.z; 0. |];
        [| 0.; 0.; 0.; 1. |]
    |] in
    Matrix.multiply orientation (translation neg_from.x neg_from.y neg_from.z)
    )