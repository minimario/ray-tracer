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
