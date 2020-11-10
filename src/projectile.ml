open Tuple
open Canvas

type projectile = {pos: tuple; vel: tuple}
type environment = {grav: tuple; wind: tuple}

let tick {grav; wind} {pos; vel} = 
    let pos' = addTuple pos vel in
    let vel' = addTuple (addTuple vel grav) wind in 
    {pos=pos'; vel=vel'}

let rec sim canvas (env, {pos; vel}) =
    if (pos.y <= 0.) then canvas
    else 
        let x = round (pos.x) in 
        let y = canvas.height - (round (pos.y)) in
        let inBounds = 0 <= x && x < canvas.width && 
                       0 <= y && y < canvas.height in
    begin
        Printf.printf "Projectile position: (%f, %f)\n" pos.x pos.y;
        Printf.printf "Canvas position: (%d, %d)\n" x y;
        if inBounds then write_pixel canvas x y red;
        sim canvas (env, tick env {pos; vel}) (* any equivalent of Haskell @ in OCaml? *)
    end

let env = {grav = (vector 0. (-0.1) 0.); wind = (vector (-0.01) 0. 0.)}
let proj = {pos = point 0. 1. 0.; vel = vector 3. 7. 0.}

let () =
    let blankCanvas = createCanvas 400 300 in
    let canvas = sim blankCanvas (env, proj) in
    let ppm = canvasToPPM canvas in
    let file = open_out "images/projectile.ppm" in
    output_string file ppm;
    ()
