type projectile = {pos: Tuple.tuple; vel: Tuple.tuple}
type environment = {grav: Tuple.tuple; wind: Tuple.tuple}

let tick {grav; wind} {pos; vel} = 
    let pos' = Tuple.add pos vel in
    let vel' = Tuple.add (Tuple.add vel grav) wind in 
    {pos=pos'; vel=vel'}

let rec sim canvas (env, {pos; vel}) =
    if (pos.y <= 0.) then canvas
    else 
        Canvas.(
        let x = Util.round (pos.x) in 
        let y = canvas.height - (Util.round (pos.y)) in
        let inBounds = 0 <= x && x < canvas.width && 
                       0 <= y && y < canvas.height in
    begin
        Printf.printf "Projectile position: (%f, %f)\n" pos.x pos.y;
        Printf.printf "Canvas position: (%d, %d)\n" x y;
        if inBounds then write_pixel canvas x y Color.red;
        sim canvas (env, tick env {pos; vel})
    end
        )

let env = {grav = (Tuple.vector 0. (-0.1) 0.); wind = (Tuple.vector (-0.01) 0. 0.)}
let proj = {pos = Tuple.point 0. 1. 0.; vel = Tuple.vector 3. 7. 0.}

let () =
    let blankCanvas = Canvas.create_canvas 400 300 in
    let canvas = sim blankCanvas (env, proj) in
    let ppm = Canvas.canvas_to_ppm canvas in
    let file = open_out "images/projectile.ppm" in
    output_string file ppm;
    ()
