open Tuple

type projectile = {pos: tuple; vel: tuple}
type environment = {grav: tuple; wind: tuple}

let tick {grav; wind} {pos; vel} = 
    let pos' = addTuple pos vel in
    let vel' = addTuple (addTuple vel grav) wind in 
    {pos=pos'; vel=vel'}

let rec sim (env, {pos; vel}) =
    if (pos.y <= 0.) then ()
    else let _ = Printf.printf "Projectile position: (%f, %f)\n" pos.x pos.y in
         sim (env, tick env {pos; vel}) (* any equivalent of Haskell @ in OCaml? *)

let env = {grav = (vector 0. (-0.1) 0.); wind = (vector (-0.01) 0. 0.)}
let proj = {pos = point 0. 1. 0.; vel = normalize (vector 1. 1. 0.)}

let () =
    sim (env, proj)
