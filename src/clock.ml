open Transformations
open Tuple
open Matrix
open Canvas

(* takes integer 0 to 11 and returns the point *)
let pos i = 
    multiplyMatrixTuple (rotation_z ((float_of_int i) *. Float.pi/.6.)) (point 1. 0. 0.)

let rec draw canvas time =
    if (time = 12) then canvas
    else 
        let {x; y; _} = pos time in 
        let radius = 50. in
        let canvas_x = canvas.width / 2 + round (radius *. x) in
        let canvas_y = canvas.height / 2 + round (radius *. y) in
        let inBounds = 0 <= canvas_x && canvas_x < canvas.width && 
                       0 <= canvas_y && canvas_y < canvas.height in
    begin
        (* Printf.printf "Clock position: (%f, %f)\n" x y; *)
        (* Printf.printf "Canvas position: (%d, %d)\n" canvas_x canvas_y; *)
        if inBounds then write_pixel canvas canvas_x canvas_y red 
        else raise (Failure "writing out of bounds");
        draw canvas (time+1)
    end

let () =
    let blankCanvas = createCanvas 120 120 in
    let canvas = draw blankCanvas 0 in
    let ppm = canvasToPPM canvas in
    let file = open_out "images/clock.ppm" in
    Printf.printf "Clock drawn!\n";
    output_string file ppm;


