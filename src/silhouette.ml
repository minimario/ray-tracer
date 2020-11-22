open Tuple
open Canvas
open Color
open Intersections
open Matrix
open Rays
open Sphere
open Transformations

let ray_origin = point 0. 0. (-5.)
let wall_z = 10.
let wall_size = 7.
let canvas_pixels = 100
let pixel_size = wall_size /. float_of_int canvas_pixels
let half = wall_size /. 2.

let my_canvas = createCanvas canvas_pixels canvas_pixels
let my_color = color 1. 0. 0.
let my_shape = sphere

(* shrink it along the y axis *)
(* let () = set_transform my_shape (scaling 1. 0.5 1.) *)
(* shrink it along the x axis *)
(* let () = set_transform my_shape (scaling 0.5 1. 1.) *)
(* shrink it, and rotate it! *)
(* let () = set_transform my_shape (multiplyMatrix (rotation_z (Float.pi /. 4.)) (scaling 0.5 1. 1.)) *)
(* shrink it, and skew it! *)
let () = set_transform my_shape (multiplyMatrix (shearing 1. 0. 0. 0. 0. 0.) (scaling 0.5 1. 1.))

let rec project width height =  (* still don't know how to write functions *)
    if height <= 0 then my_canvas
    else begin
	let world_y = half -. pixel_size *. float_of_int (height-1) in
	let rec write_row i = if i >= 0 then begin
	    let world_x = (-.half) +. pixel_size *. float_of_int i in
	    let position = point world_x world_y wall_z in
	    let r = {origin=ray_origin; direction=normalize (subtractTuple position ray_origin)} in
	    let xs = intersect my_shape r in
	    if (hit xs != None) then write_pixel my_canvas (height-1) i my_color;
	    write_row (i-1)
	end in
	write_row (width-1);
        project width (height-1)
    end

let my_canvas = project canvas_pixels canvas_pixels
let ppm = canvasToPPM my_canvas
let () = output_string (open_out "images/silhouette.ppm") ppm;
