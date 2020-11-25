let ray_origin = Tuple.point 0. 0. (-5.)
let wall_z = 10.
let wall_size = 7.
let canvas_pixels = 100
let pixel_size = wall_size /. float_of_int canvas_pixels
let half = wall_size /. 2.

let my_canvas = Canvas.create_canvas canvas_pixels canvas_pixels
let my_color = Color.color 1. 0. 0.
let sphere_material = {Reflection.default_material with color = Color.color 1. 0.2 1.}
let my_shape = {Sphere.sphere with material = sphere_material}
let light = 
    let light_position = Tuple.point (-10.) 10. (-10.) in
    let light_color = Color.color 1. 1. 1. in
    Reflection.point_light light_position light_color
    

(* shrink it along the y axis *)
(* let () = set_transform my_shape (scaling 1. 0.5 1.) *)
(* shrink it along the x axis *)
(* let () = set_transform my_shape (scaling 0.5 1. 1.) *)
(* shrink it, and rotate it! *)
(* let () = set_transform my_shape (Matrix.multiply (rotation_z (Float.pi /. 4.)) (scaling 0.5 1. 1.)) *)
(* shrink it, and skew it! *)
(* let () = set_transform my_shape (Matrix.multiply (shearing 1. 0. 0. 0. 0. 0.) (scaling 0.5 1. 1.)) *)

let rec project width height =  (* still don't know how to write functions *)
    if height <= 0 then my_canvas
    else begin
	let world_y = half -. pixel_size *. float_of_int (height-1) in
	let rec write_row i = if i >= 0 then begin
	    let world_x = (-.half) +. pixel_size *. float_of_int i in
	    let position = Tuple.point world_x world_y wall_z in
	    let r:Rays.ray = {origin=ray_origin; direction=Tuple.normalize (Tuple.subtract position ray_origin)} in
	    let xs = Intersections.intersect my_shape r in
		match Intersections.hit xs with
			| (Some hit) -> 
				let point = Rays.position r hit.t in 
				let normal = Sphere.normal_at hit.intersectionObject point in
				let eye = Tuple.negate r.direction in
				let new_color = Reflection.lighting hit.intersectionObject.material light point eye normal in
				Canvas.write_pixel my_canvas (height-1) i new_color; write_row (i-1)
			| None ->  write_row (i-1)
	end in
	write_row (width-1);
        project width (height-1)
    end

let my_canvas = project canvas_pixels canvas_pixels
let ppm = Canvas.canvas_to_ppm my_canvas
let () = output_string (open_out "images/silhouette.ppm") ppm;
