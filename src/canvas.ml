type canvas = {width: int; height: int; pixels: Color.color array array}

let color_array w h color = Array.make_matrix h w color
let black_array w h = color_array w h Color.black

let create_color_canvas w h color = {width=w; height=h; pixels=color_array w h color}
let create_canvas w h = create_color_canvas w h Color.black

let write_pixel canvas x y color = canvas.pixels.(y).(x) <- color
let pixel_at canvas x y = canvas.pixels.(y).(x)

let round f = int_of_float (floor (f +. 0.5))

let rec row_to_string row i = 
    let currentPixelString = Color.to_string row.(i) in 
    if (i = Array.length row - 1) then currentPixelString 
    else (if (i mod 5 = 4) then currentPixelString ^ "\n" ^ row_to_string row (i+1)
          else Color.to_string row.(i) ^ " " ^ row_to_string row (i+1))

let canvas_to_string canvas = 
    Array.fold_left (fun res r -> res ^ (row_to_string r 0) ^ "\n") "" canvas.pixels

let canvas_to_ppm canvas = 
    "P3\n" ^ 
    (string_of_int canvas.width) ^ " " ^ (string_of_int canvas.height) ^ "\n" ^
    "255\n" ^
    canvas_to_string canvas