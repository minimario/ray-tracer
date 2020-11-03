open Color

type canvas = {width: int; height: int; pixels: color array array}

let black = color 0. 0. 0.
let red = color 1. 0. 0.


let colorArray w h color = Array.make_matrix h w color
let blackArray w h = colorArray w h black

let createCanvasColor w h color = {width=w; height=h; pixels=colorArray w h color}
let createCanvas w h = createCanvasColor w h black

let write_pixel canvas x y color = canvas.pixels.(y).(x) <- color
let pixel_at canvas x y = canvas.pixels.(y).(x)

let round f = int_of_float (floor (f +. 0.5))
let colorToInt c = round (min 255. (max 0. (255.*.c)))
let colorToString {red; green; blue} = String.concat " " 
                                        (List.map (fun color -> string_of_int (colorToInt color))
                                            [red; green; blue])

let rec rowToString row i = 
    let currentPixelString = colorToString row.(i) in 
    if (i = Array.length row - 1) then currentPixelString 
    else (if (i mod 5 = 4) then currentPixelString ^ "\n" ^ rowToString row (i+1)
          else colorToString row.(i) ^ " " ^ rowToString row (i+1))

let canvasToString canvas = 
    Array.fold_left (fun res r -> res ^ (rowToString r 0) ^ "\n") "" canvas.pixels

let canvasToPPM canvas = 
    "P3\n" ^ 
    (string_of_int canvas.width) ^ " " ^ (string_of_int canvas.height) ^ "\n" ^
    "255\n" ^
    canvasToString canvas