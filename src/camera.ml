type camera = {hsize: int; (* horizontal size in pixels *)
               vsize: int; (* vertical size in pixels *)
               field_of_view: float; (* angle representing field of view *)
               half_width: float;
               half_height: float;
               pixel_size: float;
               transform: Matrix.matrix
              }

let create_camera (hsize:int) (vsize:int) (field_of_view:float) = 
    let f_hsize = float_of_int hsize in
    let f_vsize = float_of_int vsize in
    let half_view = tan (field_of_view /. 2.) in
    let aspect = f_hsize /. f_vsize in
    let half_width = if aspect >= 1. then half_view else half_view *. aspect in
    let half_height = if aspect >= 1. then half_view /. aspect else half_view in
    let pixel_size = half_width *. 2. /. f_hsize in
    { hsize; 
      vsize; 
      field_of_view; 
      half_width; 
      half_height; 
      pixel_size; 
      transform = Matrix.identity_matrix
    }

let set_transform (camera:camera) (transform:Matrix.matrix) =
    {camera with transform = transform}

let ray_for_pixel (camera:camera) (px:float) (py:float) = 
    let xoffset = (px +. 0.5) *. camera.pixel_size in
    let yoffset = (py +. 0.5) *. camera.pixel_size in
    let world_x = camera.half_width -. xoffset in
    let world_y = camera.half_height -. yoffset in
    let inv_transform = Matrix.inverse camera.transform in
    let pixel = Matrix.multiply_tuple inv_transform (Tuple.point world_x world_y (-1.)) in
    let origin = Matrix.multiply_tuple inv_transform (Tuple.point 0. 0. 0.) in
    let direction = Tuple.(normalize (subtract pixel origin)) in
    Rays.ray origin direction

let render (camera:camera) (world:World.world) =
    let image = Canvas.create_canvas camera.hsize camera.vsize in
    for y = 0 to camera.vsize - 1 do
        for x = 0 to camera.hsize - 1 do
            let ray = ray_for_pixel camera (float_of_int x) (float_of_int y) in
            let color = World.color_at world ray in 
            Canvas.write_pixel image x y color
        done
    done;
    image
