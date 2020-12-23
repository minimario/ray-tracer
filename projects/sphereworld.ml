let floor_material = {Reflection.default_material with color = Color.color 1. 0.9 0.9; specular = 0.}
let floor_transform = Transformations.scaling 10. 0.01 10.
let floor = {Sphere.sphere with material = floor_material; transform = floor_transform}

let left_wall_transform = Transformations.(
    let t1 = translation 0. 0. 5. in
    let t2 = rotation_y (-.Float.pi/.4.) in
    let t3 = rotation_x (Float.pi/.2.) in
    let t4 = scaling 10. 0.01 10. in
    Matrix.(multiply (multiply (multiply t1 t2) t3) t4)
)
let left_wall = {Sphere.sphere with material = floor_material; transform = left_wall_transform}
let right_wall_transform = Transformations.(
    let t1 = translation 0. 0. 5. in
    let t2 = rotation_y (Float.pi/.4.) in
    let t3 = rotation_x (Float.pi/.2.) in
    let t4 = scaling 10. 0.01 10. in
    Matrix.(multiply (multiply (multiply t1 t2) t3) t4)
)
let right_wall = {Sphere.sphere with material = floor_material; transform = right_wall_transform}

let middle_material = {Reflection.default_material with 
                        color = Color.color 0.1 1. 0.5;
                        diffuse = 0.7;
                        specular = 0.3}
let middle_transform = Transformations.translation (-0.5) 1. 0.5
let middle = {Sphere.sphere with material = middle_material; transform = middle_transform}

let right_material = {Reflection.default_material with 
                        color = Color.color 0.5 1. 0.1;
                        diffuse = 0.7;
                        specular = 0.3}
let right_transform = Transformations.(
    let t1 = translation 1.5 0.5 (-0.5) in
    let t2 = scaling 0.5 0.5 0.5 in
    Matrix.(multiply t1 t2)
)
let right = {Sphere.sphere with material = right_material; transform = right_transform}

let left_material = {Reflection.default_material with 
                        color = Color.color 1. 0.8 0.1;
                        diffuse = 0.7;
                        specular = 0.3}
let left_transform = Transformations.(
    let t1 = translation (-1.5) 0.33 (-0.75) in
    let t2 = scaling 0.33 0.33 0.33 in
    Matrix.(multiply t1 t2)
)
let left = {Sphere.sphere with material = left_material; transform = left_transform}

let light_source = Reflection.point_light 
                    (Tuple.point (-10.) 10. (-10.)) 
                    (Color.color 1. 1. 1.)
let (world:World.world) = {objects = [floor; left_wall; right_wall; middle; right; left]; 
             light = Some light_source}

let camera_transform = Transformations.view_transform 
                          (Tuple.point 0. 1.5 (-5.))
                          (Tuple.point 0. 1. 0.)
                          (Tuple.vector 0. 1. 0.)

let camera = {(Camera.create_camera 200 100 (Float.pi/.3.)) with transform = camera_transform}

let canvas = Camera.render camera world
let ppm = Canvas.canvas_to_ppm canvas
let () = output_string (open_out "images/sphereworld.ppm") ppm;
