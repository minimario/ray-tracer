let floor_material = {Reflection.default_material with color = Color.color 1. 0.9 0.9; specular = 0.}
let floor = {Shape.plane with material = floor_material}

let middle_material = {Reflection.default_material with 
                        color = Color.color 0.1 1. 0.5;
                        diffuse = 0.7;
                        specular = 0.3}
let middle_transform = Transformations.translation (-0.5) 1. 0.5
let middle = {Shape.sphere with material = middle_material; transform = middle_transform}

let right_material = {Reflection.default_material with 
                        color = Color.color 0.5 1. 0.1;
                        diffuse = 0.7;
                        specular = 0.3}
let right_transform = Transformations.(
    let t1 = translation 1.5 0.5 (-0.5) in
    let t2 = scaling 0.5 0.5 0.5 in
    Matrix.(multiply t1 t2)
)
let right = {Shape.sphere with material = right_material; transform = right_transform}

let left_material = {Reflection.default_material with 
                        color = Color.color 1. 0.8 0.1;
                        diffuse = 0.7;
                        specular = 0.3}
let left_transform = Transformations.(
    let t1 = translation (-1.5) 0.33 (-0.75) in
    let t2 = scaling 0.33 0.33 0.33 in
    Matrix.(multiply t1 t2)
)
let left = {Shape.sphere with material = left_material; transform = left_transform}

let light_source = Reflection.point_light 
                    (Tuple.point (-10.) 10. (-10.)) 
                    (Color.color 1. 1. 1.)
let (world:World.world) = {objects = [floor; middle; right; left]; 
             light = Some light_source}

let camera_transform = Transformations.view_transform 
                          (Tuple.point 0. 1.5 (-5.))
                          (Tuple.point 0. 1. 0.)
                          (Tuple.vector 0. 1. 0.)

let camera = {(Camera.create_camera 300 150 (Float.pi/.3.)) with transform = camera_transform}

let canvas = Camera.render camera world
let ppm = Canvas.canvas_to_ppm canvas
let () = output_string (open_out "images/sphereplane.ppm") ppm;
