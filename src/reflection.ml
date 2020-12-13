type point_light = {position: Tuple.tuple; intensity: Color.color}
let point_light position intensity = {position; intensity}
let point_light_equals pl other = (Tuple.equals pl.position other.position) && (Color.equals pl.intensity other.intensity)

type material = {color: Color.color; ambient: float; diffuse: float; 
              specular: float; shininess: float}

let default_material = {color=Color.color 1. 1. 1.; ambient=0.1; diffuse=0.9;
                   specular=0.9; shininess=200.0}

let reflect in_vec normal = 
    Tuple.subtract in_vec
        (Tuple.multiply_scalar normal (2.*.(Tuple.dot in_vec normal)))

(* Phong Reflection Model *)
let lighting material light point eyev normalv =
    let effective_color = Color.multiply material.color light.intensity in
    let lightv = Tuple.normalize (Tuple.subtract light.position point) in 
    let ambient = Color.multiply_scalar effective_color material.ambient in 
    let light_dot_normal = Tuple.dot lightv normalv in
    if light_dot_normal < 0. then 
        let diffuse = Color.color 0. 0. 0. in 
        let specular = Color.color 0. 0. 0. in
        Color.add ambient (Color.add diffuse specular)
    else 
        let diffuse = Color.multiply_scalar effective_color (material.diffuse *. light_dot_normal) in 
        let reflectv = reflect (Tuple.negate lightv) normalv in
        let reflect_dot_eye = Tuple.dot reflectv eyev in
        if (reflect_dot_eye <= 0.) then
            let specular = Color.color 0. 0. 0. in
            Color.add ambient (Color.add diffuse specular)
        else 
            let factor = reflect_dot_eye ** material.shininess in
            let specular = Color.multiply_scalar light.intensity (material.specular *. factor) in
            Color.add ambient (Color.add diffuse specular)
