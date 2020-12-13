type world = {objects: Sphere.shape list; light: Reflection.point_light option}

let world_create = {objects=[]; light=None}
let default_world =
    let s1 = Sphere.sphere
    and (material: Reflection.material) = {color=Color.color 1. 1. 1.; ambient=0.1; diffuse=0.9;
                   specular=0.9; shininess=200.0} in
    Sphere.set_material s1 material;
    let s2 = Sphere.sphere
    and new_transform = Transformations.scaling 0.5 0.5 0.5 in
    Sphere.set_transform s2 new_transform;
    let light = Reflection.point_light (Tuple.point ~-.10. 10. ~-.10.) (Color.color 1. 1. 1.) in
    {objects=[s1;s2]; light=Some light}
let contains world obj = List.mem obj world.objects

type computations =  (* should probably be in intersections *)
    {t: float; comps_object: Sphere.shape; point: Tuple.tuple; eyev: Tuple.tuple; normalv: Tuple.tuple}

let prepare_computations (intersection: Intersections.intersection) (ray: Rays.ray) =
    let t = intersection.t
    and obj = intersection.intersection_object
    and point = Rays.position ray intersection.t
    and eyev = Tuple.negate ray.direction in
    let normalv = Sphere.normal_at obj point in
    {t=t; comps_object=obj; point=point; eyev=eyev; normalv=normalv}
