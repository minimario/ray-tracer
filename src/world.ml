type world = {objects: Sphere.shape list; light: Reflection.point_light option}

let world_create = {objects=[]; light=None}
let default_world =
    let s1 = Sphere.sphere
    and (material: Reflection.material) = 
        {Reflection.default_material with color=Color.color 0.8 1. 0.6; diffuse=0.7; specular=0.2;} in
    let s1' = Sphere.set_material s1 material in 
    let s2 = Sphere.sphere
    and new_transform = Transformations.scaling 0.5 0.5 0.5 in
    let s2' = Sphere.set_transform s2 new_transform in
    let light = Reflection.point_light (Tuple.point ~-.10. 10. ~-.10.) (Color.color 1. 1. 1.) in
    {objects=[s1';s2']; light=Some light}
let contains world obj = List.mem obj world.objects

type computations =  (* should probably be in intersections *)
    {t: float; 
     comps_object: Sphere.shape; 
     point: Tuple.tuple;
     eyev: Tuple.tuple;
     normalv: Tuple.tuple;
     inside: bool
    }

let prepare_computations (intersection: Intersections.intersection) (ray: Rays.ray) =
    let t = intersection.t
    and obj = intersection.intersection_object
    and point = Rays.position ray intersection.t
    and eyev = Tuple.negate ray.direction in
    let normalv = Sphere.normal_at obj point in
    let base_computation_object = (* object when hit is on outside *)
        {t=t; 
         comps_object=obj;
         point=point;
         eyev=eyev;
         normalv=normalv;
         inside=false} in
    if Tuple.dot normalv eyev < 0. then (* if hit is on inside, invert normal vector *)
        {base_computation_object with inside=true; normalv=Tuple.negate normalv}
    else base_computation_object

let intersect_world world ray =
    let intersect_with_ray shape = Intersections.intersect shape ray in
    let unsorted_list = List.flatten (List.map intersect_with_ray world.objects) in 
    List.sort (Intersections.cmp) unsorted_list

let shade_hit world comps = 
    Reflection.lighting 
        comps.comps_object.material
        (Option.get world.light)
        comps.point
        comps.eyev
        comps.normalv

let color_at world ray = 
    let intersections = intersect_world world ray in
    let first_hit = Intersections.hit intersections in (* gets the hit from the intersection list *)
    match first_hit with 
    | None -> Color.black
    | Some hit -> shade_hit world (prepare_computations hit ray)
