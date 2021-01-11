type world = {objects: ShapeType.shape list; light: Reflection.point_light option}

let world = {objects=[]; light=None}
let default_world =
    let s1 = Shape.sphere
    and (material: ShapeType.material) = 
        {Reflection.default_material with color=Color.color 0.8 1. 0.6; diffuse=0.7; specular=0.2;} in
    let s1' = Shape.set_material s1 material in 
    let s2 = Shape.sphere
    and new_transform = Transformations.scaling 0.5 0.5 0.5 in
    let s2' = Shape.set_transform s2 new_transform in
    let light = Reflection.point_light (Tuple.point ~-.10. 10. ~-.10.) (Color.color 1. 1. 1.) in
    {objects=[s1';s2']; light=Some light}
let contains world obj = List.mem obj world.objects

type computations =  (* should probably be in intersections *)
    {t: float; 
     comps_object: ShapeType.shape; 
     point: Tuple.tuple;
     eyev: Tuple.tuple;
     normalv: Tuple.tuple;
     inside: bool;
     over_point: Tuple.tuple;
     reflectv: Tuple.tuple;
    }

let prepare_computations (intersection: Intersections.intersection) (ray: Rays.ray) =
    let t = intersection.t
    and obj = intersection.intersection_object
    and point = Rays.position ray intersection.t
    and eyev = Tuple.negate ray.direction in
    let normalv = Shape.normal_at obj point in
    let adj_normalv = if Tuple.dot normalv eyev < 0. then Tuple.negate normalv else normalv in
    let over_point = Tuple.add point (Tuple.multiply_scalar adj_normalv Util.epsilon) in (* negate normal if hit on inside *)
    let reflectv = Reflection.reflect ray.direction adj_normalv in
    let base_computation_object = (* object when hit is on outside *)
        {t=t; 
         comps_object=obj;
         point=point;
         eyev=eyev;
         normalv=adj_normalv;
         inside=false;
         over_point=over_point;
         reflectv=reflectv;
         } in
    if Tuple.dot normalv eyev < 0. then
        {base_computation_object with inside=true}
    else base_computation_object

let intersect_world world ray =
    let intersect_with_ray shape = Shape.intersect shape ray in
    let unsorted_list = List.flatten (List.map intersect_with_ray world.objects) in 
    List.sort (Intersections.cmp) unsorted_list

let is_shadowed world point =
    let v = Tuple.subtract (Option.get world.light).position point in
    let distance = Tuple.magnitude v in
    let direction = Tuple.normalize v in
    let r = Rays.ray point direction in
    let intersections = intersect_world world r in
    let first_hit = Intersections.hit intersections in
    match first_hit with
    | None -> false
    | Some hit -> hit.t < distance

let shade_hit world comps = 
    let shadowed = is_shadowed world comps.over_point in
    Reflection.lighting 
        comps.comps_object.material
        comps.comps_object
        (Option.get world.light)
        comps.over_point
        comps.eyev
        comps.normalv
        shadowed

let color_at world ray = 
    let intersections = intersect_world world ray in
    let first_hit = Intersections.hit intersections in (* gets the hit from the intersection list *)
    match first_hit with 
    | None -> Color.black
    | Some hit -> shade_hit world (prepare_computations hit ray)

let reflected_color world comps =
    let reflect_ray = Rays.ray comps.over_point comps.reflectv in
    let color = color_at world reflect_ray in
    Color.multiply_scalar color comps.comps_object.material.reflective
