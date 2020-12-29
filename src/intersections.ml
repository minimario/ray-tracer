type intersection = {t: float; intersection_object: ShapeType.shape}
let intersection t intersection_object = {t; intersection_object}

let cmp x y = 
    if (Tuple.float_equals x.t y.t) then 0
    else if (x.t < y.t) then (-1)
    else 1

let hit intersection_list  = 
    let positive_intersections = (List.filter (fun x->x.t >= 0.) intersection_list) in
    let sorted_intersections = 
        List.sort cmp positive_intersections
    in 
    match sorted_intersections with 
    | [] -> None
    | _ -> Some (List.hd sorted_intersections)

let print_matrix m = m |> Array.iter (fun xs -> xs |> Array.iter (fun x -> print_endline x))