open ShapeType
let local_normal_at testshape (local_point:Tuple.tuple) =
    Tuple.vector local_point.x local_point.y local_point.z

let local_intersect testshape {Rays.origin; direction} =
    Intersections.(
        [{t=origin.x; intersection_object=testshape};
         {t=origin.y; intersection_object=testshape};
         {t=origin.z; intersection_object=testshape};
         {t=origin.w; intersection_object=testshape};
         {t=direction.x; intersection_object=testshape};
         {t=direction.y; intersection_object=testshape};
         {t=direction.z; intersection_object=testshape};
         {t=direction.w; intersection_object=testshape}]
    ) (* just for testing so bad code doesn't hurt too much :D *)
