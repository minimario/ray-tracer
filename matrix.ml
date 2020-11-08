open Tuple

type matrix = Array
let matrix_of_list list = Array.of_list list (* untested *)
let matrix_get m row col = m.(row).(col)

(* equality *)
let matrix_height m = Array.length m
let matrix_width m =
    if (matrix_height m) == 0 then 0
    else Array.length m.(0)
let equal_row a b =
    let is_zero f = equalFloat f 0. in
    Array.for_all is_zero (Array.map2 (fun x y -> x -. y) a b)
let equalMatrix a b =  (* questionable code style *)
    if (matrix_height a != matrix_height b || matrix_width a != matrix_width b) then false
    else Array.for_all (fun x -> x) (Array.map2 equal_row a b)

(* operations *)
let matrix_of_tuple tuple = [| tuple.x; tuple.y; tuple.z; tuple.w |]
let getColumn m col = Array.map (fun row -> row.(col)) m
let multiplyMatrix a b =  (* unimplemented *)
    if matrix_width a != matrix_height b then raise (Failure "bad dimensions")
    else
        let c = Array.make_matrix (matrix_height a) (matrix_width b) 0. in
        for i = 0 to matrix_height a - 1 do
            for j = 0 to matrix_width b - 1 do
		let sum arr = Array.fold_left (+.) 0. arr in
                c.(i).(j) <- sum (Array.map2 (fun x y -> x *. y) a.(i) (getColumn b j))
            done
        done;
        c
