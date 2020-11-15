open Tuple

type matrix = float array array
let matrix_of_list list = Array.of_list list (* untested *)
let matrix_get m row col = m.(row).(col)

(* equality *)
let matrix_height m = Array.length m
let matrix_width m =
    if (matrix_height m) = 0 then 0
    else Array.length m.(0)
let equal_row a b =
    let is_zero f = equalFloat f 0. in
    Array.for_all is_zero (Array.map2 (fun x y -> x -. y) a b)
let equalMatrix a b =  (* questionable code style *)
    if (matrix_height a != matrix_height b || matrix_width a != matrix_width b) then false
    else Array.for_all (fun x -> x) (Array.map2 equal_row a b)

(* matrix-tuple casting *)
let matrix_of_tuple tuple = [| [| tuple.x |]; [| tuple.y |]; [| tuple.z |]; [| tuple.w |] |]
let tuple_of_matrix matrix = 
    if matrix_width matrix != 1 || matrix_height matrix != 4 
        then raise (Failure "bad dimensions when converting matrix to tuple")
    else
        tuple matrix.(0).(0) matrix.(1).(0) matrix.(2).(0) matrix.(3).(0)

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

let multiplyMatrixTuple m t = tuple_of_matrix (multiplyMatrix m (matrix_of_tuple t))

let make_identity_matrix n = (* rename to identity? *)
    Array.init n (fun r ->
        Array.init n (fun c -> if r = c then 1. else 0.)
    )

let identity_matrix = make_identity_matrix 4

let transpose matrix = 
    Array.mapi 
    (fun r _-> Array.mapi (fun c _ -> matrix.(c).(r)) matrix) 
    matrix
        
let submatrix matrix r c =
    let h = matrix_height matrix in 
    let w = matrix_width matrix in 
    let remove_element len i arr = (* array of length len with ith element removed *)
        Array.concat [
            Array.sub arr 0 i;
            Array.sub arr (i+1) (len-1-i)
        ] in 
    let row_removed_matrix = remove_element h r matrix in 

    Array.map (remove_element w c) row_removed_matrix 

let determinant_2d matrix = 
    match matrix with 
    | [| [| a; b |]; [| c; d |] |] -> a *. d -. b *. c
    | _ -> raise (Failure "2d matrix dimensions bad")

let rec determinant matrix = 
    let h = matrix_height matrix in 
    if h = 2 then determinant_2d matrix 
    else 
        Array.fold_left (+.) 0.
            (Array.mapi (fun c _ -> 
                matrix.(0).(c) *. (cofactor matrix 0 c)
             ) matrix)

    and minor matrix r c = determinant (submatrix matrix r c)
    and cofactor matrix r c =
        let minor = minor matrix r c in if (r+c) mod 2 = 0 then minor else (-.minor)

let invertible matrix = not (equalFloat (determinant matrix) 0.)

let inverse matrix = 
    if not (invertible matrix) then raise (Failure "matrix not invertible")
    else 
        Array.mapi 
        (fun r _-> Array.mapi (
            fun c _ -> (cofactor matrix c r) /. (determinant matrix))
        matrix) 
        matrix