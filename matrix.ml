open Tuple

type matrix = Array
let matrix_of_list list = Array.of_list list (* untested *)
let matrix_get m row col = m.(row).(col)

(* equality *)

(* operations *)
