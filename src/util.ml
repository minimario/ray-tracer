(* x mod y, but always positive *)
let mod_positive x y = 
    let mod_default = x mod y in
    if mod_default >= 0 then mod_default else mod_default + y

(* floor *)
let floor x = int_of_float (Float.floor x)