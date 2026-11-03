open! Base

let count_int _ = 1
let count_int32 _ = 1
let count_int32_u (_ : int32_u) = 1
let count_int64 _ = 1
let count_int64_u (_ : int64_u) = 1
let count_string = String.length
let count_char _ = 1
let count_char_u (_ : char#) = 1
let count_float _ = 1
let count_float_u (_ : float#) = 1
let count_bool _ = 1
let count_bool_u (_ : bool#) = 1
let count_unit _ = 1
let count_unit_u (_ : unit#) = 1
let count_ref count_elt ref = 1 + count_elt !ref
let count_option count_elt option = 1 + Option.value_map option ~f:count_elt ~default:0
let count_list count_elt list = List.sum (module Int) list ~f:(fun x -> count_elt x)
let count_array count_elt array = Array.sum (module Int) array ~f:(fun x -> count_elt x)
