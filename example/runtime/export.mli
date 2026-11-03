open! Base

type 'a count := 'a -> int

val count_int : int count
val count_int32 : int32 count
val count_int32_u : int32_u -> int
val count_int64 : int64 count
val count_int64_u : int64_u -> int
val count_string : string count
val count_char : char count
val count_char_u : char -> int
val count_float : float count
val count_float_u : float -> int
val count_bool : bool count
val count_bool_u : bool -> int
val count_unit : unit count
val count_unit_u : unit -> int
val count_ref : 'a count -> 'a ref count
val count_option : 'a count -> 'a option count
val count_list : 'a count -> 'a list count
val count_array : 'a count -> 'a array count
