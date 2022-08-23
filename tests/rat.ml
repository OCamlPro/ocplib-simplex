open Num

type t = num
let add = ( +/ )
let minus = minus_num
let mult = ( */ )
let abs = abs_num
let compare = compare_num
let equal = ( =/ )
let zero = Int 0
let one = Int 1
let m_one = Int (-1)
let is_zero n = n =/ zero
let to_string = string_of_num

let print fmt t = Format.fprintf fmt "%s" (to_string t)
let is_int = is_integer_num
let div = (//)
let sub = (-/)
let is_one v = v =/ Int 1
let is_m_one v = v =/ Int (-1)
let sign = sign_num
let min = min_num
