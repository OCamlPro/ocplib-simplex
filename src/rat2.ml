(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module type SIG = sig
  module R : ExtSigs.R_SIG

  type t = R.t * R.t

  val zero : t
  val minus : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  val mult_by_const : R.t -> t -> t
  val div_by_const : R.t -> t -> t
  val compare : t -> t -> int
  val is_zero : t -> bool

end

module Make(R : ExtSigs.R_SIG) : SIG with module R = R = struct

  module R = R

  type t = R.t * R.t

  let zero = R.zero, R.zero
  let add (a, b) (x, y) = R.add a x, R.add b y
  let sub (a, b) (x, y) = R.sub a x, R.sub b y
  let mult (a, b) (x, y) = R.mult a x, R.mult b y

  let mult_by_const c e =
    if R.is_one c then e
    else let a, b = e in R.mult a c, R.mult b c

  let div_by_const  c e =
    if R.is_one c then e
    else let a, b = e in R.div  a c, R.div  b c

  let compare (a, b) (x, y) =
    let c = R.compare a x in
    if c <> 0 then c else R.compare b y

  let is_zero (a, b) = R.is_zero a && R.is_zero b

  let minus (a, b) = R.minus a, R.minus b

end
