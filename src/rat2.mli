(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module type SIG = sig
  module R : ExtSigs.R_SIG
  type t = R.t * R.t
  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  val mult_by_const : R.t -> t -> t
  val div_by_const : R.t -> t -> t
  val compare : t -> t -> int
  val is_zero : t -> bool

end

module Make(R : ExtSigs.R_SIG) : SIG with module R = R
