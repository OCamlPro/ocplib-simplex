(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

open ExtSigs

module type SIG = sig
  module Var : Variables
  module R : Coefs

  type t
  type var_status = New | Exists | Removed

  val empty : t
  val is_polynomial : t -> bool
  val is_empty : t -> bool

  val replace    : Var.t -> R.t -> t -> t * var_status
  val accumulate : Var.t -> R.t -> t -> t * var_status
  val append     : t -> R.t -> t -> t * (Var.t * var_status) list
  val subst : Var.t -> t -> t -> t * (Var.t * var_status) list
  val from_list : (Var.t * R.t) list -> t
  val print : Format.formatter -> t -> unit

  val fold: (Var.t -> R.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter: (Var.t -> R.t -> unit) -> t -> unit
  val partition: (Var.t -> R.t -> bool) -> t -> t * t
  val compare : t -> t -> int
  val mem : Var.t -> t -> bool
  val equal : t -> t -> bool
  val bindings : t -> (Var.t * R.t) list
  val find : Var.t -> t -> R.t
  val remove : Var.t -> t -> t
end

module Make(Var: Variables)(R : Rationals) : SIG
  with module Var = Var and module R = R

