(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

(** A representation of rationals bound, which is a pair of rationals:
    - the raw value of the bound
    - an epsilon used as an offset for representing strict bounds.

    For example, for `x < 3`, the upper bound of `x` will be represented by
    `3 - Ɛ`, where `Ɛ` is a positive integer that will be calculated later.
*)

open ExtSigs

module type SIG = sig
  module R : Rationals

  type t = private {
    v: R.t;
    (** The raw value of the bound *)

    offset: R.t
    (** The number of epsilons to add to the bound. *)
  }

  (** The zero bound with no offset *)
  val zero : t

  (** From a rational `r`, returns the Rat2 representation with no offset *)
  val of_r : R.t -> t

  (** From a rational `r`, returns `r - Ɛ`. *)
  val upper : R.t -> t

  (** From a rational `r`, returns `r + Ɛ`. *)
  val lower : R.t -> t

  (** Returns `true` iff the offset is 0. *)
  val is_pure_rational : t -> bool

  (** Returns `true` iff the offset is 0 and the field `v` is an integer *)
  val is_int : t -> bool

  (** From a bound `r + kƐ`, returns `-r -kƐ` *)
  val minus : t -> t

  (** Adds two bounds *)
  val add : t -> t -> t

  (** Substracts two bounds. *)
  val sub : t -> t -> t

  (** Multiplies two bounds. *)
  val mult : t -> t -> t

  (** Multiplies a bound by a rational constant (both v and offset are multiplied. *)
  val mult_by_const : R.t -> t -> t

  (** Divides a bound by a constant. Fails if the constant is zero. *)
  val div_by_const : R.t -> t -> t

  (** Compares two bounds; returns 0 iff the two bounds are strictly equal. *)
  val compare : t -> t -> int

  (** Returns true iff the bounds are strictly equal. *)
  val equal : t -> t -> bool

  (** Returns true iff the bound in argument is zero. *)
  val is_zero : t -> bool

  (** Returns the greatest (pure) integer smaller or equal to the argument. *)
  val floor : t -> t

  (** Returns the smallest (pure) integer greater or equal to the argument. *)
  val ceiling : t -> t

  (** Prints a bound. *)
  val print : Format.formatter -> t -> unit
end

module Make(R : Rationals) : SIG with module R = R
