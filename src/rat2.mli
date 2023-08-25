(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

(** A representation of rationals bound, which is a pair of rationals:
    - the raw value of the bound
    - an epsilon used as an offset for representing strict bounds.

    For example, for the inequality [x < 3], the upper bound of [x] will
    be represented by [3 - Ɛ], where [Ɛ] is a positive integer that will
    be calculated later.
*)

open ExtSigs

module type SIG = sig
  module R : Coefs
  module V : Value with type r = R.t

  (** {1 Type} *)

  type t = private {
    v : V.t;  (** The raw value of the bound. *)
    offset : R.t;  (** The number of epsilons to add to the bound. *)
  }

  (** {1 Constructors} *)

  val zero : t
  (** The zero bound with no offset. *)

  val of_r : V.t -> t
  (** From a rational [r], returns the Rat2 representation with no offset. *)

  val upper : V.t -> t
  (** From a rational [r], returns [r - Ɛ]. *)

  val lower : V.t -> t
  (** From a rational [r], returns [r + Ɛ]. *)

  (** {1 Algebraic operations} *)

  val minus : t -> t
  (** From a bound [r + kƐ], returns [-r -kƐ]. *)

  val add : t -> t -> t
  (** Adds two bounds. *)

  val sub : t -> t -> t
  (** Substracts two bounds. *)

  val mult_by_const : R.t -> t -> t
  (** Multiplies a bound by a rational constant 
      (both v and offset are multiplied). *)

  val div_by_const : R.t -> t -> t
  (** Divides a bound by a constant. Fails if the constant is zero. *)

  (** {1 Comparison functions} *)

  val compare : t -> t -> int
  (** Compares two bounds; returns 0 iff the two bounds are strictly equal. *)

  val equal : t -> t -> bool
  (** Returns [true] iff the bounds are strictly equal. *)

  val is_zero : t -> bool
  (** Returns [true] iff the bound in argument is zero. *)

  val is_pure_rational : t -> bool
  (** Returns [true] iff the offset is 0. *)

  val is_int : t -> bool
  (** Returns [true] iff the offset is 0 and the field [v] is an integer. *)

  (** {1 Misc} *)

  val floor : t -> t
  (** Returns the greatest (pure) integer smaller or equal to the argument. *)

  val ceiling : t -> t
  (** Returns the smallest (pure) integer greater or equal to the argument. *)

  val print : Format.formatter -> t -> unit
  (** Prints a bound. *)
end

module Make (R : Rationals) (V : Value with type r = R.t) :
  SIG with module R = R and module V = V
