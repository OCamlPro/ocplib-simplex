(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

open ExtSigs

(** Interface of the main types and auxiliary of the simplex. *)
module type S = sig

  (** {1 Modules} *)

  (** The type of variables maipulated by the simplex algorithm. *)
  module Var : Variables

  (** An interface for explanations; in practice, they are labels attached to
      bounds used for backtracking information on how bounds were discovered.
      The simplex algorithm does not create explanations: it will only attach
      empty explanations to bounds, build the union of explanations and print
      them. It is the user's job to provide the initial explanations when
      initializing the simplex core. *)
  module Ex  : Explanations

  (** The interface for rationals provided by users for the coefficient. *)
  module R   : Coefs

  (** The interface for values of variable and bounds provided by users. *)
  module V   : Value with type r = R.t

  (** Pairs of rationals R representing bounds with an offset [x + kƐ]. *)
  module R2  : Rat2.SIG with module R = R and module V = V

  (** Linear relations of variables. *)
  module P : Polys.SIG with module Var = Var and module R = R

  (** Collections of variables. *)
  module MX : MapSig with type key = Var.t
  module SX : SetSig with type elt = Var.t

  (*module SLAKE : Map.S with type key = P.t*)

  (** {1 Types} *)
  
  (** A bound is a value of the form [x + kƐ] and an explanation. *)
  type bound = {
    bvalue : R2.t;
    explanation : Ex.t
  }

  type value_status =
    | ValueOK (** The value is inbetween bounds. *)
    | LowerKO (** The value is smaller than the lower bound. *)
    | UpperKO (** The value is greater than the upper bound. *)

  type var_info = {
      mini : bound option; (* None -> -inf *)
      maxi : bound option; (* None -> +inf *)
      value : R2.t;
      vstatus : value_status;
      empty_dom : bool;
    }

  type solution = {
    main_vars : (Var.t * V.t) list;
    slake_vars : (Var.t * V.t) list;
    int_sol : bool; (* Always set to false for rational simplexes. *)
    epsilon : V.t;
  }

  type maximum = {
    max_v : bound;
    is_le : bool; (* bool = true <-> large bound *)
  }

  type result =
    | Unknown
    | Unsat of Ex.t Lazy.t
    | Sat of solution Lazy.t
    | Unbounded of solution Lazy.t
    | Max of maximum Lazy.t * solution Lazy.t

  type simplex_status = UNK | UNSAT of Var.t | SAT

  type t = {
      basic : (var_info * P.t) MX.t;
      non_basic : (var_info * SX.t) MX.t;
      slake : P.t MX.t;
      fixme : SX.t;
      is_int : bool;
      status : simplex_status;
      check_invs : bool;
      nb_pivots : int ref;
    }

  (** Returns a simplex environment with three parameters:
      - [is_int]: will the simplex work on an integer optimization problem or a
        rational problem?
      - [check_invs]: processes checks after the calculation (deprecated).
  *)
  val empty : is_int : bool -> check_invs : bool -> t

  (** Returns [true] if the simplex environment is on integers. *)
  val on_integers : t -> bool

  (** Equality check between bounds. *)
  val equals_optimum : R2.t -> bound option -> bool

  (** Checks if the lower bound of a variable is smaller than
      its upper bound. *)
  val consistent_bounds : var_info -> bool

  (** [violates_min_bound b mb] returns [true] if [b] is smaller than [mb]. *)
  val violates_min_bound : R2.t -> bound option -> bool

  (** [violates_max_bound b mb] returns [true] if [b] is greater than [mb]. *)
  val violates_max_bound : R2.t -> bound option -> bool

  (* The returned bool is [true] if the asserted bounds are not trivial
      (i.e. not implied by known bounds). *)
  (** [set_min_bound vinfo b] returns a couple [(vinfo', changed)] where:
      - [vinfo'] is the new variable info where the new min bound [b] has
        been set.
      - [changed] is [true] if the new bound has changed the variable info
  *)
  val set_min_bound : var_info -> bound option -> var_info * bool

  (** Same as {!val:set_min_bound}, but for max bounds. *)
  val set_max_bound : var_info -> bound option -> var_info * bool

  (** [ajust_value_of_non_basic vinfo] updates the info's value with the upper
      bound (resp. the lower bound), if [vinfo]'s status is {!const:UpperKO}
      (resp. {!const:LowerKO}). Otherwise, do nothing.
      The boolean returned is [true] if the new variable [var_info]
      has changed. *)
  val ajust_value_of_non_basic: var_info -> var_info * bool
  (* vstatus is supposed to be well set *)

  (** [ajust_status_of_basic vinfo] checks a variable info's bound matches
      its status. If its value violates its lower bound, its status is set
      to {!const:LowerKO}. In the other case, it is set to {!const:UpperKO}.
      If the value is between the two bounds, it is set to {!const:ValueOK}. *)
  val ajust_status_of_basic : var_info -> var_info
  (* valuation is supposed to be well computed *)

  (** Evaluates a polynomial of non basic variables. *)
  val evaluate_poly : t -> P.t -> R2.t

  (** [poly_of_slake env slake] returns the polynomial associated
      to the variable [slake] in [env]. *)
  val poly_of_slake : t -> Var.t -> P.t option

  (** Returns the explanation associated to a variable lower bound. *)
  val expl_of_min_bound : var_info -> Ex.t

  (** Same as `expl_of_min_bound`, but for upper bounds. *)
  val expl_of_max_bound : var_info -> Ex.t

  (** {1 Debug functions} *)
  (** Only use for internal debugging *)

  (** Checks several invariants in the project *)
  val check_invariants : t -> (t -> result) -> unit

  (** Pretty prints the environment. *)
  val print : result -> Format.formatter -> t -> unit

  (** @deprecated *)
  val debug : string -> t -> (t -> result) -> unit
end
