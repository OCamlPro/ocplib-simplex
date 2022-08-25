(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

(** Interface of the main types and auxiliary of the simplex *)
module type SIG = sig

  (** The type of variables maipulated by the simplex algorithm. *)
  module Var : ExtSigs.VAR_SIG

  (** An interface for explanations; in practice, they are labels attached to
      bounds used for backtracking information on how bounds were discovered.
      The simplex algorithm does not create explanations: it will only attach
      empty explanations to bounds, build the union of explanations and print
      them. It is the user's job to provide the initial explanations when
      initializing the simplex core. *)
  module Ex  : ExtSigs.EX_SIG

  (** The interface for rationals provided by users. *)
  module R   : ExtSigs.R_SIG

  (** Pairs of rationals R representing bounds with an offset (x + kƐ). *)
  module R2  : Rat2.SIG with module R = R

  (** Linear relations of variables. *)
  module P : Polys.SIG with module Var = Var and module R = R

  (** Collections of variables *)
  module MX : Map.S with type key = Var.t
  module SX : Set.S with type elt = Var.t

  (*module SLAKE : Map.S with type key = P.t*)

  (** A bound is a value (x + kƐ) and an explanation. *)
  type bound = {
    bvalue : R2.t;
    explanation: Ex.t
  }

  type value_status =
    | ValueOK (** The value is inbetween bounds *)
    | LowerKO (** The value is smaller than the lower bound *)
    | UpperKO (** The value is greater than the upper bound *)

  type var_info =
    {
      mini     : bound option; (* None -> -inf *)
      maxi     : bound option; (* None -> +inf *)
      value    : R2.t;
      vstatus  : value_status;
      empty_dom : bool;
    }

  type solution = {
    main_vars : (Var.t * R.t) list;
    slake_vars : (Var.t * R.t) list;
    int_sol : bool; (* always set to false for rational simplexes*)
    epsilon : R.t;
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

  type t =
    {
      basic     : (var_info * P.t) MX.t;
      non_basic : (var_info * SX.t) MX.t;
      slake     : P.t MX.t;
      fixme     : SX.t;
      is_int    : bool;
      status    : simplex_status;
      check_invs: bool;
      nb_pivots : int ref;
    }

  (** Returns a simplex environment with three parameters:
      - `is_int`: will the simplex work on an integer optimization problem or a
        rational problem?
      - `check_invs`: processes checks after the calculation (deprecated)
      - `debug`: sets the debug level for printing messages (0: no debug,
        1: basic debug, 2: full debug)
  *)
  val empty : is_int : bool -> check_invs : bool -> t

  (** Returns true if the simplex environment is on integers. *)
  val on_integers : t -> bool

  (** Equality check betweeb bounds. *)
  val equals_optimum : R2.t -> bound option -> bool

  (** Checks if the lower bound of a variable is smaller than its upper bound. *)
  val consistent_bounds : var_info -> bool

  (** [violates_min_bound b mb]
      Returns true if `b` is smaller than mb. *)
  val violates_min_bound : R2.t -> bound option -> bool

  (** [violates_max_bound b mb]
      Returns true if `b` is greater than mb. *)
  val violates_max_bound : R2.t -> bound option -> bool

  (* The returned bool is true if the asserted bounds are not trivial
     (i.e. not implied by known bounds) *)
  (** [set_min_bound vinfo b]
      Returns a couple `(vinfo', changed)` where:
      - `vinfo'` is the new variable info where the new min bound `b` has
        been set
      - `changed` is `true` if the new bound has changed the variable info
  *)
  val set_min_bound : var_info -> bound option -> var_info * bool

  (** Same as `set_min_bound`, but for max bounds. *)
  val set_max_bound : var_info -> bound option -> var_info * bool

  (** [ajust_value_of_non_basic vinfo]
      If `vinfo`'s status is UpperKO (resp. LowerKO), updates
      the variable info's value with the upper bound (rest. the lower bound).
      Otherwise, do nothing.
      The boolean returnes is `true` if the new `var_info` has changed. *)
  val ajust_value_of_non_basic: var_info -> var_info * bool
  (* vstatus is supposed to be well set *)

  (** [ajust_status_of_basic vinfo]
      Checks a variable info's bound matches its status. If its value violates
      its lower bound, its status is set to LowerKO.
      In the other case, it is set to UpperKO.
      If the value is between the two bounds, it is set to ValueOK. *)
  val ajust_status_of_basic : var_info -> var_info
  (* valuation is supposed to be well computed *)

  (** Evaluates a polynomial of non basic variables. *)
  val evaluate_poly : t -> P.t -> R2.t

  (** [poly_of_slake env slake]
      Returns the polynomial associated to the variable `slake` in `env`. *)
  val poly_of_slake : t -> Var.t -> P.t option

  (** Returns the explanation associated to a variable lower bound. *)
  val expl_of_min_bound : var_info -> Ex.t

  (** Same as `expl_of_min_bound`, but for upper bounds. *)
  val expl_of_max_bound : var_info -> Ex.t

  (* debug functions and invariants *)

  (** Checks several invariants in the project *)
  val check_invariants : t -> (t -> result) -> unit

  (** Pretty prints the environment. *)
  val print : result -> Format.formatter -> t -> unit

  (** (deprecated) *)
  val debug : string -> t -> (t -> result) -> unit
end
