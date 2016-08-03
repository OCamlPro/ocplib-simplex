(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

(** Interface of the main types and auxiliary of the simplex *)
module type SIG = sig

  module Var : ExtSigs.VAR_SIG
  module Ex  : ExtSigs.EX_SIG
  module R   : ExtSigs.R_SIG
  module R2  : Rat2.SIG with module R = R

  module P : Polys.SIG with module Var = Var and module R = R

  module MX : Map.S with type key = Var.t
  module SX : Set.S with type elt = Var.t

  (*module SLAKE : Map.S with type key = P.t*)

  type bound = R2.t option

  type value_status = ValueOK | LowerKO | UpperKO

  type var_info =
    {
      mini     : bound;
      maxi     : bound;
      min_ex   : Ex.t;
      max_ex   : Ex.t;
      value    : R2.t;
      vstatus  : value_status;
    }

  type solution =
    { main_vars : (Var.t * R.t) list;
      slake_vars : (Var.t * R.t) list;
      int_sol : bool (* always set to false for rational simplexes*) }

  type maximum =
    { max_v : R.t;
      is_le : bool; (* bool = true <-> large bound *)
      reason: Ex.t }

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
      debug     : int;
      check_invs: bool;
    }

  val empty_info : var_info

  val empty : is_int : bool -> check_invs : bool -> debug : int -> t

  val on_integers : t -> bool

  val equals_optimum : R2.t -> bound -> bool

  val consistent_bounds : var_info -> bool

  val violates_min_bound : R2.t -> bound -> bool

  val violates_max_bound : R2.t -> bound -> bool

  val set_min_bound : var_info -> bound -> Ex.t -> var_info

  val set_max_bound : var_info -> bound -> Ex.t -> var_info

  (* vstatus is supposed to be well set *)
  val ajust_value_of_non_basic: var_info -> var_info * bool

  (* valuation is supposed to be well computed *)
  val ajust_status_of_basic : var_info -> var_info

  val evaluate_poly : t -> P.t -> R2.t

  val poly_of_slake : t -> Var.t -> P.t option

  (* debug functions and invariants *)

  val check_invariants : t -> (t -> result) -> unit

  val print : result -> Format.formatter -> t -> unit

  val debug : string -> t -> (t -> result) -> unit

end
