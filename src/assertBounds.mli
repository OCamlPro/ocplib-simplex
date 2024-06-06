(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module type S = sig

  module Core : CoreSig.S

  (*  The returned bool is [true] if the asserted bounds are not trivial
      (i.e. not implied by known bounds) *)
  (** [var env min max x] returns a new environment obtained by changing
      the bounds of [x] in [env] to [min] and [max].
      If the bounds were implied by other known bounds (in other words, if the
      environment did not change) the associated boolean will be [false].
  *)
  val var :
    Core.t ->
    ?min:Core.bound ->
    ?max:Core.bound ->
    Core.Var.t ->
    Core.t * bool

  (** [poly env poly min max x] returns a new environment obtained by changing
      the bounds of [poly] in [env] to [min] and [max].
      The polynomial is represented by the slack variable [x].

      [poly] must be a polynomial (as tested by [Core.P.is_polynomial]), that
      is, it must contain at least two distinct variables. Use [var] instead
      for constraints that apply to a single variable.

      The returned bool is [true] if the asserted bounds are not trivial (i.e.
      not implied by known bounds). If the bounds were implied by other known
      bounds (in other words, if the environment did not change) the associated
      boolean will be [false].

      @raise Invalid_argument if [poly] contains zero or one variables (use [var]
      to add constraints to a single variable).
  *)
  val poly :
    Core.t ->
    Core.P.t ->
    ?min:Core.bound ->
    ?max:Core.bound ->
    Core.Var.t ->
    Core.t * bool

end

module Make(Core : CoreSig.S) : S with module Core = Core
