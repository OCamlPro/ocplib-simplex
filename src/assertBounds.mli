(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module type S = sig
  module Core : CoreSig.S

  (* The returned bool is [true] if the asserted bounds are not trivial
     (i.e. not implied by known bounds) *)

  val var :
    Core.t -> ?min:Core.bound -> ?max:Core.bound -> Core.Var.t -> Core.t * bool
  (** [var env min max x] returns a new environment obtained by changing
      the bounds of [x] in [env] to [min] and [max].
      If the bounds were implied by other known bounds (in other words, if the
      environment did not change) the associated boolean will be [false].
  *)

  (* The returned bool is [true] if the asserted bounds are not trivial
     (i.e. not implied by known bounds) *)

  val poly :
    Core.t ->
    Core.P.t ->
    ?min:Core.bound ->
    ?max:Core.bound ->
    Core.Var.t ->
    Core.t * bool
  (** [poly env poly min max x] returns a new environment obtained by changing
      the bounds of [poly] in [env] to [min] and [max].
      The polynomial is represented by the slack variable [x].
      If the bounds were implied by other known bounds (in other words, if the
      environment did not change) the associated boolean will be [false].
  *)
end

module Make (Core : CoreSig.S) : S with module Core = Core
