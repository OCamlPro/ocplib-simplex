(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module type SIG = sig

  module Core : CoreSig.SIG

  (* The returned bool is true if the asserted bounds are not trivial
     (i.e. not implied by known bounds) *)
  (** [var env min max x]
      From an environment `env`, returns an environment `env'` in which the
      bounds of `x` are updated to `min` and `max`.
      If the bounds were implied by other known bounds (in other words, if the
      environment did not change) the associated boolean will be `false`.
  *)
  val var :
    Core.t ->
    ?min:Core.bound ->
    ?max:Core.bound ->
    Core.Var.t ->
    Core.t * bool

  (* The returned bool is true if the asserted bounds are not trivial
     (i.e. not implied by known bounds) *)
  (** [poly env poly min max x]
      From an environment `env`, returns an environment `env'` in which the
      bounds of `poly` are updated to `min` and `max`. The polynomial is represented
      by the slack variable `x`.
      If the bounds were implied by other known bounds (in other words, if the
      environment did not change) the associated boolean will be `false`.
  *)
  val poly :
    Core.t ->
    Core.P.t ->
    ?min:Core.bound ->
    ?max:Core.bound ->
    Core.Var.t ->
    Core.t * bool

end

module Make(Core : CoreSig.SIG) : SIG with module Core = Core
