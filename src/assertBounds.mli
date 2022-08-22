(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module type SIG = sig

  module Core : CoreSig.SIG

  (* The returned bool is true if the asserted bounds are not trivial
     (i.e. not implied by known bounds) *)
  (** [var env x min max]
      From an environment `env`, returns an environment `env'` in which the
      bounds of `x` are updated to `min` and `max`. If the bounds were not
      implied by other known bounds, the associated boolean will be `true`.
 *)
  val var :
    Core.t ->
    Core.Var.t ->
    Core.bound option ->
    Core.bound option ->
    Core.t * bool

  (* The returned bool is true if the asserted bounds are not trivial
     (i.e. not implied by known bounds) *)
  val poly :
    Core.t ->
    Core.P.t ->
    Core.Var.t ->
    Core.bound option ->
    Core.bound option ->
    Core.t * bool

end

module Make(Core : CoreSig.SIG) : SIG with module Core = Core
