(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module type SIG = sig

  module Core : CoreSig.SIG

  (* The returned bool is true if the asserted bounds are not trivial
     (i.e. not implied by known bounds) *)
  val var :
    Core.t ->
    Core.Var.t ->
    Core.bound ->
    Core.Ex.t ->
    Core.bound ->
    Core.Ex.t ->
    Core.t * bool

  (* The returned bool is true if the asserted bounds are not trivial
     (i.e. not implied by known bounds) *)
  val poly :
    Core.t ->
    Core.P.t ->
    Core.Var.t ->
    Core.bound ->
    Core.Ex.t ->
    Core.bound ->
    Core.Ex.t ->
    Core.t * bool

end

module Make(Core : CoreSig.SIG) : SIG with module Core = Core
