(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module type SIG = sig

  module Core : CoreSig.SIG

  val var :
    Core.t ->
    Core.Var.t ->
    Core.bound ->
    Core.Ex.t ->
    Core.bound ->
    Core.Ex.t ->
    Core.t

  val poly :
    Core.t ->
    Core.P.t ->
    Core.Var.t ->
    Core.bound ->
    Core.Ex.t ->
    Core.bound ->
    Core.Ex.t ->
    Core.t

end

module Make(Core : CoreSig.SIG) : SIG with module Core = Core
