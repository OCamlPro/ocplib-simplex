(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

val src : Logs.src

module type S = sig
  module Core : CoreSig.S
  val solve : Core.t -> Core.t
  val maximize : Core.t -> Core.P.t -> Core.t * (Core.P.t * bool) option
end

module Make(Core : CoreSig.S) : S with module Core = Core
