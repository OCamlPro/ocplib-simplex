(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module type SIG = sig
  module Core : CoreSig.SIG
  val solve : Core.t -> Core.t
  val maximize : Core.t -> Core.P.t -> Core.t * (Core.P.t * bool) option
end

module Make(Core : CoreSig.SIG) : SIG with module Core = Core
