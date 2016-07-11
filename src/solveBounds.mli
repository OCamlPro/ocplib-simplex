(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module type SIG = sig
  module Core : CoreSig.SIG
  val solve : Core.t -> Core.t
end

module Make(Core : CoreSig.SIG) : SIG with module Core = Core
