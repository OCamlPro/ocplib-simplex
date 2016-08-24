(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module type SIG = sig
  module Core : CoreSig.SIG
  val get : (Core.P.t * bool) option -> Core.t -> Core.result
end

module Make(Core : CoreSig.SIG) : SIG with module Core = Core
