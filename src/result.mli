(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module type S = sig
  module Core : CoreSig.S
  val get : (Core.P.t * bool) option -> Core.t -> Core.result
end

module Make(Core : CoreSig.S) : S with module Core = Core
