(******************************************************************************)
(*                              ocp-fun-sim                                   *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module Ty = Core.Make(Var)(Rat)(Ex)
module AB = AssertBounds.Make(Ty)

module Basic = OcpFunSim.MakeBasic(Var)(Rat)(Ex)
