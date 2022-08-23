(******************************************************************************)
(*                              ocp-fun-sim                                   *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module Ty = OcplibSimplex.Core.Make(Var)(Rat)(Ex)
module AB = OcplibSimplex.AssertBounds.Make(Ty)

module Basic = OcplibSimplex.Basic.Make(Var)(Rat)(Ex)
