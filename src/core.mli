(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module Make
  (Var : ExtSigs.VAR_SIG)
  (R   : ExtSigs.R_SIG)
  (Ex  : ExtSigs.EX_SIG)
  : CoreSig.SIG with module Var = Var and module R = R and module Ex = Ex
