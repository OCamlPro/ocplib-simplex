(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

open ExtSigs

module Make
  (Var : Variables)
  (R   : Rationals)
  (Ex  : Explanations)
  : CoreSig.S
    with module Var = Var
     and module R = R
     and module Ex = Ex
