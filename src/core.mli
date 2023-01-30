(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

open ExtSigs

val src : Logs.src

module Make
  (Var : Variables)
  (R   : Rationals)
  (Ex  : Explanations)
  : CoreSig.S
    with module Var = Var
     and module R = R
     and module Ex = Ex

module MakeExpert
    (Var : Variables)
    (R   : Rationals)
    (Ex  : Explanations)
    (R2  : Rat2.SIG with module R = R)
    (P  : Polys.SIG with module Var = Var and module R = R)
    (MX : MapSig with type key = Var.t)
    (SX : SetSig with type elt = Var.t)
  : CoreSig.S with module Var=Var and module R=R and module Ex=Ex and
  module P = P and module MX = MX and module SX = SX
(** Same than Make but allows to choose the implementation of polynomials, maps
    and sets *)
