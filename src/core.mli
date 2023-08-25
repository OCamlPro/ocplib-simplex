(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

open ExtSigs

val src : Logs.src

module Make (Var : Variables) (R : Rationals) (Ex : Explanations) :
  CoreSig.S
    with module Var = Var
     and type R.t = R.t
     and type V.t = R.t
     and module Ex = Ex

(** Same than Make but allows to choose the implementation of polynomials, maps
    and sets *)
module MakeExpert
    (Var : Variables)
    (R : Coefs)
    (V : Value with type r = R.t)
    (Ex : Explanations)
    (R2 : Rat2.SIG with module R = R and module V = V)
    (P : Polys.SIG with module Var = Var and module R = R)
    (MX : MapSig with type key = Var.t)
    (SX : SetSig with type elt = Var.t) :
  CoreSig.S
    with module Var = Var
     and module R = R
     and module V = V
     and module Ex = Ex
     and module P = P
     and module MX = MX
     and module SX = SX
