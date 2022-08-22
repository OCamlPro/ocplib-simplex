(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)


(** The main entry point of the library.
    It provides a functor building each key module of OcplibSimplex. *)

module Make
    (Var : ExtSigs.VAR_SIG)(R : ExtSigs.R_SIG)(Ex : ExtSigs.EX_SIG) : sig

  (** The core module defines the different data types used by the project
      and some functions to handle them. *)
  module Core : CoreSig.SIG
    with module Var=Var and module R=R and module Ex=Ex

  module Assert : AssertBounds.SIG with module Core := Core
  module Solve  : SolveBounds.SIG  with module Core := Core
  module Result : Result.SIG  with module Core := Core

end

