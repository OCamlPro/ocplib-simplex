(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

open ExtSigs

(** The main entry point of the library.
    It provides a functor building each key module of OcplibSimplex. *)

module Make
    (Var : Variables)(R : Rationals)(Ex : Explanations) : sig

  (** The core module defines the different data types used by the project
      and some functions to handle them. *)
  module Core : CoreSig.S
    with module Var=Var
     and module R=R
     and module Ex=Ex

  module Assert : AssertBounds.S with module Core := Core
  module Solve  : SolveBounds.S  with module Core := Core
  module Result : Result.S       with module Core := Core

end

