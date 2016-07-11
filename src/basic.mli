(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module Make
    (Var : ExtSigs.VAR_SIG)(R : ExtSigs.R_SIG)(Ex : ExtSigs.EX_SIG) : sig

  module Core : CoreSig.SIG
    with module Var=Var and module R=R and module Ex=Ex

  module Assert : AssertBounds.SIG with module Core := Core
  module Solve  : SolveBounds.SIG  with module Core := Core
  module Result : Result.SIG  with module Core := Core

end

