(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

open ExtSigs

module Make (Var : Variables) (R : Rationals) (Ex : Explanations) : sig
  module Core :
    CoreSig.S
      with module Var = Var
       and type R.t = R.t
       and type V.t = R.t
       and module Ex = Ex

  module Assert : AssertBounds.S with module Core := Core
  module Solve : SolveBounds.S with module Core := Core
  module Result : Result.S with module Core := Core
end = struct
  module Core = Core.Make (Var) (R) (Ex)
  module Assert = AssertBounds.Make (Core)
  module Solve = SolveBounds.Make (Core)
  module Result = Result.Make (Core)
end
