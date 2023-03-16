(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module type S = sig
  module Core : CoreSig.S
  val get : (Core.P.t * bool) option -> Core.t -> Core.result
  (** [get (objective, is_max_bounded) env] retrieves the result from a simplex [env].

      This needs to be called after the system [env] has been solved by {!module-SolveBounds}.

      @param objective the optimization objective if any
      @param is_max_bounded whether the result is bounded
      @param env the simplex environment (system of linear inequalities) containing the solution

      @return solution that satisfies the constraints if any
  *)


end

module Make(Core : CoreSig.S) : S with module Core = Core
