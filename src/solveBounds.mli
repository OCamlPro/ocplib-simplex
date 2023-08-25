(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

val src : Logs.src

module type S = sig
  module Core : CoreSig.S

  val solve : Core.t -> Core.t
  (** [solve env] solves the linear inequalities in the simplex [env].

    Use {!module-Result} to retrieve the solution if any.
   *)

  val maximize : Core.t -> Core.P.t -> Core.t * (Core.P.t * bool) option
  (** [maximize env objective] finds the maximum value of [objective] that satisfies the system of linear inequalities in [env].

    Use {!module-Result} to retrieve the solution if any.

    @return env,(objective,is_max_bounded)
   *)
end

module Make (Core : CoreSig.S) : S with module Core = Core
