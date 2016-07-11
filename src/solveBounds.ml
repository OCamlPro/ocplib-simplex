(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)


module type SIG = sig
  module Core : CoreSig.SIG
  val solve : Core.t -> Core.t
end

module Make(Core : CoreSig.SIG) : SIG with module Core = Core = struct

  module Core = Core
  module Result = Result.Make(Core)
  open Core


  let gauss_pivot s p x c =
    let p, _ = P.replace s R.m_one (P.remove x p) in
    let c = R.div R.m_one c in
    if R.is_one c then p
    else P.fold (fun y d q -> fst (P.replace y (R.mult c d) q)) p P.empty

  exception Out of Var.t * R.t * var_info * SX.t

  let look_for_next_pivot si pi non_basic =
    let status = si.vstatus in
    let is_lower =
      match status with
      | ValueOK -> assert false | LowerKO -> 1 | UpperKO -> -1
    in
    try
      P.iter
        (fun x coef ->
          let xi,use = try MX.find x non_basic with Not_found -> assert false in
          let c = is_lower * R.sign coef in
          assert (c <> 0);
          if c > 0 && not (equals_optimum xi.value xi.maxi) then
            raise (Out (x, coef, xi, use));
          if c < 0 && not (equals_optimum xi.value xi.mini) then
            raise (Out (x, coef, xi, use));
        )pi;
      None
    with Out (x, c, xi, use) -> Some (x, c, xi, use)


  let adapt_valuation_of_newly_basic old_si new_si old_xi c_x =
    let diff = R2.div_by_const c_x (R2.sub new_si.value old_si.value) in
    { old_xi with value = R2.add diff old_xi.value }


(*
  let string_of_var_status stt =
    match stt with
    | P.Removed -> "Removed"
    | P.New     -> "New"
    | P.Exists   -> "Exists"
*)

  let rec solve_rec env round =
    Core.debug (Format.sprintf "[solve] round %d" round) env Result.get;
    Core.check_invariants env Result.get;
    if SX.is_empty env.fixme then {env with status = SAT}
    else
      let s = SX.choose env.fixme in
      let fixme = SX.remove s env.fixme in
      let si, p = try MX.find s env.basic with Not_found -> assert false in
      match look_for_next_pivot si p env.non_basic with
      | None -> {env with fixme = SX.empty; status = UNSAT s}

      | Some(x, c, xi, use_x) ->
        if env.debug > 1 then
          Format.eprintf "pivot basic %a and non-basic %a@."
            Var.print s Var.print x;
        let basic = MX.remove s env.basic in
        let non_basic = MX.remove x env.non_basic in
        let q = gauss_pivot s p x c in
        assert (SX.mem s use_x);
        let use_x = SX.remove s use_x in
        let old_si = si in
        let si, changed = Core.ajust_value_of_non_basic si in
        assert (changed);

        let old_xi = xi in
        let xi = adapt_valuation_of_newly_basic old_si si xi c in
        let xi = ajust_status_of_basic xi in
        let diff_xi_val = R2.sub xi.value old_xi.value in
        let fixme = (* do this earlier to detect bad pivots *)
          if xi.vstatus == ValueOK then fixme
          else SX.add x fixme
        in
        let non_basic =
          P.fold
            (fun y _ non_basic ->
              let yi, use_y =
                try MX.find y non_basic with Not_found -> assert false in
              MX.add y (yi, SX.add x (SX.remove s use_y)) non_basic
            )(P.remove s q) non_basic
        in

        let non_basic = MX.add s (si, SX.add x use_x) non_basic in

        let basic, non_basic, fixme =
          SX.fold
            (fun t (basic, non_basic, fixme) ->
              let ti0, r = try MX.find t basic with Not_found -> assert false in
              let cx = try P.find x r with Not_found -> assert false in
              (*should update_ti*)
              let diff_cx = R2.mult_by_const cx diff_xi_val in
              let ti = {ti0 with value = R2.add ti0.value diff_cx} in
              let ti = ajust_status_of_basic ti in
              let r', changed = P.subst x q r in
              (*
              Format.eprintf "update poly of basic %a@." Var.print t;
              List.iter
                (fun (v, vstt) ->
                  Format.eprintf "   %a ---> %s@."
                    Var.print v (string_of_var_status vstt);
                )changed;
              *)
              let non_basic =
                List.fold_left
                  (fun non_basic (z, vstt) ->
                    match vstt with
                    | P.Exists -> non_basic
                    | P.New ->
                      let zi, use_z =
                        try MX.find z non_basic with Not_found -> assert false
                      in
                      MX.add z (zi, SX.add t use_z) non_basic

                    | P.Removed ->
                      if Var.compare z x = 0 then non_basic
                      else
                        let zi, use_z =
                          try MX.find z non_basic with Not_found -> assert false
                        in
                        MX.add z (zi, SX.remove t use_z) non_basic
                  )non_basic changed
              in
             (*val subst : Var.t -> t -> t -> t * (Var.t * var_status) list*)

              let basic = MX.add t (ti, r') basic in
              let fixme =
                if ti.vstatus == ValueOK then
                  if ti0.vstatus == ValueOK then fixme
                  else SX.remove t fixme
                else SX.add t fixme
              in
              basic, non_basic, fixme
            )use_x (basic, non_basic, fixme)
        in

        (* ... *)

        let basic = MX.add x (xi, q) basic in

        (* ... *)

        let env = {env with fixme; basic; non_basic} in
        solve_rec env (round + 1)




  let solve env =
    Core.debug "[entry of solve]" env Result.get;
    Core.check_invariants env Result.get;
    let env =
      match env.Core.status with
      | Core.UNSAT _ | Core.SAT -> env
      | Core.UNK -> solve_rec env 1
    in
    Core.debug "[exit of solve]" env Result.get;
    Core.check_invariants env Result.get;
    env

end
