(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)


module type SIG = sig
  module Core : CoreSig.SIG
  val solve : Core.t -> Core.t
  val maximize : Core.t -> Core.P.t -> Core.t * (Core.P.t * bool) option
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

  (* TODO : review and improve this function *)

  let rec solve_rec env round =
    Core.debug (Format.sprintf "[solve] round %d" round) env (Result.get None);
    Core.check_invariants env (Result.get None);
    if SX.is_empty env.fixme then {env with status = SAT}
    else
      let s = SX.choose env.fixme in
      let fixme = SX.remove s env.fixme in
      let si, p = try MX.find s env.basic with Not_found -> assert false in
      match look_for_next_pivot si p env.non_basic with
      | None -> {env with fixme = SX.empty; status = UNSAT s}

      | Some(x, c, xi, use_x) ->
        if env.debug > 1 then
          Format.eprintf "[solve_rec] pivot basic %a and non-basic %a@."
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
        env.nb_pivots := !(env.nb_pivots) + 1;
        solve_rec env (round + 1)


  let solve env =
    Core.debug "[entry of solve]" env (Result.get None);
    Core.check_invariants env (Result.get None);
    let env =
      match env.Core.status with
      | Core.UNSAT _ | Core.SAT -> env
      | Core.UNK -> solve_rec env 1
    in
    Core.debug "[exit of solve]" env (Result.get None);
    Core.check_invariants env (Result.get None);
    env






  let non_basic_to_maximize {non_basic=n_b; _} opt =
    let acc = ref None in
    try
      P.iter
        (fun x c ->
           let xi, use = try MX.find x n_b with Not_found -> assert false in
           let sg = R.sign c in
           if sg > 0 && not (equals_optimum xi.value xi.maxi) ||
              sg < 0 && not (equals_optimum xi.value xi.mini) then begin
             acc := Some (x, c, xi, use, sg > 0);
             raise Exit
           end
        )opt;
      !acc
    with Exit -> !acc


  type 'a maximiza_basic =
  | Free
  | Stuck
  | Progress of 'a


  let basic_var_to_pivot_for_maximization =
    let choose_best_pivot acc s si p c_px bnd_opt is_min =
      match bnd_opt with
      | None ->
        if !acc = Stuck then acc := Free (* !!! to check *)

      | Some bnd ->
        let tmp = if is_min then R2.sub si.value bnd else R2.sub bnd si.value in
        let ratio = R2.div_by_const (R.abs c_px) tmp in
        begin
          match !acc with
          | Free | Stuck ->
            acc := Progress (ratio, s, si, p, c_px, bnd, is_min)

          | Progress (r_old,_,_,_,_,_,_) ->
            if R2.compare r_old ratio > 0 then
              acc := Progress (ratio, s, si, p, c_px, bnd, is_min)
        end;
        if R2.is_zero ratio then raise Exit (* in the case, the pivot is found*)
    in
    fun {basic; _} x use_x should_incr_x ->
      (* Initially, we assume that we are stuck, unless, use_x is empty *)
      let acc = ref (if SX.is_empty use_x then Free else Stuck) in
      try
        SX.iter
          (fun s ->
             let si, p = try MX.find s basic with Not_found -> assert false in
             let c_px = try P.find x p with Not_found -> assert false in
             let sg = R.sign c_px in
             assert (sg <> 0);
             match should_incr_x, sg > 0, si.mini, si.maxi with
             | true , true , _, mx_opt ->
               (* by increasing x, s will increase and max(s) <> +infty *)
               choose_best_pivot acc s si p c_px mx_opt false

             | true , false, mn_opt, _ ->
               (* by increasing x, s will decrease and min(s) <> -infty *)
               choose_best_pivot acc s si p c_px mn_opt true

             | false, true , mn_opt, _ ->
               (* by decreasing x, s will decreease and min(s) <> -infty *)
               choose_best_pivot acc s si p c_px mn_opt true

             | false, false, _, mx_opt ->
               (* by decreasning x, s will increase and max(s) <> +infty *)
               choose_best_pivot acc s si p c_px mx_opt false

             (*| true, true, _, None
             | true, false, None, _
             | false, true, None, _
             | false, false, _, None ->
               (* for the cases where max or max = infty, we keep acc unchanged.
                  if acc = None at the end,  the problem is unbounded *)
               ()
             *)
          )use_x;
        !acc
      with Exit -> !acc


  let can_fix_valuation_without_pivot should_incr xi ratio_opt =
    if should_incr then
      match xi.maxi, ratio_opt with
      | None, _ -> None
      | Some bnd, Some ratio ->
        let diff = R2.sub bnd xi.value in
        if R2.compare diff ratio < 0 then Some ({xi with value = bnd}, diff)
        else None

      | Some bnd, None ->
        let diff = R2.sub bnd xi.value in
        Some ({xi with value = bnd}, diff)

    else
      match xi.mini, ratio_opt with
      | None, _ -> None
      | Some bnd, Some ratio ->
        let diff = R2.sub xi.value bnd in
        if R2.compare diff ratio < 0 then Some ({xi with value = bnd}, diff)
        else None

      | Some bnd, None ->
        let diff = R2.sub xi.value bnd in
        Some ({xi with value = bnd}, diff)


  let update_valuation_without_pivot
      ({basic; non_basic; _ } as env) x use_x new_xi diff _should_incr =
    let non_basic = MX.add x (new_xi, use_x) non_basic in
    let diff = if _should_incr then diff else R2.minus diff in
    let basic =
      SX.fold
        (fun s basic ->
           let si, p = try MX.find s basic with Not_found -> assert false in
           let cx = try P.find x p with Not_found -> assert false in
           assert (not (R.is_zero cx));
           let delta = R2.mult_by_const cx diff in
           let si = {si with value = R2.add si.value delta} in
           MX.add s (si, p) basic
        )use_x basic
    in
    {env with basic; non_basic}

  let rec maximize_rec env opt rnd =
    if env.debug > 1 then
      Format.eprintf "[maximize_rec] round %d // OPT = %a@." rnd P.print opt;
    Core.debug
      (Format.sprintf "[maximize_rec] round %d" rnd) env (Result.get None);
    Core.check_invariants env (Result.get None);
    match non_basic_to_maximize env opt with
    | None ->
      if env.debug > 1 then Format.eprintf "max reached@.";
      rnd, env, Some (opt, true) (* max reached *)

    | Some (_x, _c, _xi, _use_x, _should_incr) ->
      if env.debug > 1 then
        Format.eprintf "pivot non basic var %a ?@." Var.print _x;
      match basic_var_to_pivot_for_maximization env _x _use_x _should_incr with
      | Free ->
        if env.debug > 1 then
          Format.eprintf
            "non basic %a not constrained by basic vars: Set it to max@."
            Var.print _x;
        begin
          match can_fix_valuation_without_pivot _should_incr _xi None with
          | Some (new_xi, diff) ->
            if env.debug > 1 then
              Format.eprintf
                "No --> I can set value of %a to min/max WO pivot@."
                Var.print _x;
            let env, opt =
              update_valuation_without_pivot
                env _x _use_x new_xi diff _should_incr, opt
            in
            (* no pivot *)
            maximize_rec env opt (rnd + 1)
          | None ->
            if env.debug > 1 then
              Format.eprintf "no pivot finally(no upper bnd), pb unbounded@.";
            rnd, env, Some (opt, false) (* unbounded *)
        end
      | Stuck ->
        if env.debug > 1 then
          Format.eprintf "no pivot finally, pb unbounded@.";
        rnd, env, Some (opt, false) (* unbounded *)

      | Progress (ratio, s, si, p, c_px, bnd, _is_min) ->
        if env.debug > 1 then
          Format.eprintf "pivot with basic var %a ?@." Var.print s;
        let env, opt =
          match
            can_fix_valuation_without_pivot _should_incr _xi (Some ratio) with
          | Some (new_xi, diff) ->
            if env.debug > 1 then
              Format.eprintf
                "No --> I can set value of %a to min/max WO pivot@."
                Var.print _x;
            update_valuation_without_pivot
              env _x _use_x new_xi diff _should_incr, opt

          | None ->
            let x = _x in
            let c = c_px in
            let use_x = _use_x in
            let xi = _xi in

            if env.debug > 1 then
              Format.eprintf "[maximize_rec] pivot basic %a and non-basic %a@."
                Var.print s Var.print x;
            let basic = MX.remove s env.basic in
            let non_basic = MX.remove x env.non_basic in
            let q = gauss_pivot s p x c in
            assert (SX.mem s use_x);
            let use_x = SX.remove s use_x in
            let old_si = si in

            let si = {si with value = bnd} in (* difference wrt solve *)
            (*
               because the code of solve below, assumes that value in si
               violotas a bound
            let si, changed = Core.ajust_value_of_non_basic si in
            assert (changed);
            *)

            let old_xi = xi in
            let xi = adapt_valuation_of_newly_basic old_si si xi c in
            let xi = ajust_status_of_basic xi in
            let diff_xi_val = R2.sub xi.value old_xi.value in
            assert(xi.vstatus == ValueOK);
            let non_basic =
              P.fold
                (fun y _ non_basic ->
                   let yi, use_y =
                     try MX.find y non_basic with Not_found -> assert false in
                   MX.add y (yi, SX.add x (SX.remove s use_y)) non_basic
                )(P.remove s q) non_basic
            in

            let non_basic = MX.add s (si, SX.add x use_x) non_basic in

            let basic, non_basic =
              SX.fold
                (fun t (basic, non_basic) ->
                   let ti0, r =
                     try MX.find t basic with Not_found -> assert false in
                   let cx = try P.find x r with Not_found -> assert false in
                   (*should update_ti*)
                   let diff_cx = R2.mult_by_const cx diff_xi_val in
                   let ti = {ti0 with value = R2.add ti0.value diff_cx} in
                   let ti = ajust_status_of_basic ti in
                   let r', changed = P.subst x q r in

                   let non_basic =
                     List.fold_left
                       (fun non_basic (z, vstt) ->
                          match vstt with
                          | P.Exists -> non_basic
                          | P.New ->
                            let zi, use_z =
                              try MX.find z non_basic
                              with Not_found -> assert false
                            in
                            MX.add z (zi, SX.add t use_z) non_basic

                          | P.Removed ->
                            if Var.compare z x = 0 then non_basic
                            else
                              let zi, use_z =
                                try MX.find z non_basic
                                with Not_found -> assert false
                              in
                              MX.add z (zi, SX.remove t use_z) non_basic
                       )non_basic changed
                   in

                   let basic = MX.add t (ti, r') basic in
                   assert(ti.vstatus == ValueOK);
                   basic, non_basic
                )use_x (basic, non_basic)
            in

            (* ... *)
            let basic = MX.add x (xi, q) basic in

            (* ... *)
            {env with basic; non_basic}, (fst (P.subst x q opt))



        in
        env.nb_pivots := !(env.nb_pivots) + 1;
        maximize_rec env opt (rnd + 1)


  let maximize env opt0 =
    let env = solve env in
    match env.status with
    | UNK -> assert false
    | UNSAT _ -> env, None
    | SAT ->
      if env.debug > 1 then
        Format.eprintf "[maximize] pb SAT! try to maximize %a@." P.print opt0;
      let {basic; non_basic; _} = env in
      let unbnd = ref false in
      let opt =
        P.fold
          (fun x c acc ->
             if MX.mem x non_basic then fst (P.accumulate x c acc)
             else
               try fst (P.append acc c (snd (MX.find x basic)))
               with Not_found ->
                 unbnd := true;
                 fst (P.accumulate x c acc)
          )opt0 P.empty
      in
      if !unbnd then env, Some (opt, false) (* unbounded *)
      else
        begin
          if env.debug > 1 then Format.eprintf "start maximization@.";
          let rnd, env, is_max = maximize_rec env opt 1 in
          Core.check_invariants env (Result.get is_max);
          if env.debug > 1 then
            Format.eprintf "[maximize] pb SAT! Max found ? %b for %a == %a@."
              (is_max != None) P.print opt0 P.print opt;
          if env.debug > 1 then
            Format.eprintf "maximization done after %d steps@." rnd;
          env, is_max
        end




end
