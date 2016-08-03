(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module type SIG = sig

  module Core : CoreSig.SIG

  val var :
    Core.t ->
    Core.Var.t ->
    Core.bound ->
    Core.Ex.t ->
    Core.bound ->
    Core.Ex.t ->
    Core.t

  val poly :
    Core.t ->
    Core.P.t ->
    Core.Var.t ->
    Core.bound ->
    Core.Ex.t ->
    Core.bound ->
    Core.Ex.t ->
    Core.t

end

module Make(Core : CoreSig.SIG) : SIG with module Core = Core = struct

    module Core  = Core
    module Result = Result.Make(Core)
    open Core

    let new_status_basic stt fixme s info consistent_bnds =
      let has_bad_value = info.vstatus != ValueOK in
      match stt, consistent_bnds with
      | UNSAT _, _    -> stt, fixme
      | _, false      ->
        UNSAT s, SX.empty

      | UNK, true ->
        stt, if has_bad_value then SX.add s fixme else SX.remove s fixme

      | SAT, _ ->
        assert (fixme == SX.empty);
        if has_bad_value then UNK, SX.add s fixme else stt, fixme

    let assert_basic_var env x mini min_ex maxi max_ex =
      let info, poly =
        try
          let info, poly = MX.find x env.basic in
          let info = set_min_bound info mini min_ex in
          let info = set_max_bound info maxi max_ex in
          info, poly
        with Not_found -> assert false
      in
      let status, fixme =
        new_status_basic env.status env.fixme x info (consistent_bounds info)
      in
      {env with basic = MX.add x (info, poly) env.basic; status; fixme}

    (* *)

    let new_status_non_basic x stt ({mini; maxi; value; _} as info) =
      assert (not (violates_min_bound value mini));
      assert (not (violates_max_bound value maxi));
      match stt with
      | UNSAT _ -> stt
      | SAT | UNK  -> if consistent_bounds info then stt else UNSAT x

    let adapt_values_of_basic_vars env _old _new x use =
      let {basic; _} = env in
      let diff = R2.sub _new _old in
      SX.fold
        (fun s env ->
           let info, p = try MX.find s basic with Not_found -> assert false in
           let c_x = try P.find x p with Not_found -> assert false in
           let info =
             {info with value = R2.add info.value (R2.mult_by_const c_x diff)}
           in
           let info = ajust_status_of_basic info in
           let status, fixme =
             new_status_basic env.status env.fixme s info true
           in
           {env with status; fixme; basic = MX.add s (info, p) env.basic}
        )use env

    let assert_non_basic_var env x mini min_ex maxi max_ex =
      let info, use =
        try MX.find x env.non_basic
        with Not_found -> empty_info, SX.empty
      in
      let info = set_min_bound info mini min_ex in
      let info = set_max_bound info maxi max_ex in
      let old_val = info.value in
      let info, changed = ajust_value_of_non_basic info in
      let status = new_status_non_basic x env.status info in
      let env =
        {env with
          non_basic = MX.add x (info, use) env.non_basic; status}
      in
      if not changed then env
      else adapt_values_of_basic_vars env old_val info.value x use


    (* exported function: check_invariants called before and after *)
    let var env x mini ex_min maxi ex_max =
      debug "[entry of assert_var]" env (Result.get None);
      check_invariants env (Result.get None);
      let env =
        if MX.mem x env.basic then
          assert_basic_var env x mini ex_min maxi ex_max
        else
          assert_non_basic_var env x mini ex_min maxi ex_max
      in
      debug "[exit of assert_var]" env (Result.get None);
      check_invariants env (Result.get None);
      env

    let register_slake slk p env =
      if MX.mem slk env.slake then env, false
      else {env with slake = MX.add slk p env.slake}, true

    let update_use is_fresh_slk x_status slk use =
      match x_status with
      | P.Exists ->
        assert (SX.mem slk use);
        use

      | P.Removed ->
        assert (SX.mem slk use);
        SX.remove slk use

      | P.New ->
        assert (not is_fresh_slk || not (SX.mem slk use));
        SX.add slk use

    let update_use_list is_fresh modified_stt slk non_basic =
      List.fold_left
        (fun non_basic (x, x_status) ->
          try
            let i, u = MX.find x non_basic in
            MX.add x (i, update_use is_fresh x_status slk u) non_basic
          with Not_found -> assert false
        )non_basic modified_stt

    let normalize_polynomial is_fresh slk p env =
      P.fold
        (fun x c (q, env) ->
          try
            let info, use = MX.find x env.non_basic in
            let new_q, x_status = P.accumulate x c q in
            let use = update_use is_fresh x_status slk use in
            new_q, {env with non_basic = MX.add x (info, use) env.non_basic}

          with Not_found ->
            try
              let _ , p_of_x = MX.find x env.basic in
              let new_q, modified_stt = P.append q c p_of_x in
              new_q,
              {env with
               non_basic =
                 update_use_list is_fresh modified_stt slk env.non_basic}

            with Not_found ->
              (* var not initied -> new non_basic *)
              let env =
                assert_non_basic_var env x None Ex.empty None Ex.empty in
              let new_q, x_status = P.replace x c q in
              assert (x_status == P.New);
              let info, use =
                try MX.find x env.non_basic
                with Not_found -> assert false
              in
              let use = update_use is_fresh x_status slk use in
              new_q,
              { env with
                non_basic = MX.add x (info, use) env.non_basic}
        )p (P.empty, env)


    (* exported function: check_invariants called before and after *)
    let poly env p slk mini min_ex maxi max_ex =
      debug "[entry of assert_poly]" env (Result.get None);
      check_invariants env (Result.get None);
      assert (P.is_polynomial p);
      let env, is_fresh = register_slake slk p env in
      let info, is_basic, env =
        try (* non basic existing var ? *)
          let info, use = MX.find slk env.non_basic in
          assert (
            let np, _ = normalize_polynomial is_fresh slk p env in
            let zp, _ = P.accumulate slk R.m_one np in
            P.is_empty zp
          );
          let info = set_min_bound info mini min_ex in
          let info = set_max_bound info maxi max_ex in
          let old_val = info.value in
          let info, changed = ajust_value_of_non_basic info in
          let env =
            {env with non_basic = MX.add slk (info, use) env.non_basic}
          in
          info, false,
          if not changed then env
          else adapt_values_of_basic_vars env old_val info.value slk use

        with Not_found ->
          try (* basic existing var ? *)
            let info, poly = MX.find slk env.basic in
            assert (
              let np, _ = normalize_polynomial is_fresh slk p env in
              P.equal np poly
            );
            let info = set_min_bound info mini min_ex in
            let info = set_max_bound info maxi max_ex in
            info, true, {env with basic = MX.add slk (info, poly) env.basic}

          with Not_found -> (* fresh basic var *)
            assert (is_fresh);
            let np, env = normalize_polynomial is_fresh slk p env in
            let info = {empty_info with value = evaluate_poly env np} in
            let info = set_min_bound info mini min_ex in
            let info = set_max_bound info maxi max_ex in
            info, true, {env with basic = MX.add slk (info, np) env.basic}
      in
      let status, fixme =
        if is_basic then
          new_status_basic env.status env.fixme slk info
            (consistent_bounds info)
        else
          new_status_non_basic slk env.status info, env.fixme
      in
      let env = {env with status; fixme } in
      debug "[exit of assert_poly]" env (Result.get None);
      check_invariants env (Result.get None);
      env


  end
(* end of functor Make *)
