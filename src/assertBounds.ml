(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module type S = sig
  module Core : CoreSig.S

  val var :
    Core.t -> ?min:Core.bound -> ?max:Core.bound -> Core.Var.t -> Core.t * bool

  val poly :
    Core.t ->
    Core.P.t ->
    ?min:Core.bound ->
    ?max:Core.bound ->
    Core.Var.t ->
    Core.t * bool
end

module Make (Core : CoreSig.S) : S with module Core = Core = struct
  module Core = Core
  module Result = Result.Make (Core)
  open Core

  let empty_info value =
    { mini = None; maxi = None; value; vstatus = ValueOK; empty_dom = false }

  (* If the environment works on integers and bounds are strict,
     we shift the bound so that it is a large bound. Same goes
     on rational bounds in an integer simplex.
     Ex:
     * x > 5 + Ɛ -> x >= 6
     * x > 4/3 -> x >= 2
  *)
  let update_bound (get_closer : R2.t -> R2.t) (env : Core.t)
      (bnd : bound option) =
    match bnd with
    | Some b when env.is_int -> Some { b with bvalue = get_closer b.bvalue }
    | other -> other

  let update_min_bound = update_bound R2.ceiling
  let update_max_bound = update_bound R2.floor

  let new_status_basic stt fixme s info consistent_bnds =
    let has_bad_value = info.vstatus != ValueOK in
    match (stt, consistent_bnds) with
    | UNSAT _, _ -> (stt, fixme)
    | _, false -> (UNSAT s, SX.empty)
    | UNK, true ->
        (stt, if has_bad_value then SX.add s fixme else SX.remove s fixme)
    | SAT, _ ->
        assert (fixme == SX.empty);
        if has_bad_value then (UNK, SX.add s fixme) else (stt, fixme)

  let assert_basic_var env x mini maxi =
    let info, poly, changed =
      try
        let info, poly = MX.find x env.basic in
        let info, chang1 = set_min_bound info mini in
        let info, chang2 = set_max_bound info maxi in
        (info, poly, chang1 || chang2)
      with Not_found -> assert false
    in
    let status, fixme =
      new_status_basic env.status env.fixme x info (consistent_bounds info)
    in
    ( { env with basic = MX.add x (info, poly) env.basic; status; fixme },
      changed )

  (* *)

  let new_status_non_basic x stt fixme ({ mini; maxi; value; _ } as info) =
    match stt with
    | UNSAT _ -> (stt, fixme)
    | (SAT | UNK) when consistent_bounds info ->
        assert (not (violates_min_bound value mini));
        assert (not (violates_max_bound value maxi));
        assert (stt != SAT || fixme == SX.empty);
        (stt, fixme)
    | SAT | UNK -> (UNSAT x, SX.empty)

  let adapt_values_of_basic_vars env _old _new x use =
    let { basic; _ } = env in
    let diff = R2.sub _new _old in
    SX.fold
      (fun s env ->
        let info, p = try MX.find s basic with Not_found -> assert false in
        let c_x = try P.find x p with Not_found -> assert false in
        let info =
          { info with value = R2.add info.value (R2.mult_by_const c_x diff) }
        in
        let info = ajust_status_of_basic info in
        let status, fixme = new_status_basic env.status env.fixme s info true in
        { env with status; fixme; basic = MX.add s (info, p) env.basic })
      use env

  let assert_non_basic_var env x mini maxi =
    let info, use =
      try MX.find x env.non_basic
      with Not_found -> (empty_info R2.zero, SX.empty)
    in
    let info, chang1 = set_min_bound info mini in
    let info, chang2 = set_max_bound info maxi in
    let old_val = info.value in
    let info, changed = ajust_value_of_non_basic info in
    let status, fixme = new_status_non_basic x env.status env.fixme info in
    let env =
      { env with non_basic = MX.add x (info, use) env.non_basic; status; fixme }
    in
    let env =
      if not changed then env
      else adapt_values_of_basic_vars env old_val info.value x use
    in
    (env, chang1 || chang2)

  (* exported function: check_invariants called before and after *)
  let var env ?min ?max x =
    debug "[entry of assert_var]" env (Result.get None);
    check_invariants env (Result.get None);
    let mini = update_min_bound env min in
    let maxi = update_max_bound env max in
    let env, changed =
      if MX.mem x env.basic then assert_basic_var env x mini maxi
      else assert_non_basic_var env x mini maxi
    in
    debug "[exit of assert_var]" env (Result.get None);
    check_invariants env (Result.get None);
    (env, changed)

  let register_slake slk p env =
    if MX.mem slk env.slake then (env, false)
    else ({ env with slake = MX.add slk p env.slake }, true)

  let update_use is_fresh_slk x_status slk use =
    match x_status with
    | P.Exists ->
        assert (SX.mem slk use);
        use
    | P.Removed ->
        assert (SX.mem slk use);
        SX.remove slk use
    | P.New ->
        assert ((not is_fresh_slk) || not (SX.mem slk use));
        SX.add slk use

  let update_use_list is_fresh modified_stt slk non_basic =
    List.fold_left
      (fun non_basic (x, x_status) ->
        try
          let i, u = MX.find x non_basic in
          MX.add x (i, update_use is_fresh x_status slk u) non_basic
        with Not_found -> assert false)
      non_basic modified_stt

  let normalize_polynomial is_fresh slk p env =
    P.fold
      (fun x c (q, env) ->
        try
          let info, use = MX.find x env.non_basic in
          let new_q, x_status = P.accumulate x c q in
          let use = update_use is_fresh x_status slk use in
          (new_q, { env with non_basic = MX.add x (info, use) env.non_basic })
        with Not_found -> (
          try
            let _, p_of_x = MX.find x env.basic in
            let new_q, modified_stt = P.append q c p_of_x in
            ( new_q,
              {
                env with
                non_basic =
                  update_use_list is_fresh modified_stt slk env.non_basic;
              } )
          with Not_found ->
            (* var not initied -> new non_basic *)
            let env, chang = assert_non_basic_var env x None None in
            assert (not chang);
            let new_q, x_status = P.replace x c q in
            assert (x_status == P.New);
            let info, use =
              try MX.find x env.non_basic with Not_found -> assert false
            in
            let use = update_use is_fresh x_status slk use in
            (new_q, { env with non_basic = MX.add x (info, use) env.non_basic })))
      p (P.empty, env)

  (* exported function: check_invariants called before and after *)
  let poly env p ?min ?max slk =
    debug "[entry of assert_poly]" env (Result.get None);
    check_invariants env (Result.get None);
    assert (P.is_polynomial p);
    let mini = update_min_bound env min in
    let maxi = update_max_bound env max in
    let env, is_fresh = register_slake slk p env in
    let info, is_basic, env, change =
      try
        (* non basic existing var ? *)
        let info, use = MX.find slk env.non_basic in
        assert (
          let np, _ = normalize_polynomial is_fresh slk p env in
          let zp, _ = P.accumulate slk R.m_one np in
          P.is_empty zp);
        let info, chang1 = set_min_bound info mini in
        let info, chang2 = set_max_bound info maxi in
        let old_val = info.value in
        let info, changed = ajust_value_of_non_basic info in
        let env =
          { env with non_basic = MX.add slk (info, use) env.non_basic }
        in
        let env =
          if not changed then env
          else adapt_values_of_basic_vars env old_val info.value slk use
        in
        (info, false, env, chang1 || chang2)
      with Not_found -> (
        try
          (* basic existing var ? *)
          let info, poly = MX.find slk env.basic in
          assert (
            let np, _ = normalize_polynomial is_fresh slk p env in
            P.equal np poly);
          let info, chang1 = set_min_bound info mini in
          let info, chang2 = set_max_bound info maxi in
          ( info,
            true,
            { env with basic = MX.add slk (info, poly) env.basic },
            chang1 || chang2 )
        with Not_found ->
          (* fresh basic var *)
          assert is_fresh;
          let np, env = normalize_polynomial is_fresh slk p env in
          let info = empty_info (evaluate_poly env np) in
          let info, chang1 = set_min_bound info mini in
          let info, chang2 = set_max_bound info maxi in
          ( info,
            true,
            { env with basic = MX.add slk (info, np) env.basic },
            chang1 || chang2 ))
    in
    let status, fixme =
      if is_basic then
        new_status_basic env.status env.fixme slk info (consistent_bounds info)
      else new_status_non_basic slk env.status env.fixme info
    in
    let env = { env with status; fixme } in
    debug "[exit of assert_poly]" env (Result.get None);
    check_invariants env (Result.get None);
    (env, change)
end
(* end of functor Make *)
