(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module type SIG = sig
  module Core : CoreSig.SIG
  val get : (Core.P.t * bool) option -> Core.t -> Core.result
end

module Make(Core : CoreSig.SIG) : SIG with module Core = Core = struct

  module Core = Core
  open Core

  let explain_poly non_basic p ex is_lower =
    let invert_sign = if is_lower then 1 else -1 in
    P.fold
      (fun x coef ex ->
         let c = invert_sign * R.sign coef in
         assert (c <> 0);
         let xi, _ = try MX.find x non_basic with Not_found -> assert false in
         let ex, optimum =
           if c > 0 then Ex.union ex xi.max_ex, xi.maxi
           else Ex.union ex xi.min_ex, xi.mini
         in
         assert (equals_optimum xi.value optimum);
         ex
      )p ex

  let get_unsat_core s env =
    try
      let si, p = MX.find s env.basic in
      if not (consistent_bounds si) then Ex.union si.min_ex si.max_ex
      else match si.vstatus with
        | ValueOK -> assert false
        | LowerKO -> explain_poly env.non_basic p si.min_ex true
        | UpperKO -> explain_poly env.non_basic p si.max_ex false
    with Not_found ->
      let si, _ = MX.find s env.non_basic in
      if not (consistent_bounds si) then Ex.union si.min_ex si.max_ex
      else assert false

  let get_int_solution env =
    let is_int_sol = ref true in
    let sol =
      MX.fold (fun x (xi,_) sol ->
          let re, im = xi.value in
          assert (R.sign im = 0);
          is_int_sol := !is_int_sol && R.is_int re;
          (x, re) :: sol
        ) env.non_basic []
    in
    let sol =
      MX.fold (fun x (xi, _) sol ->
          let re, im = xi.value in
          assert (R.sign im = 0);
          is_int_sol := !is_int_sol && R.is_int re;
          (x, re) :: sol
        )env.basic sol
    in
    let slake = env.slake in
    let sol_slk, sol = List.partition (fun (x, _) -> MX.mem x slake) sol in
    { main_vars = sol; slake_vars = sol_slk; int_sol = !is_int_sol }



  let eval_eps eps inf_r inf_d sup_r sup_d =
    let c = R.compare inf_r sup_r in
    assert (c <= 0);
    if c = 0 || R.compare inf_d sup_d <= 0 then eps
    else R.min eps (R.div (R.sub sup_r inf_r) (R.sub inf_d sup_d))

  let get_rat_solution =
    let compute_epsilon mp eps =
      MX.fold (fun _ (i, _) eps ->
          match i.mini , i.maxi with
          | None, None -> eps

          | Some (min_r, min_d), None ->
            let q1,q2 = i.value in
            assert (R.equal min_d R.zero || R.equal min_d R.one);
            let eps = eval_eps eps min_r min_d q1 q2 in
            assert (R.compare eps R.zero > 0);
            eps

          | None, Some (max_r, max_d) ->
            let q1,q2 = i.value in
            assert (R.equal max_d R.zero || R.equal max_d R.m_one);
            let eps = eval_eps eps q1 q2 max_r max_d in
            assert (R.compare eps R.zero > 0);
            eps

          | Some (min_r, min_d), Some (max_r, max_d) ->
            let q1,q2 = i.value in
            assert (R.equal min_d R.zero || R.equal min_d R.one);
            assert (R.equal max_d R.zero || R.equal max_d R.m_one);
            let eps = eval_eps eps min_r min_d q1 q2 in
            let eps = eval_eps eps q1 q2 max_r max_d in
            assert (R.compare eps R.zero > 0);
            eps
        ) mp eps
    in
    let compute_solution slake mp eps acc =
      MX.fold
        (fun x (info, _) (m, s) ->
         let q1,q2 = info.value in
         let q = R.add q1 (R.mult q2 eps) in
         assert (not (violates_min_bound (q, R.zero) info.mini));
         assert (not (violates_max_bound (q, R.zero) info.maxi));
         if MX.mem x slake then m, (x, q) :: s else (x,q) :: m, s
        )mp acc
    in
    fun env ->
      let eps = compute_epsilon env.basic R.one in
      let eps = compute_epsilon env.non_basic eps in
      let acc = compute_solution env.slake env.basic eps ([], []) in
      let m,s = compute_solution env.slake env.non_basic eps acc in
      { main_vars = m ; slake_vars = s; int_sol = false }


  let get_solution env =
    if env.is_int then get_int_solution env else get_rat_solution env

  let get_max_info {non_basic; _} p =
    let (max_v, symb), reason =
      Core.P.fold
        (fun x c (max_v, reason) ->
           let xi, _ = try MX.find x non_basic with Not_found -> assert false in
           let ex, bnd =
             if R.sign c > 0 then xi.max_ex, xi.maxi
             else xi.min_ex, xi.mini
           in
           let value = xi.value in
           assert (equals_optimum value bnd);
           R2.add max_v (R2.mult_by_const c value), Ex.union reason ex
        )p (R2.zero, Ex.empty)
    in
    {max_v; is_le = R.is_zero symb; reason}


  let get opt env =
    match env.status with
    | UNK     -> Unknown
    | UNSAT s -> Unsat (lazy (get_unsat_core s env))
    | SAT     ->
      match opt with
      | None -> Sat (lazy (get_solution env))
      | Some(_, false) -> Unbounded (lazy (get_solution env))
      | Some(p, true)  -> Max (lazy(get_max_info env p), lazy(get_solution env))

end
