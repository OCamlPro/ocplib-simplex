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
           if c > 0 then Ex.union ex (Core.expl_of_max_bound xi), xi.maxi
           else Ex.union ex (Core.expl_of_min_bound xi), xi.mini
         in
         assert (equals_optimum xi.value optimum);
         ex
      )p ex

  let get_unsat_core s env =
    try
      let si, p = MX.find s env.basic in
      if not (consistent_bounds si)
      then Ex.union
          (Core.expl_of_min_bound si)
          (Core.expl_of_max_bound si)
      else match si.vstatus with
        | ValueOK -> assert false
        | LowerKO -> explain_poly env.non_basic p (Core.expl_of_min_bound si) true
        | UpperKO -> explain_poly env.non_basic p (Core.expl_of_max_bound si) false
    with Not_found ->
      let si, _ = MX.find s env.non_basic in
      assert (not (consistent_bounds si));
      Ex.union
        (Core.expl_of_min_bound si)
        (Core.expl_of_max_bound si)

  let get_int_solution env =
    let is_int_sol = ref true in
    let sol =
      MX.fold (fun x (xi,_) sol ->
          let v = xi.value in
          assert (R2.is_pure_rational v);
          is_int_sol := !is_int_sol && R.is_int v.R2.v;
          (x, v.R2.v) :: sol
        ) env.non_basic []
    in
    let sol =
      MX.fold (fun x (xi, _) sol ->
          let v = xi.value in
          assert (R2.is_pure_rational v);
          is_int_sol := !is_int_sol && R.is_int v.R2.v;
          (x, v.R2.v) :: sol
        )env.basic sol
    in
    let slake = env.slake in
    let sol_slk, sol = List.partition (fun (x, _) -> MX.mem x slake) sol in
    { main_vars = sol; slake_vars = sol_slk; int_sol = !is_int_sol; epsilon = R.zero}



  let eval_eps
      (eps : R.t)
      (inf : R2.t)
      (sup : R2.t) =
    let {R2.v = inf_r; offset = inf_d} : R2.t = inf in
    let {R2.v = sup_r; offset = sup_d} : R2.t = sup in
    let c = R.compare inf_r sup_r in
    assert (c <= 0);
    if c = 0 || R.compare inf_d sup_d <= 0 then eps
    else R.min eps (R.div (R.sub sup_r inf_r) (R.sub inf_d sup_d))

  let eps_for_mini
      (i : Core.var_info)
      (min : R2.t)
      (eps : R.t) : R.t =
    assert (R2.is_pure_rational min || R.equal min.R2.offset R.one);
    let eps = eval_eps eps min i.value in
    eps

  let eps_for_maxi
      (i : Core.var_info)
      (max : R2.t)
      (eps : R.t) : R.t =
    assert (R2.is_pure_rational max || R.equal max.R2.offset R.m_one);
    let eps = eval_eps eps i.value max in
    eps

  let get_rat_solution =
    let compute_epsilon mp eps =
      MX.fold (fun _ (i, _) eps ->
          let eps' =
            match i.mini , i.maxi with
            | None, None -> eps

            | Some min, None ->
              eps_for_mini i min.bvalue eps

            | None, Some max ->
              eps_for_maxi i max.bvalue eps

            | Some min, Some max ->
              eps
              |> eps_for_mini i min.bvalue
              |> eps_for_maxi i max.bvalue
          in
          assert (R.compare eps' R.zero > 0);
          eps'
        ) mp eps
    in
    let compute_solution slake mp eps acc =
      MX.fold
        (fun x (info, _) (m, s) ->
           let {R2.v = q1; offset = q2} = info.value in
           let q = R.add q1 (R.mult q2 eps) in
           let q2 = R2.of_r q in
           assert (not (violates_min_bound q2 info.mini));
           assert (not (violates_max_bound q2 info.maxi));
           if MX.mem x slake then m, (x, q) :: s else (x,q) :: m, s
        )mp acc
    in
    fun env ->
      let eps = compute_epsilon env.basic R.one in
      let eps = compute_epsilon env.non_basic eps in
      let acc = compute_solution env.slake env.basic eps ([], []) in
      let m,s = compute_solution env.slake env.non_basic eps acc in
      { main_vars = m ; slake_vars = s; int_sol = false; epsilon = eps }


  let get_solution env =
    if env.is_int then get_int_solution env else get_rat_solution env

  let get_max_info {non_basic; _} p =
    let max_v : Core.bound =
      Core.P.fold
        (fun x c (max_v : Core.bound) ->
           let xi, _ = try MX.find x non_basic with Not_found -> assert false in
           let bnd =
             if R.sign c > 0
             then xi.maxi
             else xi.mini
           in
           let ex = match bnd with
             | None -> Core.Ex.empty
             | Some {explanation; _} -> explanation
           in
           let value = xi.value in
           assert (equals_optimum value bnd);
           {
             bvalue = R2.add max_v.bvalue (R2.mult_by_const c value);
             explanation = Ex.union max_v.explanation ex
           }
        )
        p
        {bvalue = R2.zero; explanation = Ex.empty}
    in
    {max_v; is_le = R.is_zero max_v.bvalue.R2.offset}


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
