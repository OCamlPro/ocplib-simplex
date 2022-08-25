(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

open Format

module Make
    (Var : ExtSigs.VAR_SIG)
    (R   : ExtSigs.R_SIG)
    (Ex  : ExtSigs.EX_SIG)
  : CoreSig.SIG with module Var=Var and module R=R and module Ex=Ex = struct

  module Var = Var
  module R   = R
  module Ex  = Ex

  module R2  = Rat2.Make(R)

  module P : Polys.SIG
    with module Var = Var and module R = R = Polys.Make(Var) (R)

  module MX : Map.S with type key = Var.t = Map.Make(Var)
  module SX : Set.S with type elt = Var.t = Set.Make(Var)

  module SP : Set.S with type elt = P.t = Set.Make(P)

  type bound = {
    bvalue : R2.t;
    explanation : Ex.t
  }

  type value_status = ValueOK | LowerKO | UpperKO

  type var_info =
    {
      mini     : bound option;
      maxi     : bound option;
      value    : R2.t;
      vstatus  : value_status;
      empty_dom : bool;
    }

  type solution = {
    main_vars : (Var.t * R.t) list;
    slake_vars : (Var.t * R.t) list;
    int_sol : bool; (* always set to false for rational simplexes*)
    epsilon : R.t;
  }

  type maximum =
    { max_v : bound;
      is_le : bool; (* bool = true <-> large bound *)
    }

  type result =
    | Unknown
    | Unsat of Ex.t Lazy.t
    | Sat of solution Lazy.t
    | Unbounded of solution Lazy.t
    | Max of maximum Lazy.t * solution Lazy.t

  type simplex_status = UNK | UNSAT of Var.t | SAT

  type t =
    {
      basic     : (var_info * P.t) MX.t;
      non_basic : (var_info * SX.t) MX.t;
      slake     : P.t MX.t;
      fixme     : SX.t;
      is_int    : bool;
      status    : simplex_status;
      debug     : int;
      check_invs: bool;
      nb_pivots : int ref;
    }

  let empty ~is_int ~check_invs ~debug =
    {
      basic     = MX.empty;
      non_basic = MX.empty;
      slake     = MX.empty;
      fixme     = SX.empty;
      status    = UNK;
      is_int;
      check_invs;
      debug;
      nb_pivots = ref 0
    }

  let on_integers env = env.is_int

  let equals_optimum (b : R2.t) (opt : bound option) = match opt with
    | None -> false
    | Some opt -> R2.compare b opt.bvalue = 0

  let violates_min_bound (b : R2.t) (mn : bound option) =
    match mn with
    | None -> false (* min is -infinity *)
    | Some min -> R2.compare b min.bvalue < 0

  let violates_max_bound (b : R2.t) (mx : bound option) =
    match mx with
    | None -> false (* max is +infinity *)
    | Some max -> R2.compare b max.bvalue > 0

  let consistent_bound_value min max = R2.compare min max <= 0

  let consistent_bounds_aux (mini : bound option) (maxi : bound option) =
    match mini, maxi with
    | None, None | Some _, None | None, Some _ -> true
    | Some {bvalue = min; _}, Some {bvalue = max; _} -> consistent_bound_value min max

  let consistent_bounds info =
    consistent_bounds_aux info.mini info.maxi

  let set_min_bound info (bnd : bound option) =
    match bnd with
    | None -> info, false
    | Some _new ->
      let mini = info.mini in
      if violates_min_bound _new.bvalue mini || equals_optimum _new.bvalue mini then
        info, false
      else
        let empty_dom = not (consistent_bounds_aux bnd info.maxi) in
        let i' =
          if violates_min_bound info.value bnd then
            {info with mini = bnd; vstatus = LowerKO; empty_dom}
          else
            {info with mini = bnd; empty_dom}
        in i', true

  let set_max_bound info (bnd : bound option) =
    match bnd with
    | None -> info, false
    | Some _new ->
      let maxi = info.maxi in
      if violates_max_bound _new.bvalue maxi || equals_optimum _new.bvalue maxi then
        info, false
      else
        let empty_dom = not (consistent_bounds_aux info.mini bnd) in
        let i' =
          if violates_max_bound info.value bnd then
            {info with maxi = bnd; vstatus = UpperKO; empty_dom}
          else
            {info with maxi = bnd; empty_dom}
        in
        i', true

  let ajust_value_of_non_basic info =
    if info.empty_dom then begin
      assert (info.vstatus != ValueOK);
      info, false (* not changed if not sat_bnds *)
    end
    else
      match info.vstatus with
      | ValueOK ->
        info, false
      | UpperKO ->
        {info with
         vstatus = ValueOK;
         value = match info.maxi with
           | None -> assert false
           | Some bnd -> bnd.bvalue},
        true

      | LowerKO ->
        {info with
         vstatus = ValueOK;
         value = match info.mini with
           | None -> assert false
           | Some bnd -> bnd.bvalue},
        true


  let ajust_status_of_basic info =
    let _new =
      if violates_min_bound info.value info.mini then LowerKO
      else if violates_max_bound info.value info.maxi then UpperKO
      else ValueOK
    in
    if info.vstatus == _new then info else {info with vstatus = _new}


  let evaluate_poly {non_basic; _} p =
    P.fold
      (fun x c acc ->
         let {value = v; _}, _ =
           try MX.find x non_basic with Not_found -> assert false
         in
         R2.add acc (R2.mult_by_const c v)
      )p R2.zero


  let poly_of_slake env slk =
    try Some (MX.find slk env.slake) with Not_found -> None

  let expl_of_min_bound vinfo =
    match vinfo.mini with
    | None -> Ex.empty
    | Some {explanation; _} -> explanation

  let expl_of_max_bound vinfo =
    match vinfo.maxi with
    | None -> Ex.empty
    | Some {explanation; _} -> explanation

  (* debug functions *)

  module Debug = struct

    let string_of_status = function
      | ValueOK -> "OK       "
      | UpperKO -> "KO(Upper)"
      | LowerKO -> "KO(Lower)"

    let print_min_bound fmt (i : var_info) =
      match i.mini with
      | None -> fprintf fmt "-∞ <"
      | Some min -> fprintf fmt "%a <=" R2.print min.bvalue
    let print_max_bound fmt i =
      match i.maxi with
      | None -> fprintf fmt "< +∞"
      | Some max ->
        fprintf fmt "<= %a" R2.print max.bvalue

    let re_computed_status_of_info v =
      if violates_min_bound v.value v.mini then LowerKO
      else if violates_max_bound v.value v.maxi then UpperKO
      else ValueOK

    let print_bounds_and_values fmt mx =
      MX.iter
        (fun x (info, _) ->
           let v = info.value in
           let comp_status = re_computed_status_of_info info in
           Format.fprintf fmt
             "%a   [ %a == %a ]   %a   (computed %s) (flag %s)@."
             print_min_bound info
             Var.print x
             R2.print v
             print_max_bound info
             (string_of_status comp_status)
             (string_of_status info.vstatus);
           assert (info.empty_dom || comp_status == info.vstatus);
        )mx

    let print_uses fmt non_basic =
      MX.iter
        (fun x (_, use_x) ->
           Format.fprintf fmt
             "variables that use %a are:%a@."
             Var.print x
             (fun fmt s -> SX.iter(fprintf fmt " %a," Var.print) s) use_x;
        )non_basic

    let print_solution =
      let aux fmt l =
        List.iter
          (fun (x, q) ->
             fprintf fmt "  %a --> %a@." Var.print x R.print q;
          )l
      in
      fun is_int fmt s ->
        if is_int
        then fprintf fmt "  (int solution ? %b)@."
            s.int_sol;
        aux fmt s.main_vars;
        aux fmt s.slake_vars

    let print_result is_int fmt status =
      match status with
      | Unknown  -> fprintf fmt "Unknown"
      | Sat s ->
        fprintf fmt "Sat:@.%a@." (print_solution is_int) (Lazy.force s)

      | Unsat ex  ->
        fprintf fmt "Unsat:%a@." Ex.print (Lazy.force ex)

      | Unbounded s ->
        fprintf fmt "Unbounded:@.%a@."  (print_solution is_int) (Lazy.force s)

      | Max(mx, s) ->
        let mx = Lazy.force mx in
        fprintf fmt "Max: (v=%a, is_le=%b, ex=%a)@.%a@."
          R2.print mx.max_v.bvalue mx.is_le Ex.print mx.max_v.explanation
          (print_solution is_int) (Lazy.force s)

    let print_fixme fmt sx =
      match SX.elements sx with
      | [] -> fprintf fmt "  (fixme is empty)@.";
      | l -> List.iter (fprintf fmt " >> %a@." Var.print) l

    let print_matrix fmt env =
      MX.iter
        (fun x (_, p) ->
           fprintf fmt "%a = %a@."
             Var.print x
             P.print p)
        env.basic

    let print result fmt env =
      Format.fprintf fmt
        "== begin <simplex env> ========================================@.";
      fprintf fmt "on integers ? %b@." env.is_int;
      fprintf fmt "--- values of non-basic ---------------------------@.";
      print_bounds_and_values fmt env.non_basic;
      fprintf fmt "---------------------------------------------------@.";
      fprintf fmt "--- values of basic -------------------------------@.";
      fprintf fmt "---------------------------------------------------@.";
      print_bounds_and_values fmt env.basic;
      fprintf fmt "--- matrix ----------------------------------------@.";
      print_matrix fmt env;
      fprintf fmt "---------------------------------------------------@.";
      fprintf fmt "--- sets of uses ----------------------------------@.";
      print_uses fmt env.non_basic;
      fprintf fmt "---------------------------------------------------@.";
      fprintf fmt "--- basic variables in fixme ----------------------@.";
      print_fixme fmt env.fixme;
      fprintf fmt "---------------------------------------------------@.";
      fprintf fmt "--- simplex status --------------------------------@.";
      fprintf fmt "%a@." (print_result env.is_int) result;
      fprintf fmt
        "== end <simplex env> ==========================================@.";
  end
  (* end of module Debug *)

  let print = Debug.print


  let debug msg env get_result =
    if env.debug > 0 then
      let result = get_result env in
      Format.eprintf "@.%s@.%a@." msg (print result) env

    (*
      check invariants of the simplex:
      these invariants are listed in extra/simplexe_invariants.txt
    *)

  let get_all_polys env =
    let sp = MX.fold (fun _ (_,p) sp -> SP.add p sp) env.basic SP.empty in
    MX.fold (fun _ p sp -> SP.add p sp) env.slake sp

  let get_all_vars env all_polys =
    let sx = env.fixme in
    let sx = MX.fold (fun x _ sx -> SX.add x sx) env.basic sx in
    let sx =
      MX.fold (fun x (_, use) sx ->
          SX.union use (SX.add x sx)) env.non_basic sx
    in
    let sx = MX.fold (fun x _ sx -> SX.add x sx) env.slake sx in
    SP.fold (P.fold (fun x _ sx -> SX.add x sx)) all_polys sx

  let info_of x env =
    try fst (MX.find x env.non_basic)
    with Not_found ->
    try fst (MX.find x env.basic) with Not_found -> assert false

  let _01__check_type is_int all_vars =
    SX.iter (fun x -> assert (is_int == Var.is_int x)) all_vars

  let _02__check_basic_non_basic_disjoint env =
    MX.iter (fun x _ -> assert (not (MX.mem x env.non_basic))) env.basic;
    MX.iter (fun x _ -> assert (not (MX.mem x env.basic))) env.non_basic

  let _03__check_vars_of_polys env polys =
    SP.iter
      (P.iter
         (fun x c ->
            assert (R.sign c <> 0);
            assert (MX.mem x env.basic || MX.mem x env.non_basic)
         ))polys

  let _04_05_06__check_use env =
    MX.iter
      (fun x (_, use) ->
         SX.iter (fun s ->
             assert (not (MX.mem s env.non_basic)); (* 04 *)
             try assert (P.mem x (snd (MX.find s env.basic))) (*05*)
             with Not_found -> assert false
           ) use
      )env.non_basic;
    MX.iter
      (fun s (_, p) ->
         P.iter (fun x _ ->
             try assert (SX.mem s (snd (MX.find x env.non_basic))); (*06*)
             with Not_found -> assert false
           )p;
      )env.basic

  let _07__values_ok_for_non_basic_vars env =
    MX.iter
      (fun _ (info, _) ->
         if consistent_bounds info then begin
           assert (not (violates_min_bound info.value info.mini));
           assert (not (violates_max_bound info.value info.maxi));
         end
         else
           match env.status with
           | UNSAT _ -> ()
           | SAT | UNK -> assert false
      )env.non_basic

  let _08_09__values_ok_when_sat env result =
    let check mx int_sol =
      MX.iter
        (fun _ (info, _) ->
           let v = info.value in
           assert (not (violates_min_bound v info.mini));
           assert (not (violates_max_bound v info.maxi));
           assert (not int_sol || R.is_int v.R2.v && R.is_zero v.R2.offset)
        ) mx
    in
    match result with
    | Unsat _ | Unknown -> ()
    | Sat s | Unbounded s | Max(_,s) ->
      let s = Lazy.force s in
      check env.basic s.int_sol; check env.non_basic s.int_sol


  let _10_11__check_handling_strict_ineqs env =
    let is_int = env.is_int in
    let aux _ (info, _) =
      begin
        match info.mini with
        | None -> ()
        | Some m ->
          let i = m.bvalue.R2.offset in
          assert (not is_int || R.is_zero i);
          assert (is_int || R.is_zero i || R.is_one i);
      end;
      begin
        match info.maxi with
        | None -> ()
        | Some m ->
          let i = m.bvalue.R2.offset in
          assert (not is_int || R.is_zero i);
          assert (is_int || R.is_zero i || R.is_m_one i);
      end
    in
    MX.iter aux env.basic;
    MX.iter aux env.non_basic

  let _12__check_solution_when_sat =
    let aux l env =
      List.iter
        (fun (x, v) ->
           let info = info_of x env in
           let v2 = R2.of_r v in
           assert (not (violates_min_bound v2 info.mini));
           assert (not (violates_max_bound v2 info.maxi));
        )l
    in fun env result ->
      match result with
      | Unsat _ | Unknown -> ()
      | Sat s | Unbounded s | Max(_,s) ->
        let s = Lazy.force s in
        let v = List.length s.main_vars + List.length s.slake_vars in
        let w = MX.cardinal env.non_basic + MX.cardinal env.basic in
        assert (
          if v <> w then
            eprintf "model length = %d, but basic + non_basic = %d@." v w;
          v = w);
        aux s.main_vars env;
        aux s.slake_vars env

  let _13__check_reason_when_unsat env =
    if env.debug > 0 then
      Format.eprintf
        "@.[check-invariants] _13__check_reason_when_unsat: TODO@.@."

  let _14_15__fixme_is_subset_of_basic env =
    SX.iter
      (fun x ->
         try
           let info, _ = MX.find x env.basic in
           assert
             ((violates_min_bound info.value info.mini) ||
              (violates_max_bound info.value info.maxi)); (*15*)
         with Not_found -> assert false (*14*)

      ) env.fixme

  let _16__fixme_containts_basic_with_bad_values_if_not_unsat
      env all_vars result =
    match result with
    | Unsat _ | Unbounded _ | Max _ -> ()
    | Unknown | Sat _ ->
      SX.iter
        (fun x ->
           if not (SX.mem x env.fixme) then
             let info = info_of x env in
             assert (not (violates_min_bound info.value info.mini));
             assert (not (violates_max_bound info.value info.maxi))
        )all_vars

  let _17__fixme_is_empty_if_not_unknown env result =
    match result with
    | Unknown -> ()
    | Unsat _ | Sat _ | Unbounded _ | Max _ -> assert (SX.is_empty env.fixme)

  let _18__vals_of_basic_vars_computation env =
    MX.iter
      (fun _ ({value = s; _}, p) ->
         let vp = evaluate_poly env p in
         assert (R2.equal vp s);
      )env.basic

  let _19__check_that_vstatus_are_well_set env =
    let aux _ (info, _) =
      if info.empty_dom then
        assert (info.vstatus != ValueOK)
      else
        let vmin = violates_min_bound info.value info.mini in
        let vmax = violates_max_bound info.value info.maxi in
        match info.vstatus with
        | ValueOK -> assert (not vmin); assert(not vmax);
        | UpperKO -> assert (not vmin); assert(vmax);
        | LowerKO -> assert (vmin); assert(not vmax);
    in
    MX.iter aux env.basic;
    MX.iter aux env.non_basic

  let _20__bounds_are_consistent_if_not_unsat env result =
    match result with
    | Unsat _ -> ()
    | Unknown | Sat _ | Unbounded _ | Max _ ->
      let aux _ (info, _) = assert (consistent_bounds info) in
      MX.iter aux env.basic;
      MX.iter aux env.non_basic

  let _21__check_coherence_of_empty_dom =
    let aux mx =
      MX.iter
        (fun _ (info, _) ->
           assert (consistent_bounds info == not info.empty_dom);
           if info.empty_dom then
             assert (violates_min_bound info.value info.mini ||
                     violates_max_bound info.value info.maxi);
        )mx
    in
    fun env ->
      aux env.non_basic;
      aux env.basic

  let check_invariants env get_result =
    if env.check_invs then
      let polys = get_all_polys env in
      let all_vars = get_all_vars env polys in
      let result = get_result env in
      _01__check_type env.is_int all_vars;
      _02__check_basic_non_basic_disjoint env;
      _03__check_vars_of_polys env polys;
      _04_05_06__check_use env;
      _07__values_ok_for_non_basic_vars env;
      _08_09__values_ok_when_sat env result;
      _10_11__check_handling_strict_ineqs env;
      _12__check_solution_when_sat env result;
      _13__check_reason_when_unsat env;
      _14_15__fixme_is_subset_of_basic env;
      _16__fixme_containts_basic_with_bad_values_if_not_unsat
        env all_vars result;
      _17__fixme_is_empty_if_not_unknown env result;
      _18__vals_of_basic_vars_computation env;
      _19__check_that_vstatus_are_well_set env;
      _20__bounds_are_consistent_if_not_unsat env result;
      (*_21__check_coherence_of_empty_dom env;*)


end
