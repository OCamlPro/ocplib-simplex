(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module Sim = Simplex.Basic
module Rat = Simplex.Rat
module Ex = Simplex.Ex

let aux header (sim, opt) =
  Format.printf "%a" header ();
  Format.printf "%a"
    (Sim.Core.print (Sim.Result.get opt sim)) sim

let test_empty () =
  let sim = Sim.Core.empty ~is_int:false ~check_invs:true ~debug:1 in
  let sim = Sim.Solve.solve sim in
  aux (
    fun fmt () ->
      Format.fprintf fmt "\n### Test Solve Empty@."
  ) (sim, None)

let test_unsat () =
  let sim = Sim.Core.empty ~is_int:true ~check_invs:true ~debug:1 in
  let zero = Some (Rat.zero, Rat.zero) in
  let m_one = Some (Rat.m_one, Rat.zero) in

  (* x >= 0 *)
  let sim, _ =
    Sim.Assert.var sim "x" zero (Ex.singleton "x>=0") None Ex.empty
  in

  (* y >= 0 *)
  let sim, _ =
    Sim.Assert.var sim "y" zero (Ex.singleton "y>=0") None Ex.empty
  in
  let x_y = Sim.Core.P.from_list ["x", Rat.one; "y", Rat.one] in

  let sim, _ =
    Sim.Assert.poly sim x_y "z" None Ex.empty m_one (Ex.singleton "x+y<=-1")
  in
  let sim = Sim.Solve.solve sim in

  aux (
    fun fmt () ->
      Format.fprintf fmt "\n### Test Solve Unsat@."
  ) (sim, None)

let test_maximization () =
  let sim = Sim.Core.empty ~is_int:true ~check_invs:true ~debug:0 in

  let x_y = Sim.Core.P.from_list ["x", Rat.one; "y", Rat.one] in
  let ten = Some (Num.Int (10), Num.Int 0) in
  let three = Some (Num.Int (3), Num.Int 0) in

  let y1 = Sim.Core.P.from_list ["y", Rat.one] in
  let ym1 = Sim.Core.P.from_list ["y", Rat.m_one] in

  (* s == x + y >= 10
     let sim = Sim.Assert.poly sim x_y "s" ten Ex.empty None Ex.empty in
  *)

  (* x <= 5 *)
  let sim, _ =
    Sim.Assert.var sim "x" three (Ex.singleton "x>=3") None Ex.empty
  in

  (* s == x + y <= 10 *)
  let sim, _ =
    Sim.Assert.poly sim x_y "s" None Ex.empty ten (Ex.singleton "x+y<=10")
  in

  let max_hdr pb fmt () =
    Format.fprintf fmt "### Problem 'max %a'@." Sim.Core.P.print pb
  in

  aux (max_hdr x_y) (Sim.Solve.maximize sim x_y);
  aux (max_hdr y1) (Sim.Solve.maximize sim y1);
  aux (max_hdr ym1) (Sim.Solve.maximize sim ym1)

let () =
  List.iter (fun f -> f ())
    [test_empty; test_unsat; test_maximization;]
