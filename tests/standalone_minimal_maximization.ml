
open Simplex

let () =
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
