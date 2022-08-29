
open Simplex

let large i = Sim.Core.R2.of_r (Num.Int i)
let upper i = Sim.Core.R2.upper (Num.Int i)
let lower i = Sim.Core.R2.lower (Num.Int i)

let bnd r e = {Sim.Core.bvalue = r; explanation = e}

let () =
  let sim = Sim.Core.empty ~is_int:true ~check_invs:true ~debug:0 in

  let x_y = Sim.Core.P.from_list ["x", Rat.one; "y", Rat.one] in

  (* s == x + y >= 10
  let sim = Sim.Assert.poly sim x_y "s" (large 10) Ex.empty None Ex.empty in
  *)

  (* x <= 5 *)
  let sim, _ =
    Sim.Assert.var sim "x"
      ~min:(bnd (large 3) (Ex.singleton "x>=3"))
  in

  (* s == x + y <= 10 *)
  let sim, _ =
    Sim.Assert.poly sim x_y "s"
      ~max:(bnd (large 10) (Ex.singleton "x+y<=10")) in

  let max_hdr pb fmt () =
    Format.fprintf fmt "### Problem 'max %a'@." Sim.Core.P.print pb
  in

  aux (max_hdr x_y) (Sim.Solve.maximize sim x_y);
  aux (max_hdr y1) (Sim.Solve.maximize sim y1);
  aux (max_hdr ym1) (Sim.Solve.maximize sim ym1)
