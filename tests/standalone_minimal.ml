
open Simplex

let () =
  let sim = Sim.Core.empty ~is_int:true ~check_invs:true in
  let zero = Sim.Core.R2.zero in
  let m_one = (Sim.Core.R2.of_r Rat.m_one) in

  (* x >= 0 *)
  let sim, _ =
    Sim.Assert.var sim "x"
      (Some {Sim.Core.bvalue = zero; explanation = Ex.singleton "x>=0"})
      None
  in

  (* y >= 0 *)
  let sim, _ =
    Sim.Assert.var sim "y"
      (Some {Sim.Core.bvalue = zero; explanation = Ex.singleton "y>=0"})
      None
  in
  let x_y = Sim.Core.P.from_list ["x", Rat.one; "y", Rat.one] in

  (* z == x + y <= -1 *)
  let sim, _ =
    Sim.Assert.poly sim x_y "z"
      None
      (Some {Sim.Core.bvalue = m_one; explanation = Ex.singleton "x+y<=-1"})
  in
  let sim = Sim.Solve.solve sim in

  aux (
    fun fmt () ->
      Format.fprintf fmt "\n### Test Solve Unsat@."
  ) (sim, None)
