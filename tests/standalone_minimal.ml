
open Simplex

let () =
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