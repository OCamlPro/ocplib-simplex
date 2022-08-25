
open Simplex

let () =
  let sim = Sim.Core.empty ~is_int:false ~check_invs:true in
  let sim = Sim.Solve.solve sim in
  aux (
    fun fmt () ->
      Format.fprintf fmt "\n### Test Solve Empty@."
  ) (sim, None)
