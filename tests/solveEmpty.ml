module Sim = InstantiateBasicFunctor.Basic

let () =
  let sim = Sim.Core.empty ~is_int:false ~check_invs:true ~debug:1 in
  let sim = Sim.Solve.solve sim in
  ignore (sim);
  match Sim.Result.get None sim with
  | Sim.Core.Unknown -> assert false
  | Sim.Core.Unsat _ -> assert false
  | Unbounded _ -> assert false
  | Max _ -> assert false
  | Sim.Core.Sat _ -> Format.printf "[test solveEmpty]: result is Sat@."
