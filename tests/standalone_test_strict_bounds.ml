open Simplex

let sep () =
  Format.printf "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+@."

let pp_epsilon fmt (max_v, eps) =
  let pp =
    if Sim.Core.R2.is_pure_rational max_v.Sim.Core.bvalue then Format.ifprintf
    else Format.fprintf
  in
  pp fmt "(epsilon: %a)" Rat.print eps

let aux sim opt_p =
  let sim, opt = Sim.Solve.maximize sim opt_p in
  sep ();
  Format.printf "The problem 'max %a' ...@." Sim.Core.P.print opt_p;
  (match Sim.Result.get opt sim with
  | Sim.Core.Unknown -> assert false
  | Sim.Core.Sat _ -> assert false
  | Sim.Core.Unsat ex ->
      Format.printf " is unsat (reason = %a)@." Ex.print (Lazy.force ex)
  | Sim.Core.Unbounded _ -> Format.printf " is unbounded@."
  | Sim.Core.Max (mx, sol) ->
      let { Sim.Core.max_v; is_le } = Lazy.force mx in
      let sol = Lazy.force sol in
      Format.printf " has an upper bound: %a (is_le = %b)(reason: %a)%a@."
        Sim.Core.R2.print max_v.Sim.Core.bvalue is_le Ex.print
        max_v.Sim.Core.explanation pp_epsilon
        (max_v, sol.Sim.Core.epsilon));
  sep ();
  Format.printf "@."

let large i = Sim.Core.R2.of_r (Q.of_int i)
let upper i = Sim.Core.R2.upper (Q.of_int i)
let lower i = Sim.Core.R2.lower (Q.of_int i)
let bnd r e = { Sim.Core.bvalue = r; explanation = e }
let r_two = Rat.add Rat.one Rat.one

let () =
  let sim = Sim.Core.empty ~is_int:true ~check_invs:false in

  let x_m_y = Sim.Core.P.from_list [ ("x", Rat.one); ("y", Rat.m_one) ] in
  let tx_ty = Sim.Core.P.from_list [ ("x", r_two); ("y", r_two) ] in

  (* 3 < y < 5*)
  let sim, _ =
    Sim.Assert.var sim "y"
      ~min:(bnd (lower 3) (Ex.singleton "y>3"))
      ~max:(bnd (upper 5) (Ex.singleton "y<5"))
  in

  (* 3 < x < 4 *)
  let sim, _ =
    Sim.Assert.var sim "x"
      ~min:(bnd (lower 3) (Ex.singleton "x>3"))
      ~max:(bnd (large 5) (Ex.singleton "x<=5"))
  in

  (* 0 <= x - y *)
  let sim, _ =
    Sim.Assert.poly sim x_m_y "s'" ~min:(bnd (large 1) (Ex.singleton "x-y>=1"))
  in

  (* s == 2x + 2y <= 20 *)
  let sim, _ =
    Sim.Assert.poly sim tx_ty "s"
      ~max:(bnd (large 20) (Ex.singleton "2x+2y<=20"))
  in

  aux sim tx_ty
