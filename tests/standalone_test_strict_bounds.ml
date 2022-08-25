(*== Build the example with:

"ocamlopt -o standalone_test_strict_bounds \
-I `ocamlfind query ocplib-simplex` ocplibSimplex.cmxa nums.cmxa \
standalone_test_strict_bounds"

if the lib is installed, or with:

"ocamlopt -o standalone_minimal_maximization -I ../src ocplibSimplex.cmxa \
nums.cmxa standalone_minimal_maximization.ml"

if the lib is built but not installed

==*)

module Var = struct
  type t = string
  let print fmt s = Format.fprintf fmt "%s" s
  let compare = String.compare
  let is_int _ = true
end

module Ex = struct
  module S = Set.Make(String)
  include S
  let print fmt s = match elements s with
    | [] -> Format.fprintf fmt "()"
    | e::l ->
      Format.fprintf fmt "%s" e;
      List.iter (Format.fprintf fmt ", %s") l
end

module Rat = struct
  open Num
  type t = num
  let add = ( +/ )
  let mult = ( */ )
  let compare = compare_num
  let equal = ( =/ )
  let zero = Int 0
  let one = Int 1
  let m_one = Int (-1)
  let is_zero n = n =/ zero
  let to_string = string_of_num

  let print fmt t = Format.fprintf fmt "%s" (to_string t)
  let is_int = is_integer_num
  let div = (//)
  let sub = (-/)
  let is_one v = v =/ Int 1
  let is_m_one v = v =/ Int (-1)
  let sign = sign_num
  let min = min_num
  let abs = abs_num
  let minus = minus_num

  let floor = floor_num
  let ceiling = ceiling_num
end


module Sim = OcplibSimplex.Basic.Make(Var)(Rat)(Ex)

let sep () =
  Format.printf "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+@."

let pp_epsilon fmt (max_v, eps) =
  let pp =
    if Sim.Core.R2.is_pure_rational max_v.Sim.Core.bvalue
    then Format.ifprintf
    else Format.fprintf
  in
  pp fmt "(epsilon: %a)" Rat.print eps

let aux sim opt_p =
  let sim, opt = Sim.Solve.maximize sim opt_p in
  sep ();
  Format.printf "The problem 'max %a' ...@." Sim.Core.P.print opt_p;
  begin
    match Sim.Result.get opt sim with
    | Sim.Core.Unknown     -> assert false
    | Sim.Core.Sat _       -> assert false

    | Sim.Core.Unsat ex    ->
      Format.printf " is unsat (reason = %a)@." Ex.print (Lazy.force ex);

    | Sim.Core.Unbounded _ -> Format.printf " is unbounded@."

    | Sim.Core.Max (mx,sol)  ->
      let {Sim.Core.max_v; is_le} = Lazy.force mx in
      let sol = Lazy.force sol in
      Format.printf
        " has an upper bound: %a (is_le = %b)(reason: %a)%a@."
        Sim.Core.R2.print max_v.Sim.Core.bvalue
        is_le
        Ex.print max_v.Sim.Core.explanation
        pp_epsilon (max_v, sol.Sim.Core.epsilon)
      ;
  end;
  sep ();
  Format.printf "@."

let large i = Sim.Core.R2.of_r (Num.Int i)
let upper i = Sim.Core.R2.upper (Num.Int i)
let lower i = Sim.Core.R2.lower (Num.Int i)

let bnd r e = Some {Sim.Core.bvalue = r; explanation = e}

let r_two = Rat.add Rat.one Rat.one

let () =
  let sim = Sim.Core.empty ~is_int:true ~check_invs:false in

  let x_m_y = Sim.Core.P.from_list ["x", Rat.one; "y", Rat.m_one] in
  let tx_ty = Sim.Core.P.from_list ["x", r_two; "y", r_two] in

  (* 3 < y < 5*)
  let sim, _ =
    Sim.Assert.var sim "y"
      (bnd (lower 3) (Ex.singleton "y>3"))
      (bnd (upper 5) (Ex.singleton "y<5"))
  in

  (* 3 < x < 4 *)
  let sim, _ =
    Sim.Assert.var sim "x"
      (bnd (lower 3) (Ex.singleton "x>3"))
      (bnd (large 5) (Ex.singleton "x<=5"))
  in

  (* 0 <= x - y *)
  let sim, _ =
    Sim.Assert.poly sim x_m_y "s'"
      (bnd (large 1) (Ex.singleton "x-y>=1"))
      None
  in

  (* s == 2x + 2y <= 20 *)
  let sim, _ =
    Sim.Assert.poly sim tx_ty "s"
      None
      (bnd (large 20) (Ex.singleton "2x+2y<=20")) in

  aux sim tx_ty
