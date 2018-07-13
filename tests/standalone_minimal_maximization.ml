(*== Build the example with:

"ocamlopt -o standalone_minimal_maximization \
-I `ocamlfind query ocplib-simplex` ocplibSimplex.cmxa nums.cmxa \
standalone_minimal_maximization.ml"

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
end


module Sim = OcplibSimplex.Basic.Make(Var)(Rat)(Ex)

let sep () =
  Format.printf "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+@."


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

    | Sim.Core.Max (mx,_)  ->
      let {Sim.Core.max_v; is_le; reason} = Lazy.force mx in
      Format.printf
        " has an upper bound: %a (is_le = %b)(reason: %a)@."
        Rat.print max_v is_le Ex.print reason;
  end;
  sep ();
  Format.printf "@."

let () =
  let sim = Sim.Core.empty ~is_int:true ~check_invs:true ~debug:0 in

  let x_y = Sim.Core.P.from_list ["x", Rat.one; "y", Rat.one] in
  let ten = Some (Num.Int (10), Num.Int 0) in
  let three = Some (Num.Int (3), Num.Int 0) in

  (* s == x + y >= 10
  let sim = Sim.Assert.poly sim x_y "s" ten Ex.empty None Ex.empty in
  *)

  (* x <= 5 *)
  let sim, _ =
    Sim.Assert.var sim "x" three (Ex.singleton "x>=3") None Ex.empty in

  (* s == x + y <= 10 *)
  let sim, _ =
    Sim.Assert.poly sim x_y "s" None Ex.empty ten (Ex.singleton "x+y<=10") in

  aux sim x_y;
  aux sim (Sim.Core.P.from_list ["y", Rat.one]);
  aux sim (Sim.Core.P.from_list ["y", Rat.m_one]);
