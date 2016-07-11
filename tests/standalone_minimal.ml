(*== Build the example with:

"ocamlopt -o standalone_minimal -I `ocamlfind query ocplib-simplex` \
ocplibSimplex.cmxa nums.cmxa standalone_minimal.ml"

if the lib is installed, or with:

"ocamlopt -o standalone_minimal -I ../src` ocplibSimplex.cmxa \
nums.cmxa standalone_minimal.ml"

if the lib is built but not installed

==*)

module Var = struct
  type t = string
  let print fmt s = Format.fprintf fmt "%s" s
  let compare = String.compare
  let is_int _ = true
end

module Ex = struct
  type t = unit
  let empty = ()
  let union _ _ = ()
  let print fmt _ =
    Format.fprintf fmt "[no explanations for this example]"
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
end


module Sim = OcplibSimplex.Basic.Make(Var)(Rat)(Ex)

let () =
  let sim = Sim.Core.empty ~is_int:true ~check_invs:true ~debug:1 in
  let zero = Some (Rat.zero, Rat.zero) in
  let m_one = Some (Rat.m_one, Rat.zero) in

  (* x >= 0 *)
  let sim = Sim.Assert.var sim "x" zero Ex.empty None Ex.empty in

  (* y >= 0 *)
  let sim = Sim.Assert.var sim "y" zero Ex.empty None Ex.empty in
  let x_y = Sim.Core.P.from_list ["x", Rat.one; "y", Rat.one] in

  (* z == x + y <= -1 *)
  let sim = Sim.Assert.poly sim x_y "z" None Ex.empty m_one Ex.empty in
  let sim = Sim.Solve.solve sim in
  match Sim.Result.get sim with
  | Sim.Core.Unknown -> assert false
  | Sim.Core.Sat _   -> assert false
  | Sim.Core.Unsat _ -> Format.printf "problem is unsat@."
