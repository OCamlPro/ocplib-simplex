(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module Var = struct
  type t = string

  let print fmt s = Format.fprintf fmt "%s" s

  let compare = String.compare

  let is_int _ = true
end

module Rat = struct
  open Num

  type t = num
  let add = ( +/ )
  let minus = minus_num
  let mult = ( */ )
  let abs = abs_num
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

  let floor = floor_num
  let ceiling = ceiling_num

end

module Ex = struct
  include Set.Make(String)

  let print fmt s = match elements s with
    | [] -> Format.fprintf fmt "()"
    | e::l ->
      Format.fprintf fmt "%s" e;
      List.iter (Format.fprintf fmt ", %s") l
end

module Ty = OcplibSimplex.Core.Make(Var)(Rat)(Ex)
module AB = OcplibSimplex.AssertBounds.Make(Ty)

module Sim = OcplibSimplex.Basic.Make(Var)(Rat)(Ex)

let aux header (sim, opt) =
  Format.printf "%a" header ();
  Format.printf "%a"
    (Sim.Core.print (Sim.Result.get opt sim)) sim

let () =
  Logs.Src.set_level OcplibSimplex.Core.src (Some Debug);
  Logs.Src.set_level OcplibSimplex.SolveBounds.src (Some Debug)
