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
  type t = Q.t
  let add = Q.add
  let minus = Q.neg
  let mult = Q.mul
  let abs = Q.abs
  let compare = Q.compare
  let equal = Q.equal
  let zero = Q.zero
  let one = Q.one
  let m_one = Q.minus_one
  let is_zero n = Q.equal n Q.zero
  let to_string = Q.to_string

  let print = Q.pp_print
  let is_int v = Z.equal (Q.den v) Z.one
  let div = Q.div
  let sub = Q.sub
  let is_one v = Q.equal v Q.one
  let is_m_one v = Q.equal v Q.minus_one
  let sign = Q.sign
  let min = Q.min

  let floor v = Z.fdiv (Q.num v) (Q.den v) |> Q.of_bigint
  let ceiling v = Z.cdiv (Q.num v) (Q.den v) |> Q.of_bigint

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
