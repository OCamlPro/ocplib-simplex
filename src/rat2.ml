(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

open ExtSigs

module type SIG = sig
  module R : Coefs
  module V : Value with type r = R.t

  type t = private {v: V.t; offset: R.t}

  val zero : t
  val of_r : V.t -> t
  val upper : V.t -> t
  val lower : V.t -> t

  val minus : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult_by_const : R.t -> t -> t
  val div_by_const : R.t -> t -> t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val is_zero : t -> bool
  val is_pure_rational : t -> bool
  val is_int : t -> bool

  val floor : t -> t
  val ceiling : t -> t

  val print : Format.formatter -> t -> unit
end

module Make(R : Rationals)(V : Value with type r = R.t)
  : SIG with module R = R and module V = V = struct

  module R = R
  module V = V

  type t = {
    v: V.t;

    offset: R.t;
    (* When working on strict bounds, an epsilon is added to the bounds.
       The offset represents the amount of epsilon are added. *)
  }

  let of_r v = {v; offset = R.zero}
  let upper v = {v; offset = R.m_one}
  let lower v = {v; offset = R.one}
  let zero = of_r V.zero

  let is_pure_rational r = R.equal r.offset R.zero
  let is_int r = is_pure_rational r && V.is_int r.v

  let map f g a = {v = f a.v; offset = g a.offset}
  let map2 f g a b = {v = f a.v b.v; offset = g a.offset b.offset}

  let add  = map2 V.add R.add
  let sub  = map2 V.sub R.sub

  let mult_by_const c e =
    if R.is_one c then e
    else map (fun v -> V.mult_by_coef v c) (fun v -> R.mult v c) e

  let div_by_const c e =
    if R.is_one c then e
    else map (fun v -> V.div_by_coef v c) (fun v -> R.div v c) e

  let compare a b =
    let c = V.compare a.v b.v in
    if c <> 0 then c else R.compare a.offset b.offset

  let equal a b = compare a b = 0

  let is_zero a = V.is_zero a.v && R.is_zero a.offset

  let minus = map V.minus R.minus

  let floor r =
    if V.is_int r.v
    then
      if is_pure_rational r
      then r
      else of_r (V.sub r.v V.one)
    else of_r (V.floor r.v)

  let ceiling r =
    if V.is_int r.v
    then
      if is_pure_rational r
      then r
      else of_r (V.add r.v V.one)
    else of_r (V.ceiling r.v)

  let print_offset fmt off =
    let c = R.compare off R.zero in
    if c = 0 then ()
    else if c > 0
    then Format.fprintf fmt "+%aƐ" R.print off
    else Format.fprintf fmt "%aƐ" R.print off

  let print fmt r = Format.fprintf fmt "%a%a"
      V.print r.v
      print_offset r.offset
end
