(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

open ExtSigs

module type SIG = sig
  module R : Rationals

  type t = private {v: R.t; offset: R.t}

  val zero : t
  val of_r : R.t -> t
  val upper : R.t -> t
  val lower : R.t -> t

  val minus : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
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

module Make(R : Rationals) : SIG with module R = R = struct

  module R = R

  type t = {
    v: R.t;

    offset: R.t;
    (* When working on strict bounds, an epsilon is added to the bounds.
       The offset represents the amount of epsilon are added. *)
  }

  let of_r v = {v; offset = R.zero}
  let upper v = {v; offset = R.m_one}
  let lower v = {v; offset = R.one}
  let zero = of_r R.zero

  let is_pure_rational r = R.equal r.offset R.zero
  let is_int r = is_pure_rational r && R.is_int r.v

  let map f a = {v = f a.v; offset = f a.offset}
  let map2 f a b = {v = f a.v b.v; offset = f a.offset b.offset}

  let add  = map2 R.add
  let sub  = map2 R.sub
  let mult = map2 R.mult

  let mult_by_const c e =
    if R.is_one c then e
    else map (R.mult c) e

  let div_by_const c e =
    if R.is_one c then e
    else map (fun v -> R.div v c) e

  let compare a b =
    let c = R.compare a.v b.v in
    if c <> 0 then c else R.compare a.offset b.offset

  let equal a b = compare a b = 0

  let is_zero a = R.is_zero a.v && R.is_zero a.offset

  let minus = map R.minus

  let floor r =
    if R.is_int r.v
    then
      if is_pure_rational r
      then r
      else of_r (R.sub r.v R.one)
    else of_r (R.floor r.v)

  let ceiling r =
    if R.is_int r.v
    then
      if is_pure_rational r
      then r
      else of_r (R.add r.v R.one)
    else of_r (R.ceiling r.v)

  let print_offset fmt off =
    let c = R.compare off R.zero in
    if c = 0 then ()
    else if c > 0
    then Format.fprintf fmt "+%aƐ" R.print off
    else Format.fprintf fmt "%aƐ" R.print off

  let print fmt r = Format.fprintf fmt "%a%a"
      R.print r.v
      print_offset r.offset
end
