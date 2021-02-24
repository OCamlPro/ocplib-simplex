(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

module type SIG = sig
  module Var : ExtSigs.VAR_SIG
  module R : ExtSigs.R_SIG

  type t
  type var_status = New | Exists | Removed

  val empty : t
  val is_polynomial : t -> bool
  val is_empty : t -> bool

  val replace    : Var.t -> R.t -> t -> t * var_status
  val accumulate : Var.t -> R.t -> t -> t * var_status
  val append     : t -> R.t -> t -> t * (Var.t * var_status) list
  val subst : Var.t -> t -> t -> t * (Var.t * var_status) list
  val from_list : (Var.t * R.t) list -> t
  val print : Format.formatter -> t -> unit

  val fold: (Var.t -> R.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter: (Var.t -> R.t -> unit) -> t -> unit
  val for_all: (Var.t -> R.t -> bool) -> t -> bool

  val partition: (Var.t -> R.t -> bool) -> t -> t * t
  val compare : t -> t -> int
  val mem : Var.t -> t -> bool
  val equal : t -> t -> bool
  val bindings : t -> (Var.t * R.t) list
  val find : Var.t -> t -> R.t
  val remove : Var.t -> t -> t
end

module Make(Var: ExtSigs.VAR_SIG)(R : ExtSigs.R_SIG) : SIG
  with module Var = Var and module R = R = struct

    module Var = Var
    module R = R

    module MV = Map.Make(Var)

    type t = R.t MV.t

    type var_status = New | Exists | Removed

    let empty = MV.empty
    let fold = MV.fold
    let iter = MV.iter
    let for_all = MV.for_all
    let compare = MV.compare R.compare
    let partition = MV.partition

    let remove = MV.remove
    let find = MV.find
    let bindings = MV.bindings
    let equal = MV.equal R.equal
    let mem = MV.mem
    let is_empty = MV.is_empty

    let is_polynomial p =
      try
        let cpt = ref 0 in
        iter (fun _ _ -> incr cpt; if !cpt > 1 then raise Exit) p;
        false
      with Exit ->
        true

    let replace v q t =
      if R.is_zero q then MV.remove v t, Removed
      else MV.add v q t, (if MV.mem v t then Exists else New)

    let accumulate v q t =
      let new_q = try R.add q (find v t) with Not_found -> q in
      replace v new_q t

    (* TODO: We can maybe replace mp with a list, since keys are unique ... *)
    let append_aux p coef q =
      fold (fun x c (p, mp) ->
        let p, x_status = accumulate x (R.mult coef c) p in
        p, MV.add x x_status mp
      ) q (p, MV.empty)

    let append p coef q =
      let p, mp = append_aux p coef q in p, MV.bindings mp

    let subst v p q =
      try
        let new_q, modified = append_aux (remove v q) (find v q) p in
        new_q, MV.bindings (MV.add v Removed modified)
      with Not_found ->
        (* This will oblige us to enforce strong invariants !!
           We should know exactly where we have to substitute !! *)
        assert false

    let from_list l =
      List.fold_left (fun p (x, c) -> fst (accumulate x c p)) empty l

    let print fmt p =
      let l = MV.bindings p in
      match l with
      | [] -> Format.fprintf fmt "(empty-poly)"
      | (x, q)::l ->
        Format.fprintf fmt "(%a) * %a" R.print q Var.print x;
        List.iter
          (fun (x,q) ->
            Format.fprintf fmt " + (%a) * %a" R.print q Var.print x) l

  end
