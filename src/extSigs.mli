(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)


(*----------------------------------------------------------------------------*)

(** Interface required for variables *)
module type Variables = sig

  (** type of variables used in the simplex *)
  type t

  (** compare function on vars *)
  val compare : t -> t -> int

  (** [is_int v] returns true if the variable has integer type,
      and false otherwise *)
  val is_int : t -> bool

  (** [print fmt v] prints the given var *)
  val print : Format.formatter -> t -> unit

end

(*----------------------------------------------------------------------------*)

(** Interface required for rationnals *)
module type Rationals = sig

  (** type of rationnal numbers *)
  type t
  val zero : t
  val one : t
  val m_one : t
  val sign : t -> int (* can be used to quickly compare with zero *)
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val is_zero : t -> bool
  val is_one : t -> bool
  val is_m_one : t -> bool
  val add : t -> t -> t
  val sub : t -> t -> t
  val div : t -> t -> t
  val mult : t -> t -> t
  val abs : t -> t
  val is_int : t -> bool
  val print : Format.formatter -> t -> unit
  val to_string : t -> string
  val min : t -> t -> t
  val minus : t -> t
  val floor : t -> t
  val ceiling : t -> t
end

(** Interface required for coefs *)
module type Coefs = sig

  (** type of rationnal numbers *)
  type t
  val zero : t
  val one : t
  val m_one : t
  val sign : t -> int (* can be used to quickly compare with zero *)
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val is_zero : t -> bool
  val is_one : t -> bool
  val is_m_one : t -> bool
  val add : t -> t -> t
  val sub : t -> t -> t
  val div : t -> t -> t
  val mult : t -> t -> t
  val abs : t -> t
  val is_int : t -> bool
  val print : Format.formatter -> t -> unit
  val to_string : t -> string
  val min : t -> t -> t
  val minus : t -> t
  val floor : t -> t
  val ceiling : t -> t
end

(** Interface required for bounds and solutions *)
module type Value = sig

  (** type of rationnal numbers *)
  type t
  val zero : t
  val one : t
  val m_one : t
  val sign : t -> int (* can be used to quickly compare with zero *)
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val is_zero : t -> bool
  val add : t -> t -> t
  val sub : t -> t -> t
  val div : t -> t -> t
  val mult : t -> t -> t
  val is_int : t -> bool
  val print : Format.formatter -> t -> unit
  val to_string : t -> string
  val min : t -> t -> t
  val minus : t -> t
  val floor : t -> t
  val ceiling : t -> t

  type r
  val mult_by_coef: t -> r -> t
  val div_by_coef: t -> r -> t
end

(*----------------------------------------------------------------------------*)

(** Interface of explanations *)
module type Explanations = sig
  type t
  val empty : t
  val union : t -> t -> t
  val print : Format.formatter -> t -> unit
end

module type MapSig = sig
  type 'a t
  type key

  val empty : 'a t
  val find : key -> 'a t -> 'a
  val add : key -> 'a -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
  val mem: key -> 'a t -> bool
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val cardinal : 'a t -> int
end

module type SetSig = sig
  type t
  type elt

  val empty : t
  val is_empty : t -> bool
  val choose : t -> elt
  val elements : t -> elt list
  val add : elt -> t -> t
  val mem : elt -> t -> bool
  val remove : elt -> t -> t
  val fold : (elt -> 'b -> 'b) -> t -> 'b -> 'b
  val iter : (elt -> unit) -> t -> unit
  val union : t -> t -> t
end
