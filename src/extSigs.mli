(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)


(*----------------------------------------------------------------------------*)

(** Interface required for variables *)
module type VAR_SIG = sig

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
module type R_SIG = sig

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

(*----------------------------------------------------------------------------*)

(** Interface of explanations *)
module type EX_SIG = sig
  type t
  val empty : t
  val union : t -> t -> t
  val print : Format.formatter -> t -> unit
end
