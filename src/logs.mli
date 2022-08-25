(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

(** The module charged of the library feedbacks.
    Logs can be activated by changing the environment variable
    `OCPLIB_SIMPLEX_DEBUG` *)

(** The value of OCPLIB_SIMPLEX_DEBUG as an integer; by default to `0`
    if not set or set to something else than an integer. *)
val debug_level : int

(** Prints if `debug_level > 0` *)
val feedback : ('a, Format.formatter, unit) format -> 'a

(** Prints if `debug_level > 1` *)
val debug : ('a, Format.formatter, unit) format -> 'a
