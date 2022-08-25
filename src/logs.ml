(******************************************************************************)
(*                              ocplib-simplex                                *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

open Format

let varname = "OCPLIB_SIMPLEX_DEBUG"

let debug_level =
  try int_of_string (Sys.getenv varname) with
  | Not_found
  | Failure _ -> 0

let talk_if cond =
  if cond
  then printf
  else ifprintf std_formatter

let feedback msg = talk_if (debug_level > 0) msg
let debug msg = talk_if (debug_level > 1) msg
