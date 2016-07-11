(******************************************************************************)
(*                              ocp-fun-sim                                   *)
(*                                                                            *)
(* Copyright (C) --- OCamlPro --- See README.md for information and licensing *)
(******************************************************************************)

type t = string

let print fmt s = Format.fprintf fmt "%s" s

let compare = String.compare

let is_int _ = true
