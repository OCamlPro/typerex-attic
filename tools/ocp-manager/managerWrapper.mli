(**************************************************************************)
(*                                                                        *)
(*   Typerex Tools                                                        *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU General Public License version 3 described in the file       *)
(*   LICENSE.                                                             *)
(*                                                                        *)
(**************************************************************************)



open ManagerMisc

type compiler_kind =
  DISTRIBUTION
| OCAML_MANAGER of string
| OPAM_COMPILER of string * string


type compiler = {
  compiler_name : string;
  compiler_kind : compiler_kind;
  compiler_prefix : string;
}

val current_version : string
val current_version_reason : string
val basename : string
val manager_bindir : string
val path : string list
val path_sep : char
val current_filename : string
val homedir : string
val ocpdir : string

val get_current_compiler : unit -> compiler
val get_current_compiler_opt : unit -> compiler option

val compilers_list : compiler list
val compilers : compiler StringMap.t

val compiler_bindir : compiler -> string
val compiler_libdir : compiler -> string
val compiler_prefix : compiler -> string

val add_compiler : compiler -> unit

val manager_roots_dir : string
val pwd : string
val manager_defaults : string

