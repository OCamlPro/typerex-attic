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




(*

type man_block =
  | S of string
  | P of string
  | I of string * string
  | NOBLANK

type pager =
  | PLAIN
  | PAGER
  | GROFF

val print :
  ?subst:(string -> string) -> pager ->
  Format.formatter ->
  man_block man_page -> unit
*)

module Types : sig

  type div =
    | SH of span list
    | LI of span list * span list
    | P of span list
    | P2 of span list

  and span =
    | S of string
    | B of string
    | I of string

  type 'man_block man_page = {
    man_command : string;   (* "opam" "gcc" *)
    man_short_descr : string; (* one line descr *)
    man_section : int;
    man_date : string;
    man_version : string; (* "SOFT VERSION" *)
    man_org : string; (* GNU, OPAM Manual *)
    man_text1 :'man_block list;
    man_args : (string * Arg.spec * string) list;
    man_text2 :'man_block list;
    man_authors : string list;
    man_bug : string;
    man_copyright_years : string;
    man_copyright_holder : string;
    man_license : string;
    man_also : (string * string) list;
  }

end

open Types

val groff_page : div man_page -> string
