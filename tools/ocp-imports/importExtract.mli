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



(** The type corresponding to the data extracted from a .cmt file
    imp_filename : name of the .cmt file
    imp_modname : corresponding module name
    imp_digest : CRC of the corresponding .cmi file
    imp_imports : list of module names and CRC of imports
    imp_values : array of fully-qualified values imported
    imp_types : array of fully-qualified types imported
*)
type t = {
  imp_filename : string;
  imp_modname : string;
  imp_digest : Digest.t option;
  imp_imports : (string * Digest.t option) list;
  imp_values : string list array;
  imp_types : string list array;
}



(* Extract all persistent imports from a .cmt file *)
val of_cmt : string -> t
