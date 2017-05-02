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



open StringCompat

type linkable =
  | Unchecked
  | Linkable
  | LinkErrors of link_errors

and link_errors = {
  mutable link_errors : link_error list;
}

and link_error =
  | NoByteImpl of string
  | NoAsmImpl of cmi_crc
  | WrongCmx of asm_unit
  | NonLinkable of asm_unit

and cmi_crc = {
  cmi_comp : component;
  cmi_digest : Digest.t;

  (* which .cmi file implements that interface *)
  mutable cmi_cmi_impls : cmi_file list;
  (* which .cmi file use this interface *)
  mutable cmi_cmi_users : cmi_file list;

  (* which .cmo files implements that interface *)
  mutable cmi_byte_impls : byte_unit list;
  (* which .cmo files use this interface *)
  mutable cmi_byte_users : byte_unit list;
  mutable cmi_byte_all_users : byte_unit list;

  (* which .cmx files implements that interface *)
  mutable cmi_asm_impls : asm_unit list;
  (* which .cmx files use this interface *)
  mutable cmi_asm_users : asm_unit list;
  mutable cmi_asm_all_users : asm_unit list;
}

and cmi_file = {
  cmi_filename : string;
  cmi_crc : cmi_crc;
  cmi_imports : cmi_crc list;
}

and component = {
  comp_modname : string;
  mutable comp_cmi_crcs : cmi_crc list;
}

and 'impl comp_unit = {
  unit_file : 'impl comp_unit file_kind;
  unit_cmi_crc : cmi_crc;
  unit_cmi_imports : (cmi_crc * Digest.t option) list; (* Some_=strong, None=weak *)
  unit_impl_crc : 'impl; (* [unit] for bytecode, [cmx_crc] for native code *)
  unit_impl_imports : 'impl list;
  (* [unit list] for bytecode, [cmx_crc list] for native code *)

  mutable unit_linkable : linkable;
}

and byte_unit = unit comp_unit
and asm_unit = cmx_crc comp_unit

and cmx_crc = {
  cmx_comp : component;
  cmx_digest : Digest.t;
}

and 'a file_kind =
  | COMP of 'a file
  | LIB of 'a list file

and cmx_file = asm_unit file
and cmxa_file = asm_unit list file

and cmo_file = byte_unit file
and cma_file = byte_unit list file

and 'a file = {
  file_name : string;
  mutable file_contents : 'a;
}













type warning = {
  warn_id : string;
  mutable warn_revlines : string list;
}

type args = {
  mutable arg_check_cmt : bool;
  mutable arg_save_filename : string option;
  mutable arg_load_filename : string option;
  mutable arg_ignore_files : string option;
  mutable arg_chdir : string option;
  mutable arg_ignore_warnings : string option;
  mutable arg_output : out_channel option;
}

type env = {
  components : (string, component) Hashtbl.t;
  cmi_crcs : (string*Digest.t,cmi_crc) Hashtbl.t;
  cmx_crcs : (string*Digest.t option,unit) Hashtbl.t;
  mutable cmi_files : cmi_file list;
  mutable cmo_files : cmo_file list;
  mutable cma_files : cma_file list;
  mutable cmx_files : cmx_file list;
  mutable cmxa_files : cmxa_file list;
  mutable ignore_warnings : StringSet.t;
  (* initial list of warnings while reading the file hierarchy *)
  mutable rev_warnings : warning list;
}
