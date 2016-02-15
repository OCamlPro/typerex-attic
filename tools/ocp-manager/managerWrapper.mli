(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*      (GNU Public Licence version 3.0).                                 *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
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

