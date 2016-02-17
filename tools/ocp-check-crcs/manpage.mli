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
