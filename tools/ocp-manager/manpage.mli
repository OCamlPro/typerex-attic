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



type man_block =
  | S of string
  | P of string
  | I of string * string
  | NOBLANK

type pager =
  | PLAIN
  | PAGER
  | GROFF

type 'div man_page = {
    man_name : string;   (* "OPAM" "GCC" *)
    man_section : int;
    man_date : string;
    man_release : string; (* "SOFT VERSION" *)
    man_org : string; (* GNU, OPAM Manual *)
    man_text : 'div list;
}

val print :
  ?subst:(string -> string) -> pager ->
  Format.formatter ->
  man_block man_page -> unit

module RAW : sig

  type div =
    | SH of span list
    | LI of span list * span list
    | P of span list
    | P2 of span list

  and span =
    | S of string
    | B of string
    | I of string

  val groff_page : div man_page -> string

end
