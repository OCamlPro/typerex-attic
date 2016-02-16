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

open StringCompat

type set = {
  set_name : string;
  mutable set_ok : bool;
  mutable set_units : set_unit list;

  mutable set_unresolved : StringSet.t;
  (* transitive closure of reachable sets *)
  mutable set_subsets : set StringMap.t;
  (* transitive closure of reachable units *)
  mutable set_subunits : set_unit StringMap.t;
  mutable set_unkunits : StringSet.t;
}

and unit_desc = {
  unit_name : string;
  mutable unit_sets : set_unit list;
}

and set_unit = {
  su_unit : unit_desc;
  su_set : set;
  su_file : string option;
  mutable su_imp : ImportExtract.t option;
  mutable su_values : unit_path StringMap.t;
  mutable su_import_values : unit_path StringMap.t;
}

and unit_path = {
  path_name : string;
  path_unit : set_unit;
  path_path : string list;
  mutable path_users : set_unit StringMap.t;
}

val display :
(* cmt info *)  ImportExtract.t list ->
(* interesting sets *)  string list ->
(* sets: *)  (string * (string * string option) list)  list ->
  set StringMap.t
