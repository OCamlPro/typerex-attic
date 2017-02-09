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
