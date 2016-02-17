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
open CrcTypes

let rev_warnings = ref []
let ignore_warnings = ref StringSet.empty
let ignore warn_id =
  ignore_warnings := StringSet.add warn_id !ignore_warnings

let create warn_id line =
  if StringSet.mem warn_id !ignore_warnings then
    (fun line -> ())
  else
  let w = { warn_id; warn_revlines = [ line] } in
  rev_warnings := w :: !rev_warnings;
  let line line = w.warn_revlines <- line :: w.warn_revlines in
  line

let print_all args =
  let oc = match args.arg_output with
      None -> stderr
    | Some oc -> oc
  in
  let warnings = List.rev !rev_warnings in
  rev_warnings := [];
  let warnings = List.sort compare warnings in
  List.iter (fun w ->
      Printf.fprintf oc "Warning: %S\n" w.warn_id;
      List.iter (fun line ->
          Printf.fprintf oc "  %s\n" line)
        (List.rev w.warn_revlines);
    ) warnings;
  flush oc
