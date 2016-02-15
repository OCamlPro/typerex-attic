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
open ManagerWrapper
open ManagerInit

let print_current_version msg =
  Printf.printf "Current version: %s (%s)\n" current_version current_version_reason ;
  begin
    match get_current_compiler_opt () with
    | Some c ->
      Printf.printf "\tbinaries in %S\n" (compiler_bindir c)
    | None ->
      Printf.printf "\tWarning: could not find binaries of %S\n" current_version
  end

let list_switches () =
  let alternatives = compilers_list in
  print_current_version "Current version";
  Printf.printf "Alternatives:\n";
  List.iter (fun c ->
    Printf.printf "\t%s\n" c.compiler_name) alternatives;
  Printf.printf "%!"

let set_current_switch version =
  try
    let c = StringMap.find version compilers in
    let oc = open_out current_filename in
    output_string oc c.compiler_name;
    output_char oc '\n';
    close_out oc;
    print_current_version "Old version";
    Printf.printf "New version: %s\n%!" version;
  with Not_found ->
    Printf.fprintf stderr "Error: no such alternative [%s]\n" version;
    Printf.fprintf stderr "\tuse -list to list alternatives\n%!";
    exit 2

let print_directory s =
  let c = get_current_compiler () in
  Printf.printf "%s\n%!"
    (
      match s with
          "bin" -> compiler_bindir c
        | "prefix" -> compiler_prefix c
        | "lib" -> compiler_libdir c
        | _ -> failwith "bad dir kind"
    )




