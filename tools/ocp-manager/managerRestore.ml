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


let install_prefix = "/usr/"
let libdir = Filename.concat install_prefix "lib/ocp-manager"
let bindir = Filename.concat install_prefix "bin"
let distrib_dir = Filename.concat libdir "distrib"

let list_of_binaries_filename = Filename.concat libdir "binaries.txt"
let manager_binary_filename = Filename.concat libdir basename

let load_binaries () =
  ManagerMisc.lines_of_file list_of_binaries_filename

let save_binaries binaries =
  let oc = open_out list_of_binaries_filename in
  List.iter (fun s -> Printf.fprintf oc "%s\n" s) binaries;
  close_out oc

let manage_binary binary =
  let binary_filename = Filename.concat bindir binary in
  if Sys.file_exists binary_filename then begin
    Printf.fprintf stderr "Saving executable %s to %s\n%!"
      binary_filename (Filename.concat distrib_dir binary);
    Sys.rename binary_filename (Filename.concat distrib_dir binary);
  end;

  if not (Sys.file_exists binary_filename) then begin
    Printf.fprintf stderr "Creating stub executable %s\n%!" binary_filename;
    symlink "ocp-manager" binary_filename;
  end;
  Printf.fprintf stderr "%s is now managed\n%!" binary


let arg_handler () =
  if not (Sys.file_exists distrib_dir) then begin
    Printf.fprintf stderr "ocp-manager not in control !l\n%!";
    exit 2;
  end;
  let binaries = ManagerMisc.lines_of_file list_of_binaries_filename in
  List.iter (fun binary ->
    let binary_filename = Filename.concat distrib_dir binary in
    let target_filename = Filename.concat bindir binary in

    begin
      try
        let st = MinUnix.lstat target_filename in
        match st.MinUnix.st_kind with
          MinUnix.S_LNK ->
            Printf.fprintf stderr "Removing stub executable %s\n%!" target_filename;
            (try
               Sys.remove target_filename
             with e ->
               Printf.fprintf stderr "\tError: %s\n%!" (Printexc.to_string e)
            )
        | _ -> ()
      with _ ->  ()
    end;

    if Sys.file_exists binary_filename then begin
      Printf.fprintf stderr "Restoring executable %s to %s\n%!"
        binary_filename target_filename;
      (try Sys.rename binary_filename target_filename with e ->
        Printf.fprintf stderr "\tError: %s\n%!" (Printexc.to_string e)
      );
    end
  ) binaries;
  MinUnix.rmdir distrib_dir;
