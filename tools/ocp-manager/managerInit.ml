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



open ManagerMisc
open ManagerWrapper

let first_install =
  if not (Sys.file_exists manager_bindir) then begin
    Printf.eprintf "Creating directory for binaries: %S\n%!" manager_bindir;
    safe_mkdir manager_bindir;
    true
  end else false

let manager_binary = Filename.concat manager_bindir "ocp-manager"

let resolve_binary path file =
  let rec iter path =
    match path with
      [] -> None
    | bindir :: next_path ->
      let filename = Filename.concat bindir file in
      if Sys.file_exists filename then
        Some filename
      else
        iter next_path
  in
  iter path

let argv0 =
  if Filename.is_implicit Sys.argv.(0) then
    match resolve_binary path basename with
    | Some a -> a
    | None ->
      Printf.eprintf "Error: could not find %S in PATH\n%!" basename;
      exit 2
  else
    Sys.argv.(0)

let auto_update () =
  if manager_binary = argv0 then
    Printf.eprintf "Warning: no-update needed\n%!"
  else begin
    Printf.eprintf "Updating %S with %S\n%!" manager_binary argv0;
    (* To avoid "binary in use" lock *)
    (try Sys.remove manager_binary with _ -> ());
    let content = FileString.read_file argv0 in
    FileString.write_file manager_binary content;
    chmod manager_binary 0o755
  end

let update_done =
  if argv0  <> manager_binary then
    (* TODO: we should compare versions before updating ! *)
    let manager_updates_filename = Filename.concat ocpdir "manager-updates.txt" in
    let hashes = try
      lines_of_file manager_updates_filename
    with _ ->
      Printf.eprintf "No hash file\n%!";
      [] in
    let hash = Digest.file argv0 in
    let hash = Digest.to_hex hash in
    if List.mem hash hashes && Sys.file_exists manager_binary then
      match Sys.argv with
        [| _ ; "-force-update" |] ->
        auto_update ();
        exit 0;
      | _ ->
        Sys.argv.(0) <- manager_binary;
        Unix.execv manager_binary Sys.argv
    else
      begin
        auto_update ();
        let oc = open_out manager_updates_filename in
        List.iter (fun hash -> Printf.fprintf oc "%s\n" hash) hashes;
        Printf.fprintf oc "%s\n" hash;
        close_out oc
      end;
    true
  else
    false

let _ =
  List.iter (fun basename ->
    let filename = Filename.concat manager_bindir basename in
    if not (Sys.file_exists filename) then begin
      Printf.eprintf "Creating default wrapper for %S\n%!" basename;
      symlink "ocp-manager" filename;
    end
  ) (ManagerBinaries.binaries)

    (* Disable management of OPAM
let _ =
  let filename = Filename.concat manager_defaults "opam" in
  if not (Sys.file_exists filename) then begin
    let simple_path = List.filter (fun p -> p <> manager_bindir) path in
    match resolve_binary simple_path "opam" with
    | Some real_opam ->
        if not (Sys.file_exists manager_defaults) then
          safe_mkdir manager_defaults;
        symlink real_opam filename
    | None -> ()
  end
    *)
