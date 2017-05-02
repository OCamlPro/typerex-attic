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
open ManagerInit

type switch_status = {
 available_and_managed : string list;
 available_not_managed : string list;
 unavailable : string list;
}

let get_switch_status c =
  let binaries = Sys.readdir manager_bindir in
  let managed_binaries = ref StringMap.empty in
  Array.iter (fun tool ->
    if tool <> "ocp-manager" then
      managed_binaries := StringMap.add tool (ref false) !managed_binaries)
    binaries;

  let available_and_managed = ref [] in
  let available_not_managed = ref [] in
  let unavailable = ref [] in

  List.iter (fun line ->
    try
      let ref = StringMap.find line !managed_binaries in
      ref := true;
      available_and_managed := line :: !available_and_managed
    with Not_found ->
      available_not_managed := line :: !available_not_managed
  ) (list_directory (compiler_bindir c));

  StringMap.iter (fun tool ref ->
    if not !ref then unavailable := tool :: !unavailable
  ) !managed_binaries;

  { available_not_managed = !available_not_managed;
    available_and_managed = !available_and_managed;
    unavailable = !unavailable
  }

let print_commands () =
  let c = get_current_compiler () in
  let s = get_switch_status c in

  Printf.printf "Available tools in %s:\n" c.compiler_name;

  Printf.printf "Tools managed:\n";
  list_printer "  " (List.sort compare s.available_and_managed);

  Printf.printf "Tools not managed:\n";
  list_printer "  " (List.sort compare s.available_not_managed);

  Printf.printf "Tools missing:\n";
  list_printer "  " (List.sort compare s.unavailable);

  Printf.printf "%!"

let print_missing () =
  let c = get_current_compiler () in
  let s = get_switch_status c in

  Printf.printf "Tools missing:\n";
  list_printer "  " (List.sort compare s.unavailable);

  Printf.printf "%!"

let add_command cmd =
  let filename = Filename.concat manager_bindir cmd in
  if Sys.file_exists filename then
    Printf.eprintf "Warning: %S is already managed\n%!" cmd
  else
    begin
      Printf.eprintf "Creating wrapper for %S\n%!" cmd;
      symlink "ocp-manager" filename;
    end

let remove_command cmd =
  let filename = Filename.concat manager_bindir cmd in
  if Sys.file_exists filename then begin
    Printf.eprintf "Removing %S from managed commands\n%!" cmd;
    (try Sys.remove filename with _ -> ());
  end else begin
    Printf.eprintf "Warning: command %S was not managed.\n%!" cmd;
  end

let add_all_commands () =
  let c = get_current_compiler () in
  let s = get_switch_status c in

  List.iter (fun cmd ->
    let filename = Filename.concat manager_bindir cmd in
    if Sys.file_exists filename then
      Printf.eprintf "Warning: %S is already managed\n%!" cmd
    else
      begin
        Printf.eprintf "Creating wrapper for %S\n%!" cmd;
        symlink "ocp-manager" filename;
      end
  ) s.available_not_managed

let add_default_command cmd =

  let basename = Filename.basename cmd in

  begin
    let filename = Filename.concat manager_bindir basename in
    if not ( Sys.file_exists filename ) then
      begin
        Printf.eprintf "Creating wrapper for %S\n%!" basename;
        symlink "ocp-manager" filename;
      end
  end;

  if not (Sys.file_exists cmd) then begin
    Printf.eprintf "Error: Command %S does not exist !\n" cmd;
    Printf.eprintf "  You must specify the command path.\n%!";
    exit 2
  end;
  let cmd =
    if Filename.is_relative cmd then
      Filename.concat pwd cmd
    else cmd
  in
  if not (Sys.file_exists manager_defaults) then
    safe_mkdir manager_defaults;
  let cmd_default = Filename.concat manager_defaults basename in
  Printf.eprintf "Creating default for %S\n%!" basename;
  (try Sys.remove cmd_default with _ -> ());
  symlink cmd cmd_default

let remove_default cmd =
  let cmd_default = Filename.concat manager_defaults cmd in
  if Sys.file_exists cmd_default then begin
    Printf.eprintf "Removing default for %S\n%!" basename;
    (try Sys.remove cmd_default with _ -> ());
  end else begin
    Printf.eprintf "Warning: no default for %S\n%!" basename;
  end
