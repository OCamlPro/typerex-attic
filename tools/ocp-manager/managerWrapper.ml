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

let homedir = Sys.getenv "HOME"

let ocpdir = Filename.concat homedir ".ocp"
let manager_bindir = Printf.sprintf "%s/.ocp/manager-bin" homedir
let path_sep = match Sys.os_type with
  | "win32" -> ';'
  | _ -> ':'
let path = OcpString.split (Sys.getenv "PATH") path_sep

let old_manager_roots_dir = Filename.concat homedir ".ocaml/roots"
let manager_roots_dir = Filename.concat ocpdir "manager-switches"
let manager_defaults = Filename.concat ocpdir "manager-defaults"
let resolution_filename = Filename.concat ocpdir "manager-order.txt"
let current_filename = Filename.concat ocpdir "manager-current.txt"

let pwd = Sys.getcwd ()
let find_up version_files =
  let rec iter dirname files =
    match files with
    | [] ->
      let new_dirname = Filename.dirname dirname in
      if new_dirname = dirname then None else
        iter new_dirname version_files
    | (version_file, prefix) :: next_files ->
      let filename = Filename.concat dirname version_file in
      match
        if Sys.file_exists filename then
          prefix filename
        else None
      with None -> iter dirname next_files
      | Some _ as v -> v
  in
  try
    iter pwd version_files
  with e ->
    Printf.eprintf "ocp-manager: Warning, exception %S while looking for per-project files\n%!"
      (Printexc.to_string e);
    None

let get_version_of_file filename =
  match FileLines.read_file filename with
    version :: _ -> Some (version, filename)
  | _ -> None

let opamroot_default =
  try Sys.getenv "OPAMROOT"
  with Not_found -> Filename.concat homedir ".opam"
let opamroot = ref opamroot_default

let get_version_of_opam_file filename =
  match FileLines.read_file filename with
  | [] -> None
  | version :: root :: _ ->
    opamroot := root;
    Some ("opam:" ^ version, filename)
  | version :: _ ->
    Some ("opam:" ^ version, filename)

let basename = Filename.basename Sys.argv.(0)
let is_ocpmanager =
  OcpString.starts_with basename "ocp-manager"
let is_opam =
  OcpString.starts_with basename "opam"

type config_option =
  Get_manager_variable
| Get_opam_variable
| Get_project_config
| Get_manager_config
| Get_opam_config

let default_resolution_order =
  [ Get_manager_variable ;
    Get_opam_variable;
    Get_project_config;
    Get_manager_config;
    Get_opam_config
  ]

let resolution_order =
  if Sys.file_exists resolution_filename then
    let ic = open_in resolution_filename in
    let rec loop acc =
      match try Some (input_line ic) with _ -> None with
      | None -> List.rev acc
      | Some "manager_variable" -> loop (Get_manager_variable :: acc)
      | Some "opam_variable" -> loop (Get_opam_variable :: acc)
      | Some "project_config" -> loop (Get_project_config :: acc)
      | Some "manager_config" -> loop (Get_manager_config :: acc)
      | Some "opam_config" -> loop (Get_opam_config :: acc)
      | Some s ->
        Printf.eprintf "[ocp-manager] unknown sources in %s: %S\n%!"
          resolution_filename s;
        loop acc
    in
    let res = loop [] in
    close_in ic;
    if res = [] then default_resolution_order else res
  else
    default_resolution_order

let opam_default_version =
  lazy
    (try
       let real_opam = Filename.concat manager_defaults "opam" in
       let (ic, _, _) as opam =
         Unix.open_process_full
           (real_opam ^ " config var switch")
           (Unix.environment ()) in
       try
         let res = Some ("opam:" ^ input_line ic) in
         if Unix.close_process_full opam = Unix.WEXITED 0 then
           res
         else
           None
       with _ -> None
     with _ -> None)

let resolve m () = match m with
  | Get_manager_variable -> begin
    try Some (Sys.getenv "OCAML_VERSION", "getenv(OCAML_VERSION")
    with Not_found -> None
  end
  | Get_opam_variable -> begin
    try Some ("opam:" ^ Sys.getenv "OPAMSWITCH", "getenv(OPAM_SWITCH)")
    with Not_found -> None
  end
  | Get_project_config ->
    find_up [".ocp-switch", get_version_of_file;
             ".opam-switch", get_version_of_opam_file (* Side-effects !*) ]
  | Get_manager_config -> begin
    try
      let ic = open_in current_filename in
      (try
         let line = input_line ic in
         close_in ic;
         Some (line, "ocp-manager config")
       with _ ->
         close_in ic;
         None)
    with _ -> None
  end
  | Get_opam_config ->
    match Lazy.force opam_default_version with
    |  None -> None
    | Some version -> Some (version, "opam config")

let current_version, current_version_reason =
  let version =
    List.fold_left
      (fun acc f -> match acc with None -> f () | Some _ as version -> version)
      None
      (List.map resolve resolution_order) in
  match version with
  | None -> ("distrib", "distrib")
  | Some version -> version

let opamroot = !opamroot

let compilers = ref StringMap.empty
let compilers_list = ref []

let add_compiler c =
  compilers := StringMap.add c.compiler_name c !compilers;
  compilers_list := c :: !compilers_list;
  match c.compiler_kind with
    DISTRIBUTION
  | OCAML_MANAGER _ -> ()
  | OPAM_COMPILER (comp, alias) ->
    if not (StringMap.mem alias !compilers) then
      compilers := StringMap.add alias c !compilers

let read_compilers () =

  (*  if Sys.file_exists (Filename.concat distrib_dir "ocamlc") then *)
  add_compiler {
    compiler_name = "distrib";
    compiler_kind = DISTRIBUTION;
    compiler_prefix = "";
  };

  List.iter (fun manager_roots_dir ->
    List.iter (fun dir ->
      let prefix = Filename.concat manager_roots_dir dir in
      if Sys.file_exists (Filename.concat prefix "bin/ocamlc") then
        add_compiler {
          compiler_name = dir;
          compiler_prefix = prefix;
          compiler_kind = OCAML_MANAGER dir;
        }
    )
      (try
         list_directory manager_roots_dir
       with _ -> [])
  ) [ old_manager_roots_dir; manager_roots_dir ];

  let comps =
    try
      FileLines.read_file (Filename.concat opamroot "aliases")
    with _ -> []
  in
  List.iter (fun line ->
    let (alias, comp) = OcpString.cut_at line ' ' in
    if comp <> "" && alias <> "" then
      add_compiler {
        compiler_name = "opam:" ^ alias;
        compiler_kind = OPAM_COMPILER (comp, alias);
        compiler_prefix = Filename.concat opamroot alias;
      }
  ) comps

let _ = read_compilers ()
let compilers_list = !compilers_list
let compilers = !compilers

let find_bindir exe =
  let rec iter path =
    match path with
      [] -> None
    | bindir :: next_path ->
      if bindir = manager_bindir then
        iter next_path
      else
        if Sys.file_exists (Filename.concat bindir exe) then
          Some bindir
        else
          iter next_path
  in
  iter path

let compiler_bindir c =
  match c.compiler_kind with
  | DISTRIBUTION -> begin match find_bindir "ocamlc" with
    None -> "/usr/bin"
    | Some bindir -> bindir
  end
  | OPAM_COMPILER _ | OCAML_MANAGER _ ->
    Filename.concat c.compiler_prefix "bin"

let compiler_prefix c =
  match c.compiler_kind with
    DISTRIBUTION -> Filename.dirname (compiler_bindir c)
  | OPAM_COMPILER _
  | OCAML_MANAGER _
    -> c.compiler_prefix

let compiler_libdir c =
  let prefix_dir = compiler_prefix c in
  match c.compiler_kind with
    DISTRIBUTION ->
    (* TODO: call "ocamlc -where" ! *)
      assert false
  | OPAM_COMPILER _ ->
    Filename.concat prefix_dir "lib"
  | OCAML_MANAGER _ ->
    Filename.concat prefix_dir "lib/ocaml"

let get_current_compiler_opt () =
  try
    Some (StringMap.find current_version compilers)
  with Not_found -> None

let get_current_compiler () =
  match get_current_compiler_opt () with
    Some c -> c
  | None ->
    Printf.fprintf stderr "Error: could not find current version %S (%s)"
      current_version current_version_reason ;
    exit 2

let is_exec =
  let nargs = Array.length Sys.argv in
  if is_ocpmanager then
    if nargs > 2 && Sys.argv.(1) = "-exec" then
      Some (Array.sub Sys.argv 2 (nargs - 2))
    else None
  else Some Sys.argv

let _ =
  match is_exec with
  | None -> ()
  | Some argv ->
    let c = get_current_compiler () in
    let nargs = Array.length argv in
    let basename = Filename.basename argv.(0) in
    let argv =
      if basename = "ocaml" then
        match c.compiler_kind with
          OPAM_COMPILER (_, alias) ->
            let libdir = compiler_libdir c in
            Array.concat [
              [| argv.(0) |];
              [| "-I"; Filename.concat libdir "toplevel" |];
              Array.sub argv 1 (nargs - 1)
            ]
        | _ -> argv
      else argv
    in
    let dirname = compiler_bindir c in
    let find_basename basename =
      let filename = Filename.concat dirname basename in
      if Sys.file_exists filename then filename else
        match
          match c.compiler_kind with
            OPAM_COMPILER ("system", _) ->
              begin match find_bindir "ocamlc" with
              | None -> None
              | Some dirname ->
                let filename = Filename.concat dirname basename in
                if Sys.file_exists filename then Some filename else None
              end
          | _ -> None
        with
        | Some filename -> filename
        | None ->
          let filename = Filename.concat manager_defaults basename in
          if Sys.file_exists filename then filename else
            match
              if is_ocpmanager then
                find_bindir basename
              else None
            with
            | Some bindir -> Filename.concat bindir basename
            | None ->
              raise Not_found
    in
    let filename =
      try
        try
          find_basename basename
        with e ->
          if Filename.check_suffix basename ".opt" then
            find_basename (Filename.chop_suffix basename ".opt")
          else raise e
      with Not_found ->
        begin
          Printf.fprintf stderr "Error: command %s does not exist\n%!" basename;
          let alternatives = compilers_list in
          let versions = ref [] in
          List.iter (fun c ->
            let exec_name = Filename.concat (compiler_bindir c) basename in
            if Sys.file_exists exec_name then
              versions := c :: !versions) alternatives;
          let versions = List.sort compare !versions in
          if versions = [] then
            Printf.fprintf stderr "This executable is not available in any version\n%!"
          else begin
            Printf.fprintf stderr "This executable is only available in the following versions:\n";
            List.iter (fun c -> Printf.fprintf stderr " %s" c.compiler_name) versions;
            Printf.fprintf stderr "\n%!";
          end;
          exit 2
        end
    in

    (*
    begin
      match c.compiler_kind with
      | OPAM_COMPILER (name, alias) ->
        if is_opam then begin
          let opam_default_version = Lazy.force opam_default_version in
          if (opam_default_version <> None &&
                Some current_version <> opam_default_version) ||
            opamroot <> opamroot_default then
            Printf.eprintf "[ocp-manager] pre-selecting opam switch %s%s (%s)\n%!"
              alias
              (if opamroot = opamroot_default
               then ""
               else Printf.sprintf " (with OPAMROOT = %s)" opamroot)
              current_version_reason;
          putenv "OPAMROOT" opamroot;
          putenv "OPAMSWITCH" alias;
        end;
        let libdir = compiler_libdir c in
        putenv "CAML_LD_LIBRARY_PATH"
          (Printf.sprintf "%s/stublibs:%s/ocaml/stublibs" libdir libdir);
        let manpath = try Sys.getenv "MANPATH" with Not_found -> "" in
        putenv "MANPATH"
          (Printf.sprintf "%s/man:%s" c.compiler_prefix manpath);

      | _ -> ()
    end;
    *)

    (* Use ~/.ocp/manager-env.txt to add env variables to switch *)
    begin try
            FileString.iter_lines (fun line ->
              let (switch, line) = OcpString.cut_at line ' ' in
              let (var, value) = OcpString.cut_at line '=' in
              if switch = c.compiler_name || switch = "*" then
                putenv var value
            ) (Filename.concat ocpdir "manager-env.txt")
      with _ -> ()
    end;

    (* Use .ocp-env files to add env variables to switch *)
    let env = ref [] in
    let store_env filename = env := filename :: !env; None in
    assert (find_up [ ".ocp-env", store_env ] = None);
    List.iter (fun filename ->
      try
        FileString.iter_lines (fun line ->
          let (switch, line) = OcpString.cut_at line ' ' in
          let (var, value) = OcpString.cut_at line '=' in
          if switch = c.compiler_name || switch = "*" then
            putenv var value
        ) filename
      with _ -> ()
    ) !env;

    (*
      let get_env () =
      let list = ref [] in
      StringMap.iter (fun var s ->
      list := s :: !list;
      Printf.eprintf "env %S\n%!" s;
      ) !environ;
      Array.of_list !list
      in
    *)
    argv.(0) <- filename;
    (try execv argv.(0) argv with
      e ->
        Printf.fprintf stderr "Error \"%s\" executing %s\n%!"
          (Printexc.to_string e) argv.(0);
        exit 2
    )
