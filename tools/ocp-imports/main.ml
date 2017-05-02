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



open StringCompat
open ImportExtract

let raw_results = ref []

let extract_imports filename =
  try
    let res = ImportExtract.of_cmt filename in
    raw_results := res :: !raw_results;
  with exn ->
    Printf.eprintf "Warning: %S could not be loaded (exn %S)\n%!"
      filename (Printexc.to_string exn)

let only_modules = ref None
let blacklisted_modules = ref StringSet.empty

let print_types = ref true
let print_values = ref true
let remove_empty = ref false

let path_access path =
  match path with
  | modname :: ids -> modname, ids
  | _ -> assert false

let filter array =
  let list = ref [] in
  Array.iter (fun path ->
    let name, _ = path_access path in
    if not (StringSet.mem name !blacklisted_modules)
      && (match !only_modules with
      | None -> true
      | Some modules -> StringSet.mem name modules) then
      list := path :: !list
  ) array;
  Array.of_list !list

let print_paths paths =
  let array = Array.map (String.concat ".") paths in
  Array.sort compare array;
  Array.iter (fun path ->
    Printf.printf "   %s\n%!" path
  ) array

let print_raw_results results =
  List.iter (fun { imp_modname; imp_values; imp_types } ->
    let expr_paths = if !print_values then imp_values else [||] in
    let type_paths = if !print_types then imp_types else [||] in
    if expr_paths <> [||] || type_paths <> [||] then begin
      Printf.printf "Module: %s\n" imp_modname;
      if !print_values then begin
        Printf.printf "  Values:\n";
        print_paths expr_paths;
      end;
      if !print_types then begin
        Printf.printf "  Types:\n";
        print_paths type_paths;
      end
    end
  ) results

let stdlib_modules =
  [ "Pervasives"; "Buffer"; "Map"; "Int32"; "Int64"; "Marshal"; "Set"; "Stream"; "Array"; "List"; "Hashtbl"; "Sys"; "Filename";
    "Printf"; "Arg"; "String"; "StringCompat"; "Obj"; "Printexc";
    "Digest"; "Lazy"]

let set_no_stdlib () =
  List.iter (fun name ->
    blacklisted_modules := StringSet.add name !blacklisted_modules)
    stdlib_modules

let get_only_modules () =
  match !only_modules with
  | None -> StringSet.empty
  | Some set -> set

let get_modname_of_filename name =
    try
      ignore (String.index name '.');
      String.capitalize (Filename.chop_extension (Filename.basename name))
    with Not_found -> name

let add_to_blacklisted_modules name =
  let modname = get_modname_of_filename name in
  blacklisted_modules := StringSet.add modname !blacklisted_modules

let add_to_only_modules name =
  let modname = get_modname_of_filename name in
  let set = get_only_modules () in
  only_modules := Some (StringSet.add modname set)

let external_kinds = 2
let external_expr = 0
let external_type = 1

let external_names = [| "values"; "types" |]
type ocaml_module = {
  mod_name : string;
  mutable mod_ok : bool;

  (* which values from this modules are referenced elsewhere *)
  mod_internals : ocaml_identifier StringMap.t array;

  mod_externals : ocaml_identifier StringMap.t array;
}

and ocaml_identifier = {
  id_path : string;
  id_mod : ocaml_module;

  mutable id_uses : ocaml_module StringMap.t;
}

let modules = ref StringMap.empty
let get_module modname =
  try
    StringMap.find modname !modules
  with Not_found ->
    let mod_ml = {
      mod_name = modname;
      mod_ok = false;
      mod_internals = Array.init external_kinds (fun i -> StringMap.empty);
      mod_externals = Array.init external_kinds (fun i -> StringMap.empty);
    } in
    modules := StringMap.add modname mod_ml !modules;
    mod_ml

let filenames = ref []
let packages = ref []

let add_package lib_name =
  let dirname = Filename.concat "_obuild" lib_name in
  let files = Sys.readdir dirname in
  let modnames = ref StringMap.empty in
  Array.iter (fun file ->
    if Filename.check_suffix file ".cmt" then begin
      let modname = get_modname_of_filename file in
      let filename = Filename.concat dirname file in
      filenames := filename :: !filenames;
      let mod_ml = get_module modname in
      mod_ml.mod_ok <- true;
      modnames := StringMap.add modname mod_ml !modnames
    end
  ) files;
  packages := (lib_name, !modnames) :: !packages

let print_as_dot results =

  List.iter (fun { imp_modname; imp_values; imp_types } ->

    let mod_ml = get_module imp_modname in

    let external_kind = 0 in
    let paths = imp_values in

    Array.iter (fun path ->
      let (modname, id_path) = path_access path in
      let id_path = String.concat "." id_path in
      let id_mod = get_module modname in

      if id_mod.mod_ok then
        let id = try
                   StringMap.find id_path id_mod.mod_internals.(external_kind)
          with Not_found ->
            let id = {
              id_path;
              id_mod;
              id_uses = StringMap.empty;
            } in
            id_mod.mod_internals.(external_kind) <- StringMap.add
              id_path id id_mod.mod_internals.(external_kind);
            id
        in

        id.id_uses <- StringMap.add imp_modname mod_ml id.id_uses

    ) paths
  ) results;

  let external_kind = 0 in
  let dot_filename = Printf.sprintf "ocp-imports-%s.dot"
    external_names.(external_kind) in
  let oc = open_out dot_filename in
  output_string oc ImportGraph.dot_header;

  let links = ref [] in
  List.iter (fun (lib_name, modules) ->
    Printf.fprintf oc "subgraph \"cluster-%s\" {\n" lib_name;
    Printf.fprintf oc "  label = %S\n" lib_name;

    StringMap.iter (fun modname mod_ml ->

      Printf.fprintf oc "  subgraph \"cluster-%s\" {\n" modname;
      Printf.fprintf oc "    label = %S\n" modname;
      Printf.fprintf oc "    %S [ label = \".\" ]\n" modname;

      StringMap.iter (fun _ id ->
        let full_path = Printf.sprintf "%s.%s" modname id.id_path in
        Printf.fprintf oc "    %S [ label = %S]\n" full_path id.id_path;

        StringMap.iter (fun modname _ ->
          links := (modname, full_path) :: !links;
        ) id.id_uses;

      ) mod_ml.mod_internals.(external_kind);
      Printf.fprintf oc "  }\n";

    ) modules;

    Printf.fprintf oc "}\n";
  ) !packages;

  List.iter (fun (modname, id_path) ->
    Printf.fprintf oc "  %S -> %S\n" modname id_path;
  ) !links;

  output_string oc ImportGraph.dot_trailer;

  close_out oc;
  Printf.printf "Dot file %S generated\n%!" dot_filename

let sets = ref []

  (*
  "stdlib", List.map (fun s -> (s, None)) stdlib_modules;
  *)

let load_sets filename =
  let b = Buffer.create 1000 in
  let flush () =
    let line = Buffer.contents b in
    Buffer.clear b;
    let set_name, line = OcpString.cut_at line ':' in
    let units = OcpString.split_simplify line ' ' in
    let units = List.map (fun name ->
      let name =
        if Filename.check_suffix name ".cmi" then
          Filename.chop_extension name
        else name in
      let name = String.capitalize name in
      name, None
    ) units in
    sets := (set_name, units) :: !sets
  in
  FileString.iter_lines (fun line ->
    if line <> "" && line.[0] != '#' then begin
      if String.contains line ':' then flush ();
      Buffer.add_string b line;
      Buffer.add_string b " ";
    end
  ) filename;
  flush ()

let extract_from_obuild () =
  let rec iter set dir =
    let files = Sys.readdir dir in
    Array.iter (fun file ->
      let lfile = String.lowercase file in
      match lfile with
      | "_mutable_tree" | "_temp" | "_doc" -> ()
      | _ ->
        let dirfile = Filename.concat dir file in
        if Sys.is_directory dirfile then
          let set = ref [] in
          iter (Some set) dirfile;
          sets := (file, !set) :: !sets
        else
          match set with
          | None -> ()
          | Some set ->
            if Filename.check_suffix lfile ".cmt" then begin
              extract_imports dirfile;
              set := (Filename.chop_extension (String.capitalize file), Some dirfile) :: !set
            end
    ) files
  in
  let dir = "_obuild" in
  iter None dir


let to_dot = ref false
let only_cross = ref false

let display_arch = ref false
let only_packages = ref []
let use_obuild = ref false
let load_packages = ref []

let arg_list = [
  "-dot", Arg.Set to_dot, " Generate a ocp-imports.dot file";
  "-no-stdlib", Arg.Unit set_no_stdlib, " Remove imports from stdlib";
  "-no-values", Arg.Clear print_values, " Don't print values";
  "-no-types", Arg.Clear print_types, " Don't print types";
  "-remove-empty", Arg.Set remove_empty, " Don't print empty sources";
  "-only-cross", Arg.Set only_cross, " Imports between provided filenames";
  "-from-rest", Arg.Rest add_to_only_modules, "MODULES Add the remaining modules as targets";
  "-not-from-rest", Arg.Rest add_to_blacklisted_modules, "MODULES Black-list the remaining modules";
  "-ocp", Arg.String add_package, "PACKAGE Add files from PACKAGE";
  "", Arg.Unit (fun _ -> ()), "--------------- display arch --------------";
  "-arch", Arg.Set display_arch, " Display architecture";
  "-only", Arg.String (fun s ->
    only_packages := s :: !only_packages),
  "PACKAGE Display only imports related to PACKAGE";
  "-packages", Arg.String (fun s -> load_packages := s :: !load_packages),
  "FILE Load description of packages";
  "-obuild", Arg.Set use_obuild, " Iter on _obuild directory";
]

let arg_usage = "ocp-imports *.cmt"
let _ =
  Arg.parse arg_list (fun filename -> filenames := filename :: !filenames)
    arg_usage;
  let filenames = List.rev !filenames in
  List.iter extract_imports filenames;
  if !use_obuild then extract_from_obuild ();
  let results = List.rev !raw_results in

  if !display_arch then begin
    List.iter (fun file ->
      load_sets file
    ) !load_packages;
    let sets = ImportArch.display results !only_packages !sets in
    ImportGraph.print sets
  end else begin
    (* raw display *)
    if !only_cross then begin
      let modules = ref (get_only_modules ()) in
      List.iter (fun filename ->
        let modname = String.capitalize (
          Filename.chop_extension (Filename.basename filename)) in
        modules := StringSet.add modname !modules
      ) filenames;
      only_modules := Some !modules
    end;
    let results = List.map (fun res ->
      { res with
        imp_values = filter res.imp_values;
        imp_types = filter res.imp_types;
      }
    ) results in
    if !to_dot then
      print_as_dot results
    else
      print_raw_results results
  end
