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

module Filepath : sig
  type t
  val of_string : string -> t
  val to_string : t -> string

  val basename : t -> string
  val dirname : t -> t
  val add_basename : t -> string -> t
  val add_basenames : t -> string list -> t
  val add_suffix : t -> string -> t
  val check_suffix : t -> string -> bool
  val to_basenames : t -> string list
  val chop_extension : t -> t

end = struct
  type t = string
  let of_string  t= t
  let to_string t = t

  let basename t = Filename.basename t
  let dirname t = Filename.dirname t
  let add_basename t s = Filename.concat t s
  let add_basenames t list = String.concat "/" (t :: list)
  let add_suffix t s = t ^ s
  let check_suffix t s = Filename.check_suffix t  s
  let chop_extension t = try
                           Filename.chop_extension t
    with _ -> t

  let to_basenames s = OcpString.split s '/'
end

let chop_extension t = try
                         Filename.chop_extension t
  with _ -> t


module Sys = struct

  include Sys

  let file_exists f = file_exists (Filepath.to_string f)

end

module Pervasives = struct

  include Pervasives
  let open_in f = open_in (Filepath.to_string f)
  let open_out f = open_out (Filepath.to_string f)

end

include Pervasives

let file_prefix = ref None
let functors_arg = ref []
let pack_functor_arg = ref None
let target_arg = ref None
let sources_arg = ref []
let rec_arg = ref false
let mli_arg = ref false
let ml_arg = ref true
let with_ns = ref false
let verbosity = ref 0
let file_number = ref 0

let oc_ml = ref None
let oc_mli = ref None

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type namespace = {
  ns_name : string;
  mutable ns_closed : StringSet.t;
  mutable ns_open : namespace option;
}

let ns = {
  ns_name = "";
  ns_closed = StringSet.empty;
  ns_open = None;
}

let _ml s =
  match !oc_ml with
    None -> ()
  | Some oc -> output_string oc s

let _mli s =
  match !oc_mli with
    None -> ()
  | Some oc -> output_string oc s

let rec close_ns_open ns =
  match ns.ns_open with
    None -> ()
  | Some ns_in ->
    _ml "end\n";
    _mli "end\n";
    ns.ns_open <- None;
    ns.ns_closed <- StringSet.add ns_in.ns_name ns.ns_closed;
    close_ns_open ns_in

let dump_file _p filename =
  let filename =
    match !file_prefix with
    | None -> filename
    | Some prefix ->
      Filepath.of_string (Filename.concat prefix
                            (Filepath.to_string filename))
  in
  if !verbosity > 0 then
    Printf.eprintf "dump_file %s\n" (Filepath.to_string filename);
  _p (Printf.sprintf "#1 \"%s\"\n" (Filepath.to_string filename));
  let ic = open_in filename in
  try
    while true do
      let line = input_line ic in
      _p (Printf.sprintf "%s\n" line)
    done;
  with End_of_file ->
    close_in ic

(*
  let split_annot filename =
  let annots = AnnotParser.parse_file filename in
  AnnotParser.expand_sources annots;
  AnnotParser.write_file filename annots
*)


let name = Sys.argv.(0)

let arg_usage = Printf.sprintf "\
Usage:

   %s -o target.ml [options] files.ml*

Options:
" name

let version () = Printf.printf "\
ocp-pack version %s-%s

Copyright (C) 2011 OCamlPro S.A.S.

This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

"
  Version.number Version.git_commit;
  exit 0

let check_correct_modname modname = () (* TODO *)
let check_correct_filename filename =  () (* TODO *)

let split_filename filename =
  let (sourcename, modname) = OcpString.cut_at filename '!' in
  assert (Filename.is_relative filename);
  let sourcename = Filepath.of_string sourcename in
  let modnames = if modname = "" then
      let modnames = Filepath.to_basenames sourcename in
      let modnames = List.map chop_extension modnames in
      let modnames = List.map String.capitalize modnames in
      modnames
    else
      OcpString.split modname '.'
  in
  check_correct_filename sourcename;
  (sourcename, modnames)

let arg_list = Arg.align [
  "-o", Arg.String (fun s -> target_arg := Some (Filepath.of_string s)),
  " <filename.ml> : generate filename filename.ml";
  "-rec", Arg.Set rec_arg, " : use recursive modules (all .ml files must have a corresponding .mli file)";
  "-pack-functor", Arg.String (fun s -> pack_functor_arg := Some s),
  "<modname> : create functor with name <modname>";
  "-functor", Arg.String (fun s -> functors_arg := split_filename s :: !functors_arg),
  " <filename.mli> : use filename as an argument for functor";
  "-mli", Arg.Set mli_arg, " : output the .mli file too (.ml files without .mli file will not export any value)";
  "-no-ml", Arg.Clear ml_arg, " : do not output the .ml file";
  "-with-ns", Arg.Set with_ns, " : use directory structure to create a hierarchy of modules";
  (*  "-split-annot", Arg.String split_annot, " <file.annot> : split a .annot file in multiple .annot files"; *)
  "-v", Arg.Unit (fun _ -> incr verbosity), " : increment verbosity";
  "-version", Arg.Unit version,
  "               display version information";
  "-prefix", Arg.String (fun s -> file_prefix := Some s),
  "PREFIX Set a filename prefix for locations";
]

let error msg =
  Printf.eprintf "ERROR: %s\n\n%!" msg;
  Arg.usage arg_list arg_usage;
  exit 2

let _ =
  Arg.parse arg_list (fun s -> sources_arg := split_filename s :: !sources_arg) arg_usage



let rec output_file ns prefix modnames filename =

  (*
    let full_filename = Filepath.add_basenames prefix  filename in
    let dirname = Filepath.dirname full_filename in
  *)

  match modnames with
    [] -> assert false
  | "" :: modnames ->
    output_file ns prefix modnames filename
  | [ modname ] ->
    let basename = Filepath.chop_extension filename in
    let ml_filename = Filepath.add_suffix basename ".ml" in
    let mli_filename = Filepath.add_suffix basename ".mli" in

    close_ns_open ns;
    if StringSet.mem modname ns.ns_closed then
      error (Printf.sprintf "module %s already opened when reading %s" modname (Filepath.to_string ml_filename));


    let has_ml_file = Sys.file_exists ml_filename in
    let has_mli_file = Sys.file_exists mli_filename in

    let keyword =
      if !rec_arg then
        if !file_number = 0 then "module rec" else "and"
      else "module"
    in

    if has_ml_file then begin
      if has_mli_file then
        begin
          _mli (Printf.sprintf "%s %s : sig\n" keyword modname);
          dump_file _mli mli_filename;
          _mli (Printf.sprintf "end\n");
        end
      else
        if !rec_arg then
          failwith (Printf.sprintf "File %s needs an interface with -rec option" (Filepath.to_string ml_filename));

      _ml (Printf.sprintf "%s %s" keyword modname);
      if has_mli_file then begin
        _ml (Printf.sprintf ": sig\n");
        dump_file _ml mli_filename;
        _ml (Printf.sprintf "end = struct\n");
        if !rec_arg then begin
          _ml (Printf.sprintf "module type INTERFACE = sig\n");
          dump_file _ml mli_filename;
          _ml (Printf.sprintf "end\n");
          _ml (Printf.sprintf "module IMPLEMENTATION = struct\n");
          dump_file _ml ml_filename;
          _ml (Printf.sprintf "end\n");
          _ml (Printf.sprintf "include (IMPLEMENTATION : INTERFACE)\n");
        end else begin
          dump_file _ml ml_filename;
        end;
        _ml (Printf.sprintf "end\n");
      end else begin
        _ml (Printf.sprintf " = struct\n");
        dump_file _ml ml_filename;
        _ml (Printf.sprintf "end\n");
      end
    end else begin
      _ml (Printf.sprintf  "%s %s : sig\n" keyword modname);
      dump_file _ml mli_filename;
      _ml (Printf.sprintf  "end = struct\n");
      dump_file _ml mli_filename;
      _ml (Printf.sprintf  "end\n");

      _mli (Printf.sprintf "%s %s : sig\n" keyword modname);
      dump_file _mli mli_filename;
      _mli (Printf.sprintf "end\n");
    end;

    ns.ns_closed <- StringSet.add modname ns.ns_closed

  | modname :: modnames ->
    if !with_ns then begin
      if StringSet.mem modname ns.ns_closed then
        failwith (Printf.sprintf "module %s already closed when reading %s" modname
                    (Filepath.to_string filename));
      let ns_in =
        match ns.ns_open with
          Some ns_in when ns_in.ns_name = modname -> ns_in
        | _ ->
          close_ns_open ns;
          let ns_in = {
            ns_name = modname;
            ns_closed = StringSet.empty;
            ns_open = None;
          } in
          _mli (Printf.sprintf  "module %s : sig\n" modname);
          _ml (Printf.sprintf  "module %s = struct \n" modname);
          ns.ns_open <- Some ns_in;
          ns_in
      in
      output_file ns_in (prefix @ [modname]) modnames filename
    end else
      output_file ns (prefix @ [modname]) modnames filename

let _ =
  sources_arg := List.rev !sources_arg;
  match !target_arg with
    None -> error "You must specify a target with -o target.ml"
  | Some target ->
    if !ml_arg then oc_ml := Some (open_out target);
    if !mli_arg then oc_mli  := Some ( open_out (Filepath.add_suffix target  "i") );
    (match !pack_functor_arg with
      None -> ()
    | Some modname ->
      _ml (Printf.sprintf "module %s" modname);
      List.iter (fun (mli_filename, modnames) ->
        (*       let modname = String.capitalize (Filename.chop_suffix (Filename.basename mli_filename) ".mli")in *)
        match modnames with
          [modname] ->
            _ml (Printf.sprintf "(%s : sig\n" modname);
            dump_file _ml mli_filename;
            _ml ("\nend)\n");
        | _ -> Printf.kprintf failwith "Invalid functor argument name [%s]" (String.concat "." modnames)
      ) (List.rev !functors_arg);
      _ml (Printf.sprintf " = struct\n");
    );
    List.iter (fun (filename, modnames) ->
      if Filepath.check_suffix filename ".ml" ||
        Filepath.check_suffix filename ".mli"
      then begin
        if !verbosity > 0 then
          Printf.eprintf "Inserting %s\n" (Filepath.to_string filename);
        output_file ns [] modnames  filename;
        incr file_number;
      end else
        (*   if Filename.check_suffix filename ".mli" then
             Printf.fprintf stderr "Discarding interface file %s\n%!" filename
             else *)
        error (Printf.sprintf "Don't know what to do with anonymous argument [%s]" (Filepath.to_string filename))
    ) !sources_arg;
    close_ns_open ns;
    (match !pack_functor_arg with
      None -> ()
    | Some modname ->
      _ml (Printf.sprintf "\nend\n");
    );
    (match !oc_ml with None -> () | Some oc ->
      close_out oc; oc_ml := None);
    (match !oc_mli with None -> () | Some oc ->
      close_out oc; oc_mli := None)
