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

type header_sep = {
  sep_name : string;
  sep_regexp : Str.regexp;
  sep_add_line : int; (* add the header at this line by default *)
  mutable sep_headers : header list;
}

and header = {
  header_id : string;
  header_lines : string list;
  header_sep : header_sep;
  mutable header_files : (int * file) list;
}

and file = {
  file_name : string;
  file_headers : (int * header) list; (* position x header *)
}

type config = {
  ignore_headers : StringSet.t;
  ignore_files : StringSet.t;
  ignore_extensions : StringSet.t;
}

type env = {
  config : config;
  headers : (string, header) Hashtbl.t;
  files : (string, file) Hashtbl.t;
  mutable save_to_ignore : StringSet.t;
}


let ignore_files_filename = ".ocp-check-headers-ignore-files"
let ignore_files_more_filename = "ocp-check-headers-ignore-files"
let ignore_headers_filename = ".ocp-check-headers-ignore-headers"

let homedir = try
    Sys.getenv "HOME"
  with Not_found -> "/"
let config_dir = Filename.concat homedir ".ocp/check-headers"

let max_header_lines = ref 30
let min_char_repetition = ref 50

let stars = String.concat "" (
    Array.to_list (Array.init !min_char_repetition (fun _ -> "\\*")))
let spaces = "[\t ]*"
let new_header_sep ?(sep_add_line=0) sep_name sep_regexp =
  { sep_name;
    sep_regexp = Str.regexp sep_regexp;
    sep_headers = [];
    sep_add_line;
  }

(* Morally, these structures should be in [env], as they are modified
   during the scan. Instead, we reset them at the beginning of
   [scan_dirs].
*)
let ml_header = new_header_sep  "ML Header" (spaces ^ "(" ^ stars)
let cc_header = new_header_sep "C header" (spaces ^ "/" ^ stars)
let sh_header = new_header_sep ~sep_add_line:2 "Shell header"
    (spaces ^ String.make !min_char_repetition '#')

let reset_headers () =
  List.iter (fun sep ->
      sep.sep_headers <- []) [
    ml_header; cc_header; sh_header
  ]

let is_header_sep line header_sep =
  Str.string_match header_sep.sep_regexp line 0

let new_header_id s = Digest.to_hex (Digest.string s)

let new_header env header_sep header_pos header_lines  =
  let header = String.concat " " header_lines in
  let header_id = new_header_id header in
  if StringSet.mem header_id env.config.ignore_headers then
    []
  else
    let h =
      try
        Hashtbl.find env.headers header_id
      with Not_found ->
        let h = {
          header_sep;
          header_id;
          header_lines;
          header_files = [];
        } in
        Hashtbl.add env.headers header_id h;
        header_sep.sep_headers <- h :: header_sep.sep_headers;
        h
    in
    [header_pos, h]

let read_headers env lines header_sep =
  let rec iter_out pos lines headers =
    match lines with
    | [] -> List.rev headers
    | line :: lines ->
      if is_header_sep line header_sep then
        iter_in (pos+1) lines pos [line] headers
      else
        iter_out (pos+1) lines headers
  and iter_in pos lines header_pos header_lines headers =
    match lines with
    | [] -> (* abort header *)
      List.rev headers
    | line :: lines ->
      if is_header_sep line header_sep then
        let header_lines = List.rev (line :: header_lines) in
        let header = new_header env header_sep header_pos header_lines in
        iter_out (pos+1) lines (header @ headers)
      else
      if pos - header_pos > !max_header_lines then (* not a header *)
        iter_out (pos+1) lines headers
      else
        iter_in (pos+1) lines header_pos (line :: header_lines) headers
  in
  iter_out 0 lines []

let record_header ?(config=false) env file_name header_sep =
  let lines = FileLines.read_file file_name in
  let file_headers = read_headers env lines header_sep in
  let file = {
    file_name;
    file_headers;
  } in
  Hashtbl.add env.files file_name file;
  let file_headers = match file_headers with
    | [] ->
      (* We create a specific header for no-header. This specific header has
             its id generated from the name of the header_sep, because we want
             each header_sep to have a different set of no-header files. *)
      new_header env header_sep 0 [ header_sep.sep_name ]
    | _ -> file_headers in
  if not config then
    List.iter (fun (header_pos, header) ->
      header.header_files <- (header_pos, file) :: header.header_files
    ) file_headers

let list_ignore_files env list =
  List.fold_left (fun env file ->
      if file = "" then env else
      if file.[0] = '.' then
        { env with
          ignore_extensions = StringSet.add file env.ignore_extensions }
      else
        { env with
          ignore_files = StringSet.add file env.ignore_files
        }) env list


let list_ignore_headers env list =
  List.fold_left (fun env line ->
      { env with
        ignore_headers = StringSet.add line env.ignore_headers }
    ) env list


let add_default_ignored config =
  list_ignore_files config [
    ignore_files_more_filename;
    ignore_files_filename;
    ignore_headers_filename;
    "opam";
    "url";
    "descr";
    "_tags"; "_oasis";
    "meta";
    "readme"; "todo";
    "license";
    "authors";  "copying"; "changes";
    "check-headers.undo";

    ".cmo" ; ".cmi" ; ".cmxs" ; ".cmxa" ; ".cma"
    ; ".cmt" ; ".cmti" ; ".cmx" ; ".annot"

    ; ".mlmods" ; ".mlimods" ; ".mlpp" ; ".mlipp"
    ; ".asm" ; ".byte" ; ".native" ; ".out"

    ; ".mllib" ; ".mldylib" ; ".odocl"

    ; ".so" ; ".o" ; ".a"
    ; ".exe" ; ".dll"

    ; ".log" ; ".status"
    ; ".md" ; ".txt" ; ".tex" ; ".plot"
  ; ".html" ; ".css" ; ".xml" ; ".dtd" ; ".sgml"
        ; ".el"
          ; ".png" ; ".jpg" ; ".jpeg" ; ".git"
        ; ".old"
          ; ".gz" ; ".pdf"


  ]


let rec scan_dir env dir =
  let files = Sys.readdir dir in
  let config = env.config in
  let config =
    let dirfile = Filename.concat dir ignore_files_filename in
    if Sys.file_exists dirfile then
      list_ignore_files config (FileLines.read_file dirfile)
    else config in

  let config =
    let dirfile = Filename.concat dir ignore_headers_filename in
    if Sys.file_exists dirfile then
      list_ignore_headers config (FileLines.read_file dirfile)
    else config
  in
  let env = if config == env.config then env else
      { env with config }
  in

  Array.iter (fun file ->
    let lfile = String.lowercase file in
    match lfile with
    | "_obuild" | "_build" | ".git" | ".svn" -> ()
    | _ ->
      let dirfile = Filename.concat dir file in
      match try Some ( Sys.is_directory dirfile ) with
      | _ -> None
      with
      | None -> ()
      | Some true ->
        scan_dir env dirfile
      | Some false ->
        check_file env file dirfile
  ) files

and check_file env lfile dirfile =
  let len = String.length lfile in
  if lfile.[len-1] <> '~' && lfile.[0] <> '.' then
    let ext = try
                let pos = String.rindex lfile '.' in
                String.sub lfile pos (len-pos)
      with _ -> ""
    in
    match ext with
    | ".ml" | ".mli" | ".mll" | ".ocp" | ".mlp" | ".ml4" ->
      record_header env dirfile ml_header
    | ".c" | ".h" | ".cpp" | ".mly" | ".js" ->
      record_header env dirfile cc_header
    | ".sh" | ".ac" | ".in" | ".m4" ->
      record_header env dirfile sh_header

    | _ ->
      if not (StringSet.mem ext env.config.ignore_extensions) then
        match lfile with
        | "configure" | "makefile" ->
          record_header env dirfile sh_header
        | _ ->
          if not (StringSet.mem lfile env.config.ignore_files) then begin
            env.save_to_ignore <- StringSet.add lfile env.save_to_ignore;
            Printf.eprintf "Warning: unknown extension for file %S\n%!"
              dirfile;
          end

let scan_dirs config dirs =
  let env = {
    headers = Hashtbl.create 113;
    files = Hashtbl.create 113;
    config;
    save_to_ignore = StringSet.empty;
  } in

  reset_headers ();

  List.iter (fun (file, header_sep) ->
    let dirfile = Filename.concat config_dir file in
    if Sys.file_exists dirfile then
      record_header ~config:true env dirfile header_sep
  ) [ "headers.ml", ml_header;
      "headers.cc", cc_header;
      "headers.sh", sh_header];

  List.iter (fun dir ->
    if Sys.is_directory dir then
      scan_dir env dir
    else
      let lfile = String.lowercase (Filename.basename dir) in
      check_file env lfile dir
  ) dirs;
  env

let fprintf_loc oc file_name line_pos =
  Printf.fprintf oc "File %S, line %d, characters 0-1:\n" file_name line_pos


let print_headers skip_headers sep file_name =
  if sep.sep_headers <> [] then begin
    let oc = open_out file_name in
    Printf.fprintf oc "Report on %s\n" sep.sep_name;

    if sep.sep_headers <> [] then begin

      Printf.fprintf oc "\nExtracted headers\n";

      List.iter (fun header ->
        if not (StringSet.mem header.header_id skip_headers) then begin
          Printf.fprintf oc "\nHeader id: %s\n" header.header_id;
          if header.header_lines = [ header.header_sep.sep_name ] then begin
            Printf.fprintf oc "\n\n\n\n         EMPTY HEADER\n\n\n\n\n";
          end else begin
            Printf.fprintf oc "<<<\n";
            List.iter (fun line ->
                Printf.fprintf oc "   %s\n" line;
              ) header.header_lines;
            Printf.fprintf oc ">>>\n";
          end;
          List.iter (fun (line_pos, file) ->
              fprintf_loc oc file.file_name line_pos;
              Printf.fprintf oc "Warning: file with %d headers\n%!"
                (List.length file.file_headers)
          ) header.header_files;
        end
        ) sep.sep_headers;

    end;
    close_out oc;
    Printf.printf "File %S generated\n%!" file_name;
  end



let save_ignored env =
  if env.save_to_ignore <> StringSet.empty then begin
    let oc = open_out ignore_files_more_filename in
    StringSet.iter (fun line ->
      Printf.fprintf oc "%s\n" line) env.save_to_ignore;
    close_out oc;
    Printf.eprintf "Ignored files saved to %s\n%!" ignore_files_more_filename;
    Printf.eprintf "You can add it to your %s\n%!" ignore_files_filename

  end

let replace_header src_header dst_header line_pos file =
  Printf.printf "Replacing %s by %s on %s\n%!" src_header.header_id
    dst_header.header_id file.file_name;
  let lines = FileLines.read_file file.file_name in
  let rec insert_header pos lines rev_lines =
    if pos = line_pos then
      check_src_header lines src_header.header_lines rev_lines
    else
      match lines with
      | [] ->
        Printf.eprintf "Error: header %s not found in %S (EOF before pos)\n%!"
          src_header.header_id file.file_name;
        raise Not_found
      | line :: lines ->
        insert_header (pos+1) lines (line :: rev_lines)

  and check_src_header lines header_lines rev_lines =
    match lines, header_lines with
    | _, [] -> (List.rev rev_lines) @ dst_header.header_lines @ ("" :: lines)
    | [], _ ->
      Printf.eprintf "Error: header %s not found in %S (truncated header)\n%!"
        src_header.header_id file.file_name;
      raise Not_found
    | left :: lines, right :: header_lines ->
      if left <> right then begin
        Printf.eprintf "Error: header %s not found in %S (line mismatch)\n%!"
          src_header.header_id file.file_name;
        raise Not_found
      end;
      check_src_header lines header_lines rev_lines
  in
  try
    let lines = insert_header 0 lines [] in
    FileLines.write_file file.file_name lines;
    true
  with Not_found -> false

let add_default_header header file =
  Printf.printf "Adding header %s on %s\n%!" header.header_id file.file_name;
  (* This is the easiest one *)
  let sep = header.header_sep in
  let lines = FileLines.read_file file.file_name in
  let rec insert_header pos lines rev_lines =
    if pos = sep.sep_add_line then
      (List.rev rev_lines) @ header.header_lines @ (
        match lines with
        | "" :: _ -> lines
        | _ -> "" :: lines)
    else
      match lines with
      | [] -> (List.rev rev_lines) @ header.header_lines @ [""]
      | line :: lines ->
        insert_header (pos+1) lines (line :: rev_lines)
  in
  let lines = insert_header 0 lines [] in
  FileLines.write_file file.file_name lines;
  true

let () =
  let arg_add_default = ref [] in
  let arg_replace = ref [] in
  let arg_dirs = ref [] in
  let arg_replace_by = ref None in
  let arg_skip_headers = ref StringSet.empty in

  let arg_list = Arg.align [
    "--add-default", Arg.String (fun s ->
      arg_add_default := s :: !arg_add_default),
    "HEADER_ID Add this header as the default for these files";

    "--replace", Arg.String (fun s ->
      arg_replace := s :: !arg_replace),
    "SRC:DST Replace header SRC by header DST";
    "--replace-by", Arg.String (fun s ->
      arg_replace_by := Some s),
    "HEADER_ID Replace by this header";
    "--from", Arg.String (fun src_id ->
      match !arg_replace_by with
      | None ->
        Printf.eprintf "Error: --from should come after --replace-by\n%!";
        exit 2
      | Some dst_id ->
        arg_replace := (Printf.sprintf "%s:%s" src_id dst_id) :: !arg_replace
    ),
    "HEADER_ID Replace this header";
    "--skip", Arg.String (fun id ->
      arg_skip_headers := StringSet.add id !arg_skip_headers),
    "HEADER_ID skip this header";
  ] in
  let arg_usage =
    "ocp-check-headers [OPTIONS] DIRS : check OCaml headers in DIRS" in
  Arg.parse arg_list (fun dir ->
    arg_dirs := dir :: !arg_dirs) arg_usage;
  if !arg_dirs = [] then arg_dirs := ["."];
  let dirs = List.rev !arg_dirs in

  let config = {
    ignore_headers = StringSet.empty;
    ignore_files = StringSet.empty;
    ignore_extensions = StringSet.empty;
  }
  in
  let config = add_default_ignored config in

  let env = scan_dirs config dirs in

  save_ignored env;

  let need_undo = ref false in
  let undo_oc = lazy (open_out "check-headers.undo") in

  let env =
    if !arg_add_default <> [] then
      List.fold_left (fun env header_id ->
        try
          let header = Hashtbl.find env.headers header_id in
          let sep = header.header_sep in
          let empty_header_id = new_header_id sep.sep_name in
          try
            let empty_header = Hashtbl.find env.headers empty_header_id in
            let updates = ref 0 in
            List.iter (fun (_, file) ->
              if add_default_header header file then begin
                need_undo := true;
                Printf.fprintf (Lazy.force undo_oc)
                  "add:%s:%s\n" header_id file.file_name;
                incr updates
              end
            ) empty_header.header_files;
            Printf.printf "add_default %s: %d files changed\n%!"
              header_id !updates;
            if !updates > 0 then begin
              Printf.printf
                "Scanning again after %d changes for %s\n%!" !updates
                header_id;
              scan_dirs config dirs
            end else env
          with Not_found ->
            Printf.printf "add-default %s: no file with no header\n%!"
              header_id;
            env
        with Not_found ->
          Printf.eprintf "Error: default header %s not found\n%!" header_id;
          env
      ) env (List.rev !arg_add_default)
    else env
  in

  let _env =
    if !arg_replace <> [] then
      List.fold_left (fun env header_pair ->
        let src_id, dst_id = try
                               let pos = String.index header_pair ':' in
                               let len = String.length header_pair in
                               String.sub header_pair 0 pos,
                               String.sub header_pair (pos+1) (len-pos-1)
          with Not_found ->
            Printf.eprintf "Error: cannot parse pair %S\n%!" header_pair;
            exit 2
        in
        let src_header = try
                           Hashtbl.find env.headers src_id
          with Not_found ->
            Printf.eprintf "Error: source header of %S not found\n%!" header_pair;
            exit 2
        in
        let src_sep = src_header.header_sep in
        let dst_header = try
                           Hashtbl.find env.headers dst_id
          with Not_found ->
            Printf.eprintf "Error: destination header of %S not found\n%!" header_pair;
            exit 2
        in
        let dst_sep = dst_header.header_sep in

        if dst_sep != src_sep then begin
          Printf.eprintf "Error: %s and %s of different kind\n%!"
            src_id dst_id;
          exit 2
        end;
        let updates = ref 0 in
        List.iter (fun (line_pos, file) ->
          if replace_header src_header dst_header line_pos file then begin
            need_undo := true;
            incr updates;
            Printf.fprintf (Lazy.force undo_oc)
              "replace:%s:%d:%s:%s\n" src_id line_pos dst_id file.file_name;
          end
        ) src_header.header_files;
        Printf.printf "replace %s: %d files changed\n%!"
          src_id !updates;
        if !updates > 0 then begin
          Printf.printf
            "Scanning again after %d changes for %s\n%!" !updates
            src_id;
          scan_dirs config dirs
        end else env
      ) env (List.rev !arg_replace)
    else env
  in

  if !need_undo then close_out (Lazy.force undo_oc);

  print_headers !arg_skip_headers ml_header "headers-ml.txt";
  print_headers !arg_skip_headers cc_header "headers-cc.txt";
  print_headers !arg_skip_headers sh_header "headers-sh.txt";
  ()
