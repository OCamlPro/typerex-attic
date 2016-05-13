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

let release_date = CrcVersion.release_date
let command = "ocp-check-crcs"
let version = CrcVersion.version

let gen_manpage arg_list =
  let open Manpage2.Types in
  let man_command = command in
  let man_section = 1 in
  let man_date = release_date in
  let man_version = version in
  let man_org = "OCamlPro" in
  let man_short_descr = "Check consistency of an installed OCaml distribution"
  in
  let man_text1 = [
    P [ S "ocp-check-crcs "; I "OPTIONS" ];

    SH [ S "DESCRIPTION" ];
    P [ S "ocp-check-crcs can be used to check the consistency of an installed OCaml distribution.";
        S "It checks that all object files are linkable (no missing dependency) and consistent (checksums are OK).";
      ];
  ] in
  let man_text2 = [
  ]
  in
  Printf.printf "%s\n%!"
    (Manpage2.groff_page
       {
         man_command;
         man_short_descr;
         man_section;
         man_date;
         man_version;
         man_org;
         man_text1;
         man_args = arg_list;
         man_authors = CrcVersion.authors;
         man_bug = "contact@ocamlpro.com";
         man_text2;
         man_copyright_years = CrcVersion.copyright_years;
         man_copyright_holder = CrcVersion.copyright;
         man_license = CrcVersion.license;
         man_also = [ "ocp-build", "1"; "opam", "1"; ]
       };
    );

  exit 0



let () =
  let dirs = ref [] in
  let queries = ref [] in
  let manpage = ref false in
  let args = {
    arg_check_cmt = false;
    arg_load_filename = None;
    arg_save_filename = None;
    arg_ignore_files = None;
    arg_chdir = None;
    arg_ignore_warnings = None;
    arg_output = None;
  } in
  let arg_list = Arg.align [
      "-save", Arg.String (fun s -> args.arg_save_filename <- Some s),
      "FILE Save env in FILE";
      "-load", Arg.String (fun s -> args.arg_load_filename <- Some s),
      "FILE Load env from FILE";
      "-cmt", Arg.Unit (fun () -> args.arg_check_cmt <- true),
      " Check .cmt/.cmti too";
      "-ignore-files", Arg.String (fun filename ->
          args.arg_ignore_files <- Some filename),
      "FILE File containing files/directories to ignore";
      "-query", Arg.String (fun s -> queries := s :: !queries),
      "MODULE Query info on module";
      "-chdir", Arg.String (fun s -> args.arg_chdir <- Some s),
      "DIR Specified dirs are relative to DIR";
      "-ignore-warnings", Arg.String (fun filename ->
          args.arg_ignore_warnings <- Some filename
        ),
      "FILE File containing warning ids to ignore";
      "-o", Arg.String (fun filename ->
          args.arg_output <- Some (
              if filename = "-" then stdout else
                open_out filename)),
      "FILE Target output to FILE (default is stderr, - for stdout)";
      "-man", Arg.Set manpage, " Display manpage";
    ] in
  Arg.parse arg_list (fun dir -> dirs := dir :: !dirs) "ocp-check-crcs DIRS";
  if !manpage then gen_manpage arg_list;
  let env = match args.arg_load_filename, args.arg_chdir, !dirs with
    | None, None, [] ->
      Printf.eprintf "Error: you must specify either a dir or -load\n%!";
      exit 2
    | None, Some _, []
    | None, _, _ :: _ ->
      let dirs = match !dirs with
        | [] -> ["."]
        | dirs -> dirs in
      let env = CrcLoad.load_dirs args dirs in
      begin
        match args.arg_save_filename with
        | None -> ()
        | Some file ->
          CrcLoad.save_env file env;
          Printf.printf "Scan results saved in %S\n%!" file
      end;
      env
    | Some _, _,  _::_ ->
      Printf.eprintf "Error: you cannot specify both a dir and -load\n%!";
      exit 2
    | Some file, _, [] ->
      match args.arg_save_filename with
      | Some _ ->
        Printf.eprintf "Error: you cannot specify both -load and -save\n%!";
        exit 2
      | None ->
        CrcLoad.load_env file
  in
  match !queries with
  | [] ->
    CrcWarning.ignore_warnings := env.ignore_warnings;
    CrcMisc.ignore_file CrcWarning.ignore_warnings args.arg_ignore_warnings;

    CrcWarning.rev_warnings := env.rev_warnings;
    CrcWarning.print_all args;

    CrcChecks.check_conflicting_libraries args env;
    CrcWarning.print_all args;

    CrcChecks.check_missing_impls args env;
    CrcWarning.print_all args;

    CrcChecks.check_byte_linkable args env;
    CrcWarning.print_all args;

    CrcChecks.check_asm_linkable args env;
    CrcWarning.print_all args;

  | queries ->
    List.iter (fun m ->
        CrcPrint.print_module args env m
      ) (List.rev queries)
