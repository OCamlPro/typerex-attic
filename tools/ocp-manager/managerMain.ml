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

open ManagerMisc
open ManagerInit

let something_done = ref false
let ok () = something_done := true

let force_arg = ref false

let arg_list = ref []

let tool = Filename.basename Sys.argv.(0)

let arg_usage =
  String.concat "\n" [
    Printf.sprintf "%s [options] files" tool;
    "";
    "ocp-manager is a tool to easily manage multiple installations";
    "of OCaml, such as OPAM compiler switches. It creates a";
    "directory ~/.ocp/manager-bin/ containing wrappers for";
    "most OCaml commands.";
    "";
    "You should add this directory to your PATH in your shell";
    "configuration, with a command such as:";
    "";
    "     eval `ocp-manager -config`";
    "";
    "Once you are using ocp-manager, you don't need to use";
    "`opam config env` anymore, as switching in ocp-manager";
    "is immediatly propagated to the OCaml tools wrappers.";
    "";
    "See the manpage for deeper information on how to use";
    "ocp-manager.";
    "";
    "Available options are:";
  ]


let get_arg arg_help default =
  let open OcpManpage.RAW in
  let (n, arg_help) = OcpString.cut_at arg_help ' ' in
  let n =
    if n = "" then
      default
    else
      n
  in
  [I n], arg_help

let groff_of_args arg_list =
  let open OcpManpage.RAW in
  List.map (fun (arg_name, arg_spec, arg_help) ->
    let (arg_spec, arg_help) = match arg_spec with
      | Arg.Int _ -> get_arg arg_help "INT"
      | Arg.String _ -> get_arg arg_help "STRING"
      | Arg.Unit _
      | _ -> ([], arg_help)
    in
    let arg_help = skip_chars arg_help " \t" in
    LI (
      (B arg_name) :: (S " ") :: arg_spec
      ,
      [ S arg_help ]
    )
  ) arg_list

let gen_manpage () =
  let open OcpManpage in
  let open RAW in
  let man_name = String.uppercase ManagerVersion.command in
  let man_section = 1 in
  let man_date = ManagerVersion.release_date in
  let man_release = Printf.sprintf "%s %s"
      ManagerVersion.command ManagerVersion.version in
  let man_org = "OCamlPro" in
  let man_text1 = [
    SH [ S "NAME" ];
    P [ S "ocp-manager - tool to manage different OCaml versions" ];
    SH [ S "SYNOPSIS" ];
    P [ S "ocp-manager "; I "OPTIONS" ];

    SH [ S "DESCRIPTION" ];
    P [ S "ocp-manager is a tool to easily manage different installations of OCaml on the same user account." ;
        S "It is simpler to use than 'opam switch', because it does not require to re-run 'eval `opam config env`' all the time." ;
      ];

    SH [ S "INSTALLATION" ];
    P [ S "ocp-manager uses a directory of wrappers around OCaml tools.";
        S " This directory is located in ~/.ocp/manager/bin/, and should be";
        S " added in top position on your PATH variable.";
          S "The easiest way is to do it in your shell configuration file";
        S "(~/.bashrc, ~/.shrc, etc.), using the command:";
      ];
    P [ S "    eval `ocp-manager -config`"; ];

    SH [ S "MANAGING SWITCHES" ];

    P [ S "Switches are different installations of OCaml, either in ";
        S "~/.ocaml/roots/, or in ~/.opam/. To discover which switches ";
        S "are currently available, use :";
      ];
    P [ S "    ocp-manager -list" ];
    P [ S "Switches starting with 'opam:' are installed the ones ";
        S "created and managed by OPAM."; ];
    P [ S "To globally change the current switch, you can use for example:"; ];
    P [ S "    ocp-manager -set opam:4.01.0" ];
    P [ S "Now, the current switch is 4.01.0 from OPAM. Running 'ocaml'";
        S " should print:"; ];
    P [ S "        OCaml version 4.01.0"; ];
    P [ S "It is also possible to change the current switch only in the";
        S  " current shell. For that, set the OCAML_VERSION variable:"
      ];
    P [ S "    export OCAML_VERSION=opam:3.12.1"; ];
    P2 [ S "    ocaml" ];
    P [ S " should print:"; ];
    P [ S "        Objective Caml version 3.12.1"; ];
    P [ S "but only in the current shell." ];

    SH [ S "PROJECT SWITCH" ];
    P [ S "It is also possible to define a per-project switch.";
        S " This can be used to always use the same version of";
        S " OCaml in some project or directory.";
        S " For that, you should either define a file '.ocp-switch'";
        S " or a file '.opam\\-switch' in the top-directory of the project.";
        S " The '.ocp\\-switch' should contain the switch name as expected";
        S " by ocp\\-manager, and the '.opam\\-switch' should contain the";
        S " OPAM switch name instead."
      ];

    SH [ S "MANAGING WRAPPERS" ];
    P [ S "For each OCaml tool, ocp\\-manager should have a wrapper that";
        S " will decide which command to run when called.";
        S " You can list all wrappers using:";
      ];
    P [ S "    ocp-manager -tools" ];
    P [ S "If there is no wrapper for some tool, you can add it:"; ];
    P [ S "    ocp-manager -add-tool atdgen" ];
    P [ S "Or you can just add all the tools from the current switch";
        S " bin/ directory:"; ];
    P [ S "    ocp-manager -add-all" ];

    SH [ S "DEFAULT TOOLS" ];
    P [ S "Some tools are sometimes available in some switches and not";
        S " in other switches. You can use ocp\\-manager to create";
        S " a default tool for all switches. For example:" ];
    P [ S "    ocp-manager -add-default ~/.opam/4.01.0/bin/ocp-indent" ];
    P [ S "will make ocp-indent from the 4.01.0 OPAM switch the default";
        S " ocp-indent tool for all switches, when no other ocp-indent";
        S " command is available.";
      ];

    SH [ S "AUTO-UPDATE" ];
    P [ S "ocp\\-manager uses an auto-update mechanism. If you want to install";
        S " a new version of ocp-manager, you just need to run it.";
        S "The new ocp\\-manager will notice that it is running for";
        S " the first time and will replace any previously installed";
        S " version of ocp\\-manager. You can use the \\-force\\-update";
        S " option to re-install an older version of ocp\\-manager." ];

    SH [ S "OPTIONS" ];
  ] in
  let man_text2 = [
    SH [ S "AUTHOR" ];
    P [ S "Fabrice Le Fessant <Fabrice.Le_Fessant@inria.fr>" ];

    SH [ S "BUGS" ];

    P [ S "Use" ];
    P [ B "https://github.com/OCamlPro/ocp-manager/issues" ];
    P [ S "to report bugs in ocp-manager." ];

    SH [ S "COPYRIGHT" ];
    P [ S "Copyright (c) 2011, 2012, 2013, 2014" ];
    P [ S "INRIA & OCamlPro SAS." ];

    SH [ S "SEE ALSO" ];
    P [ B "opam"; S "(1)" ];
  ]
  in
  let man_text = man_text1 @ groff_of_args !arg_list @ man_text2 in
  Printf.printf "%s\n%!"
    (groff_page
       {
         man_name;
         man_section;
         man_date;
         man_release;
         man_org;
         man_text;
       };
    );

  exit 0

let _ =
  arg_list := Arg.align [
      "-list", Arg.Unit (fun _ ->
        ManagerSwitch.list_switches ();
        ok ()), " List available switches";
      "-set", Arg.String (fun s ->
        ManagerSwitch.set_current_switch s;
        ok ()), "SWITCH Set current switch";
      "-dir", Arg.String (fun s ->
        ManagerSwitch.print_directory s;
        ok ()), "KIND Echo current directory for KIND (bin,lib,prefix)";
      "-config", Arg.Unit (fun _ ->
        ManagerConfig.arg_handler ();
        ok ()), " Shell config for using ocp-manager wrappers";

      "-tools", Arg.Unit (fun _ ->
        ManagerTools.print_commands ();
        ok ()), " List available ocaml tools";
      "-missing-tools", Arg.Unit (fun _ ->
        ManagerTools.print_missing ();
        ok ()), " List missing tools (in current switch)";
      "-add-tool", Arg.String (fun s ->
        ManagerTools.add_command s;
        ok ()), "CMD Manage this command from now on";
      "-remove-tool", Arg.String (fun s ->
        ManagerTools.remove_command s;
        ok ()), "CMD Remove this command from managed tools";
      "-add-all", Arg.Unit (fun s ->
        ManagerTools.add_all_commands ();
        ok ()), " Manage all commands from the switch from now on";

      "-add-default", Arg.String (fun s ->
        ManagerTools.add_default_command s;
        ok ()), "CMD Set this executable as the default for this command";
      "-remove-default", Arg.String (fun s ->
        ManagerTools.remove_default s;
        ok ()), "CMD Remove this command from default";


      "-compile", Arg.String (fun s ->
        ManagerCompile.arg_handler s !force_arg;
        ok ()), "SWITCH Compile ocaml distribution in current directory as SWITCH";

      "-restore", Arg.Unit (fun _ ->
        ManagerRestore.arg_handler ();
        ok ()), " Restore computer distribution";

      "-force-update", Arg.Unit (fun _ ->
        ManagerInit.auto_update ();
        ok ()), " Force auto-update with this binary";

      "-version", Arg.Unit (fun () ->
        Printf.printf "%s\n%!" ManagerVersion.version;
        exit 0;
      ), " Print version and exit";

      "-v", Arg.Unit (fun () ->
        Printf.printf "ocp-manager, version %s\n" ManagerVersion.version;
        Printf.printf "Author: Fabrice Le Fessant <fabrice.le_fessant@inria.fr>\n";
        Printf.printf "Maintained by: OCamlPro SAS\n";
        Printf.printf "Bug-tracker: https://github.com/OCamlPro/ocp-manager/issues\n%!";
        exit 0;
      ), " Print version and exit";

      "-f", Arg.Set force_arg, " force when possible";
      "-man", Arg.Unit gen_manpage, " Print Manpage to stdout";
    ]

let _ =
  try
    Arg.parse !arg_list
      (fun s -> Arg.usage !arg_list arg_usage) arg_usage;


    if not !something_done then
      Arg.usage !arg_list arg_usage
    else
      flush stdout;
    if ManagerInit.first_install then begin
      Printf.eprintf "%s\n%!"
        (String.concat "\n" [
      "";
    "Don't forget to update your configuration files with";
    "";
    "    eval `ocp-manager -config`";
   ""])
    end

  with
  | e ->
    Printf.fprintf stderr "Uncaught exception %s\nExiting\n%!" (Printexc.to_string e);
    exit 2
