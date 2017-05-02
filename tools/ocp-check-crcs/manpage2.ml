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

module Types = struct

  type div =
  | SH of span list
  | LI of span list * span list
  | P of span list
  | P2 of span list

  and span =
  | S of string
  | B of string
  | I of string

  type 'man_block man_page = {
    man_command : string;   (* "opam" "gcc" *)
    man_short_descr : string; (* one line descr *)
    man_section : int;
    man_date : string;
    man_version : string; (* "SOFT VERSION" *)
    man_org : string; (* GNU, OPAM Manual *)
    man_text1 :'man_block list;
    man_args : (string * Arg.spec * string) list;
    man_text2 :'man_block list;
    man_authors : string list;
    man_bug : string;
    man_copyright_years : string;
    man_copyright_holder : string;
    man_license : string;
    man_also : (string * string) list;
  }

end



let before s pos =
  String.sub s 0 pos
let after s pos =
  String.sub s (pos + 1) (String.length s - pos - 1)
let cut s pos =
  try
    before s pos, after s pos
  with _ -> s, ""

let cut_at s c =
  try
    let pos = String.index s c in
    cut s pos
  with _ -> s, ""

let skip_chars s cs =
  let rec iter s cs i len=
    if i = len then "" else
      if String.contains cs s.[i] then
        iter s cs (i+1) len
      else
        String.sub s i (len-i)
  in
  iter s cs 0 (String.length s)


open Types

  let rec groff_div div =
    match div with
    | P spans ->
      [ ".P";
        groff_spans spans
      ]
    | P2 spans ->
      [
        ".sp -1";
        ".P";
        groff_spans spans
      ]
    | SH spans ->
      [ ".SH " ^ groff_spans spans ]
    | LI (title_spans, text_spans) ->
      [ ".TP 4";
        groff_spans title_spans;
        groff_spans text_spans
      ]

  and groff_spans spans =
    String.concat "" (List.map groff_span spans)

  and groff_span span =
    match span with
    | S s -> s
    | B s -> Printf.sprintf "\\fB%s\\fR" s
    | I s -> Printf.sprintf "\\fI%s\\fR" s

let get_arg arg_help default =
  let (n, arg_help) = cut_at arg_help ' ' in
  let n =
    if n = "" then
      default
    else
      n
  in
  [I n], arg_help

let groff_of_args arg_list =
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

let groff_page p =
  let man_name = String.uppercase p.man_command in
  let man_text = [
    SH [ S "NAME" ];
    P [ S (Printf.sprintf "%s - %s"
          p.man_command p.man_short_descr)
      ];
    SH [ S "SYNOPSIS" ];
  ] @ p.man_text1
    @ (match p.man_args with
        | [] -> []
        | args ->
          groff_of_args args
      )
    @ p.man_text2
    @ [
      SH [ S "AUTHOR" ];
    ] @
    List.map (fun name ->
        P [ S name ]
      ) p.man_authors
    @ [
    SH [ S "BUGS" ];

    P [ S "Use" ];
    P [ B p.man_bug ];
    P [ S (Printf.sprintf "to report bugs in %s." p.man_command) ];

    SH [ S "COPYRIGHT" ];
    P [ S ("Copyright (c) " ^ p.man_copyright_years) ];
    P [ S p.man_copyright_holder ];
    P [ S p.man_license ];

    SH [ S "SEE ALSO" ];
    P (List.flatten (List.map (fun (tool, section) ->
        [ B tool; S (Printf.sprintf "(%s) " section) ]) p.man_also));

    ]
  in

  String.concat "\n" ([
      ".\\\" Pipe this output to groff -man -Tutf8 | less";
      ".\\\"";
      Printf.sprintf ".TH %S %d %S %S %S"
        man_name
        p.man_section
        p.man_date
        (Printf.sprintf "%s %s" p.man_command p.man_version)
        p.man_org;
      ".\\\" Disable hyphenantion and ragged-right";
      ".nh";
      ".ad l";
    ] @
      (List.flatten (List.map groff_div man_text))
    )
