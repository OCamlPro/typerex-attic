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


(*
  Display with which types polymorphic values are used, especially the ones
relying on structural comparisons (=, <, >, List.assoc, etc.)
*)

open StringCompat

open Cmt_format
open Typedtree
open TypedtreeIter

type occurrence = {
  mutable occ_name : string;
  mutable occ_type : Types.type_expr;
  mutable occ_locs : Location.t list;
}

type suspect = {
  suspect_name : string;
  mutable suspect_types : occurrence StringMap.t;
}

let ppf = Format.std_formatter

let suspects = ref StringMap.empty

let usual_suspects_Pervasives = [
  "Pervasives.<";
  "Pervasives.>";
  "Pervasives.compare";
  "Pervasives.=";
]

(* We only use 'Hashtbl.add', as we only want to print the creation of
   an Hashtbl.t *)
let usual_suspects_Hashtbl = [
  "Hashtbl.add";
]

let usual_suspects_List = [
  "List.mem";
  "List.mem_assoc";
  "List.remove_assoc";
  "List.assoc";
]

let simple_paths = [
  Predef.path_int;
  Predef.path_char;
  Predef.path_string;
  Predef.path_float;
  Predef.path_bool;
  Predef.path_unit;
  Predef.path_array;
  Predef.path_list;
  Predef.path_option;
  Predef.path_nativeint;
  Predef.path_int32;
  Predef.path_int64;
]

let rec simple_function ty =
  let open Types in
  match ty.desc with
  | Tarrow (_, ty1, ty2, _) -> simple_type ty1 && simple_function ty2
  | Tlink ty -> simple_function ty
  | _ -> simple_type ty

and simple_type ty =
  let open Types in
  match ty.desc with
  | Tvar _ -> false
  | Tarrow (_, ty1, ty2, _) -> false
  | Ttuple list -> List.for_all simple_type list
  | Tconstr (path, list, _) ->
    simple_path path && List.for_all simple_type list
  | Tlink ty -> simple_type ty
  | _ -> false

and simple_path path =
  let simple = List.mem path simple_paths in
(*  Printf.eprintf "simple_path %s %b\n" (Path.name path) simple; *)
  simple

let simple_function name ty =
  let simple = simple_function ty in
(*  Printf.eprintf "simple_function %s %b\n" name simple; *)
  simple

let new_suspect name =
  if not (StringMap.mem name !suspects) then begin
    let s = {
      suspect_name = name;
      suspect_types = StringMap.empty;
    } in
    suspects := StringMap.add name s !suspects
  end

let new_occurrence s ty loc =
  Printtyp.reset ();
  Buffer.clear Format.stdbuf;
  Printtyp.type_expr Format.str_formatter ty;
  let name = Format.flush_str_formatter () in
  try
    let occ = StringMap.find name s.suspect_types in
    occ.occ_locs <- loc :: occ.occ_locs
  with Not_found ->
    let occ = {
      occ_locs = [ loc ];
      occ_name = name;
      occ_type = ty;
    } in
    s.suspect_types <- StringMap.add name occ s.suspect_types

module ForIterator = struct
  open Asttypes

  include DefaultIteratorArgument

  let enter_expression exp =
    match exp.exp_desc with
    | Texp_apply ({ exp_desc = Texp_ident (path, _, vdesc);
                    exp_loc = loc } as f, args) ->
      let name = Path.name path in
      begin try
              let s = StringMap.find name !suspects in
              new_occurrence s f.exp_type loc
        with Not_found ->
          ()
      end

    | _ -> ()
end

module Iterator = MakeIterator(ForIterator)


let cmd= Filename.basename Sys.argv.(0)

let arg_by_file = ref true
let arg_one_line = ref false
let arg_simple_types = ref false

let print_results () =
  StringMap.iter (fun _ s ->
    StringMap.iter (fun _ occ ->
      if !arg_simple_types || not (simple_function occ.occ_name occ.occ_type) then
      if !arg_one_line then
        List.iter (fun loc ->
          Format.fprintf ppf "Value %s : %s at %a\n" s.suspect_name occ.occ_name
            Location.print_loc loc
        ) occ.occ_locs
      else begin
        Format.fprintf ppf "Value %s : %s at:\n" s.suspect_name occ.occ_name;
        List.iter (fun loc ->
          Format.fprintf ppf "%a\n"  Location.print_loc loc
        ) occ.occ_locs;
      end
    ) s.suspect_types
  ) !suspects;

  Format.pp_print_flush ppf ()

let clean_results () =
  StringMap.iter (fun _ s ->
    s.suspect_types <- StringMap.empty;
  ) !suspects

let add_stdlib_suspects () =
  List.iter new_suspect (List.concat [
    usual_suspects_Pervasives;
    usual_suspects_Hashtbl;
    usual_suspects_List;
  ])

let _ =
  Clflags.annotations := true;

  Arg.parse [
    "-stdlib", Arg.Unit add_stdlib_suspects, " : add suspects from stdlib";
    "-Pervasives", Arg.Unit (fun _ ->
      List.iter new_suspect usual_suspects_Pervasives;
    ), " : add suspects from Pervasives (=,<,>,etc.)";
    "-List", Arg.Unit (fun _ ->
      List.iter new_suspect usual_suspects_List;
    ), " : add suspects from List (assoc, mem)";
    "-Hashtbl", Arg.Unit (fun _ ->
      List.iter new_suspect usual_suspects_Hashtbl;
    ), " : add suspects from Hashtbl (add, find)";

    "-suspect", Arg.String new_suspect, " <Path> : add to suspects";
    "-merge-files", Arg.Clear arg_by_file, " : merge results between files ";
    "-one-line", Arg.Set arg_one_line, " : display each occurrence on one line";
    "-simple-types", Arg.Set arg_simple_types, " : print also simple types";
  ] (fun filename ->

    if !suspects = StringMap.empty then
      add_stdlib_suspects ();

    if Filename.check_suffix filename ".cmt" then begin
      let cmt = Cmt_format.read_cmt filename in
      match cmt.cmt_annots with
        Implementation typedtree ->
          Iterator.iter_structure typedtree;
          if !arg_by_file then begin
            print_results ();
            clean_results ();
          end
      | _ ->
        Printf.fprintf stderr "File was generated with an error\n%!";
        exit 2
    end
  )
    (Printf.sprintf "%s <filename>.cmt : check polymorphic functions" cmd);
  print_results ()
