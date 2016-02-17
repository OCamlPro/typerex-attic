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

type t = {
  imp_filename : string;
  imp_modname : string;
  imp_digest : Digest.t option;
  imp_imports : (string * Digest.t option) list;
  imp_values : string list array;
  imp_types : string list array;
}

let expr_paths = Hashtbl.create 1123
let type_paths = Hashtbl.create 1123

let clear_tables () =
  Hashtbl.clear expr_paths;
  Hashtbl.clear type_paths;
  ()

let record paths path =
  match path with
  | [] -> ()
  | _ ->
    if not (Hashtbl.mem paths path) then
      Hashtbl.add paths path ()

module Iterator = TypedtreeIter.MakeIterator(struct

  include TypedtreeIter.DefaultIteratorArgument

  let rec persist_path_apply paths = function
    | Path.Pident ident ->
      let name = Ident.name ident in
      if Ident.persistent ident then [ name ] else []
    | Path.Papply (p1, p2) ->
      record paths (persist_path_apply paths p1);
      record paths (persist_path_apply paths p2);
      []
    | Path.Pdot(path,name,_) ->
      match persist_path_apply paths path with
      | [] -> []
      | path -> path @ [ name ]

  let persist_path_apply paths path =
    record paths (persist_path_apply paths path)


  open Types

  let rec enter_abbrev abbrev =
    match abbrev with
    | Mcons (_, path, _,_, abbrev) ->
      persist_path_apply type_paths path;
      enter_abbrev abbrev
    | Mlink abbrev -> enter_abbrev !abbrev
    | Mnil -> ()

  let enter_type ty =
    Btype.iter_type_expr (function ty ->
      match ty.desc with
      | Tconstr (path, _, abbrev) ->
        persist_path_apply type_paths path;
        enter_abbrev  !abbrev;
      | Tpackage (path, _, _) ->
        persist_path_apply type_paths path
      | Tobject (_, tyor) -> begin
        match !tyor with
        | None -> ()
        | Some (path, _) -> persist_path_apply type_paths path
      end
      | Tvariant { row_name = Some (path, _) } ->
        persist_path_apply type_paths path
      | _ -> ()
    ) ty;
    ()

  open Typedtree

  let enter_core_type ctyp =
    match ctyp.ctyp_desc with
    | Ttyp_constr (path, _, _ ) -> persist_path_apply type_paths path
#if OCAML_VERSION < "4.02"
    | Ttyp_class (path, _,_,_) -> persist_path_apply type_paths path
#else
    | Ttyp_class (path, _,_) -> persist_path_apply type_paths path
#endif
    | _ -> ()

  let enter_expression exp =
    enter_type exp.exp_type;
    match exp.exp_desc with
    | Texp_ident ( path, _, _ )
    | Texp_new (path, _, _)
      ->
      persist_path_apply expr_paths path
    | Texp_instvar ( path1, path2, _ )
    | Texp_setinstvar (path1, path2, _, _)
      ->
      persist_path_apply expr_paths path1;
        persist_path_apply expr_paths path2
    | Texp_override (path, list) ->
      persist_path_apply expr_paths path;
      List.iter (fun (path, _, _) ->
        persist_path_apply expr_paths path) list
    | _ -> ()

  let enter_pattern pat =
    enter_type pat.pat_type

  let enter_class_expr = function
    | { cl_desc = Tcl_ident(path, _,_) } ->
      persist_path_apply expr_paths path
    | _ -> ()

  let enter_module_expr = function
    | { mod_desc = Tmod_ident (path, _) } ->
      persist_path_apply expr_paths path
    | _ -> ()

  let enter_structure_item str = match str.str_desc with
#if OCAML_VERSION < "4.02"
    | Tstr_exn_rebind (_, _, path, _) ->
#else
    | Tstr_exception { ext_kind = Text_rebind (path, _) } ->
#endif
      persist_path_apply expr_paths path
#if OCAML_VERSION < "4.02"
    | Tstr_open (_, path, _) ->
#else
    | Tstr_open { open_path = path } ->
#endif
    (* Opening is not using ! *)
    (* persist_path_apply expr_paths path *) ()
    | _ -> ()
end)

let get_paths paths =
  let list = ref [] in
  Hashtbl.iter (fun path () ->
    list := path :: !list
  ) paths;
  Array.of_list !list

let of_cmt imp_filename =
  let cmt = Cmt_format.read_cmt imp_filename in
  match cmt.Cmt_format.cmt_annots with
  | Cmt_format.Implementation str ->
    clear_tables ();
    Iterator.iter_structure str;
    let imp_digest = match cmt.Cmt_format.cmt_interface_digest with
      | Some crc -> Some crc
      | None ->
        let cmi_filename =
          String.sub imp_filename 0 (String.length imp_filename-1) ^ "i" in
        try
          let cmi = Cmi_format.read_cmi cmi_filename in
          match cmi.Cmi_format.cmi_crcs with
            [] -> assert false
#if OCAML_VERSION < "4.02"
         | (_, crc) :: _ -> Some crc
#else
         | (_, crc) :: _ -> crc
#endif
        with _ -> None
    in
    let imp_imports =
#if OCAML_VERSION < "4.02"
      List.map (fun (name, crc) -> (name, Some crc))
#endif
        cmt.Cmt_format.cmt_imports
    in
    {
      imp_filename;
      imp_modname = cmt.Cmt_format.cmt_modname;
      imp_values = get_paths expr_paths;
      imp_types = get_paths type_paths;
      imp_imports;
      imp_digest;
    }
  | _ -> failwith "Not an implementation"

let () =
  Printexc.register_printer (fun exn ->
    match exn with
    | Cmi_format.Error error ->
      let b = Buffer.create 100 in
      let ppf = Format.formatter_of_buffer b in
      Cmi_format.report_error ppf error;
      Format.fprintf ppf "@.";
      Some (Buffer.contents b)
    | _ -> None
  )
