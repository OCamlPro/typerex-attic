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
  Display the potentially global mutable values of a module.
*)

open StringCompat

open Cmt_format
open Typedtree
open TypedtreeIter

module TYPES : sig

  val string_of_type : Types.type_expr -> string

end = struct
open Format

let new_fmt () =
  let buf = Buffer.create 512 in
  let fmt = formatter_of_buffer buf in
  let flush () =
    pp_print_flush fmt ();
    let s = Buffer.contents buf in
    Buffer.reset buf ;
    s
  in
  (fmt, flush)

let (type_fmt, flush_type_fmt) = new_fmt ()
let _ =
  (* obsolete ?
  let (out, flush, outnewline, outspace) =
    Format.pp_get_all_formatter_output_functions type_fmt ()
   in
  pp_set_all_formatter_output_functions type_fmt
    ~out ~flush
    ~newline: (fun () -> out "\n  " 0 3)
    ~spaces: outspace
  *)
  let out = Format.pp_get_formatter_out_functions type_fmt () in
  Format.pp_set_formatter_out_functions type_fmt { out with
    out_newline = (fun () -> out.out_string "\n  " 0 3); };
  ()

let (modtype_fmt, flush_modtype_fmt) = new_fmt ()

let string_of_type t =
  Printtyp.mark_loops t;
  Printtyp.type_scheme_max ~b_reset_names: false type_fmt t;
  flush_type_fmt ()

end

let ppf = Format.std_formatter

let impure_init = ref false
let arg_ok = ref true
let pure_functions = ref StringSet.empty
let ignore_types = ref StringSet.empty
let ignore_modules = ref StringSet.empty

let nerrors = ref 0

let immutable_types =
 [
    Predef.path_int;
    Predef.path_char;
    Predef.path_float;
    Predef.path_bool;
    Predef.path_unit;
    Predef.path_list;
    Predef.path_option;
    Predef.path_nativeint;
    Predef.path_int32;
    Predef.path_int64;
    Predef.path_string; (* TODO: make it optional *)
  ]

module ForIterator = struct
  open Asttypes

  include DefaultIteratorArgument

  let in_structure_item = ref 0

  let impure_apply loc msg =
    incr nerrors;
    Format.fprintf ppf "%a\nApplication of potential impure function %s@."
      Location.print_loc loc msg


  let rec check_structure_item str =
    match str.str_desc with
#if OCAML_VERSION < "4.02"
    | Tstr_eval exp ->
#else
    | Tstr_eval (exp,_) ->
#endif
      check_expression exp
    | Tstr_value (rec_flag, bindings) ->
      check_bindings bindings

#if OCAML_VERSION < "4.02"
    | Tstr_module (_, _, m)
    | Tstr_include (m, _)
#else
    | Tstr_module { mb_expr = m }
    | Tstr_include { incl_mod = m }
#endif
      ->
      check_module_expr m

    | Tstr_recmodule mod_bindings ->
      (* (Ident.t * string loc * module_type * module_expr) list *) assert false
    | Tstr_class classes ->
      (* (class_declaration * string list * virtual_flag) list *) assert false

    | Tstr_primitive _
    | Tstr_type _
    | Tstr_exception _
#if OCAML_VERSION < "4.02"
    | Tstr_exn_rebind _
#else
    | Tstr_typext _
    | Tstr_attribute _
#endif
    | Tstr_modtype _
    | Tstr_open _
    | Tstr_class_type _
      -> ()

  and check_bindings bindings =
    List.iter check_binding bindings

  and check_module_expr m =
    match m.mod_desc with
    | Tmod_ident _ -> ()
    | Tmod_structure str -> check_structure str
    | Tmod_functor _ -> ()
    | Tmod_apply ( { mod_desc = Tmod_ident (path, _) }, _loc, _) ->
      let f_name = Path.name path in
      let pure = StringSet.mem f_name !pure_functions in
      if not pure then
        Format.fprintf ppf "%a\nPotential escape of potential mutable type through functor application@."
          Location.print_loc m.mod_loc

    | Tmod_apply (_m, _loc, _) ->
      Format.fprintf ppf "%a\nPotential escape of potential mutable type through functor application@."
          Location.print_loc m.mod_loc

    | Tmod_constraint (m, _mtype, _mtype_constr, _mcoerce) -> check_module_expr m
    | Tmod_unpack (exp, _mtype) -> check_expression exp

  and check_structure str =
    List.iter check_structure_item str.str_items

#if OCAML_VERSION < "4.02"
  and check_binding (pat, exp) =
#else
  and check_binding { vb_pat = pat; vb_expr = exp } =
#endif
    check_pattern pat;
    check_expression exp

  and check_expression exp =
    match exp.exp_desc with
    | Texp_let (rec_flag, bindings, exp) ->
      check_bindings bindings;
      check_expression exp

    | Texp_apply (
        { exp_desc = Texp_ident (f_path, _loc, _) }, f_args) ->
      let f_name = Path.name f_path in
      let pure = StringSet.mem f_name !pure_functions in
      if not pure && !impure_init then
        impure_apply exp.exp_loc (Printf.sprintf "%S" f_name);
      List.iter (fun (label, exp_o, opt) ->
        match exp_o with None -> () | Some exp ->
          if not pure && not !arg_ok then
            check_type (Printf.sprintf "arg to function %S" f_name)
              exp.exp_loc exp.exp_type;
          check_expression exp
      ) f_args

    | Texp_apply (f_exp, f_args) ->
      if !impure_init then
        impure_apply exp.exp_loc "anonymous";
      check_expression f_exp;
      List.iter (fun (label, exp_o, opt) ->
        match exp_o with None -> () | Some exp ->
          if not !arg_ok then
            check_type "arg to anon function" exp.exp_loc exp.exp_type;
          check_expression exp
      ) f_args

#if OCAML_VERSION < "4.02"
    | Texp_match (exp_match, bindings, _partial) ->
      check_expression exp_match; check_bindings bindings
    | Texp_try (exp_try, bindings) ->
      check_expression exp_try; check_bindings bindings
#else
    | Texp_match (exp_match, val_cases, exn_cases, _partial) ->
      check_expression exp_match; check_cases val_cases; check_cases exn_cases
    | Texp_try (exp_try, cases) ->
      check_expression exp_try; check_cases cases
#endif
    | Texp_array list
    | Texp_tuple list
      -> check_expressions list

    | Texp_variant ( _label, None) -> ()
    | Texp_variant ( _label, Some exp)
      -> check_expression exp
    | Texp_assert exp
    | Texp_lazy exp
      -> check_expression exp

    | Texp_pack m -> check_module_expr m
    | Texp_letmodule (_ident, _loc, m, exp) ->
      check_module_expr m;
      check_expression exp

    | Texp_ifthenelse (exp1, exp2, Some exp3) ->
      check_expressions [exp1; exp2; exp3]

#if OCAML_VERSION < "4.02"
    | Texp_when (exp1, exp2)
#endif
    | Texp_ifthenelse (exp1, exp2, None)
    | Texp_sequence (exp1, exp2)
    | Texp_while (exp1, exp2)
      -> check_expressions [ exp1; exp2]

#if OCAML_VERSION < "4.02"
    | Texp_construct (_loc, _constr, list, _bool) ->
#else
    | Texp_construct (_loc, _constr, list) ->
#endif
      check_expressions list
    | Texp_record (fields, None) ->
      List.iter (fun (_loc, _label, exp) -> check_expression exp) fields
    | Texp_record (fields, Some exp) ->
      check_expression exp;
      List.iter (fun (_loc, _label, exp) -> check_expression exp) fields

    | Texp_field (exp, _loc, _label) -> check_expression exp
    | Texp_setfield (exp1, _loc, _label, exp2) ->
      check_expressions [exp1; exp2]
    | Texp_for (_var, _loc, exp1, exp2, _dirflag, exp3) ->
      check_expressions [exp1; exp2; exp3]

#if OCAML_VERSION < "4.02"
    | Texp_assertfalse
#endif

    | Texp_function _
    | Texp_ident _
    | Texp_constant _
      -> ()
    | Texp_send (_, _, _)|Texp_new (_, _, _)|Texp_instvar (_, _, _)|
      Texp_setinstvar (_, _, _, _)|Texp_override (_, _)|Texp_object (_, _) ->
      assert false (* object style not supported *)

#if !(OCAML_VERSION < "4.02")

and check_cases cases =
   List.iter (fun c ->
     check_pattern c.c_lhs;
     check_expression c.c_rhs;
     match c.c_guard with
     | None -> () | Some exp -> check_expression exp
   ) cases
#endif

  and check_expressions list =
    List.iter check_expression list

(*
    | Texp_when of expression * expression
    | Texp_send of expression * meth * expression option
    | Texp_new of Path.t * Longident.t loc * Types.class_declaration
    | Texp_instvar of Path.t * Path.t * string loc
    | Texp_setinstvar of Path.t * Path.t * string loc * expression
    | Texp_override of Path.t * (Path.t * string loc * expression) list
    | Texp_object of class_structure * string list
*)

  and check_pattern pat =
    match pat.pat_desc with
    | Tpat_any -> ()
    | Tpat_var (ident, _loc) ->
      check_type (Printf.sprintf "set var %S" (Ident.name ident)) pat.pat_loc pat.pat_type
    | Tpat_alias (pat, ident, _) ->
      check_type  (Printf.sprintf "set var %S" (Ident.name ident)) pat.pat_loc pat.pat_type;
      check_pattern pat
    | Tpat_constant _ -> ()
#if OCAML_VERSION < "4.02"
    | Tpat_construct (_, _, list, _ )
#else
    | Tpat_construct (_, _, list)
#endif
    | Tpat_array list
    | Tpat_tuple list ->
      List.iter check_pattern list
    | Tpat_variant (_, None, _) -> ()
    | Tpat_variant (_, Some pat, _) -> check_pattern pat
    | Tpat_record (fields, closed_flag) ->
      List.iter (fun (_loc, _label, pat) -> check_pattern pat) fields
    | Tpat_or (pat1, pat2, row_desc_o) -> check_pattern pat1; check_pattern pat2
    | Tpat_lazy pat -> check_pattern pat

  and check_type msg loc ty =
    let open Types in
    match ty.desc with
    | Tarrow (_, _ty1, _ty2, _) -> ()
    | Tlink ty -> check_type msg loc ty
    | Tvar _ -> ()
    | Ttuple list -> List.iter (check_type msg loc) list
    | Tconstr (path, list, _abbrev) ->
      if not !impure_init && not (List.mem path immutable_types) then begin
        let sty = TYPES.string_of_type ty in
        if not (StringSet.mem sty !ignore_types) then begin
          incr nerrors;
          Format.fprintf ppf "%a\nPotential escape of %s with potential mutable type:\n   %s@."
            Location.print_loc loc msg sty
        end;
      end else
        List.iter (check_type msg loc) list

    | Tobject (_, _) -> assert false
    | Tfield (_, _, _, _) -> assert false
    | Tnil -> assert false
    | Tsubst _ -> assert false
    | Tvariant _ -> assert false
    | Tunivar _ -> assert false
    | Tpoly (_, _) -> assert false
    | Tpackage (_, _, _) -> assert false

  let enter_structure_item str =
    if !in_structure_item = 0 then begin
      check_structure_item str
    end;
    incr in_structure_item


  let leave_structure_item str =
    decr in_structure_item

(*
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
*)

end

module Iterator = MakeIterator(ForIterator)


let arg_by_file = ref true

let cmd= Filename.basename Sys.argv.(0)

let print_results () =
  Format.pp_print_flush ppf ()

let clean_results () =
  ()

(* TODO:
  For now, we put functions such as "Sys.getenv" as pure, but
 they involve reading what is external to the program (as Arg.parse).
 Maybe we should consider these function differently.

  Some of these functions are also impure when exceptions are
taken into argument (they can raise exception, thus aborting the
program). Should we differentiate them ?
*)


let add_stdlib () =
  List.iter (fun s ->
    pure_functions := StringSet.add s !pure_functions
  ) [
    "Pervasives.ref";
    "Pervasives.!";
    "Pervasives.@";
    "Pervasives.+";
    "Pervasives.-";
    "Pervasives.*";
    "Pervasives./";
    "Pervasives.+.";
    "Pervasives.-.";
    "Pervasives.*.";
    "Pervasives./.";
    "Pervasives.=";
    "Pervasives.~-";
    "Pervasives.raise";
    "Pervasives.incr";
    "Pervasives.asr";
    "Pervasives.asl";
    "Pervasives.lsr";
    "Pervasives.lsl";

    "List.map";
    "List.iter";

    "Array.length";
    "Array.get";
    "Array.create";
    "Array.append";

    "String.create";
    "Queue.create";
    "Stack.create";
    "Hashtbl.create";
    "Hashtbl.find";

    "Map.Make";
    "Set.Make";

    "Arg.parse";

    "Printf.printf";
    "Printf.fprintf";
    "Printf.eprintf";

    "Printexc.to_string";

    "Sys.getenv";

    (* TODO ? *)
    "StringMap.find";
    "StringMap.add";
    "StringSet.find";
    "StringSet.add";
    "IntMap.find";
    "IntMap.add";
    "IntSet.find";
    "IntSet.add";
    ];
  List.iter (fun s ->
    ignore_types := StringSet.add s !ignore_types
  ) [
    "Arg.spec";
    "Arg.doc";
    "Arg.usage_msg";
    "Arg.key";
    "Arg.anon_fun";

    "Digest.t";
  ]


let add_ignore filename =
  File.iter_lines (fun s ->
    if String.length s > 0 then
       match s.[0] with
       | '#' | ' ' | '/' -> ()
       | _ ->
         match OcpString.split s ':' with
           [] -> ()
         | [s] -> pure_functions := StringSet.add s !pure_functions
         | ["pure"; s] -> pure_functions := StringSet.add s !pure_functions
         | ["type"; s] -> ignore_types := StringSet.add s !ignore_types
         | ["module"; s] -> ignore_modules := StringSet.add s !ignore_modules
       | _ -> ()
  ) filename

let arg_usage = String.concat "\n" [
    "";
    Printf.sprintf "%s <filename>.cmt : check global values" cmd;
    "";
    "-ignore IGNORE_FILENAME : can be used to specify pure functions and";
    "  types to ignore. Each line should either be a comment (starting with #),";
    "  or be pure:FUNCTION or type:TYPE, where FUNCTION is a pure function to";
    "  ignore, and TYPE a string representation of a type to ignore (immutable)";
    "";
    "Complete list of arguments:";
  ]


let nfiles = ref 0
let _ =
  Clflags.annotations := true;

  Arg.parse (Arg.align [
    "-stdlib", Arg.Unit add_stdlib, " Add functions and types to ignore from the stdlib";
    "-ignore", Arg.String add_ignore, "FILENAME File containing types and functions to ignore";
    "-arg-not-ok", Arg.Clear arg_ok,
    " Care about mutable values passed to functions during initialization";
    "-impure-init", Arg.Set impure_init,
    " Track all impure function applications during initialization";
  ]) (fun filename ->
    if Filename.check_suffix filename ".cmt" then begin
      let modname = Filename.chop_suffix (Filename.basename filename) ".cmt" in
      let modname = String.capitalize modname in
      if not (StringSet.mem modname !ignore_modules) then
      let cmt = Cmt_format.read_cmt filename in
      match cmt.cmt_annots with
        Implementation typedtree ->
        incr nfiles;
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
    arg_usage;
  print_results ();

  if !nerrors > 0 then begin
    Format.fprintf ppf "Results: %d potential hits in %d files@." !nerrors !nfiles;
end
