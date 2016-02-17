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
  TODO:
  * check inconsistent cmx files, i.e. a .cmx file with a different CRC
    from a similar .cmx in a library
  * check .cmxa without corresponding .a
  * Check .cmxa without corresponding .cmx ?
*)

open StringCompat
open CrcTypes

let check_conflicting_libraries args env =

  let record set cmi_crc unit =
    match unit.unit_file with
    | COMP _ -> ()
    | LIB { file_name } ->
      try
        let cmas = StringMap.find cmi_crc.cmi_digest !set in
        cmas :=  file_name :: !cmas
      with Not_found ->
        set := StringMap.add cmi_crc.cmi_digest (ref [file_name]) !set
  in
  let check_dups comp set kind_str =
    if StringMap.cardinal !set > 1 then begin
      let modname = comp.comp_modname in
      let w = CrcWarning.create
          (Printf.sprintf "multiple:%s:%s" kind_str modname)
          (Printf.sprintf
             "%s module %S is present in different %s libraries"
             kind_str modname kind_str)
      in
      StringMap.iter (fun digest cmas ->
          Printf.kprintf w
            "   %s:" (Digest.to_hex digest);
          List.iter (fun cma ->
              Printf.kprintf w "      %s" cma
            ) !cmas
        ) !set
    end
  in
  Hashtbl.iter (fun _ comp ->
      let cmxa_set = ref StringMap.empty in
      let cma_set = ref StringMap.empty in
      List.iter (fun cmi_crc ->

          List.iter (record cma_set cmi_crc) cmi_crc.cmi_byte_impls;
          List.iter (record cmxa_set cmi_crc) cmi_crc.cmi_asm_impls;

        ) comp.comp_cmi_crcs;
      check_dups comp cma_set "byte";
      check_dups comp cmxa_set "native";

    ) env.components;
  ()

(* Detect interfaces that (1) have no implementation and (2) are used
   by other modules. *)

let check_missing_impls args env =

  let check_intf_without_impl comp cmi_crc impls users kind_str =

    if impls = [] && users <> [] then begin
      let modname = comp.comp_modname in
      let w = CrcWarning.create
          (Printf.sprintf "no-impl:%s:%s" kind_str modname)
          (Printf.sprintf "module %S has no %s implementation"
             modname kind_str) in
      Printf.kprintf w
        "  for digest %s" (Digest.to_hex cmi_crc.cmi_digest);
      List.iter (fun unit ->
          let COMP { file_name } | LIB { file_name } = unit.unit_file in
          Printf.kprintf w "   needed for %s in %S"
            unit.unit_cmi_crc.cmi_comp.comp_modname file_name
        ) users;
    end
  in
  Hashtbl.iter (fun _ comp ->
      List.iter (fun cmi_crc ->

          check_intf_without_impl comp cmi_crc
            cmi_crc.cmi_byte_impls cmi_crc.cmi_byte_users "bytecode";

          check_intf_without_impl comp cmi_crc
            cmi_crc.cmi_asm_impls cmi_crc.cmi_asm_users "native";

        ) comp.comp_cmi_crcs
    ) env.components

(* Find compilation units that cannot be linked because one of their
   dependencies has no implementation. For bytecode only, since the
   native code version is more complex (we would have to check the
   native CRC too). *)
let check_byte_linkable args env =

  let unlinkable_units = ref [] in
  let changes = ref true in
  while !changes do

    changes := false;

    Hashtbl.iter (fun _ comp ->
        List.iter (fun cmi_crc ->

            List.iter (fun unit ->
                (* Printf.eprintf "Checking %s\n" (CrcPrint.modname_of_unit unit); *)
                match unit.unit_linkable with
                | LinkErrors _ -> ()
                | Linkable -> ()
                | Unchecked ->
                  changes := true;
                  let unchecked = ref 0 in
                  let link_errors = ref [] in
                  List.iter (function
                      | (_, None) -> ()
                      | (cmi_crc2, Some _) ->
                        (* Printf.eprintf " Requires %s\n"
                           cmi_crc2.cmi_comp.comp_modname; *)

                        (* is one of the implementations linkable ? *)
                        let dep_linkable = ref false in
                        let dep_unchecked = ref false in
                        try
                          List.iter (fun unit2 ->
                              match unit2.unit_linkable with
                              | Linkable -> dep_linkable := true; raise Exit
                              | Unchecked -> dep_unchecked := true
                              | _ -> ()
                            ) cmi_crc2.cmi_byte_impls;
                          raise Exit
                        with Exit ->
                          if !dep_linkable then begin
                            (* Printf.eprintf "   Linkable !\n" *)
                          end else
                          if !dep_unchecked then begin
                            incr unchecked;
                            (* Printf.eprintf "   Still unchecked !\n" *)
                          end else begin
                            link_errors := (
                              NoByteImpl cmi_crc2.cmi_comp.comp_modname)
                              :: !link_errors;
                            (* Printf.eprintf "   Cannot link !!!!\n"; *)
                          end
                    ) unit.unit_cmi_imports;
                  match !link_errors with
                  | [] ->
                    if !unchecked = 0 then begin
                      (*
                      Printf.eprintf "Unit %s is linkable (%s)\n%!"
                        (CrcPrint.modname_of_unit unit)
                        (CrcPrint.filename_of_unit unit)
                      ; *)
                      unit.unit_linkable <- Linkable;
                    end
                  | _ ->
                    unit.unit_linkable <-
                      LinkErrors { link_errors = !link_errors };
                    unlinkable_units := unit :: !unlinkable_units;
              ) cmi_crc.cmi_byte_impls

          ) comp.comp_cmi_crcs
      ) env.components

  done;

  List.iter (fun unit ->
      let modname = CrcPrint.modname_of_unit unit in
      let filename = CrcPrint.filename_of_unit unit in
      let w = CrcWarning.create
          (Printf.sprintf "unlinkable:byte:%s:%s" modname filename)
          (Printf.sprintf "Bytecode Unit %s not linkable (%s)"
             modname filename) in
      match unit.unit_linkable with
      | Linkable | Unchecked -> assert false
      | LinkErrors { link_errors } ->
        List.iter (function
            | NoByteImpl modname ->
              Printf.kprintf w "  No bytecode implementation for %s" modname
            | _ -> ()
        ) link_errors
    ) !unlinkable_units






(* Find compilation units that cannot be linked because one of their
   dependencies has no implementation. For asmcode only, since the
   native code version is more complex (we check the native CRC too). *)

let check_asm_linkable args env =

  let unlinkable_units = ref [] in
  let changes = ref true in
  while !changes do

    changes := false;

    Hashtbl.iter (fun _ comp ->
        List.iter (fun cmi_crc ->

            List.iter (fun unit ->
                (* Printf.eprintf "Checking %s\n" (CrcPrint.modname_of_unit unit); *)
                match unit.unit_linkable with
                | LinkErrors _ -> ()
                | Linkable -> ()
                | Unchecked ->
                  changes := true;
                  let unchecked = ref 0 in
                  let link_errors = ref [] in
                  List.iter (function
                      | (_, None) -> ()
                      | (cmi_crc2, Some cmx_digest) ->
                       (* Printf.eprintf "  Requires %s\n"
                          cmi_crc2.cmi_comp.comp_modname; *)

                        (* is one of the implementations linkable ? *)
                        let dep_linkable = ref false in
                        let dep_unchecked = ref false in
                        let errors = ref [] in
                        try
                          if cmi_crc2.cmi_asm_impls = [] then
                            errors :=
                              (NoAsmImpl cmi_crc2)
                              :: !errors
                          else
                            List.iter (fun unit2 ->
                                if cmx_digest =
                                   CrcLoad.cmx_not_found_crc
                                || unit2.unit_impl_crc.cmx_digest =
                                   cmx_digest then
                                  match unit2.unit_linkable with
                                  | Linkable -> dep_linkable := true; raise Exit
                                  | Unchecked -> dep_unchecked := true
                                  | _ ->
                                    errors :=
                                      (NonLinkable unit2) :: !errors
                                else
                                  errors :=
                                    (WrongCmx unit2) :: !errors
                              ) cmi_crc2.cmi_asm_impls;
                          raise Exit
                        with Exit ->
                          if !dep_linkable then begin
                            (* Printf.eprintf "   Linkable !\n" *)
                          end else
                          if !dep_unchecked then begin
                            incr unchecked;
                            (* Printf.eprintf "   Still unchecked !\n" *)
                          end else begin
                            link_errors := !errors @ !link_errors;
                            (* Printf.eprintf "   Cannot link !!!!\n"; *)
                          end
                    ) unit.unit_cmi_imports;
                  match !link_errors with
                  | [] ->
                    if !unchecked = 0 then begin
                      (*
                      Printf.eprintf "Unit %s is linkable (%s)\n%!"
                        (CrcPrint.modname_of_unit unit)
                        (CrcPrint.filename_of_unit unit)
                      ; *)
                      unit.unit_linkable <- Linkable;
                    end
                  | _ ->
                    unit.unit_linkable <-
                      LinkErrors { link_errors = !link_errors };
                    unlinkable_units := unit :: !unlinkable_units;
              ) cmi_crc.cmi_asm_impls

          ) comp.comp_cmi_crcs
      ) env.components

  done;

  List.iter (fun unit ->
      let modname = CrcPrint.modname_of_unit unit in
      let filename = CrcPrint.filename_of_unit unit in
      let w = CrcWarning.create
          (Printf.sprintf "unlinkable:native:%s:%s" modname filename)
          (Printf.sprintf "Native unit %s not linkable (%s)"
             modname filename) in
      match unit.unit_linkable with
      | Linkable | Unchecked -> assert false
      | LinkErrors { link_errors } ->
        List.iter (function
            | NoAsmImpl cmi_crc ->
              Printf.kprintf w "  Cause: no native implementation for %s"
                cmi_crc.cmi_comp.comp_modname
            | NonLinkable unit ->
              Printf.kprintf w "  Cause: non linkable %s in %s"
                (CrcPrint.modname_of_unit unit)
                (CrcPrint.filename_of_unit unit)
            | WrongCmx unit ->
              Printf.kprintf w "  Cause: inconsistent CRC for %s in %s"
                (CrcPrint.modname_of_unit unit)
                (CrcPrint.filename_of_unit unit)
            | _ -> ()
        ) link_errors
    ) !unlinkable_units
