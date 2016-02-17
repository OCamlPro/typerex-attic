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

let filename_of_unit unit =
  match unit.unit_file with
  | COMP { file_name } | LIB { file_name } -> file_name

let modname_of_unit unit =  unit.unit_cmi_crc.cmi_comp.comp_modname

let print_module args env m =
#if OCAML_VERSION < "4.02" && ! (OCAML_VERSION = "4.01.0+ocp1")

  match try Some (Hashtbl.find env.components m) with Not_found -> None with
  | None ->
    Printf.printf "Module %S: not found\n%!" m
  | Some comp ->
#else
  match Hashtbl.find env.components m with
  | exception Not_found ->
    Printf.printf "Module %S: not found\n%!" m
  | comp ->
#endif
    Printf.printf "Module %S:\n%!" m;
    List.iter (fun cmi_crc ->
        Printf.eprintf "  Interface %s\n%!" (Digest.to_hex cmi_crc.cmi_digest);
        List.iter (fun cmi_file ->
            Printf.eprintf "    Intf described in %s\n%!" cmi_file.cmi_filename
          ) cmi_crc.cmi_cmi_impls;
        List.iter (fun cmi_file ->
            Printf.eprintf "    Intf used by %s\n%!" cmi_file.cmi_filename
          ) cmi_crc.cmi_cmi_users;

        List.iter (fun unit ->
            Printf.eprintf "    Bytecode implementation in %s\n%!"
              (filename_of_unit unit)
          ) cmi_crc.cmi_byte_impls;
        List.iter (fun unit ->
            Printf.eprintf "    Used by %s in %s\n%!"
              (modname_of_unit unit)
              (filename_of_unit unit)
          ) cmi_crc.cmi_byte_all_users;
        List.iter (fun unit ->
            Printf.eprintf "    Bytecode needed for %s in %s\n%!"
              (modname_of_unit unit)
              (filename_of_unit unit)
          ) cmi_crc.cmi_byte_users;

        List.iter (fun unit ->
            Printf.eprintf "    Native implementation in %s with CRC %s\n%!"
              (filename_of_unit unit)
              (Digest.to_hex unit.unit_impl_crc.cmx_digest)
          ) cmi_crc.cmi_asm_impls;
        List.iter (fun unit ->
            Printf.eprintf "    Used by %s in %s\n%!"
              (modname_of_unit unit)
              (filename_of_unit unit)
          ) cmi_crc.cmi_asm_all_users;
        List.iter (fun unit ->
            Printf.eprintf "    Native code needed for %s in %s\n"
              (modname_of_unit unit)
              (filename_of_unit unit);
            List.iter (function
                | (_, None) -> ()
                | (cmi_crc2, Some cmx_digest) ->
                  if cmi_crc2 == cmi_crc then
                    Printf.eprintf "     with CRC %s\n%!"
                      (Digest.to_hex cmx_digest)
              ) unit.unit_cmi_imports
          ) cmi_crc.cmi_asm_users;

      ) comp.comp_cmi_crcs
