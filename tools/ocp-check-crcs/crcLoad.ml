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
open CrcTypes

let cmx_not_found_crc =
  "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let ignore_files = ref StringSet.empty

let load_dirs args dirs =
  let env = {
    components = Hashtbl.create 113;
    cmi_crcs = Hashtbl.create 113;
    (* it is possible to refer to a native implementation just by name,
       so each cmx is stored in two entries, one is (name, Some crc),
       and the other is (name, None). *)
    cmx_crcs = Hashtbl.create 113;
    cmi_files = [];
    cmo_files = [];
    cma_files = [];
    cmx_files = [];
    cmxa_files = [];
    ignore_warnings = StringSet.empty;
    rev_warnings = [];
  } in
  let find_cmi ( (comp_modname, cmi_digest) as crc ) =
    try
      Hashtbl.find env.cmi_crcs crc
    with Not_found ->
      let cmi_comp = try
          Hashtbl.find env.components comp_modname
        with Not_found ->
          let comp = {
            comp_modname;
            comp_cmi_crcs = [];
          } in
          Hashtbl.add env.components comp_modname comp;
          comp
      in
      let cmi_crc = {
        cmi_comp;
        cmi_digest;

        cmi_cmi_impls = [];
        cmi_cmi_users = [];
        cmi_byte_impls = [];
        cmi_byte_users = [];
        cmi_byte_all_users = [];
        cmi_asm_impls = [];
        cmi_asm_users = [];
        cmi_asm_all_users = [];
      } in
      Hashtbl.add env.cmi_crcs crc cmi_crc;
      cmi_comp.comp_cmi_crcs <- cmi_crc :: cmi_comp.comp_cmi_crcs;
      cmi_crc
  in

  let anchor_byte cu =
    let  { Cmo_format.cu_name;
           cu_imports;
         } = cu in
    let rec iter crcs byte_imports byte_crc =
      match crcs with
      | [] -> (byte_imports, byte_crc)
#if OCAML_VERSION < "4.02"
      | (cmo_modname, cmo_crc) :: crcs ->
#else
      | (cmo_modname, None) :: crcs ->
        iter crcs byte_imports byte_crc
      | (cmo_modname, Some cmo_crc) :: crcs ->
#endif
        let cmi_crc = find_cmi (cmo_modname, cmo_crc) in
        if cmo_modname = cu_name then
          match byte_crc with
          | Some _ -> assert false
          | None ->
            iter crcs byte_imports (Some cmi_crc)
        else
          iter crcs (cmi_crc :: byte_imports) byte_crc
    in
    let (cmi_imports, byte_crc) = iter cu_imports [] None in
    let byte_used = ref StringSet.empty in
    List.iter (fun (reloc, _) ->
        match reloc with
        | Cmo_format.Reloc_literal _ -> ()
        | Cmo_format.Reloc_primitive _ -> ()
        | Cmo_format.Reloc_getglobal id
        | Cmo_format.Reloc_setglobal id ->
          let modname = Ident.name id in
          if not (StringSet.mem modname !byte_used) then begin
            byte_used := StringSet.add modname !byte_used;
          end
      ) cu.Cmo_format.cu_reloc;
    (*
    if byte_imports = [] then begin
      Printf.eprintf "%s has no hard deps\n%!" cu_name;
    end;
*)
    let byte_imports = List.map (fun cmi_crc ->
        cmi_crc,
        if StringSet.mem cmi_crc.cmi_comp.comp_modname !byte_used then
          Some cmi_crc.cmi_digest
        else None
      ) cmi_imports in
    match byte_crc with
    | None -> assert false
    | Some byte_crc -> (byte_crc, byte_imports, (), [])
  in

  let anchor_asm (cu, cmx_digest) =
    let  { Cmx_format.ui_name;
           ui_imports_cmi;
           ui_imports_cmx;
         } = cu in
    let rec iter crcs asm_imports asm_crc =
      match crcs with
      | [] -> (asm_imports, asm_crc)
#if OCAML_VERSION < "4.02"
      | (cmx_modname, cmx_crc) :: crcs ->
#else
      | (cmx_modname, None) :: crcs ->
        iter crcs asm_imports asm_crc
      | (cmx_modname, Some cmx_crc) :: crcs ->
#endif
        let cmi_crc = find_cmi (cmx_modname, cmx_crc) in
        if cmx_modname = ui_name then
          match asm_crc with
          | Some _ -> assert false
          | None ->
            iter crcs asm_imports (Some cmi_crc)
        else
          iter crcs (cmi_crc :: asm_imports) asm_crc
    in
    let (cmi_imports, cmi_crc) = iter ui_imports_cmi [] None in
    let rec iter crcs asm_imports =
      match crcs with
      | [] -> asm_imports
#if OCAML_VERSION < "4.02"
      | (cmx_modname, cmx_crc) :: crcs ->
        iter crcs (StringMap.add cmx_modname cmx_crc asm_imports)
#else
      | (cmx_modname, None) :: crcs ->
        iter crcs (StringMap.add cmx_modname cmx_not_found_crc asm_imports)
      | (cmx_modname, Some cmx_crc) :: crcs ->
        iter crcs (StringMap.add cmx_modname cmx_crc asm_imports)
#endif
    in
    let asm_imports = iter ui_imports_cmx StringMap.empty in
    let cmi_imports = List.map (fun cmi_crc ->
        let modname = cmi_crc.cmi_comp.comp_modname in
        cmi_crc,
        try Some (StringMap.find modname asm_imports) with
        Not_found -> None
      ) cmi_imports in
    match cmi_crc with
    | None -> assert false
    | Some cmi_crc ->
      (cmi_crc, cmi_imports,
       { cmx_comp = cmi_crc.cmi_comp;
         cmx_digest;
       }, [])
  in

  let new_unit dirfile anchor_impl record_unit ui =
    let (unit_cmi_crc, unit_cmi_imports,
         unit_impl_crc, unit_impl_imports) = anchor_impl ui in
    let rec cmx_file = {
      file_name = dirfile;
      file_contents = unit;
    }
    and unit = {
      unit_file = COMP cmx_file;
      unit_cmi_crc;
      unit_cmi_imports;
      unit_impl_crc;
      unit_impl_imports;
      unit_linkable = Unchecked;
    }
    in
    record_unit unit;
    (unit, cmx_file)
  in

  let new_libunit cma_file anchor_impl record_unit cu =
    let (unit_cmi_crc, unit_cmi_imports,
         unit_impl_crc, unit_impl_imports) = anchor_impl cu in
    let unit = {
      unit_file = LIB cma_file;
      unit_cmi_crc;
      unit_cmi_imports;
      unit_impl_crc;
      unit_impl_imports;
      unit_linkable = Unchecked;
    } in
    cma_file.file_contents <- unit :: cma_file.file_contents;
    record_unit unit;
    unit
  in

  let record_byte unit =
    List.iter (fun (cmi_crc, need_impl) ->
        cmi_crc.cmi_byte_all_users <- unit :: cmi_crc.cmi_byte_all_users;
        match need_impl with
        | None -> ()
        | Some _ ->
          cmi_crc.cmi_byte_users <- unit :: cmi_crc.cmi_byte_users;
      ) unit.unit_cmi_imports;
    unit.unit_cmi_crc.cmi_byte_impls <- unit :: unit.unit_cmi_crc.cmi_byte_impls;
  in

  let record_asm unit =
    unit.unit_cmi_crc.cmi_asm_impls <- unit :: unit.unit_cmi_crc.cmi_asm_impls;
    List.iter (fun (cmi_crc, need_impl) ->
        cmi_crc.cmi_asm_all_users <- unit :: cmi_crc.cmi_asm_all_users;
        match need_impl with
        | None -> ()
        | Some _ ->
          cmi_crc.cmi_asm_users <- unit :: cmi_crc.cmi_asm_users
      ) unit.unit_cmi_imports;
  in

  let load_with_magic file_name magic_number input_value =
    let ic = open_in_bin file_name in
    try
      let buffer = try
#if OCAML_VERSION = "4.01.0+ocp1"
          Misc.input_bytes ic (String.length magic_number)
#else
          really_input_string ic (String.length magic_number)
#endif
        with e ->
          failwith "truncated file"
      in
      if buffer <> magic_number then begin
        if String.sub buffer 0 9 = String.sub magic_number 0 9 then
          Printf.kprintf failwith "version mismatch magic %S" buffer
        else
          Printf.kprintf failwith "bad object magic %S" buffer;
      end;
      let lib = try input_value ic with _ ->
        failwith "corrupted binary content"
      in
      close_in ic;
      lib
    with Failure failure ->
      close_in ic;
      let _w = CrcWarning.create
          (Printf.sprintf "bad-file:%s" file_name)
          (Printf.sprintf "Cannot read %s, %s" file_name failure)
      in
      raise Exit
  in

  let load_cma file_name =
    let cma = load_with_magic file_name Config.cma_magic_number
        (fun ic ->
           let pos = input_binary_int ic in
           seek_in ic pos;
           (input_value ic : Cmo_format.library)
        ) in

    let cma_file = {
      file_name;
      file_contents = [];
    } in
    List.iter (fun cu ->

        let _unit = new_libunit cma_file anchor_byte record_byte cu in
        ()
      ) cma.Cmo_format.lib_units;
    env.cma_files <- cma_file :: env.cma_files;
  in

  let load_cmo file_name =
    let cu = load_with_magic file_name Config.cmo_magic_number
        (fun ic ->
           let pos = input_binary_int ic in
           seek_in ic pos;
           (input_value ic : Cmo_format.compilation_unit)
        ) in

    let (unit, cmo_file) = new_unit file_name anchor_byte record_byte cu in
    env.cmo_files <- cmo_file :: env.cmo_files;
  in

  let load_cmi dirfile =
    let { Cmi_format.cmi_name;
          cmi_sign;
          cmi_crcs;
          cmi_flags } = load_with_magic dirfile Config.cmi_magic_number
        Cmi_format.input_cmi in

    let (cmi_modname, cmi_digest, cmi_imports) =
      match cmi_crcs with
#if OCAML_VERSION < "4.02"
      | (cmi_modname, cmi_digest) :: cmi_imports ->
#else
      | (cmi_modname, Some cmi_digest) :: cmi_imports ->
        let cmi_imports = List.fold_left (fun x y ->
            match y with
              (_,None) -> x
            | (modname, Some crc)  -> (modname, crc) :: x
          ) [] cmi_imports in
#endif
        (cmi_modname, cmi_digest, cmi_imports)
      | _ -> assert false
    in
    assert (cmi_modname = cmi_name);
    let cmi_crc = find_cmi (cmi_modname, cmi_digest) in
    let cmi_imports = List.map find_cmi cmi_imports in
    let cmi_file = {
      cmi_filename = dirfile;
      cmi_crc;
      cmi_imports;
    } in
    env.cmi_files <- cmi_file :: env.cmi_files;
    cmi_crc.cmi_cmi_impls <- cmi_file :: cmi_crc.cmi_cmi_impls;
    List.iter (fun cmi_crc ->
        cmi_crc.cmi_cmi_users <- cmi_file :: cmi_crc.cmi_cmi_users
      ) cmi_imports;
  in

  let load_cmt dirfile =
    let _cmt = Cmt_format.read_cmt dirfile in
    ()
  in

  let load_cmxa file_name =
    let cmxa = load_with_magic file_name Config.cmxa_magic_number
        (input_value : in_channel -> Cmx_format.library_infos) in

    let cmxa_file = {
      file_name;
      file_contents = []
    } in
    List.iter (fun (cu, cmx_crc) ->
        let _unit = new_libunit cmxa_file anchor_asm record_asm (cu, cmx_crc) in
        ()
      ) cmxa.Cmx_format.lib_units;
    env.cmxa_files <- cmxa_file :: env.cmxa_files;
  in

  let load_cmx file_name =
    let (ui, cmx_digest) = load_with_magic file_name Config.cmx_magic_number
        (fun ic ->
           let ui = (input_value ic : Cmx_format.unit_infos) in
           let cmx_digest = Digest.input ic in
           (ui, cmx_digest)) in

    let (_unit, cmx_file) = new_unit file_name anchor_asm record_asm
        (ui, cmx_digest) in
    env.cmx_files <- cmx_file :: env.cmx_files;

  in

  let is_directory file =
    try Sys.is_directory file with _ -> false
  in

  let filename_concat dir file =
    if dir = "." then file else Filename.concat dir file
  in

  let rec check_dir dir =
    if not ( Sys.file_exists (filename_concat dir ".ocp-check-crcs-stop" ))
    then
    let files = try Sys.readdir dir with _ -> [||]  in
    Array.iter (fun file ->
        if not (StringSet.mem file !ignore_files) then
          let dirfile = filename_concat dir file in
          if String.lowercase file = "ocp-check-crcs.ignore" then
            CrcMisc.ignore_file CrcWarning.ignore_warnings (Some dirfile)
          else
          if is_directory dirfile then check_dir dirfile else
            let ext = try
                let pos = String.rindex file '.' in
                let len = String.length file in
                String.lowercase (String.sub file pos (len-pos))
              with _ -> ""
            in
            (* Printf.eprintf "file %S -> %S\n%!" dirfile ext; *)
            try
              match ext with
              | ".cma" -> load_cma dirfile
              | ".cmi" -> load_cmi dirfile
              | ".cmo" -> load_cmo dirfile
              | ".cmt" -> if args.arg_check_cmt then load_cmt dirfile
              | ".cmti" -> if args.arg_check_cmt then load_cmt dirfile
              | ".cmx" -> load_cmx dirfile
              | ".cmxa" -> load_cmxa dirfile
              | _ -> ()
            with
            | Exit -> ()
            | exn ->
              Printf.eprintf "Error: exception %S while loading %S\n%!"
                (Printexc.to_string exn) dirfile
      ) files
  in

  CrcMisc.ignore_file ignore_files args.arg_ignore_files;
  begin match args.arg_chdir with
    | None -> List.iter check_dir dirs
    | Some chdir ->
      let current_dir = Sys.getcwd () in
      Sys.chdir chdir;
      List.iter check_dir dirs;
      Sys.chdir current_dir
  end;
  env.ignore_warnings <- !CrcWarning.ignore_warnings;
  env.rev_warnings <- !CrcWarning.rev_warnings;
  env

let save_env file env =
  let oc = open_out_bin file in
  output_value oc (env : env);
  close_out oc

let load_env file =
  let ic = open_in_bin file in
  let env = (input_value ic : env) in
  close_in ic;
  env
