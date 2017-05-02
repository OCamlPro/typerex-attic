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
open ImportExtract

type set = {
  set_name : string;
  mutable set_ok : bool;
  mutable set_units : set_unit list;

  mutable set_unresolved : StringSet.t;
  (* transitive closure of reachable sets *)
  mutable set_subsets : set StringMap.t;
  (* transitive closure of reachable units *)
  mutable set_subunits : set_unit StringMap.t;
  mutable set_unkunits : StringSet.t;
}

and unit_desc = {
  unit_name : string;
  mutable unit_sets : set_unit list;
}

and set_unit = {
  su_unit : unit_desc;
  su_set : set;
  su_file : string option;
  mutable su_imp : ImportExtract.t option;
  mutable su_values : unit_path StringMap.t;
  mutable su_import_values : unit_path StringMap.t;
}

and unit_path = {
  path_name : string;
  path_unit : set_unit;
  path_path : string list;
  mutable path_users : set_unit StringMap.t;
}

let all_units = ref StringMap.empty
let all_sets = ref StringMap.empty
let all_files = ref StringMap.empty

let unknown_units = ref StringSet.empty

let display results only_packages sets =

  (* record sets *)
  let provided_sets =
    List.map (fun (set_name, set_units) ->
      if StringMap.mem set_name !all_sets then begin
        Printf.eprintf "Error: set %S defined twice\n%!" set_name;
        exit 2
      end;
      let set = {
        set_name;
        set_ok = true;
        set_units = [];

        set_unresolved = StringSet.add "<not done>" StringSet.empty;
        set_subsets = StringMap.empty;
        set_subunits = StringMap.empty;
        set_unkunits = StringSet.empty;
      } in
      all_sets := StringMap.add set_name set !all_sets;
      List.iter (fun (unit_name, su_file) ->
        let unit = try
                     StringMap.find unit_name !all_units
          with Not_found ->
            let unit = {
              unit_name;
              unit_sets = [];
            } in
            all_units := StringMap.add unit_name unit !all_units;
            unit
        in
        let su = {
          su_file;
          su_set = set;
          su_unit = unit;
          su_imp = None;
          su_values = StringMap.empty;
          su_import_values = StringMap.empty;
        } in
        unit.unit_sets <- su :: unit.unit_sets;
        set.set_units <- su :: set.set_units;
        set.set_subunits <- StringMap.add unit_name su set.set_subunits;
        match su_file with
        | None -> ()
        | Some file ->
          all_files := StringMap.add file su !all_files
      ) set_units;
      set
    ) sets in

  let only_packages =
    if only_packages = [] then provided_sets else
      List.map (fun set_name ->
        try
          StringMap.find set_name !all_sets
        with Not_found ->
          Printf.eprintf "Error: -only %S, package not known\n%!" set_name;
          exit 2
      ) only_packages
  in

  List.iter (fun imp ->
    try
      let su = StringMap.find imp.imp_filename !all_files in
      su.su_imp <- Some imp;
    with Not_found ->
      try
        let unit = StringMap.find imp.imp_modname !all_units in
        match unit.unit_sets with
          [ su ] -> su.su_imp <- Some imp;
        | _ ->
          Printf.eprintf "Warning: file %s could not be resolved\n"
            imp.imp_filename;
          Printf.eprintf "   Module %s is included in %s\n%!"
            imp.imp_modname
            (String.concat " "
               (List.map (fun su -> su.su_set.set_name) unit.unit_sets))
      with Not_found ->
        Printf.eprintf "Warning: file %S is not attached to any set\n%!"
          imp.imp_filename;
  ) results;


  let changed = ref true in

  let rec add_sub set subset =

    if set != subset then begin (* break cycles *)

      if not (StringMap.mem subset.set_name set.set_subsets) then begin
        set.set_subsets <-
          StringMap.add subset.set_name subset set.set_subsets;
        changed := true;
      end;

      StringMap.iter (fun _ subset ->
        add_sub set subset
      ) subset.set_subsets;

      StringMap.iter (fun unit_name su ->
        try
          let su2 = StringMap.find unit_name set.set_subunits in
          if su != su2 then begin
            Printf.eprintf "Error: set %S depends on two different %S, from sets %S and %S\n%!"
              set.set_name unit_name
              su.su_set.set_name su2.su_set.set_name
            end
        with Not_found ->
          set.set_subunits <- StringMap.add unit_name su
            set.set_subunits;
          changed := true;
      ) subset.set_subunits
    end
  in

  while !changed do
    (*    Printf.eprintf "iteration\n%!"; *)
    changed := false;
    StringMap.iter (fun _ set ->
      if set.set_unresolved <> StringSet.empty then begin
        set.set_unresolved <- StringSet.empty;

        StringMap.iter (fun _ subset ->
          add_sub set subset
        ) set.set_subsets;

        List.iter (fun su ->
          match su.su_imp with
          | None -> ()
          | Some imp ->
            let require_unit unit_name =
              if not (StringMap.mem unit_name set.set_subunits) &&
                not (StringSet.mem unit_name set.set_unkunits) then
                try
                  let unit = StringMap.find unit_name !all_units in
                  match unit.unit_sets with
                  | [ su ] ->

                    let subset = su.su_set in
                    add_sub set subset;
                  | _ ->
                    set.set_unresolved <-
                      StringSet.add unit_name set.set_unresolved;
                with Not_found ->
                  set.set_unkunits <- StringSet.add unit_name set.set_unkunits;
                  unknown_units := StringSet.add unit_name !unknown_units;
            in
            (* First, solve interface dependencies *)
            List.iter (fun (unit_name, _ ) ->
              require_unit unit_name
            ) imp.imp_imports;

        (* Second, solve implementation dependencies.
           We don't do it yet, would it provide us with more dependencies ?
           or more accurate ones ?
        *)
            Array.iter (fun path ->
              match path with
              | [] -> assert false
              | unit_name :: _ ->
                require_unit unit_name
            ) imp.imp_values;

        ) set.set_units;
        end
    ) !all_sets;

  done;

  StringMap.iter (fun _ set ->
    List.iter (fun su ->
      match su.su_imp with
      | None -> ()
      | Some imp ->
        Array.iter (fun path ->
          match path with
          | [] -> assert false
          | unit_name :: _ ->
            try
              let su2 = StringMap.find unit_name set.set_subunits in
              let path_name = String.concat "." path in
              let p =
                try
                  StringMap.find path_name su2.su_values
                with Not_found ->
                  let p = {
                    path_unit = su2;
                    path_path = path;
                    path_name;
                    path_users = StringMap.empty;
                  } in
                  su2.su_values <- StringMap.add path_name p su2.su_values;
                  p
              in
              p.path_users <- StringMap.add su.su_unit.unit_name su
                p.path_users;
              su.su_import_values <- StringMap.add path_name p
                su.su_import_values
            with Not_found -> ()
        ) imp.imp_values
    ) set.set_units;
  ) !all_sets;

  if !unknown_units <> StringSet.empty then begin
    Printf.eprintf "Warning: unknown units: ";
    StringSet.iter (fun unit_name ->
      Printf.eprintf " %s" unit_name
    ) !unknown_units;
    Printf.eprintf "\n%!"
  end;

  StringMap.iter (fun _ set ->
    if set.set_unresolved <> StringSet.empty then begin
      Printf.eprintf "set %S:" set.set_name;
      StringMap.iter (fun _ set ->
        Printf.eprintf " %s" set.set_name;
      ) set.set_subsets;
      Printf.eprintf "\n";
      Printf.eprintf "   %d unresolved:"
        (StringSet.cardinal set.set_unresolved);
      StringSet.iter (fun s ->
        Printf.eprintf " %s (could be:" s;
        let unit = StringMap.find s !all_units in
        List.iter (fun su ->
          Printf.eprintf " %s" su.su_set.set_name;
        ) unit.unit_sets;
        Printf.eprintf ")";
      ) set.set_unresolved;
      Printf.eprintf "\n%!";
    end;
  ) !all_sets;

  let only = ref StringMap.empty in
  List.iter (fun set ->
    only := StringMap.add set.set_name set !only;
    StringMap.iter (fun _ set ->
      only := StringMap.add set.set_name set !only;
    ) set.set_subsets
  ) only_packages;
  !only
