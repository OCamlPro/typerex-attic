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



open ManagerMisc
open ManagerInit

let arg_handler () =
  let new_path =
    String.concat
      (String.make 1 ManagerWrapper.path_sep)
      (ManagerWrapper.manager_bindir :: ManagerWrapper.path) in
  Printf.printf "PATH=%S; export PATH\n%!" new_path

