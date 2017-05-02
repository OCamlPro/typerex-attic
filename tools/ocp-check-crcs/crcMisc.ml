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

let ignore_file ignore_set option =
  match option with
  | None -> ()
  | Some filename ->
    let ic = open_in filename in
    try
      while true do
        let line = input_line ic in
        ignore_set := StringSet.add line !ignore_set
      done;
      assert false
    with _ ->
      close_in ic
