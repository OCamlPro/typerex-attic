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

open Genlex

let lexer = Genlex.make_lexer [ ":" ]

type error =
  IdentExpected
| ColonExpected
| StringExpected

exception SyntaxError of error * string * int

let load filename =
  let ic = open_in filename in
  let lexbuf = Stream.of_channel ic in
  let lexer = lexer lexbuf in
  let idents = ref [] in

  let rec iter0 () =
    match Stream.next lexer with
    | Ident s -> iter1 (String.lowercase s)
    | Kwd _
    | Float _
    | String _
    | Char _
    | Int _
      -> raise (SyntaxError (IdentExpected, filename, Stream.count lexbuf))

  and iter1 s =
    match Stream.next lexer with
    | Kwd ":" -> iter2 s
    | _ -> raise (SyntaxError (ColonExpected, filename, Stream.count lexbuf))
  and iter2 s =
    match Stream.next lexer with
    | String v ->
      idents := (s, v) :: !idents;
      iter0 ()
    | _ -> raise (SyntaxError (StringExpected, filename, Stream.count lexbuf))

  in
  try
    iter0 ()
  with
  | Stream.Failure -> (* EOF *)
    close_in ic;
    !idents
  | e ->
    close_in ic;
    raise e
