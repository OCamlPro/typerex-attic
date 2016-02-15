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

module MinUnix = Unix
module OnlyUnix = Unix

module StringMap = Map.Make(String)

let putenv = MinUnix.putenv
let execv = MinUnix.execv
let execve = MinUnix.execve
let chmod = MinUnix.chmod

let rec safe_mkdir dirname =
  if not (Sys.file_exists dirname) then begin
    safe_mkdir (Filename.dirname dirname);
    MinUnix.mkdir dirname 0o755;
  end
let symlink = OnlyUnix.symlink

let before s pos = String.sub s 0 pos
let after s pos =
  let len = String.length s in
  String.sub s pos (len - pos)

let cut_at s c =
  try
    let pos = String.index s c in
    before s pos,
    after s (pos+1);
  with _ -> s, ""

let is_directory filename = Sys.is_directory filename
(*  (MinUnix.lstat filename).MinUnix.st_kind = MinUnix.S_DIR *)


let list_directory dirname =
  List.sort compare (Array.to_list (Sys.readdir dirname))

(*
  let list = ref [] in
  let dir = OnlyUnix.opendir dirname in
  try
  while true do
  let file = OnlyUnix.readdir dir in
  if file <> "." && file <> ".." then
  list := file :: !list
  done;
  assert false
  with End_of_file ->
  OnlyUnix.closedir dir;
  List.sort compare !list
*)

let lines_of_file filename =
  let lines = ref [] in
  let ic = open_in filename in
  begin
    try
      while true do
        lines := input_line ic :: !lines
      done
    with _ -> ()
  end;
  close_in ic;
  List.rev !lines

let buf_size = 32764
let buf = Bytes.create buf_size

let copy src dst = (* copy_file *)
  let src = open_in_bin src in
  let dst = open_out_bin dst in
  let rec iter () =
    let nread = input src buf 0 buf_size in
    if nread > 0 then begin
      output dst buf 0 nread;
      iter ()
    end

  in
  iter ();
  close_in src;
  close_out dst


let skip_chars s cs =
  let rec iter s cs i len=
    if i = len then "" else
      if String.contains cs s.[i] then
        iter s cs (i+1) len
      else
        String.sub s i (len-i)
  in
  iter s cs 0 (String.length s)


let list_printer indent list =
  let rec list_printer indent pos list =
    match list with
    | [] ->
      Printf.printf "\n"
    | s :: tail ->
      let len = String.length s in
      if pos + len > 78 then begin
        Printf.printf "\n%s" indent;
        list_printer indent (String.length indent) list
      end else begin
        Printf.printf "%s " s;
        list_printer indent (pos + len + 1) tail
      end
  in
  Printf.printf "%s" indent;
  list_printer indent (String.length indent) list

(*
  let get_stdout_lines cmd args =
  let temp_file = Filename.temp_file "ocp-build-" ".out" in
  let new_stdout = Unix.openfile temp_file
  [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644 in
  let pid = Unix.create_process cmd args Unix.stdin new_stdout Unix.stderr in
  Unix.close new_stdout;
  let status = Unix.waitpid [] pid in
  let lines = ref [] in
  begin try
  let ic = open_in temp_file in
  begin

  try
  while true do
  lines := (input_line ic) :: !lines
  done
  with _ -> ()
  end;
  close_in ic;
  Sys.remove temp_file;
  with _ -> ()
  end;
  (status, List.rev !lines)
*)
