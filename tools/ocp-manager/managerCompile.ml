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


open ManagerMisc
open ManagerWrapper
open ManagerInit

let command cmd =
  let x = Sys.command cmd in
  if x <> 0 then exit x

let arg_handler version force_arg =
  try
    let c = StringMap.find version compilers in
    if c.compiler_name <> version then raise Not_found;
    Printf.fprintf stderr
      "Version [%s] is already present among alternatives\n%!" version;
    if force_arg then raise Not_found;
    exit 2
  with Not_found ->
    let prefix = Filename.concat manager_roots_dir version in
    let cmd = Printf.sprintf
      "./configure --prefix %s" (*" -with-debug-runtime" *)
      prefix in
    (*  -no-shared-libs : does not work with 4.00.0 *)
    command cmd;
    command "make world opt opt.opt";
    command "make install";
    Printf.printf "Compilation OK. Setting new version\n%!";
    ManagerWrapper.add_compiler
      {
        compiler_name = version;
        compiler_prefix = prefix;
        compiler_kind = OCAML_MANAGER prefix; (* NOT CORRECT, but not used *)
      };
    ManagerSwitch.set_current_switch version
