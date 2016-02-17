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
open ImportExtract
open ImportArch

let dot_header =  "digraph G {

  node [
    shape=\"box\",
    style=\"rounded\",
    penwidth = 1,
    width=2.0,
    fontname = \"Arial\",
    fontsize = 12
  ];

  edge [
    color=\"#142b30\",
    arrowhead=\"vee\",
    arrowsize=0.75,
    penwidth = 2,
  ];
"

let dot_trailer = "}\n"

let with_dot dot_filename f =
  let oc = open_out dot_filename in
  output_string oc dot_header;
  f oc;
  output_string oc dot_trailer;
  close_out oc;
  let pdf_filename = (Filename.chop_extension dot_filename) ^ ".pdf" in
  let cmd = Printf.sprintf "dot -Tpdf -o %s %s"
    pdf_filename dot_filename in
  if Sys.command cmd <> 0 then
    Printf.eprintf "Command failed: %s\n%!" cmd
  else
    Printf.printf "%s generated\n%!" pdf_filename

type node = {
  node_name : string;
  mutable node_to : node StringMap.t;
}

let new_node nodes name =
    try
      StringMap.find name !nodes
    with Not_found ->
      let node = {
        node_name = name;
        node_to = StringMap.empty;
      } in
      nodes := StringMap.add name node !nodes;
      node

let print_nodes filename nodes =
  with_dot filename (fun oc ->
  StringMap.iter (fun _ n1 ->
    StringMap.iter (fun _ n2 ->
      Printf.fprintf oc "   %S -> %S\n" n1.node_name n2.node_name;
    ) n1.node_to;
  ) nodes;
  )

let print_graph_of_set_opens sets =

  let nodes = ref StringMap.empty in
  StringMap.iter (fun _ set ->
    let n = new_node nodes set.set_name in
    List.iter (fun su ->
       match su.su_imp with
          | None -> ()
          | Some imp ->
            List.iter (fun (unit_name, _ ) ->
              try
                let su = StringMap.find unit_name set.set_subunits in
                let n2 = new_node nodes su.su_set.set_name in
                n.node_to <- StringMap.add n2.node_name n2 n.node_to
              with Not_found -> ()
            ) imp.imp_imports;
    ) set.set_units;
  ) sets;

  print_nodes "archi2.dot" !nodes;

  ()


let print_graph_of_set_imports sets =

  let nodes = ref StringMap.empty in
  StringMap.iter (fun _ set ->
    let n = new_node nodes set.set_name in
    List.iter (fun su ->
       match su.su_imp with
          | None -> ()
          | Some imp ->

            Array.iter (fun path ->
              match path with
              | [] -> ()
              | unit_name :: _ ->
                try
                  let su = StringMap.find unit_name set.set_subunits in
                  let n2 = new_node nodes su.su_set.set_name in
                  n.node_to <- StringMap.add n2.node_name n2 n.node_to
                with Not_found -> ()
            ) imp.imp_values;
    ) set.set_units;
  ) sets;

  print_nodes "archi3.dot" !nodes;

  ()

type kind = S | M | V | SM | SV | MV | SMV
let string_of_kind = function
  | S -> "S" | M -> "M" | V -> "V"
  | SM -> "SM" | SV -> "SV" | MV -> "MV"
  | SMV -> "SMV"

type n = {
  mutable edges : n StringMap.t;
  mutable values : n option;
}

let rec get n list =
  match list with
  | [] -> n
  | name :: list ->
    let n =
      try StringMap.find name n.edges
      with Not_found ->
        let nn = { edges = StringMap.empty; values = None; } in
        n.edges <- StringMap.add name nn n.edges;
        nn
    in
    get n list

let add_edge edges n1 n2 =
  let n1 = get edges n1 in
  let n1 =
    match n1.values with
    | None ->
      let n = { edges = StringMap.empty; values = None; } in
      n1.values <- Some n;
      n
    | Some n -> n
  in
  let _n2 = get n1 n2 in
  ()

let print_graphes_per_set sets =

  List.iter (fun src ->
    List.iter (fun dst ->

      let edges = { edges = StringMap.empty; values = None; } in
      let rev_edges = { edges = StringMap.empty; values = None; } in

      StringMap.iter (fun _ set ->

        List.iter (fun su ->
          StringMap.iter (fun _ p ->

            let n1, set1 =
              let set_name = set.set_name in
              let su_name = su.su_unit.unit_name in
              let n1 =
                match src with
                | S -> [set_name]
                | M ->  [su_name]
                | SM ->
                  [set_name; su_name]
                | _ -> assert false
              in
              n1, set
            in
            let n2, set2 =
              let su = p.path_unit in
              let set = su.su_set in
              let set_name = set.set_name in
              let su_name = su.su_unit.unit_name in
              let path_name = p.path_name in
              let n2 =
                match dst with
                | S -> [set_name]
                | M -> [su_name]
                | SM -> [ set_name; su_name ]
                | V -> [ path_name ]
                | SV  -> [ set_name; path_name ]
                | MV -> [ su_name; path_name ]
                | SMV ->
                  [ set_name; su_name; path_name ]
              in
              n2, set
            in
            (* Discard internal edges *)
            if set1 != set2 then begin
              add_edge edges n1 n2;
              add_edge rev_edges n2 n1;
            end;
            ()
          ) su.su_import_values
        ) set.set_units;

      ) sets;

      let f n arrow src dst edges =
        let basename = Printf.sprintf "set-%s-%s-%s" n
          (string_of_kind src)
          (string_of_kind dst) in

        let oc = open_out (basename ^ ".txt") in
        let rec iter indent term break n =
          begin match n.values with
          | None -> ()
          | Some n ->
            iter arrow "" "" n
          end;
          StringMap.iter (fun name n ->
            Printf.fprintf oc "%s%s%s\n" indent name term;
            iter (indent ^ "   ") term "" n;
            output_string oc break;
          ) n.edges;
        in
        iter "" ":" "\n\n" edges;

        close_out oc
      in
      f "imports" "             -> " src dst edges;
      f "exports" "             <- " dst src rev_edges;

    ) [ S;M;V; SM; SV; MV; SMV ]
  ) [S;M; SM];
  ()

let print only =

  print_graph_of_set_opens only;
  print_graph_of_set_imports only;
  print_graphes_per_set only;

  let list = ref [] in
  StringMap.iter (fun _ set ->
    list := (StringMap.cardinal set.set_subsets, set) :: !list;
  ) only;
  let list = List.sort compare !list in
  let list = List.map snd list in


  let oc = open_out "archi1.txt" in

  Printf.fprintf oc "Set transitive dependencies:\n";
  List.iter (fun set ->
    Printf.fprintf oc "set %S:" set.set_name;
    StringMap.iter (fun _ set ->
      Printf.fprintf oc " %s" set.set_name;
    ) set.set_subsets;
    Printf.fprintf oc "\n";
    if set.set_unresolved <> StringSet.empty then begin
      Printf.fprintf oc "   %d unresolved:"
        (StringSet.cardinal set.set_unresolved);
      StringSet.iter (fun s ->
        Printf.fprintf oc " %s" s;
      ) set.set_unresolved;
      Printf.fprintf oc "\n";
    end;

  ) list;

  close_out oc;


  with_dot "archi1.dot" (fun oc ->

    List.iter (fun set ->
      StringMap.iter (fun _ subset ->
        Printf.fprintf oc "   %S -> %S\n" set.set_name subset.set_name;
      ) set.set_subsets;
    ) list;

  )
