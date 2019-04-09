(*
This file is part of teaching material of Yale-NUS College module
"YSC2229: Introductory Data Structures and Algorithms"

Copyright (c) 2019 Ilya Sergey

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

open Week_12_Graphs
open LinkedGraphs

(*****************************************)
(*   0. Reading a graph with payloads    *)
(*****************************************)

let read_graph_and_payloads size nvalue elist elabels = 
  let open AdjacencyGraphs in 
  let g = mk_graph size in
  for i = 0 to g.size - 1 do
    set_payload g i nvalue.(i) 
  done;  
  List.iter (fun (s, d) -> add_edge g s d) elist;
  List.iter (fun (s, d, l) -> set_edge_label g s d l) elabels;
  LinkedGraphs.from_simple_adjacency_graph g
  

(*****************************************)
(*        1. Reachability queries        *)
(*****************************************)

let reachable g init final = 
  let rec walk path visited n = 
    if n = final 
    then Some path
    else if List.mem n visited 
    then None
    else
      (* Try successors *)
      let node = get_node g n in
      let successors = get_next node in
      let visited' = n :: visited in
      let rec iter = function
        | [] -> None
        | h :: t -> 
          let path' = (n, h) :: path in
          match walk path' visited' h with
          | Some p -> Some p
          | None -> iter t
      in
      iter successors
  in
  match walk [] [] init with
  | Some p -> Some (List.rev p)
  | _ -> None


let is_reachable g init final = 
  reachable g init final <> None

(* Graphical representation *)

(*

Taking attributes from_simple_adjacency_graph
https://graphs.grevian.org/example
*)

let bold_edge = "[color=red,penwidth=3.0]"

let graphviz_with_path g init final out = 
  let r = reachable g init final in 
  let attrib (s, d) = match r with
    | None -> ""
    | Some p -> 
      if List.mem (s, d) p 
      then bold_edge
      else ""
  in
  let open Week_10_ReadingFiles in
  let ag = LinkedGraphs.to_adjacency_graph g in
  let s = graphviz_string_of_graph "digraph" " -> " 
      string_of_int attrib ag in
  write_string_to_file out s


(*****************************************)
(*        2. Depth-first search          *)
(*****************************************)

module GraphDFS = struct
  
  open NodeTable 
  open Week_01

  type color = White | Gray | Black

  let rec dfs g = 
    let color_map = mk_new_table (v_size g) in
    let tree_map = mk_new_table (v_size g) in
    let time_map = mk_new_table (v_size g) in
    let has_cycles = ref false in
    let roots = ref [] in
    let all_nodes = get_nodes g in

    (* Make all nodes white *)
    List.iter (fun n -> insert color_map n White) all_nodes;
    (* Insert all nodes to the tree *)
    List.iter (fun n -> insert tree_map n []) all_nodes;

    let time = ref 0 in 


    let rec dfs_visit u = 
      time := !time + 1;
      let u_in = !time in
      insert color_map u Gray;
      get_succ g u |> List.iter (fun v -> 
          let v_color = get_exn @@ get color_map v in
          if v_color = White
          then begin
            let siblings = get_exn @@ get tree_map u in
            insert tree_map u (v :: siblings);
            dfs_visit v
          end 
          else if v_color = Gray 
          then has_cycles := true) ;
      insert color_map u Black;
      time := !time + 1;
      let u_out = !time in
      insert time_map u (u_in, u_out)
    in

    List.iter (fun n -> 
        if get_exn @@ get color_map n = White
        then begin
          (* Record roots *)
          roots := n :: !roots;
          dfs_visit n
        end) 
      all_nodes;

      (!roots, tree_map, time_map, !has_cycles)  

  (* Visualise with DFS *)
  let graphviz_with_dfs g out = 
  let (_, tree, _, _) = dfs g in 
  let eattrib (s, d) = match get tree s with
    | None -> ""
    | Some p -> 
      if List.mem d p 
      then bold_edge
      else ""
  in
  let open Week_10_ReadingFiles in
  let ag = LinkedGraphs.to_adjacency_graph g in
  let s = graphviz_string_of_graph "digraph" " -> " 
      string_of_int eattrib ag in
  write_string_to_file out s
  

  (* DFS-induced search *)
  let is_reachable_via_dfs g init final = 
    let (roots, tree, _, _) = dfs g in
    let rec walk n = 
      if n = final then true
      else 
        get tree n |> 
        Week_01.get_exn |>
        List.exists (fun v -> walk v)
    in
    if List.mem init roots 
    then walk init
    else false

  (* Question: is reachability equivalent to DFS-reachability *)

end

(*****************************************)
(*         4. Topological sort           *)
(*****************************************)

let graphviz_with_payload g values out = 
  let eattrib e = "" in
  let vattrib n = values.(n) in
  let open Week_10_ReadingFiles in
  let ag = LinkedGraphs.to_adjacency_graph g in
  let s = graphviz_string_of_graph "digraph" " -> " 
      vattrib eattrib ag in
  write_string_to_file out s
    
module TopologicalSort = struct

  open Week_01
  open NodeTable 

  let get_last_time m n = get_exn @@ get m n

  let topo_sort g = 
    let (_, _, time_map, _) = GraphDFS.dfs g in
    get_nodes g |>
    List.sort (fun n1 n2 ->
        let (_, t1) = get_last_time time_map n1 in
        let (_, t2) = get_last_time time_map n2 in
        if t1 < t2 then 1
        else if t1 > t2 then -1
        else 0)

end

let clothes = 
  [|  
    "underpants";
    "phone";
    "shoes";
    "shirt";
    "tie";
    "jacket";
    "socks";
    "belt";
    "trousers";
  |]
  
let clothes_edges = [
  (0, 8);
  (0, 2);
  (8, 2);
  (8, 1);
  (8, 7);
  (3, 7);
  (3, 4);
  (4, 5);
  (7, 5);
  (6, 2);
]

let clothes_graph = 
  read_graph_and_payloads 9 clothes clothes_edges 
    ([] : (int * int * unit) list)

(*****************************************)
(*             5. Example                *)
(*****************************************)


(*
utop # let l = TopologicalSort.topo_sort clothes_graph;;

utop # List.iter (fun i -> Printf.printf "%s\n" clothes.(i)) l;;

socks
shirt
tie
underpants
trousers
belt
jacket
phone
shoes

*)




