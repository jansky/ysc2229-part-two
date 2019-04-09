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

open Core_kernel
open Week_12_Graphs
open Week_13_Reachability

(***********************************)
(*  Testing Reachability           *)  
(***********************************)

let%test _ =  
  let g = LinkedGraphs.parse_linked_int_graph small_graph_shape in
  (* True statements *)
  assert (is_reachable g 0 5);
  assert (is_reachable g 5 1);
  assert (is_reachable g 5 5);

  (* False statements *)
  assert (not (is_reachable g 4 5));
  true

let%test _ =  
  let g = LinkedGraphs.parse_linked_int_graph medium_graph_shape in
  (* True statements *)
  assert (is_reachable g 2 4);
  assert (is_reachable g 8 12);
  assert (is_reachable g 0 10);

  (* False statements *)
  assert (not (is_reachable g 5 9));
  assert (not (is_reachable g 11 7));
  true

(***********************************)
(*  Testing Reachability via DFS   *)  
(***********************************)

let test_dfs g = 
  let all_nodes = LinkedGraphs.get_nodes g in 
  let (dfs_roots, _, _, _) = GraphDFS.dfs g in

  (* Any node DFS-reachable from a root r is reachable from r *)
  (* ?? Is it true or false? *)
  let fact1 = 
    List.for_all dfs_roots ~f:(fun u ->
        List.for_all all_nodes ~f:(fun v ->
            if GraphDFS.is_reachable_via_dfs g u v
            then is_reachable g u v
            else true)) 
  in

  (* Any node is reachable from some root r *)
  (* ?? Is it true or false? *)
  let fact2 = 
    List.for_all all_nodes ~f:(fun u ->
        List.exists dfs_roots
          ~f:(fun r -> GraphDFS.is_reachable_via_dfs g r u)) in

  fact1 && fact2

let%test _ =  
  let g = LinkedGraphs.parse_linked_int_graph medium_graph_shape in
  test_dfs g
  
let%test _ =  
  let g = LinkedGraphs.parse_linked_int_graph small_graph_shape in
  test_dfs g

(***********************************)
(*        Testing for cycles       *)
(***********************************)

let%test _ =  
  let g = LinkedGraphs.parse_linked_int_graph small_graph_shape in
  let (_, _, _, c) = GraphDFS.dfs g in
  c
      
let%test _ =  
  let g = LinkedGraphs.parse_linked_int_graph medium_graph_shape in
  let (_, _, _, c) = GraphDFS.dfs g in
  not c

(***************************************)
(*    Testing for topological sort     *)
(***************************************)

let rec all_pairs ls = match ls with
  | [] -> []
  | _ :: [] -> []
  | h1 :: h2 :: t -> (h1, h2) :: (all_pairs (h2 :: t))    
                                 
let%test _ =  
  let g = LinkedGraphs.parse_linked_int_graph medium_graph_shape in
  let pairs = TopologicalSort.topo_sort g |> all_pairs in
  List.for_all pairs ~f:(fun (s, d) -> not (is_reachable g d s))

let%test _ =  
  let g = clothes_graph in
  let pairs = TopologicalSort.topo_sort g |> all_pairs in
  List.for_all pairs ~f:(fun (s, d) -> not (is_reachable g d s))
      

(***************************************)
(*    Testing for SS shortest paths    *)
(***************************************)

open Week_01
open Week_13_Paths
open LinkedGraphs
open NodeTable
open Set

(*

What is provided for testing:

* p - predecessor tree
* d - distance table
* g - the graph
* s - source node
* u - destination node

*)

(* 1. Path is connected *)
let test_path_connected p d g s u = 
  match get_shortest_path p s u with
  | None -> true
  | Some path ->
    let rec walk p = match p with
      | (u, v) :: (x, y) :: t ->
        v = x && walk ((x, y) :: t )
      | _ -> true
    in
    walk path

(* 2. Path's weight is correctly recorded *)
let test_path_weight p d g s u =
  match get_shortest_path p s u with
  | None -> true
  | Some path ->
    let w1 = get_path_weigth g path in 
    let w2 = get_exn @@ get d u |> Distance.int_of_dist in
    w1 = w2

(* 3. Has all edges *)    
let test_that_is_path_graph p d g s u =
  match get_shortest_path p s u with
  | None -> true
  | Some path ->
    let all_edges = g.edges |> elements in
    List.for_all path ~f:(
      fun e -> List.mem all_edges e ~equal:(=)
    )

(* 4. Exists for any reachable node *)
let test_reachable_hence_has_path p d g s u = 
  if is_reachable g s u 
  then get_shortest_path p s u <> None
  else true

(* 5. And is the shortest *)
let test_shortest_is_shorter p d g s u = 
  match reachable g s u with
  | None -> true
  | Some p1 ->
    match get_shortest_path p s u with
    | None -> false
    | Some p2 ->
      let w1 = get_path_weigth g p1 in 
      let w2 = get_path_weigth g p2 in 
      w2 <= w1

(*  Main testing function  *)

let test_sssp algo g = 
  let all_nodes = get_nodes g in
  List.iter all_nodes ~f:(fun u ->
      List.iter all_nodes ~f:(fun v ->
          let (p, d) = algo g u in
          assert (test_path_connected p d g u v);
          assert (test_path_weight p d g u v);
          assert (test_that_is_path_graph p d g u v);
          assert (test_reachable_hence_has_path p d g u v);
          assert (test_shortest_is_shorter p d g u v);
        ));
  true


(*  Testing Bellman-Ford  *)

let%test "Bellman-Ford-1" = 
  let algo g s = bellman_ford g s |> fst in
  test_sssp algo example_graph_bf
  
let%test "Bellman-Ford-2" = 
  let algo g s = bellman_ford g s |> fst in
  test_sssp algo example_graph_dijkstra

(*  Testing Dijkstra  *)

let%test "Dijkstra" = 
  test_sssp dijkstra example_graph_dijkstra


(***************************************)
(*              Testing MSTs           *)
(***************************************)

open Week_13_Spanning

let%test "Testing MST size" = 
  let t = mst_kruskal example_graph_undirected in 
  List.length t = v_size example_graph_undirected - 1


(*

Design tests for the following properties:

* Connectivity of the MST: any node is reachable from another (as in undirected graph)
* Minimalty wrt. other random spanning tree of an undirected graph

*)
