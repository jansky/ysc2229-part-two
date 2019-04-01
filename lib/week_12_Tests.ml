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

(****************************************)
(*            Testing Trees             *)
(****************************************)

open Week_01
open Week_03
open Week_12_BST
 

open BinarySearchTree

let mk_tree_of_size n =
  let t = mk_tree () in
  let a = generate_key_value_array n in
  for i = 0 to n - 1 do 
    assert (insert t a.(i))
  done;
  assert (get_size t = n);
  t

(******************************************)
(*          Testing insertion             *)
(******************************************)

let%test "Testing insertion" = 
  let n = 1000 in
  let t = mk_tree_of_size n in
  check_bst_inv t

(******************************************)
(*          Testing traversals            *)
(******************************************)

let check_elem_in_tree t e = 
  let n = search t e in
  (get_exn @@ n).value = e

let%test "Testing DFS" = 
  let n = 1000 in
  let t = mk_tree_of_size n in
  let l1 = depth_first_search_rec t in
  let l2 = depth_first_search_loop t in
  List.length l1 = n && l1 = l2 &&
  List.for_all (fun e -> check_elem_in_tree t e) l1

let%test "Testing BFS" = 
  let n = 1000 in
  let t = mk_tree_of_size n in
  let l1 = depth_first_search_rec t in
  let l2 = breadth_first_search_loop t in
  List.length l1 = n && 
  List.for_all (fun e -> List.mem e l2) l1 &&
  List.for_all (fun e -> List.mem e l1) l2

(******************************************)
(*          Testing retrieval             *)
(******************************************)

let%test "Testing retrieval" = 
  let n = 1000 in
  let t = mk_tree_of_size n in
  let m = Random.int n in
  let l = breadth_first_search_loop t in
  let e = List.nth l m in
  let z = search t e in
  z <> None

(******************************************)
(*          Testing deletion              *)
(******************************************)

let test_delete n = 
  let t = mk_tree_of_size n in
  let m = Random.int n in
  let l = breadth_first_search_loop t in
  let e = List.nth l m in
  let z = get_exn @@ search t e in
  delete_node t z;
  (* Checkign the tree invariant *)
  assert (check_bst_inv t);

  (* Checkign the tree size *)
  let ld = breadth_first_search_loop t in
  assert (List.length ld = n - 1);

  (* Checking integrity *)
  assert (List.for_all (fun x -> List.mem x ld || x == e) l)


let%test "Testing deletion" = 
  for i = 1 to 10 do
    test_delete 1000
  done;
  true

(******************************************)
(*          Testing rotation              *)
(******************************************)

let%test "Testing left-rotate" = 
  let n = 1000 in
  let t = mk_tree_of_size n in
  let l1 = breadth_first_search_loop t in

  let e = ref (List.nth l1 (Random.int n)) in
  let z = ref (get_exn @@ search t !e) in

  while (right !z = None) do
    let m = (Random.int n) in
    e := List.nth l1 m;
    z := get_exn @@ search t !e
  done;

  left_rotate t !z;

  (* Checkign the tree invariant *)
  assert (check_bst_inv t);
  let l2 = breadth_first_search_loop t in

  (* Checkign the tree size *)
  List.length l2 = n && 
  
  (* Checking integrity *)
  List.for_all (fun e -> List.mem e l2) l1 &&
  List.for_all (fun e -> List.mem e l1) l2


(****************************************)
(*            Testing Graphs             *)
(****************************************)

open Core_kernel
open Week_12_Graphs

(* Morphing graphs *)

let%test _ =
  let ag = AdjacencyGraphs.adjacency_int_graph_of_strings small_graph_shape in
  let g = LinkedGraphs.from_simple_adjacency_graph ag in
  let ag' = LinkedGraphs.to_adjacency_graph g in
  same_shape ag ag'

let%test _ =
  let ag = AdjacencyGraphs.adjacency_int_graph_of_strings medium_graph_shape in
  let g = LinkedGraphs.from_simple_adjacency_graph ag in
  let ag' = LinkedGraphs.to_adjacency_graph g in
  same_shape ag ag'
