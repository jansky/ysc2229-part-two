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
(*     Testing RLE Compression          *)
(****************************************)

open Week_10_BinaryEncodings
open Week_11_RunLengthEncoding

(* let%test _ = dna_rle_compression_test dna_string1
 * let%test _ = dna_rle_compression_test dna_string2
 * let%test _ = dna_rle_compression_test dna_string3
 * let%test _ = dna_rle_compression_test dna_string4 *)

(****************************************)
(*     Testing Huffman Codes            *)
(****************************************)

open Week_11_HuffmanCodes

let%test _ =
  test_tree_serialization tree1

let%test _ =
  let t = compute_frequency_tree cfreqs1 in
  test_tree_serialization t

let%test _ = 
  huffman_test abracadabra

(******************************************)
(*          Testing Union Find            *)
(******************************************)

open Week_11_UnionFind

(* let%test "UF" =
 *   let open UnionFind in
 *   let uf = mk_UF 10 in
 *   union uf 0 1;
 *   union uf 2 3;
 *   union uf 4 5;
 *   union uf 6 7;
 *   union uf 8 9;
 *   assert (get_count uf = 5);
 *   assert (connected uf 0 1);
 * 
 *   assert (not (connected uf 6 8));
 *   assert (not (connected uf 0 9));
 * 
 *   union uf 1 3;
 *   union uf 5 7;
 *   union uf 5 9;
 *   assert (get_count uf = 2);
 *   assert (connected uf 6 8);
 * 
 *   union uf 3 4;
 *   assert (get_count uf = 1);
 *   assert (connected uf 0 9);
 * 
 *   true *)
  


(****************************************)
(*            Testing Trees             *)
(****************************************)

open Week_01
open Week_03
open Week_11_BinaryTree

open BinarySearchTree

let mk_tree_of_size n =
  let t = mk_tree () in
  let a = generate_key_value_array n in
  for i = 0 to n - 1 do 
    insert t a.(i)
  done;
  t

(******************************************)
(*          Testing insertion             *)
(******************************************)

(* let%test "Testing insertion" = 
 *   let n = 1000 in
 *   let t = mk_tree_of_size n in
 *   check_bst_inv t *)

(******************************************)
(*          Testing traversals            *)
(******************************************)

(* let check_elem_in_tree t e = 
 *   let n = search t e in
 *   (get_exn @@ n).value = e *)

(* let%test "Testing DFS" = 
 *   let n = 1000 in
 *   let t = mk_tree_of_size n in
 *   let l1 = depth_first_search_rec t in
 *   let l2 = depth_first_search_loop t in
 *   List.length l1 = n && l1 = l2 &&
 *   List.for_all (fun e -> check_elem_in_tree t e) l1 *)

(* let%test "Testing BFS" = 
 *   let n = 1000 in
 *   let t = mk_tree_of_size n in
 *   let l1 = depth_first_search_rec t in
 *   let l2 = breadth_first_search_loop t in
 *   List.length l1 = n && 
 *   List.for_all (fun e -> List.mem e l2) l1 &&
 *   List.for_all (fun e -> List.mem e l1) l2 *)

(******************************************)
(*          Testing retrieval             *)
(******************************************)

let%test "Testing retrieval" = 
  let n = 1000 in
  let t = mk_tree_of_size n in
  (* Implement me! *)
  true

(******************************************)
(*          Testing deletion              *)
(******************************************)

let test_delete n = 
  let t = mk_tree_of_size n in
  (* Checkign the tree invariant *)
  (* Checkign the tree size *)
  (* Checking integrity *)
  ()


let%test "Testing deletion" = 
  for i = 1 to 10 do
    test_delete 1000
  done;
  true

(******************************************)
(*          Testing rotation              *)
(******************************************)

let%test "Testing left-rotate" = 
  (* Implement me! *)
  true
