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

open Core
open Extlib.IO

(***************************************************)
(*          Serialising/deserialising the tree     *)
(***************************************************)

type 'a tree = 
  | Node of 'a tree * 'a tree
  | Leaf of 'a

let tree1 = 
  let le = Leaf 'e' in
  let ld = Leaf 'd' in
  let la = Leaf 'a' in
  let lb = Leaf 'b' in
  let lc = Leaf 'c' in
  let lf = Leaf 'f' in
  Node (la,
        Node (Node (lc, lb), 
              Node (Node (lf, le), 

                    ld)))

let rec write_tree out t = 
  (* Implement me *)
  ()
    
let rec read_tree input = 
  match read_bits input 1 with
  (* Implement me *)  
  | _ -> raise (Failure "Cannot unparse tree!")
    
open Week_10_BinaryEncodings

(* Test functions *)

(* let write_tree_to_binary = write_to_binary write_tree
 * let read_tree_from_binary = read_from_binary read_tree *)

(* let test_tree_serialization t = 
 *   let f = "tree.tmp" in
 *   write_tree_to_binary f t;
 *   let t' = read_tree_from_binary f in
 *   Sys.remove f;
 *   t = t' *)

(*******************************************)
(*    Computing frequency trees            *)
(*******************************************)

let cfreqs1 = [|('a', 45); ('b', 13); ('c', 12); 
                ('d', 16); ('e', 9); ('f', 5)|]

let make_tree_array freq_chars = 
  let n = Array.length freq_chars in
  let ftrees = Array.create ~len:n (Leaf 'a', 1) in
  (* Implement me *)
  ftrees
  
(* Taking an array freq_chars as an input *)
let compute_frequency_tree freq_chars = 
  (* Implement me *)
  raise (Failure "Implement me!")

(*******************************************)
(*    Computing frequencies for a string   *)
(*******************************************)

let compute_freqs s = 
  let n = String.length s in
  let m = 256 in
  let freqs = Array.create ~len:m 0 in

  (* Collect frequencies *)

  let cfreqs = Array.create ~len:m ('a', 0) in

  (* Pair with characters *)

  cfreqs

(***********************************************)
(*  Create enconding table (lists of  bits)    *)
(***********************************************)

let build_table t = 
  let m = 256 in
  let table = Array.create ~len:m [] in 
  
  let rec make_codes t acc = 
    (* Traverse the tree with accumulators *)
    ()

  in
  make_codes t [];
  table

(***********************************************)
(*       Serialize the compressed file         *)
(***********************************************)

open Week_10_ReadingFiles

(* Writing the encoding tree an characters *)
let write_tree_and_data out (t, s) = 
  (* Write the tree *)
  (* Build the table *)
  (* Write string length *)
  (* Write the string via table *)
  ()

let compress_string target s = 
  (* Implement me! *)
  ()
 
let compress_file source target = 
  let s = read_file_to_single_string source in
  compress_string target s

(***********************************************)
(*         Read the compressed file            *)
(***********************************************)

let rec read_char_via_tree t input =
  (* Depending on whether the leaf or a node, return a character 
     or proceed recursively *)
  raise (Failure "Implement me!")

let read_encoded input = 
  (* Read tree *)
  (* Read length *)
  (* Read characters *)
  ""

let decompress_file filename = 
  read_from_binary read_encoded filename

(***********************************************)
(*         Read the compressed file            *)
(***********************************************)

let huffman_test s = 
  let filename = "archive.huf" in
  compress_string filename s;
  let s' = decompress_file filename in
  Sys.remove filename;
  s = s'
