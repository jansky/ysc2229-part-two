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
open Week_06
open DLLBasedQueue
open Week_10_BinaryEncodings

(*************************************)
(*   Encode interleaving lengths     *)
(*************************************)

(*

Example

  0000000000000001111111000000011111111111

Can be represented as 

  1111 0111 0111 1011

*)

(* Compression from binary input *)
let read_next_bit input = try
    let bit = read_bits input 1 in
    Some bit
  with BatInnerIO.No_more_input -> None


let compute_lengths input =
  let m = 256 in
  let q = mk_queue 100 in
  let rec read_segments acc b = 
    raise (Failure "Implement me!")
  in
  read_segments 0 0;
  (* DLQPrinter.print_queue q string_of_int; *)
  queue_to_list q


let compress_binary_via_rle binary new_binary = 
  raise (Failure "Implement me!")
      
(*************************************)
(*        Decode: Homework           *)
(*************************************)

(*
archive    -- a binary with RLE-encoding
new_binary -- an output file for decompression
*)
let decompress_via_rle_into_binary archive new_binary = 
  raise (Failure "Implement me!")

(*************************************)
(*            Tests                  *)
(*************************************)

let dna_rle_compression_test d = 
  raise (Failure "Implement me!")


    
