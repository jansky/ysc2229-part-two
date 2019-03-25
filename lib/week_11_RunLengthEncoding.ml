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
open Extlib.IO
open Week_06
open DLLBasedQueue
open Week_10_BinaryEncodings

(*************************************)
(*   Encode interleaving lengths     *)
(*************************************)

(*

Example

  000000000000000001111111000000011111111111

Can be represented as 

  1111 0000 0010 0111 0111 1011

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
    if acc >= m - 1 
    then begin
      enqueue q acc;
      read_segments 0 (1 - b)
    end
    else 
      match read_next_bit input with
      | None -> enqueue q acc
      | Some x ->
        if x = b
        then let acc' = acc + 1 in
          read_segments acc' b
        else begin
          enqueue q acc;
          read_segments 1 (1 - b)
        end
  in
  read_segments 0 0;
  queue_to_list q

let compress_binary_via_rle binary new_binary = 
  let segments = read_from_binary compute_lengths binary in
  let rec loop out segs = match segs with
    | [] -> ()
    | h :: t -> (
        write_bits out ~nbits:8 h;
        loop out t)
  in
  write_to_binary loop new_binary segments
      
(*************************************)
(*            Decode                 *)
(*************************************)

(*
archive    -- a binary with RLE-encoding
new_binary -- an output file for decompression
*)
let decompress_via_rle_into_binary archive new_binary = 
  (* Implement me! *)
  ()

(*************************************)
(*            Tests                  *)
(*************************************)

let dna_rle_compression_test d = 
  let dna = "dna.tmp" in
  let rle = "dna.rle" in
  let dna' = "dna.new" in
  write_dna_to_binary dna d;
  compress_binary_via_rle dna rle;
  decompress_via_rle_into_binary rle dna';
  let d' = read_dna_from_binary dna' in
  Sys.remove dna;
  Sys.remove rle;
  Sys.remove dna';
  d = d'

