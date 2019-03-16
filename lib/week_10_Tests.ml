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
(*     Testing Dynamic Programming      *)
(****************************************)

(* open Week_10_DynamicProgramming *)

(* let%test _ = test_fib memo_fib 25 *)

(* let%test _ = 
 *   knapsack_max_price 4 fruit_sack = 6 *)

(* let%test _ = 
 *   let (p, _) = knapsack_max_price_dynamic 4 fruit_sack in
 *   p = 6 *)

(* open Week_10_BinaryEncodings *)

(****************************************)
(*     Testing String Serialisation     *)
(****************************************)

(* let%test _ = 
 *   string_serialization_test abracadabra *)

(****************************************)
(*     Testing DNA Compression          *)
(****************************************)

(* let%test _ = dna_compression_test dna_string1
 * let%test _ = dna_compression_test dna_string2
 * let%test _ = dna_compression_test dna_string3
 * let%test _ = dna_compression_test dna_string4 *)

(****************************************)
(*     Testing RLE Compression          *)
(****************************************)

(* open Week_10_RunLengthEncoding *)

(* let%test _ = dna_rle_compression_test dna_string1
 * let%test _ = dna_rle_compression_test dna_string2
 * let%test _ = dna_rle_compression_test dna_string3
 * let%test _ = dna_rle_compression_test dna_string4 *)

(****************************************)
(*     Testing Tree Serialisation       *)
(****************************************)

(* open Week_10_HuffmanCodes *)

(* let%test _ =
 *   test_tree_serialization tree1 *)

(* let%test _ =
 *   let t = compute_frequency_tree cfreqs1 in
 *   test_tree_serialization t *)

(* let%test _ = 
 *   huffman_test abracadabra *)
