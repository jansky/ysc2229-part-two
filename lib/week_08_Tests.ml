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


(******************)
(* Testing Queues *)
(******************)

open Week_01
open Week_03
open Week_06
open Week_08_ArrayQueue

module ArrayQPrinter = QueuePrinter(ArrayQueue)

let pp_q (k, v) = Printf.sprintf "(%d, %s)" k v

let print_queue q = ArrayQPrinter.print_queue q pp_q

(* Testing the array-based queue *)
open ArrayQueue

(* Make a test_queue *)
let mk_test_q n = 
  let q = mk_queue n in
  let a = generate_key_value_array n in
  for i = 0 to n - 1 do enqueue q a.(i) done;
  (q, a)

let%test "dequeue-first" =
  let (q, a) = mk_test_q 10 in
  let first = get_exn @@ dequeue q in
  first = a.(0)


(***************************)
(*   Testing Hash tables   *)
(***************************)

open Week_08_HashTable

let%test "ListBasedHashTable insert" = 
  let open SimpleHTTester in
  let a = generate_key_value_array 1000 in
  let ht = mk_test_table_from_array_length a 50 in
  test_table_get ht a

let%test "ResizableHashTable insert" = 
  let open ResizableHTTester in
  let a = generate_key_value_array 1000 in
  let ht = mk_test_table_from_array_length a 50 in
  test_table_get ht a

(***************************)
(* Testing Bloom filters   *)
(***************************)

open Week_08_BloomFilters

let%test "bloom filter true positives" = 
  let open IntStringFilter in
  let fsize = 2000 in
  let len = 1000 in
  let (f, a) = fill_bloom_filter fsize len in 
  for i = 0 to len - 1 do
    assert (contains f a.(i))
  done;
  true

let%test "bloom filter true negatives" = 
  let open IntStringFilter in
  let fsize = 2000 in
  let len = 1000 in
  let (f, a) = fill_bloom_filter fsize len in 
  let al = array_to_list 0 len a in

  
  let b = generate_key_value_array len in
  for i = 0 to len - 1 do
    let e = b.(i) in
    if (not (contains f e))
    then assert (not (List.mem e al))
  done;
  true

(***************************)
(*   Testing Bloom tables  *)
(***************************)

let%test "BloomHashTable insert" = 
  let open BHTTester in
  let a = generate_key_value_array 1000 in
  let ht = mk_test_table_from_array_length a 50 in
  test_table_get ht a
