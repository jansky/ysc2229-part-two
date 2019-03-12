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

(***************************)
(* Testing string search   *)
(***************************)

open Week_09_StringSearch

let big = "abcdefghijklmnopeqrstuvabcsrtdsdqewgdcvaegbdweffwdajbjrag"

let patterns = ["dsd"; "jrag"; "abc"]

(*****************************)
(*   Testing Naive search    *)
(*****************************)
    
let%test "Naive Search Works" = 
  List.iter (fun p -> test_pattern_in naive_search big p) patterns;
  true

let%test "Naive Search True Positives" = 
  let (s, ps, _) = generate_string_and_patterns 500 5 in
  List.iter (fun p -> test_pattern_in naive_search s p) ps;
  true

let%test "Naive Search True Negatives" = 
  let (s, _, pn) = generate_string_and_patterns 500 5 in
  List.iter (fun p -> test_pattern_not_in naive_search s p) pn;
  true


(*   Universal tester   *)

let search_tester search = 
  let (s, ps, pn) = generate_string_and_patterns 500 5 in
  List.iter (fun p -> test_pattern_in search big p) patterns;
  List.iter (fun p -> test_pattern_in search s p) ps;
  List.iter (fun p -> test_pattern_not_in search s p) pn;
  true


let%test _ = search_tester naive_search_rec


(*****************************)  
(* Testing Rabin-Karp search *)
(*****************************)

open Week_09_RabinKarp

let%test "Rabin-Karp Search Works" = 
  search_tester rabin_karp_search

let%test _ = 
  search_tester rabin_karp_search_rec


open Week_09_Comparison

let%test "Rabin-Karp Search True Positives - repetitive" = 
  let (s, ps, _) = repetitive_string 2000 in
  List.iter (fun p -> test_pattern_in rabin_karp_search s p) ps;
  true

let%test "Rabin-Karp Search True Negatives  - repetitive" = 
  let (s, _, pn) = repetitive_string 2000 in
  List.iter (fun p -> test_pattern_not_in rabin_karp_search s p) pn;
  true


(*****************************)  
(*       Testing KMP         *)
(*****************************)

open Week_09_KMP

(* Single-loop *)

let%test _ = search_tester naive_search_one_loop

(* Recursive *)
let%test _ = search_tester search_rec

(* With invariants *)
let%test _ = search_tester search_inv

(* With expansion *)
let%test _ = search_tester search_with_shift

(* With assert *)
let%test _ = search_tester search_assert

(* With self-matching *)
let%test _ = search_tester search_via_pattern

(* With table *)
let%test _ = search_tester search_with_inefficient_init

(* KMP *)
let%test _ = search_tester search_kmp
