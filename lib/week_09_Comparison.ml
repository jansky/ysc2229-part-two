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


(*************************)
(* Comparing performance *)
(*************************)

open Week_09_StringSearch
open Week_09_RabinKarp
open Week_09_KMP

let evaluate_search search name s ps pn = 
  print_endline "";
  Printf.printf "[%s] Pattern in: " name;
  Week_03.time (List.iter (fun p -> test_pattern_in search s p)) ps;
  Printf.printf "[%s] Pattern not in: " name;
  Week_03.time (List.iter (fun p -> test_pattern_not_in search s p)) pn

(*

Run on 

compare_string_search 20000 50;;

*)

let compare_string_search n m =
  let (s, ps, pn) = generate_string_and_patterns n m in
  evaluate_search naive_search "Naive" s ps pn;
  evaluate_search rabin_karp_search "Rabin-Karp" s ps pn;
  evaluate_search search_kmp "Knuth-Morris-Pratt"  s ps pn


(* Comparing on repetitive strings *)

let repetitive_string n = 
  let ast = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa" in
  let pat1 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab" in
  let pat2 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaac" in
  let mk n = 
    let t = List.init n (fun x -> if x = n - 1 then pat1 else ast) in
    String.concat "" t 
  in
  (mk n, [pat1], [pat2])


(*
Try on compare_string_search_repetitive 50000;;
*)

let compare_string_search_repetitive n =
  let (s, ps, pn) = repetitive_string n in
  evaluate_search naive_search  "Naive"  s ps pn;
  evaluate_search rabin_karp_search "Rabin-Karp"  s ps pn;
  evaluate_search search_kmp "Knuth-Morris-Pratt"  s ps pn
