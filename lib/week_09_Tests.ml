(***************************)
(* Testing string search   *)
(***************************)

open Week_09_StringSearch

let big = "abcdefghijklmnopeqrstuvabcsrtdsdqewgdcvaegbdweffwdajbjrag"

let patterns = ["dsd"; "jrag"; "abc"]

(*****************************)
(*   Testing Naive search    *)
(*****************************)
    
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
