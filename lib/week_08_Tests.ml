(***************************)
(*   Testing Hash tables   *)
(***************************)

open Week_03
open Week_08_HashTable

let%test "ListBasedHashTable insert" = 
  let open SimpleHTTester in
  let a = generate_key_value_array 1000 in
  let ht = mk_test_table_from_array_length a 50 in
  test_table_get ht a

let%test "ListBasedHashTable insert" = 
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
  false

let%test "bloom filter true negatives" = 
  let open IntStringFilter in
  let fsize = 2000 in
  let len = 1000 in
  let (f, a) = fill_bloom_filter fsize len in 
  let al = array_to_list 0 len a in

  
  false
