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
