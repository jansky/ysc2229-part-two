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
