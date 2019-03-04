
(* Simple HashTable*)

module type HashTable = sig
  type key
  type 'v hash_table
  val mk_new_table : int -> (key * 'v) hash_table 
  val insert : (key * 'v) hash_table -> key -> 'v -> unit
  val get : (key * 'v) hash_table -> key -> 'v option
  val remove : (key * 'v) hash_table -> key -> unit
  val print_hash_table : 
    (key -> string) ->
    ('v -> string) ->
    (key * 'v) hash_table -> unit
end

(* Exposing keys *)

module type KeyType = sig
  type t
end

(* Redefining our hash-table *)

(* Redefining our hash-table *)
module SimpleListBasedHashTable(K: KeyType) = struct
  type key = K.t

  type 'v hash_table = {
    buckets : 'v list array;
    capacity : int; 
  }

  let mk_new_table cap = 
    let buckets = Array.make cap [] in
    {buckets = buckets;
     capacity = cap}
  
  let insert ht k v = 
    let hs = Hashtbl.hash k in
    let bnum = hs mod ht.capacity in 
    let bucket = ht.buckets.(bnum) in
    let clean_bucket = 
      List.filter (fun (k', _) -> k' <> k) bucket in
    ht.buckets.(bnum) <- (k, v) :: clean_bucket

  let get ht k = 
    let hs = Hashtbl.hash k in
    let bnum = hs mod ht.capacity in 
    let bucket = ht.buckets.(bnum) in
    let res = List.find_opt (fun (k', _) -> k' = k) bucket in
    match res with 
    | Some (_, v) -> Some v
    | _ -> None

  (* Slow remove - introduce for completeness *)
  let remove ht k = 
    let hs = Hashtbl.hash k in
    let bnum = hs mod ht.capacity in 
    let bucket = ht.buckets.(bnum) in
    let clean_bucket = 
      List.filter (fun (k', _) -> k' <> k) bucket in
    ht.buckets.(bnum) <- clean_bucket

  let print_hash_table ppk ppv ht = 
    let open Printf in
    print_endline @@ sprintf "Capacity: %d" (ht.capacity);
    print_endline "Buckets:";
    let buckets = (ht.buckets) in
    for i = 0 to (ht.capacity) - 1 do
      let bucket = buckets.(i) in
      if bucket <> [] then (
        (* Print bucket *)
        let s = List.fold_left 
            (fun acc (k, v) -> acc ^ (sprintf "(%s, %s); ") (ppk k) (ppv v)) "" bucket in
        printf "%d -> [ %s]\n" i s)
    done
end 

(* Testing hash-tables *)

module HashTableTester(H : HashTable) = struct

  module MyHT = H
  open MyHT
      
  (* Create a table with a capacity m from an array a 
     and return it *)
  let mk_test_table_from_array_length a m =
    mk_new_table m

  (* Test that all elements of a is in ht *)
  let test_table_get ht a = false

end




(* Testing our simple implementation *)
module IntString = struct type t = int * string end
module SHT = SimpleListBasedHashTable(IntString)
module SimpleHTTester = HashTableTester(SHT)

let pp_kv (k, v) = Printf.sprintf "(%d, %s)" k v
