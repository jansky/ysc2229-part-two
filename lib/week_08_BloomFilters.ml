(* Bloom filters *)

module type BloomHashing = sig 
  type t
  val hash_functions : (t -> int) list
end

module type BloomFilter = functor
  (H : BloomHashing) -> sig
  type t 
  val mk_bloom_filter : int -> t
  val insert : t -> H.t -> unit
  val contains : t -> H.t -> bool
end

module BloomFilterImpl : BloomFilter = functor
  (H: BloomHashing) -> struct
  type t = {
    slots : bool array;
    size : int
  }

  let mk_bloom_filter m = 
    let a = Array.make m false in
    {slots = a; size = m}
  

  let insert f e = 
    let m = f.size in 
    List.iter (fun hash -> 
        let h = (hash e) mod m in
        f.slots.(h) <- true) 
      H.hash_functions

  let contains f e =
    if H.hash_functions = [] then false
    else
      let m = f.size in 
      let res = ref true in
      List.iter (fun hash -> 
        let h = (hash e) mod m in
        res := !res && f.slots.(h)
      ) H.hash_functions;
      !res

end

module IntStringHashing = struct
  type t = int * string
  let hash1 (k, _) = Hashtbl.hash k 
  let hash2 (_, v) = Hashtbl.hash v
  let hash3 (k, _) = k
  let hash_functions = [hash1; hash2; hash3]
end

module IntStringFilter = BloomFilterImpl(IntStringHashing)

let fill_bloom_filter m n = 
  let open IntStringFilter in
  let filter = mk_bloom_filter m in
  let a = Week_03.generate_key_value_array n in
  for i = 0 to n - 1 do
    insert filter a.(i)
  done;
  (filter, a)

(* Using Bloom filter to speed up the simple hash table *)

open Week_08_HashTable

module BloomHashTable (K: BloomHashing) = struct 
  type key = K.t

  (* Adding bloom filter *)
  module BF = BloomFilterImpl(K)

  type 'v hash_table = {
    buckets : 'v list array;
    capacity : int; 
    filter   : BF.t
  }

  let mk_new_table cap = 
    let buckets = Array.make cap [] in
    (* Pick reasonably large BF size *)
    let filter = BF.mk_bloom_filter 15000 in
    {buckets = buckets;
     capacity = cap;
     filter = filter}
  
  let insert ht k v = 
    let hs = Hashtbl.hash k in
    let bnum = hs mod ht.capacity in 
    let bucket = ht.buckets.(bnum) in
    let filter = ht.filter in
    let clean_bucket = 
      (* New stuff *)
      if BF.contains filter k
      (* Only filter if ostensibly contains key *)
      then List.filter (fun (k', _) -> k' <> k) bucket 
      else bucket in
    (* Missed in the initial the implementation *)
    BF.insert filter k;
    ht.buckets.(bnum) <- (k, v) :: clean_bucket

  let get ht k = 
    let filter = ht.filter in
    if BF.contains filter k then
      let hs = Hashtbl.hash k in
      let bnum = hs mod ht.capacity in 
      let bucket = ht.buckets.(bnum) in
      let res = List.find_opt (fun (k', _) -> k' = k) bucket in
      match res with 
      | Some (_, v) -> Some v
      | _ -> None
    else None

  (* Cannot remove *)
  let remove _ _ = raise (Failure "Removal is deprecated!")

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

(* Testing Bloomfilter hash table *)


module BHT = BloomHashTable(IntStringHashing)
module BHTTester = HashTableTester(BHT)

let insert_and_get_bulk_bloom a m = 
  Printf.printf "Creating Bloom hash table:\n";
  let ht = Week_03.time (BHTTester.mk_test_table_from_array_length a) m in
  Printf.printf "Fetching from Bloom hash table on the array of size %d:\n" (Array.length a);
  let _ = Week_03.time BHTTester.test_table_get ht a in ()

let compare_hashing_time_simple_bloom n m = 
  let a = Week_03.generate_key_value_array n in
  insert_and_get_bulk_simple a m;
  print_endline "";
  insert_and_get_bulk_bloom a m





