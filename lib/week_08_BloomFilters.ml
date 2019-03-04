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

module BloomfilterImpl : BloomFilter = functor
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

module IntStringFilter = BloomfilterImpl(IntStringHashing)

let fill_bloom_filter m n = 
  let open IntStringFilter in
  let filter = mk_bloom_filter m in
  let a = Week_03.generate_key_value_array n in
  for i = 0 to n - 1 do
    insert filter a.(i)
  done;
  (filter, a)






