(*********************)
(* Rabin-Karp search *)
(*********************)

open String

(* Rolling hash *)
let rk_hash text = 
  let h = ref 0 in
  for j = 0 to length text - 1 do
    h := !h + Char.code text.[j]
  done;
  !h

let rabin_karp_search text pattern = 
  let n = length text in
  let m = length pattern in 
  if n < m then None
  else
    let hpattern = rk_hash pattern in
    let rhash = ref @@ rk_hash (sub text 0 m) in
    let k = ref 0 in
    let res = ref None in
    while !k <= n - m && !res = None do
      (if !rhash = hpattern &&
          sub text !k m = pattern 
      then res := Some !k);
      (if !k <= n - m - 1
       then 
         let c1 = Char.code text.[!k] in
         let c2 = Char.code text.[!k + m] in
         rhash := !rhash - c1 + c2);
      k := !k + 1      
    done;
    !res








    




