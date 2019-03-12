open Week_02
open Week_05


(***************************************************)
(*              Copying permutation                *)
(***************************************************)



(***************************************************)
(*              In-place permutations              *)
(***************************************************)


let perm_print arr =
  let n = Array.length arr in
  

  let rec perm_suffix_from k =
    if k = n - 1
    then 
      (for j = 0 to n - 1 do Printf.printf "%d; " arr.(j) done; print_endline "";);
    
    for i = k to n - 1 do
      swap arr k i;
      perm_suffix_from (k + 1);
      swap arr i k;
    done
  in

  perm_suffix_from 0
