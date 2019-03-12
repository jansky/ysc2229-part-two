
open Week_02

let test_smallest_missing_non_negative_integer candidate =
  (candidate (Array.copy [|0; 1|]) = 2) &&
  (candidate (Array.copy [|1; 0|]) = 2) &&
  (candidate (Array.copy [|0; 1; 3; ~-4|]) = 2) &&
  (candidate (Array.copy [|1; ~-2; 3; ~-1; 0|]) = 2) &&
  (candidate (Array.copy [|3; 1; 0|]) = 2) &&
  (candidate (Array.copy [|0; 10; 3; 33; 2; 5; 100|]) = 1) &&
  (candidate (Array.copy [|0; 10; 3; 33; 2; 5; 1|]) = 4) &&
  (candidate (Array.copy [|0; 10; 11; 2; 3; 7; 6; 6; 3; 33; 2; 5; 1|]) = 4) &&
  (candidate (Array.copy [|0; 10; 11; 2; 3; ~-33; 7; 6; 4; 4; 4; 6; 3; 33; 2; 5; 1|]) = 8) &&
  (candidate (Array.copy [|1; 2; 3; 4; 5; 6; ~-33; ~-33; ~-33; 7; 8; 9; ~-33; ~-34; 12; 12; 0|]) = 10) &&
  (candidate (Array.copy [|1; 2; 3; 4; 5; 6; 7; 8; 9|]) = 0) &&
  (candidate (Array.copy [|10; 9; 8; 7; 0; 6; 5; 4; 3; 2; 1|]) = 11)


(* A strawman for demonstration: rearrange a permutation *)

let rearrange_perm arr = 
  let n = Array.length arr in 

  let at_its_place i = 
    i = arr.(i) &&
    arr.(i) >= 0 &&
    arr.(i) < n
  in
  
  let rec put_in_place i = 
    if at_its_place i 
    then ()
    else (
      swap arr i arr.(i);
      put_in_place i)

  in

  for i = 0 to n - 1 do
    put_in_place i
  done

let min_pos_not_in arr = 
  let n = Array.length arr in 
  
  let outsider i = 
    arr.(i) < 0 || arr.(i) >= n 
  in

  let duplicate i =
    (not (outsider i)) &&
    arr.(i) <> i &&
    arr.(i) = arr.(arr.(i))

  in

  let at_its_place i = 
    arr.(i) >= 0 &&
    arr.(i) < n &&
    i = arr.(i)
  in

  let rec put_in_place_or_mark i = 
    if at_its_place i 
    then ()
    else if outsider i || duplicate i
    then arr.(i) <- -1
    else (
      swap arr i arr.(i);
      put_in_place_or_mark i)
  in
  
  (* First pass *)
  for i = 0 to n - 1 do
    put_in_place_or_mark i
  done;

  let rec second_pass i = 
    if i = n then n
    else if arr.(i) < 0 
    then i
    else second_pass @@ i + 1


  in 
  second_pass 0

(* let%test _ =
 *   test_smallest_missing_non_negative_integer min_pos_not_in *)







(* Solution 1 *)



(* Solution 2 *)
