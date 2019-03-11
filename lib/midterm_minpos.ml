
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



(* Solution 1 *)



(* Solution 2 *)
