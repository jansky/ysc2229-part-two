(*
This file is part of teaching material of Yale-NUS College module
"YSC2229: Introductory Data Structures and Algorithms"

Copyright (c) 2019 Ilya Sergey

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

(*************************************)
(*    Computing Fibonacci numbers    *)
(*************************************)

let rec naive_fib n = 
  if n <= 1 then 1
  else naive_fib (n - 1) + naive_fib (n - 2)

(* With memoization *)

let memo_fib n = 
  if n <= 1 then 1
  else begin
    let fib_prev = ref 1 in
    let fib = ref 1 in
    for i = 2 to n do 
      let tmp = !fib_prev in
      fib_prev := !fib;
      fib := tmp + !fib
    done;
    !fib
  end
    

(* Testing Fibonacci numbers *)

let test_fib fib_fun n = 
  for i = 0 to n do
    assert (fib_fun n = naive_fib n)
  done;
  true

(* Compare time *)

(*************************************)
(*        Knapsack problem           *)
(*************************************)

(* name * weight * price *)
let fruit_sack = [|
  ("apple",  1, 1);
  ("melon",  2, 2);
  ("kiwi",   1, 2);
  ("durian", 2, 3);
|]

(* Utility functions *)
let weight items i = 
  let (_, w, _) = items.(i) in w

let price items i = 
  let (_, _, p) = items.(i) in p

let allowed_weight = 4

(*************************************)
(*       Determining the weight      *)
(*************************************)

let knapsack_max_price max_weight items = 
  let num_items = Array.length items in 
  (* n - currently observed item
     w - remaining weight        *)

  (* Implement me! *)
  0


(****************************************)
(*   Solving via dynamic programming    *)
(****************************************)

let knapsack_max_price_dynamic max_weight items = 
  let num_items = Array.length items in 

  (* Make array of maximal prices 
     m.(i).(w) = max price when taking up to i items 
                 with max weight w *)

  let m = Array.make (num_items + 1) [||] in
  for i = 0 to num_items do
    m.(i) <- Array.make (max_weight + 1) 0
  done;

  (* Main operation *)
  (* Implement me! *)

  (m.(num_items).(max_weight), m)


open Week_10_Backtracking

(*

Results

n  item    w  p |  
--------------------------------
0  apple   1  1 |  0  1  1  1  1  
1  melon   2  2 |  0  1  2  3  3  
2  kiwi    1  2 |  0  2  3  4  5  
3  durian  2  3 |  0  2  3  5  6 

*)

(****************************************)
(*         Obtaining all items          *)
(****************************************)

let knapsack_obtain_items max_weight items =
  let num_items = Array.length items in 
  (* Implement me! *)
  []

