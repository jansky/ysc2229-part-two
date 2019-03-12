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

(****************************************************)
(*               Flag for output printing           *)
(****************************************************)

(****************************************************)
(* Reshaping the naive algorithm with a single loop *)
(****************************************************)

open String

let naive_search_one_loop text pattern = 
  let n = length text in
  let m = length pattern in
  if n < m then None
  else
    let k = ref 0 in
    let j = ref 0 in
    let stop = ref false in
    let res = ref None in
    while !k <= n && not !stop do
      if !j = m
      then (res := Some (!k - !j); stop := true)
      else if !k = n then stop := true
      else if text.[!k] = pattern.[!j] then
      (
        k := !k + 1;
        j := !j + 1
      ) else (
        k := !k - !j + 1;
        j := 0
      )        
    done;
    !res


(****************************************************)
(*                Making it recursive               *)
(****************************************************)

type search_result = 
  | Found of int
  | Interrupted of int

let global_search search text pattern = 
  let n = length text in
  let m = length pattern in
  let res = search pattern m text n 0 0 in
  match res with 
  | Found x -> Some x
  | _ -> None

let search_rec = 
  let rec search pattern m text n j k =
    if j = m then Found (k - j)
    else if k = n then Interrupted j
    else if text.[k] = pattern.[j] then
      search pattern m text n (j + 1) (k + 1)
    else search pattern m text n 0 (k - j + 1)
  in
  global_search search

(*****************************************************)
(*             Instrument with invariant             *)
(*****************************************************)


let search_inv = 
  let rec search pattern m text n j k =
    assert (0 <= j && j <= m);
    assert (j <= k && k <= n);
    assert (sub pattern 0 j = sub text (k - j) j);
    if j = m then Found (k - j)
    else if k = n then Interrupted j
    else if text.[k] = pattern.[j] then
      search pattern m text n (j + 1) (k + 1)
    else search pattern m text n 0 (k - j + 1)
  in
  global_search search

(**

Search within a long text can be split up as a sequential composition of two searches.

Here's an equivalence for k <= l <= n

search pattern m text n j k

is equivalent to 

 let result = search pattern m text l j k in
  match result with
  | Found _ ->
      result
  | Interrupted j' ->
      search pattern m text n j' l

As the interruption happened when we hit the imposed right end of the text (i.e., l).

**)

(*****************************************************)
(*    Fast-Forwarding Search using Interrupt Index   *)
(*****************************************************)


open Printf

let search_with_shift = 
  let rec search pattern m text n j k =
    assert (0 <= j && j <= m);
    assert (j <= k && k <= n);
    assert (sub pattern 0 j = sub text (k - j) j);
    if j = m then Found (k - j)
    else if k = n then Interrupted j
    else if text.[k] = pattern.[j] then
      search pattern m text n (j + 1) (k + 1)
    else if j = 0 then
      search pattern m text n 0 (k - j + 1)
    else 
      let res = search pattern m text k 0 (k - j + 1) in
      match res with
      | Found _ -> res
      | Interrupted j' -> search pattern m text n j' k
  in
  global_search search

(*****************************************************)
(*        Extracting the Interrupt Index             *)
(*****************************************************)

let assertInterrupted = function
  | Found _       -> assert false
  | Interrupted j -> j


let search_assert = 
  let rec search pattern m text n j k =
    if j = m then Found (k - j)
    else if k = n then Interrupted j
    else if text.[k] = pattern.[j] then
      search pattern m text n (j + 1) (k + 1)
    else if j = 0 then
      search pattern m text n 0 (k - j + 1)
    else 
      let j' = assertInterrupted @@ search pattern m text k 0 (k - j + 1) in
      assert (j' < j);
      search pattern m text n j' k
  in
  global_search search


(*****************************************************)
(*        Exploiting the Prefix Equality             *)
(*****************************************************)


let search_via_pattern =
  let rec search pattern m text n j k =
    if j = m then Found (k - j)
    else if k = n then Interrupted j
    else if text.[k] = pattern.[j] then
      search pattern m text n (j + 1) (k + 1)
    else if j = 0 then
      search pattern m text n 0 (k - j + 1)
    else 
      let j' = assertInterrupted @@ search pattern m pattern j 0 1 in
      search pattern m text n j' k

  in 
  global_search search

(*****************************************************)
(*        Tabulating the interrupt indices           *)
(*****************************************************)


let rec loop table pattern m text n j k =
    if j = m then Found (k - j)
    else if k = n then Interrupted j
    else if text.[k] = pattern.[j] then
      loop table pattern m text n (j + 1) (k + 1)
    else if j = 0 then
      loop table pattern m text n 0 (k - j + 1)
    else 
      let j' = table.(j) in
      loop table pattern m text n j' k

(****************************************************)
(*              Initialising the table              *)
(****************************************************)

let search_with_inefficient_init =

  let loop_search pattern _ text n j k = 

    let rec search pattern m text n j k =
      if j = m then
        Found (k - j)
      else if k = n then
        Interrupted j
      else if pattern.[j] = text.[k] then
        search pattern m text n (j + 1) (k + 1)
      else if j = 0 then
        search pattern m text n 0 (k + 1)
      else
        (* So we're looking in our own prefix *)
        let j' = assertInterrupted @@ search pattern m pattern j 0 1 in
        assert (j' < j);
        search pattern m text n j' k
    in
    
    let m = length pattern in
    let table = Array.make m 0 in

    for j = 1 to m - 1 do
      table.(j) <- assertInterrupted @@ search pattern m pattern j 0 1
    done;


    let rec loop table pattern m text n j k =
      if j = m then
        Found (k - j)
      else if k = n then
        Interrupted j
      else if pattern.[j] = text.[k] then
        loop table pattern m text n (j + 1) (k + 1)
      else if j = 0 then
        loop table pattern m text n 0 (k + 1)
      else
        loop table pattern m text n table.(j) k
    in

    loop table pattern m text n j k
  in

  global_search loop_search

(****************************************************)  
(*          Finally, making it efficient            *)  
(****************************************************)

let search_kmp =

  let loop_search pattern _ text n j k = 
    
    let rec loop table pattern m text n j k =
      if j = m then
        Found (k - j)
      else if k = n then
        Interrupted j
      else if pattern.[j] = text.[k] then
        loop table pattern m text n (j + 1) (k + 1)
      else if j = 0 then
        loop table pattern m text n 0 (k + 1)
      else
        loop table pattern m text n table.(j) k
    in
    let m = length pattern in
    let table = Array.make m 0 in

    (*  In the case of j = 1, j' is 0 *)
    for j = 2 to m - 1 do
      table.(j) <- assertInterrupted @@
        loop table pattern m pattern j table.(j - 1) (j - 1)
    done;
    
    loop table pattern m text n j k
  in

  global_search loop_search
  
  

  


  
