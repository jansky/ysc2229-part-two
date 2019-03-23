
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


open Week_01
open Week_06

module BinarySearchTree = struct

(*

Supported operations:

* Get minimum
* Get maximum
* Find an element
* Find a successor
* Insert an element
* Delete an element

*)

  (**********************************)
  (*    1.  Defining a tree         *)
  (**********************************)

  type 'e tree_node = {
    value : 'e;
    parent  : 'e tree_node option ref;
    left  : 'e tree_node option ref;
    right  : 'e tree_node option ref;
  }

  type 'e tree = {
    root : 'e tree_node option ref
  }

  let left n = !(n.left)
  let right n = !(n.right)
  let parent n = !(n.parent)
  let get_root t = !(t.root)

  let mk_node e = 
    {value = e;
     parent = ref None;
     left = ref None;
     right = ref None}
    
  let mk_tree _ = {root = ref None}    
    
  let map_option o f z = match o with
    | None -> z
    | Some n -> f n
      

  (**********************************)
  (*     2. Growing the tree        *)
  (**********************************)
    
  let insert t e =       
    let rec insert_element n e = 
      (* TODO: Insert element recursively *)
      ()
    in

    match !(t.root) with
    | None -> t.root := Some (mk_node e)
    | Some n -> insert_element n e
              
  (**********************************)
  (*     2.5 Tree invariant         *)
  (**********************************)

  let check_bst_inv t = 
    let rec walk node p = 
      (* TODO: Accumulate predicate recursively *)
      false
    in
    match !(t.root) with
    | None -> true
    | Some n -> walk n (fun _ -> true)

  (**********************************)
  (*     3. Printing a tree         *)
  (**********************************)

  let print_tree pp snum t = 
    (* 1. Print spaces *)

    let print_node_with_spaces l s = 
      ()
    in

    let rec walk s node = 
      (* 2. Nodes and their children *)
      ()

    in
    map_option (get_root t) (fun n -> walk 0 (Some n)) ()
    
  (**********************************)
  (*     4. Exploring the tree      *)
  (**********************************)

  let search t k = 
    let rec walk k n = 
      (* recursive traversal *)
      None
    in
    map_option (get_root t) (walk k) None

  (**********************************)
  (* 5. Traversing a tree with DFS  *)
  (**********************************)

  open DLLBasedQueue

  let depth_first_search_rec t = 
    let rec walk q n =
      enqueue q n.value;
      (match left n with
       | Some l -> walk q l
       | None -> ());
      (match right n with
       | Some r -> walk q r
       | None -> ());
    in
    let acc = (mk_queue 0) in
    map_option (get_root t) (walk acc) ();
    queue_to_list acc

  let depth_first_search_loop t = 
    let open ListBasedStack in
    let loop wlist q =
      (* Implement me! *)
      ()

    in
    let acc = (mk_queue 0) in
    let wlist = mk_stack 0 in
    (* Start from the root! *)
    ()

    
  (**********************************)
  (* 6. Traversing a tree with BFS  *)
  (**********************************)

  let breadth_first_search_loop t = 
    (* TODO: implement me! *)
    []

  (**********************************)
  (* 7.  Finding a minimum node     *)
  (**********************************)

  let rec find_min_node n = 
    match left n with
    | Some m -> find_min_node m
    | None -> n

  (* Question: how to find a successor of a node in a tree? *)

  (**********************************)
  (* 8.    Deletion of an element   *)
  (**********************************)

  (* Replacing node U by (optional) node V in T. *)
  let transplant t u v = 
    (match parent u with
    | None -> t.root := v
    | Some p -> 
      match left p with
      | Some l when u == l -> p.left := v
      | _ -> p.right := v);
    (* Update parent of v *)
    match v with 
    | Some n -> n.parent := parent u
    | _ -> ()

  (* Deleting the a node Z from tree *)

  let delete_node t z = 
    (* Consider 4 cases *)
    ()
  
  (**********************************)
  (* 9. Rotations and balanced tree *)
  (**********************************)

  let left_rotate t x = 
    match right x with
    | None -> ()
    | Some y ->

      (* turn y's left subtree into x's right subtree *)
      x.right := left y;
      (if left y <> None
       then (get_exn @@ left y).parent := Some x);

      (* link x's parent to y *)
      (if parent x = None 
       then t.root := Some y
      else if Some x = left (get_exn @@ parent x) 
      then (get_exn @@ parent x).left := Some y
      else (get_exn @@ parent x).right := Some y);

      (* Make x the left child of y *)
      y.left := Some x;
      x.parent := Some y
      
end

(* Experiments with printing the tree *)
open BinarySearchTree

(* let print_kv_tree = print_tree 
 *     (fun (k, v) -> Printf.sprintf "(%d, %s)" k v) 12 *)
