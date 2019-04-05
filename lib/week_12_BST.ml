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
    root : 'e tree_node option ref;
    size : int ref
  }

  let left n = !(n.left)
  let right n = !(n.right)
  let parent n = !(n.parent)
  let get_root t = !(t.root)
  let get_size t = !(t.size)

  let mk_node e = 
    {value = e;
     parent = ref None;
     left = ref None;
     right = ref None}
    
  let mk_tree _ = {root = ref None; size = ref 0}    
    
  let map_option o f z = match o with
    | None -> z
    | Some n -> f n
      

  (**********************************)
  (*     2. Growing the tree        *)
  (**********************************)
    
  let insert t e =       
    let rec insert_element n e = 
      let m = mk_node e in
      if e < n.value
      then match left n with
        | Some m -> insert_element m e
        | None ->
          m.parent := Some n;
          n.left := Some m;
          true
      else if e > n.value
      then match right n with
        | Some m -> insert_element m e
        | None ->
          m.parent := Some n;
          n.right := Some m;
          true
      else false
    in
    match !(t.root) with
    | None -> (
        t.root := Some (mk_node e);
        t.size := 1;
        true)
    | Some n -> 
      if insert_element n e
      then (t.size := !(t.size) + 1; true)
      else false
                     
              
  (**********************************)
  (*     2.5 Tree invariant         *)
  (**********************************)

  let check_bst_inv t = 
    let rec walk node p = 
      (p node.value) &&
      let res_left = match left node with
        | None -> true
        | Some l -> walk l (fun w -> p w && w <= node.value)
      in
      let res_right = match right node with
        | None -> true
        | Some r -> walk r (fun w -> p w && w >= node.value)
      in
      res_left && res_right
    in
    match !(t.root) with
    | None -> true
    | Some n -> walk n (fun _ -> true)

  (**********************************)
  (*     3. Printing a tree         *)
  (**********************************)

  let print_tree pp snum t = 
    let print_node_with_spaces l s = 
      for i = 0 to s - 1 do 
        Printf.printf " "
      done;
      print_endline (pp l.value);
    in

    let rec walk s node = match node with
      | None -> ()
      | Some n -> begin
          walk (s + snum) (right n);
          print_node_with_spaces n s;
          walk (s + snum) (left n);
        end      
    in
    map_option (get_root t) (fun n -> walk 0 (Some n)) ()
    
  (**********************************)
  (*     4. Exploring the tree      *)
  (**********************************)

  let search t k = 
    let rec walk k n = 
      let nk = n.value in 
      if k = nk then Some n
      else if k < nk
      then match left n with
        | None -> None
        | Some l -> walk k l
      else match right n with
        | None -> None
        | Some r -> walk k r
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
    let loop stack q =
      while not (is_empty stack) do
        let n = get_exn @@ pop stack in
        enqueue q n.value;
        (match right n with
         | Some r -> push stack r
         | _ -> ());
        (match left n with
         | Some l -> push stack l
         | _ -> ());
      done
    in
    let acc = (mk_queue 0) in
    let stack = mk_stack 0 in
    (match get_root t with
    | None -> ()
    | Some n -> begin
        push stack n;
        loop stack acc;
      end);      
    queue_to_list acc

  (**********************************)
  (* 6. Traversing a tree with BFS  *)
  (**********************************)

  let breadth_first_search_loop t = 
    let loop wlist q depth =
      while not (is_empty wlist) do
        let n = get_exn @@ dequeue wlist in
        enqueue q n.value;
        (match left n with
         | Some l -> enqueue wlist l
         | _ -> ());
        (match right n with
         | Some r -> enqueue wlist r
         | _ -> ());
      done
    in
    let acc = (mk_queue 0) in
    let wlist = mk_queue 0 in
    (match get_root t with
    | None -> ()
    | Some n -> begin
        enqueue wlist n;
        loop wlist acc 0;
      end);      
    queue_to_list acc

  let elements t = breadth_first_search_loop t

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

  (* Deleting the a node z from tree *)
  (* z must be in the tree *)
  let delete_node t z = 
    t.size := !(t.size) - 1;
    if left z = None
    then transplant t z (right z)
    else if right z = None
    then transplant t z (left z)
    else
      (* Finding the successor of `z` *)
      let z_right_child = (get_exn @@ right z) in
      let y = find_min_node z_right_child in
      (* Fact: `y` has no left child *)

      (if parent y <> None &&
          z != get_exn @@ parent y
       then 
      (*  If y is not immediately under z,
          replace y by its right subtree *)
         let x = right y in
         (transplant t y x;
          y.right := right z;
          (get_exn @@ right y).parent := Some y));

      (* Now `y` replaces `z` at its position *)
      transplant t z (Some y);
      y.left := !(z.left);
      (get_exn @@ left y).parent := Some y
  

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
      y.parent := parent x;

      (match parent x with 
       | None -> t.root := Some y
       | Some p -> match left p with
         | Some l when x == l ->
           p.left := Some y
         | _ ->
           p.right := Some y);
            
      (* Make x the left child of y *)
      y.left := Some x;
      x.parent := Some y      
      
end

(* Experiments with printing the tree *)
open BinarySearchTree

let print_kv_tree = print_tree 
    (fun (k, v) -> Printf.sprintf "(%d, %s)" k v) 12

(* 

Exercise:
Implement a procedure for finding a predecessor for and element e in
the BST.

Exercise:

Using the idea of BFS, print the tree of 1-digit integers "vertically"
   (i.e., as we normally draw them on a white board). For instance,
   you should be able to obtain the following output for a tree that
   misses one leaf (left child of the node storing 5):

      4
    2   5 
   1 3   6

Here are some ideas on what you can try:

* Use BFS to associate the "level" with each node.

* You may keep a structure with counters for each level to keep track
   of the "missing" left/right children, so they could be renderred as
   white spaces.

* You might want to compute the expected number of leaves at the
   bottom level (which depends on the height of the tree) to calculate
   the initial offset and the spacing between nodes at each of the
   higher levels.

As a bonus (for extra points), try to generalise your printing
   algorithm for arbitrary strings produced from the values stored in
   the nodes.

Exercise: Implement right-rotate and test that it preservees the tree
   contents and the invariant. Next, test on randomly chosen pairs of
   related nodes in a tree, that left-rotate and right-rotate are the
   mutual inverses (i.e., applying LR after RR and vise versa brings
   the tree to the initial state).


*)
