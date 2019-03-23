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

module UnionFind = struct
  
  type t = {
    count : int ref;
    id : int array
  }

  let mk_UF n = 
    let ints = 
      Week_04.list_to_array (Week_03.iota (n - 1)) in
    { count = ref n;
      id = ints }

  let get_count uf = !(uf.count)

  (* Question: What is the complexity of find? *)
  let find uf p = 
    (* Implement me *)
    0

  (* Question: What is the complexity of union? *)
  let union uf p q = 
    (* Implement me *)
    ()

  let connected uf p q =
    find uf p = find uf q

  let print_uf uf = 
    let n = Array.length uf.id in
    let ids = Week_03.iota (n - 1) in
    for i = 0 to n - 1 do
      let connected = List.find_all (fun e -> find uf e = i) ids in
      if connected <> [] then begin
        Printf.printf "Class %d: [" i;
        List.iter (fun j -> Printf.printf "%d; " j) connected;
        print_endline "]"
      end      
    done                      
                      
end
