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

(************************************)
(*         Working with bits        *)
(************************************)

open Core
open Extlib.IO

let read_from_binary deserialize filename =  
  In_channel.with_file ~binary:true filename 
    ~f:(fun file_input ->
        let bits_input = input_bits @@ input_channel file_input in
        deserialize bits_input)


let write_to_binary serialize filename data = 
  Out_channel.with_file filename ~append:false ~binary:true ~f:(fun file_out ->
      let bits_output = output_bits @@ output_channel file_out ~cleanup:true in
      serialize bits_output data;
      (* Padding from the end -- important! *)
      flush_bits bits_output)

(************************************)
(*      Writing encoded strings     *)
(************************************)

let write_string_to_binary filename text = 
  let serialize out text = 
    raise (Failure "Implement me!")
  in
  write_to_binary serialize filename text
    
let read_string_from_binary filename =  
  let deserialize input = 
    raise (Failure "Implement me!")
  in
  read_from_binary deserialize filename

(**

See files in binary:

xxd -b filename

**)

let string_serialization_test s = 
  raise (Failure "Implement me!")

(* Check that the files are the same *)

let abracadabra = "ABRACADABRA!"

open Week_10_ReadingFiles

let string_file_serialization_test source_file = 
  let s = read_file_to_single_string source_file in
  string_serialization_test s

(************************************)
(*    Enconding DNA sequences       *)
(************************************)

let dna_encoder = function
  | 'A' -> 0
  | 'C' -> 1
  | 'G' -> 2
  | 'T' -> 3
  | _ -> raise (Failure "DNA encoding error")


let dna_decoder = function
  | 0 -> 'A'
  | 1 -> 'C'
  | 2 -> 'G'
  | 3 -> 'T'
  | _ -> raise (Failure "DNA decoding error")

(* 2 bits *)
let dna_encoding_size = 2

let dna_string1 = "CGT"
let dna_string2 = "ATAGATGCATAGCGCATAGCTAGATAGTGCTAG"
let dna_string3 = "ATAGATGCATAGCGCATAGCTAGATAGTGCTAGCGATGCATAGCGCAGATGCATAGCGCAGGGGG"
let dna_string4 = "ATAGATGCATAGCGCATAGCTAGATAGTGCTAGCGATGCATAGCGCAGATGCATAGCGCAGGGGGATAGATGCATAGCGCATAGCTAGATAGTGCTAGCGATGCATAGCGCAGATGCATAGCGCAGGGGGATAGATGCATAGCGCATAGCTAGATAGTGCTAGCGATGCATAGCGCAGATGCATAGCGCAGGGGGATAGATGCATAGCGCATAGCTAGATAGTGCTAGCGATGCATAGCGCAGATGCATAGCGCAGGGGGATAGATGCATAGCGCATAGCTAGATAGTGCTAGCGATGCATAGCGCAGATGCATAGCGCAGGGGGATAGATGCATAGCGCATAGCTAGATAGTGCTAGCGATGCATAGCGCAGATGCATAGCGCAGGGGG"


let write_dna_to_binary filename text = 
  let serialize out text = 
    raise (Failure "Implement me!")
  in
  write_to_binary serialize filename text
    
let read_dna_from_binary filename =  
  let deserialize input = 
    raise (Failure "Implement me!")
  in
  read_from_binary deserialize filename


let dna_compression_test d = 
    raise (Failure "Implement me!")
      
(* Check the equality of the strings *)

(* Experiment with the sizes of the fiels *)
