(*********************)
(*     Testing       *)
(*********************)

(*

search : string -> string -> int option
test: string
pattern : string

*)

let test_pattern_in search text pattern = 
  let i = Week_01.get_exn @@ search text pattern in
  let p' = String.sub text i (String.length pattern) in
  assert (p' = pattern)

let test_pattern_not_in search text pattern =
  assert (search text pattern = None)

let generate_string_and_patterns n m = 
  let ps = Week_03.generate_words n m in 
  let text = String.concat "" ps in 
  let ps_not_in = 
    Week_03.generate_words n m |>
    List.filter (fun s -> not (List.mem s ps))    
  in
  (text, ps, ps_not_in)





(*********************)
(*   Naive search    *)
(*********************)

open String

let naive_search text pattern = 
  (* TODO: implement me *) None

(* Rewriting recursively *)

let naive_search_rec text pattern = 
  let n = length text in
  let m = length pattern in
  if n < m then None
  else
    let rec walk k =
      if k > n - m then None
      else (
        let j = ref 0 in
        while !j <= m - 1 &&
              text.[k + !j] = pattern.[!j] do
          j := !j + 1
        done;

        if !j = m
        then Some k
        else walk @@ k + 1
      )
    in
    walk 0










  

