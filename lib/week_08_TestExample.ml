
(* Simple tests *)

open Week_03

let a = generate_key_value_array 10

let rec fact n = if n = 1 then 1 else n * fact (n - 1)

let%test _ = fact 4 = 24

(* let%test _ = fact 4 = 25 *)

let%expect_test "todo" =
  print_endline "Hello, world!";
  [%expect{|
    Hello, world!
  |}]

