let rec fib n =
  if n < 2 then 1 else fib(n-1) + fib(n-2)

let results =
  [|1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377; 610; 987; 1597;
    2584; 4181; 6765; 10946; 17711; 28657; 46368; 75025; 121393; 196418;
    317811; 514229; 832040; 1346269; 2178309; 3524578; 5702887; 9227465;
    14930352; 24157817; 39088169; 63245986; 102334155; 165580141; 267914296;
    433494437; 701408733|]

open Micro_bench_types

let prepare i = i
let run i = fib i
let check i n =
  if n = results.(i)
  then Ok
  else Error ("fib " ^ (string_of_int i) ^ " returned " ^ (string_of_int n)
              ^ " instead of " ^ (string_of_int results.(i)))

let functions =
  [ "fib", Int (run, prepare, check,
                [ Range (0, 28), Short;
                  Range (29, 40), Long;
                  Range (40, Array.length results - 1), Longer ])
  ]

let () = add functions
