(*
  This benchmark test the capacity of the compiler to inline Array.fold_left
  and promote reference assignment to local mutable variables, or continuarion
  variables.

  This is not possible yet as of 4.03
*)

open Micro_bench_types

let prepare n =
  let state = Random.State.make [|n|] in
  Array.init n (fun _ -> Random.State.float state 1e10)

let check n r =
  let data = prepare n in
  let expected = Kahan_sum.sum_baseline data in
  if expected = r then
    Ok
  else
    Error (Printf.sprintf
             "got %f, expected %f \
              (errors might be hidden by the printer rounding)"
             r expected)

let interval_range =
  [ Range (0, 100_000), Short;
    Range (100_000, 1_000_000), Long;
    Range (10_000_000, 100_000_000), Longer ]

let functions = [
  "kahan_sum",
  Int_group
    (["baseline", Kahan_sum.sum_baseline;
      "array_fold", Kahan_sum.sum_with_array_fold],
     prepare,
     check,
     interval_range)
]

let () = add functions
