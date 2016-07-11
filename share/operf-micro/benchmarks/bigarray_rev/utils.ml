open Bigarray

let create_int n =
  let a = Array1.create int c_layout n in
  for i = 0 to Array1.dim a - 1 do
    Array1.set a i i
  done;
  a

let create_int32 n =
  let a = Array1.create int32 c_layout n in
  for i = 0 to Array1.dim a - 1 do
    Array1.set a i (Int32.of_int i)
  done;
  a

let rev a =
  if Array1.dim a > 0
  then
    for i = 0 to (Array1.dim a - 1) / 2 do
      let t = Array1.get a i in
      Array1.set a i (Array1.get a (Array1.dim a - (1 + i)));
      Array1.set a (Array1.dim a - (1 + i)) t
    done;
  a

let check_int _ a =
  try
    for i = 0 to Array1.dim a - 1 do
      if Array1.get a i <> (Array1.dim a - (1 + i))
      then raise Exit
    done;
    Micro_bench_types.Ok
  with Exit -> Micro_bench_types.Error "incorrect rev"

let check_int32 _ a =
  try
    for i = 0 to Array1.dim a - 1 do
      if Int32.to_int (Array1.get a i) <> (Array1.dim a - (1 + i))
      then raise Exit
    done;
    Micro_bench_types.Ok
  with Exit -> Micro_bench_types.Error "incorrect rev"
