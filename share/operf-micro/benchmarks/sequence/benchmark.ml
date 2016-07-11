
module Sequence = struct
  type 'a t = ('a -> unit) -> unit

  let return x k = k x
  let map f seq k = seq (fun x -> k (f x))
  let flat_map f seq k = seq (fun x -> f x k)
  let filter p seq k = seq (fun x -> if p x then k x)
  let of_list l k = List.iter k l
  let range i j k = for x = i to j do k x done

  let fold f init seq =
    let r = ref init in
    seq (fun elt -> r := f !r elt);
    !r
end

let map_fold n =
  Sequence.range 1 n
  |> Sequence.map (fun x -> n - x)
  |> Sequence.fold (+) 0

let map_fold_base n =
  let sum = ref 0 in
  for i = 1 to n do
    let x = n - i in
    sum := !sum + x
  done;
  !sum

let flat_map_fold n =
  Sequence.range 1 n
  |> Sequence.flat_map (fun x -> Sequence.range x n)
  |> Sequence.filter (fun x -> x mod 10 <> 0)
  |> Sequence.fold (+) 0

let flat_map_fold_base n =
  let sum = ref 0 in
  for i = 1 to n do
    for j = i to n do
      if j mod 10 <> 0 then sum := !sum + j
    done
  done;
  !sum

(* benchs *)

module M = Micro_bench_types

let id x = x

(* no validation *)
let valid_trivial _n _res = M.Ok
let same_as f n res = if f n = res then M.Ok else M.Error "do not match"

let bench_map_fold =
  "map_fold", M.Int_group (
    ["sequence", map_fold; "baseline", map_fold_base],
    id,
    same_as map_fold_base,
    [M.Range (10, 100), M.Short
    ;M.Range (100_000, 1_000_000), M.Short]
)

let bench_flat_map_fold =
  "flat_map_fold", M.Int_group (
  ["sequence", flat_map_fold; "baseline", flat_map_fold_base],
  id,
  same_as flat_map_fold_base,
  [M.Range (10, 100), M.Short
  ;M.Range (100_000, 1_000_000), M.Short]
)

let () =
  M.add [ bench_map_fold
        ; bench_flat_map_fold
        ]
