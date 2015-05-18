
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

let flat_map_fold n =
  Sequence.range 1 n
  |> Sequence.flat_map (fun x -> Sequence.range x n)
  |> Sequence.filter (fun x -> x mod 10 <> 0)
  |> Sequence.fold (+) 0

(* benchs *)

module M = Micro_bench_types

let id x = x

(* no validation *)
let valid_trivial _n _res = M.Ok

let bench_map_fold =
  "map_fold", M.Int (map_fold, id, valid_trivial,
                     [M.Range (100, 100), M.Short
                     ;M.Range (100_000, 100_000), M.Short])

let bench_flat_map_fold =
  "flat_map_fold", M.Int (flat_map_fold, id, valid_trivial,
                     [M.Range (100, 100), M.Short
                     ;M.Range (100_000, 100_000), M.Short])

let () =
  M.add [ bench_map_fold
        ; bench_flat_map_fold
        ]
