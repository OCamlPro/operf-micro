open Vect_instantiation
open Micro_bench_types

let (=!=) a b = if a = b then Ok else Error ""

let () =
  add [
    "vec2 record dot product",
    Int_group (
      [ "functor", (fun x -> IV2R.dot x x);
        "direct", (fun x -> VR2.dot x x)],
      (fun i -> { VR2.x = float i; VR2.y = float (i + 1) }),
      (fun i res -> (float i) *. (float i) +. (float (i+1)) *. (float (i+1)) =!= res),
      [ Any, Short ]);
  ]

(* TODO add the other ones *)
