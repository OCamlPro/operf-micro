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
    "vec2 tuple dot product",
    Int (
      (fun x -> IV2.dot x x),
      (fun i -> (float i, float (i + 1))),
      (fun i res -> (float i) *. (float i) +. (float (i+1)) *. (float (i+1)) =!= res),
      [ Any, Short ]);
    "vec3 tuple dot product",
    Int (
      (fun x -> IV3.dot x x),
      (fun i -> (float i, float (i + 1), float (i + 2))),
      (fun i res ->
         (float i) *. (float i) +. (float (i+1)) *. (float (i+1))
         +. (float (i+2)) *. (float (i+2)) =!= res),
      [ Any, Short ]);
    "vec3 record dot product",
    Int (
      (fun x -> IV3R.dot x x),
      (fun i -> { x = float i; y = float (i + 1); z = float (i + 2) }),
      (fun i res ->
         (float i) *. (float i) +. (float (i+1)) *. (float (i+1))
         +. (float (i+2)) *. (float (i+2)) =!= res),
      [ Any, Short ]);
    "nothing",
    Int (
      (fun x -> ()),
      (fun i -> ()),
      (fun i res -> Ok),
      [ Any, Short ])
  ]

(* TODO add the other ones *)
