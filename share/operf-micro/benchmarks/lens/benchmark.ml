open Lens
open Lens.Infix

type point = {
  x : int;
  y : int;
}

type rect = {
  p1 : point;
  p2 : point;
}

let x = {
  get = (fun {x} -> x);
  set = (fun x {y} -> {x;y})
}

let y = {
  get = (fun {y} -> y);
  set = (fun y {x} -> {x;y})
}

let p1 = {
  get = (fun {p1} -> p1);
  set = (fun p1 {p2} -> {p1;p2})
}

let p2 = {
  get = (fun {p2} -> p2);
  set = (fun p2 {p1} -> {p1;p2})
}

let lens_rect_area r =
  abs (((r |. (p1 |-- x)) - (r |. (p2 |-- x)))
       * ((r |. (p1 |-- y)) - (r |. (p2 |-- y))))

let direct_rect_area r =
  abs ((r.p1.x - r.p2.x) * (r.p1.y - r.p2.y))

open Micro_bench_types

(* We prepare to avoid potential constant propagation here *)
let prepare i =
  let p1 = { x = 1111 + i; y = 2222 + i } in
  let p2 = { x = 3333 + i; y = 4444 + i } in
  let r = { p1; p2 } in
  r

let check_rect_area n res =
  let r = prepare n in
  let s = abs ((r.p1.x - r.p2.x) * (r.p1.y - r.p2.y)) in
  if s = res
  then Ok
  else Error "rect_area"

let range = [ Any, Short ]

let rect_area_group =
  Int_group
    (["lens", lens_rect_area;
      "baseline", direct_rect_area],
     prepare,
     check_rect_area,
     range)

let functions =
  [ "rect_area", rect_area_group;
  ]

let () = add functions
