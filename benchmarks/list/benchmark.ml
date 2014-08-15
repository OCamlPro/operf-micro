
let rec interval_direct i j =
  if i <= j
  then i :: (interval_direct (i+1) j)
  else []

let interval_tail_rec i j =
  let rec aux acc i j =
    if i <= j
    then aux (j :: acc) i (j-1)
    else acc
  in
  aux [] i j

let interval_tail_rec_with_closure i j =
  let rec aux acc j =
    if i <= j
    then aux (j :: acc) (j-1)
    else acc
  in
  aux [] j

let list_rev l =
  let rec aux acc l =
    match l with
    | [] -> acc
    | h::t -> aux (h::acc) t
  in
  aux [] l

let list_rev_while l =
  let l = ref l in
  let acc = ref [] in
  let continue = ref true in
  while !continue do
    match !l with
    | [] -> continue := false
    | h :: t ->
      l := t;
      acc := h :: !acc
  done;
  !acc

let rec map_direct f l =
  match l with
  | [] -> []
  | h :: t -> f h :: map_direct f t

let map_direct_closure f l =
  let rec aux l =
    match l with
    | [] -> []
    | h :: t -> f h :: aux t
  in
  aux l

let rec map_tail_rec f l =
  let rec aux acc f l =
    match l with
    | [] -> acc
    | h :: t -> aux (f h :: acc) f t
  in
  list_rev (aux [] f l)

let rec rev_map_tail_rec f l =
  let rec aux acc f l =
    match l with
    | [] -> acc
    | h :: t -> aux (f h :: acc) f t
  in
  aux [] f l

let rev_map_while f l =
  let l = ref l in
  let acc = ref [] in
  let continue = ref true in
  while !continue do
    match !l with
    | [] -> continue := false
    | h :: t ->
      l := t;
      acc := f h :: !acc
  done;
  !acc

open Micro_bench_types

let interval_range =
  [ Range (0, 10_000), Short;
    Range (100_000, 1_000_000), Long;
    Range (10_000_000, 100_000_000), Longer ]

let check_interval (n, l) =
  let rec aux = function
    | (-1), [] -> Ok
    | i, h :: t ->
      if (n - i) = h
      then aux (i-1, t)
      else Error "mismatched value"
    | _, [] -> Error "mismatched length"
  in
  aux (n, l)

let mk_interval f = fun i -> i, f 0 i

let interval_group =
  Int_group
    (["direct", mk_interval interval_direct;
      "tail_rec", mk_interval interval_tail_rec;
      "tail_rec_with_closure", mk_interval interval_tail_rec_with_closure],
     (fun i -> i),
     check_interval,
     interval_range)

let rev_range = interval_range

let check_rev (orig,l) =
  if orig = List.rev l
  then Ok
  else Error ""

let mk_rev f = (fun l -> l, f l)

let rev_group =
  Int_group
    (["rec", mk_rev list_rev;
      "rev_while", mk_rev list_rev_while],
     (fun i -> interval_tail_rec 0 i),
     check_rev,
     rev_range)

let mk_map_succ f = (fun l -> l, f succ l)

let map_succ_range = interval_range

let check_map_succ (orig, l) =
  if List.map (fun i -> i - 1) l = orig
  then Ok
  else Error ""

let map_succ_group =
  Int_group
    (["direct", mk_map_succ map_direct;
      "closure", mk_map_succ map_direct_closure;
      "tail_rec", mk_map_succ map_tail_rec],
       (fun i -> interval_tail_rec 0 i),
       check_map_succ,
       map_succ_range)

let rev_map_succ_range = interval_range

let check_rev_map_succ (orig, l) =
  if List.rev_map (fun i -> i - 1) l = orig
  then Ok
  else Error ""

let mk_rev_map_succ f = (fun l -> l, f succ l)

let rev_map_succ_group =
  Int_group
    (["rev_map_tail_rec succ", mk_rev_map_succ rev_map_tail_rec;
      "rev_map_while succ", mk_rev_map_succ rev_map_while],
     (fun i -> interval_tail_rec 0 i),
     check_rev_map_succ,
     rev_map_succ_range)

let functions =
  [ "interval", interval_group;
    "rev", rev_group;
    "map succ", map_succ_group;
    "rev_map succ", rev_map_succ_group;
  ]

let () = add functions
