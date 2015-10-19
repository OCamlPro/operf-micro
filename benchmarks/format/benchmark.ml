module F = Format
module P = Printf

let int = Format.pp_print_int

let with_printf b fmt () =
  (if b then P.fprintf else P.ifprintf) fmt  "%d + %d = %d" 1 2 3

let log b fmt s k = if b then k (P.fprintf fmt s)
let with_printf_cont b fmt () =
  log b fmt "%d + %d = %d" @@ fun k -> k 1 2 3

let with_format b fmt () =
  (if b then F.fprintf else F.ifprintf) fmt "%d + %d = %d" 1 2 3

let with_format_pp b fmt () =
  (if b then F.fprintf else F.ifprintf) fmt "%a + %a = %a" int 1 int 2 int 3

let logf b fmt s k = if b then k (F.fprintf fmt s)
let with_format_pp_cont b fmt () =
  logf b fmt "%a + %a = %a" @@ fun k -> k int 1 int 2 int 3

let dev_null = open_out "/dev/null"
let dev_null_fmt = F.formatter_of_out_channel dev_null


let brackets pp fmt x = F.fprintf fmt "[@[%a@]]" pp x
let list pp =
  brackets @@ F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ";@ ") pp

let complicated_format
    (k : ('a,_,_,_) format4 -> 'a)  name x () =
  k "@[@[<2>let %s =@ %a@]@ in foo %s@]"
    name
    (list @@ list int) x
    name

open Micro_bench_types

let check _ = Ok

let b = ref true

let make_simple_benchs s b =
  let name = if b then "simple"^s else "simple_ignore"^s in
  name, Unit_group (
    [ "printf", with_printf b dev_null ;
      "printf_cont", with_printf_cont b dev_null ;
      "format", with_format b dev_null_fmt ;
      "format_pp", with_format_pp b dev_null_fmt ;
      "format_pp_cont", with_format_pp_cont b dev_null_fmt ;
    ],
    check,
    Short)

let make_complicated_benchs s x =
  let name = "complicated"^s in
  name, Unit_group (
    [ "direct", complicated_format (F.kfprintf (fun _ -> "") dev_null_fmt) s x ;
      "str", complicated_format F.asprintf s x ;
      "noop", complicated_format (F.ikfprintf (fun _ -> "") dev_null_fmt) s x ;
    ],
    check,
    Short)

let () = add [
  make_simple_benchs "" true ;
  make_simple_benchs "" false ;
  (b:= true ; make_simple_benchs "_ref" !b) ;
  (b:= false ; make_simple_benchs "_ref" !b) ;

  make_complicated_benchs "" [ [ 1 ; 2 ] ; [] ; [ 3 ; 4 ; 5 ] ] ;
  make_complicated_benchs "_empty" [] ;

]
