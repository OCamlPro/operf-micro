(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Eratosthene's sieve *)

(* interval min max = [min; min+1; ...; max-1; max] *)

let rec interval min max =
  if min > max then [] else min :: interval (min + 1) max


(* filter p L returns the list of the elements in list L
   that satisfy predicate p *)

let rec filter p = function
     []  -> []
  | a::r -> if p a then a :: filter p r else filter p r


(* Application: removing all numbers multiple of n from a list of integers *)

let remove_multiples_of n =
  filter (fun m -> m mod n <> 0)


(* The sieve itself *)

let sieve max =
  let rec filter_again = function
     [] -> []
  | n::r as l ->
      if n*n > max then l else n :: filter_again (remove_multiples_of n r)
  in
    filter_again (interval 2 max)

(**** check and register ****)

open Micro_bench_types

let range =
  [ Range (10, 100_000), Short ]

let n = 50_000

let do_not_check _ (_:int list) = Ok

let check_string l =
  let b = Buffer.create 1000 in
  let ppf = Format.formatter_of_buffer b in
  List.iter (fun i -> Format.fprintf ppf "%i;\n" i) l;
  Format.fprintf ppf "@.";
  Buffer.contents b

let check l =
  if l = Result.result
  then Ok
  else Error ""

let prepare i = i

let run i = sieve i

let functions =
  [ "Eratosthene", Int (run, prepare, do_not_check, range);
    "Eratosthene_test", Unit ((fun () -> run n), check, Short); ]

let () = add functions

let save_result () =
  let s = check_string (run n) in
  let oc = open_out "result.ml" in
  output_string oc "let result =\n[";
  output_string oc s;
  output_string oc "]";
  close_out oc

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "make-result"
  then save_result ()
