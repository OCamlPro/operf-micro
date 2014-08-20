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


let rec do_list f = function
    [] -> ()
  | a::l -> f a; do_list f l


(**** check and register ****)

open Micro_bench_types

let range =
  [ Range (10, 100_000), Short ]

let check (_:int list) = Ok

let prepare i = i

let run i = sieve i

let functions =
  [ "Eratosthene",
    Int (run, prepare, check, range) ]

let () = add functions
