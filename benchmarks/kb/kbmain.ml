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

open Terms
open Equations
open Orderings
open Kb

(****
let group_rules = [
  { number = 1; numvars = 1;
    lhs = Term("*", [Term("U",[]); Var 1]); rhs = Var 1 };
  { number = 2; numvars = 1;
    lhs = Term("*", [Term("I",[Var 1]); Var 1]); rhs = Term("U",[]) };
  { number = 3; numvars = 3;
    lhs = Term("*", [Term("*", [Var 1; Var 2]); Var 3]);
    rhs = Term("*", [Var 1; Term("*", [Var 2; Var 3])]) }
]
****)

let geom_rules = [
  { number = 1; numvars = 1;
    lhs = Term ("*",[(Term ("U",[])); (Var 1)]);
    rhs = Var 1 };
  { number = 2; numvars = 1;
    lhs = Term ("*",[(Term ("I",[(Var 1)])); (Var 1)]);
    rhs = Term ("U",[]) };
  { number = 3; numvars = 3;
    lhs = Term ("*",[(Term ("*",[(Var 1); (Var 2)])); (Var 3)]);
    rhs = Term ("*",[(Var 1); (Term ("*",[(Var 2); (Var 3)]))]) };
  { number = 4; numvars = 0;
    lhs = Term ("*",[(Term ("A",[])); (Term ("B",[]))]);
    rhs = Term ("*",[(Term ("B",[])); (Term ("A",[]))]) };
  { number = 5; numvars = 0;
    lhs = Term ("*",[(Term ("C",[])); (Term ("C",[]))]);
    rhs = Term ("U",[]) };
  { number = 6; numvars = 0;
    lhs = Term("*",
            [(Term ("C",[]));
             (Term ("*",[(Term ("A",[])); (Term ("I",[(Term ("C",[]))]))]))]);
    rhs = Term ("I",[(Term ("A",[]))]) };
  { number = 7; numvars = 0;
    lhs = Term("*",
            [(Term ("C",[]));
             (Term ("*",[(Term ("B",[])); (Term ("I",[(Term ("C",[]))]))]))]);
    rhs = Term ("B",[]) }
]

let group_rank = function
    "U" -> 0
  | "*" -> 1
  | "I" -> 2
  | "B" -> 3
  | "C" -> 4
  | "A" -> 5
  | _ -> assert false

let group_precedence op1 op2 =
  let r1 = group_rank op1
  and r2 = group_rank op2 in
    if r1 = r2 then Equal else
    if r1 > r2 then Greater else NotGE

let group_order = rpo group_precedence lex_ext

let greater pair =
  match group_order pair with Greater -> true | _ -> false

let complete greater complete_rules rules =
    let n = check_rules complete_rules
    and eqs = List.map (fun rule -> (rule.lhs, rule.rhs)) rules in
    List.rev (kb_completion greater n complete_rules [] (n,n) eqs)

open Micro_bench_types

let check_string l =
  let b = Buffer.create 10000 in
  pretty_rules b l;
  Buffer.contents b

let check_geom_rules l =
  let s = check_string l in
  if s = Result.result
  then Ok
  else Error ("\n" ^ s)

let functions =
  [ "kb geom_rules",
    Unit ((fun () -> complete greater [] geom_rules),
          check_geom_rules,
          Long) ]

let () = add functions

let save_result () =
  let s = check_string (complete greater [] geom_rules) in
  let oc = open_out "result.ml" in
  output_string oc "let result =\n\"";
  output_string oc s;
  output_string oc "\"";
  close_out oc

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "make-result"
  then save_result ()
