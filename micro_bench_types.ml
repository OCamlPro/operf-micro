type result =
  | Ok
  | Error of string

type cost =
  | Short
  | Long
  | Longer

type range =
  | Any
  | Range of int * int
  | List of int list

type 'result unit_function =
  ( unit -> 'result )     (* benchmarked function *)
  * ( 'result -> result ) (* test function *)
  * cost                  (* estimated run time *)

type 'result unit_group =
  ( string * ( unit -> 'result ) ) list
                          (* benchmarked functions *)
  * ( 'result -> result ) (* test function *)
  * cost                  (* estimated run time *)

type ('result, 'arg) int_function =
  ( 'arg -> 'result )     (* benchmarked function *)
  * ( int -> 'arg )       (* argument generation *)
  * ( int -> 'result -> result ) (* test function *)
  * (range * cost) list   (* estimated run time for parameter range *)

type ('result, 'arg) int_group =
  ( string * ( 'arg -> 'result ) ) list
                          (* benchmarked functions *)
  * ( int -> 'arg )       (* argument generation *)
  * ( int -> 'result -> result ) (* test function *)
  * (range * cost) list   (* estimated run time for parameter range *)

type function_kind =
  | Unit : 'result unit_function          -> function_kind
  | Int : ('result, 'arg) int_function    -> function_kind
  | Unit_group : 'result unit_group       -> function_kind
  | Int_group : ('result, 'arg) int_group -> function_kind

type benchmark = string (* function name *)
               * function_kind

let r : benchmark list ref = ref []
let add l = r := !r @ l
let functions () = !r
