type result =
  | Ok
  | Error of string

type cost =
  | Fast
  | Slow
  | Slower

type range =
  | Any
  | Range of int * int
  | List of int list

type function_kind =
  | Unit :  ( unit -> 'result )     (* benchmarked function *)
            * ( 'result -> result ) (* test function *)
            * cost                  (* estimated run time *)
    -> function_kind
  | Int : ( 'arg -> 'result )     (* benchmarked function *)
          * ( int -> 'arg )       (* argument generation *)
          * ( 'result -> result ) (* test function *)
          * (range * cost) list   (* estimated run time for parameter range *)
    -> function_kind

type benchmark = string (* function name *)
               * function_kind

