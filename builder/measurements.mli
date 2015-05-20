open Utils
open Detect_config
open Benchmark

type error =
  | Missing_field of string

exception Error of error

type measurement_sample = {
  runs              : int;
  cycles            : int;
  nanos             : int;
  compactions       : int;
  minor_allocated   : int;
  major_allocated   : int;
  promoted          : int;
  major_collections : int;
  minor_collections : int;
}

type measurements = string * (string * string) list * measurement_sample list

type 'a group =
  | Simple of 'a
  | Group of (string * 'a) list

type runs = {
  name : string;
  list : (measurement_sample list) group;
}

type recorded_measurements = {
  suite_name : string;
  name : string;
  date : string;
  run_list : runs list;
}

val measurement_file : context -> benchmark -> measurements list -> Files.t

val read_measurement : contents:string -> measurements list

val read_measurement_file : filename:string -> recorded_measurements

type column =
  | Cycles
  | Nanos
  | Compactions
  | Minor_allocated
  | Major_allocated
  | Promoted
  | Major_collections
  | Minor_collections

type result =
    { mean_value : float;
      constant : float;
      max_value : int * float;
      min_value : int * float;
      standard_error : float }

val analyse_measurement : column -> measurement_sample list -> result

val analyse_measurements : column -> recorded_measurements -> result group StringMap.t

val load_results : column -> Command.file list -> result group StringMap.t StringMap.t

val compare_measurements :
  ?reference:string -> result group StringMap.t StringMap.t StringMap.t ->
  string * float option group StringMap.t StringMap.t StringMap.t
