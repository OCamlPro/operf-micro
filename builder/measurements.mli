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

type measurements = string * measurement_sample list

type runs = {
  name : string;
  list : measurement_sample list;
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
      min_value : int * float }

val analyse_measurement : column -> measurement_sample list -> result

val analyse_measurements : column -> recorded_measurements -> result StringMap.t

val load_results : column -> Command.file list -> result StringMap.t StringMap.t

val compare_measurements :
  ?reference:string -> result StringMap.t StringMap.t StringMap.t ->
  string * float option StringMap.t StringMap.t StringMap.t
