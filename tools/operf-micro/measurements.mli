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

type measurements =
  { name : string;
    params : (string * string) list;
    samples : measurement_sample list }

type 'a group =
  | Simple of 'a
  | Group of (string * 'a) list

type runs = {
  name : string;
  parameter : int option;
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

type group_param = { group : string; parameter : int option }
module GroupParamMap : Map.S with type key = group_param

val analyse_measurement : ?filter_outliers:bool -> column -> measurement_sample list -> result

val analyse_measurements : column -> recorded_measurements -> result group GroupParamMap.t

val load_results : column -> Utils.file list -> result group GroupParamMap.t StringMap.t

val get_results : column -> Utils.file list -> ((int * float) list * result) group GroupParamMap.t StringMap.t

val compare_measurements :
  ?reference:string -> result group GroupParamMap.t StringMap.t StringMap.t ->
  string * (float * float) option group GroupParamMap.t StringMap.t StringMap.t
