open Detect_config
open Benchmark

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

val measurement_file : context -> benchmark -> measurements list -> Files.t

val read_measurement : string list -> measurements list
