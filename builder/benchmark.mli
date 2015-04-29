open Command
open Detect_config

type library = string

type benchmark =
  { bench_name : string;
    bench_path : directory;
    files : file list;
    link: library list }

type cost =
  | Short
  | Long
  | Longer

type run_config =
  { maximal_cost : cost option;
    time_quota : float option;
    different_values : int option;
    selected_sets : string list option }

type error =
  | Missing_file of file

exception Error of error

val benchmark_prog : native:bool -> benchmark -> file

val load_build_descrs : context -> benchmark list

val benchmark_functions : benchmark -> string list option

val benchmark_nbr_functions : benchmark -> int
