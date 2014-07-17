open Utils
open Command
open Detect_config

type library = string

type benchmark =
  { bench_name : string;
    bench_path : directory;
    files : file list;
    link: library list }

let empty_benchmark bench_path =
  let bench_name = Filename.basename bench_path in
  { bench_name;
    bench_path;
    files = [];
    link = [] }

let default_benchmark bench_path =
  { (empty_benchmark bench_path)
    with files = ["benchmark.ml"] }

type cost =
  | Short
  | Long
  | Longer

type run_config =
  { maximal_cost : cost option;
    time_quota : float option }


type error =
  | Missing_file of file

exception Error of error


let string_list e =
  match e with
  | Files.List l ->
    List.map (function
        | Files.String s -> s
        | _ -> failwith "parse error")
      l
  | _ -> failwith "parse error"

let link v e =
  match e.link with
  | [] -> { e with link = string_list v }
  | _::_ -> failwith "Link field present multiple times"

let files v e =
  match e.files with
  | [] -> { e with files = string_list v }
  | _::_ -> failwith "Files field present multiple times"

let parse_file dir v =
  let fields e = function
    | "link", v -> link v e
    | "files", v -> files v e
    | s, _ -> failwith ("Unknown field: " ^ s)
  in
  match v with
  | Files.Dict d ->
    List.fold_left fields (empty_benchmark dir) d
  | _ -> failwith ("parse error")

let check_build_descr b =
  let check_file f =
    if not (Sys.file_exists (Filename.concat b.bench_path f))
    then raise (Error (Missing_file f)) in
  List.iter check_file b.files

let subdirectories (d:directory) : directory list =
  let subdirectories =
    Array.to_list
      (Array.map (fun s -> Filename.concat d s)
         (Sys.readdir d)) in
  let subdirectories =
    List.filter Sys.is_directory subdirectories in
  subdirectories

let load_build_descr (d:directory) =
  let file_name = Filename.concat d "benchmark.build" in
  if not (Sys.file_exists file_name)
  then
    if Sys.file_exists (Filename.concat d "benchmark.ml")
    then Some (default_benchmark d)
    else None
  else
    let result = Detect_config.load_config_file file_name in
    Some (parse_file d result)

let load_build_descrs c =
  let build_dirs = subdirectories
      (Filename.concat c.operf_files_path "benchmarks") in
  let build_descrs = filter_map load_build_descr build_dirs in
  List.iter check_build_descr build_descrs;
  build_descrs

let benchmark_prog ~native b =
  if native
  then Filename.concat b.bench_path "benchmark.native"
  else Filename.concat b.bench_path "benchmark.byte"

let list_function_command b : command * file =
  let tmp = Filename.temp_file "list_function" "" in
  (benchmark_prog ~native:true b, [ A "-o"; OF tmp; A "--raw-list" ], None), tmp

let benchmark_functions b =
  let c = list_function_command b in
  run_and_read_lines c
