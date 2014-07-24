open Utils
open Detect_config
open Benchmark
open Builder

type error =
  | Mandatory_option of string
  | Unexpected of string
  | Problem_creating_save_directory of string

exception Error of error

let set_string r = Arg.String (fun v -> r := Some v)

let build_subcommand () =
  let build () =
    let config = Detect_config.load_operf_config_file () in
    let context = Detect_config.load_context config in
    let build_descr = load_build_descrs context in
    Detect_config.prepare_build context;
    build_benchmarks context build_descr
  in
  [],
  (fun _s -> ()),
  "",
  build

let list_subcommand () =
  let list () =
    let config = Detect_config.load_operf_config_file () in
    let context = Detect_config.load_context config in
    let build_descr = load_build_descrs context in
    let l =
      List.map (fun b -> b, Benchmark.benchmark_functions b) build_descr in
    List.iter (fun (b, f) ->
        match f with
        | None ->
          Printf.printf "%s: FAILED\n%!" b.bench_path
        | Some f ->
          let f = String.concat " " f in
          Printf.printf "%s: %s\n%!"
            b.bench_path
            f) l
  in
  [],
  (fun _s -> ()),
  "",
  list

let run_subcommand () =
  let time_quota = ref None in
  let maximal_cost = ref None in
  let output_dir = ref None in
  let set_quota = Arg.Float (fun v -> time_quota := Some v) in
  let set_cost c = Arg.Unit (fun () -> maximal_cost := Some c) in
  let run () =
    let config = Detect_config.load_operf_config_file () in
    let context = Detect_config.load_context config in
    let build_descr = load_build_descrs context in
    let rc = { maximal_cost = !maximal_cost; time_quota = !time_quota } in
    let m = run_benchmarks context rc build_descr in
    let output_dir =
      match !output_dir with
      | Some d -> d
      | None ->
        match Detect_config.save_directory context with
        | Err s -> raise (Error (Problem_creating_save_directory s))
        | Ok d -> d
    in
    let aux (bench, measurements) =
      let file = Measurements.measurement_file context bench measurements in
      let filename = Filename.concat output_dir (bench.bench_name ^ ".result") in
      Command.write_file filename (fun ppf -> Files.print ppf file)
    in
    List.iter aux m
    (* List.iter (fun (_, m) -> Measurements.output_measurements stdout m) m; *)
  in
  [ "--time-quota", set_quota, "t time_quota";
    "-q", set_quota, " alias of --time-quota";
    "--long", set_cost Long, " allow running long test";
    "--longer", set_cost Longer, " allow running longer test";
    "--output", set_string output_dir,
    " directory where .result files will be recorded";
    "-o", set_string output_dir, " same as --output";
  ],
  (fun _s -> ()),
  "",
  run

let init_subcommand () =
  let name = ref None in
  let bin_dir = ref None in
  let init () =
    let name = get_opt !name
        (fun () -> raise (Error (Mandatory_option "name")))
    in
    let init_dir =
      match !bin_dir with
      | None ->
        Detect_config.initialize_in_compiler_dir name
      | Some dir ->
        Detect_config.initialize_with_bin_dir name dir
    in
    Format.printf "initialised in directory %s@." init_dir
  in
  ["--bin-dir", set_string bin_dir, " path to ocaml binary directory"],
  (fun s ->
     match !name with
     | None -> name := Some s
     | Some _ -> raise (Error (Unexpected s))),
  "[<args>] <name>\n\
   initialise the .operf directory.\n",
  init

let clean_subcommand () =
  let clean () =
    failwith "TODO"
  in
  [],
  (fun _s -> ()),
  "",
  clean

let subcommands =
  [ "init", init_subcommand;
    "build", build_subcommand;
    "list", list_subcommand;
    "run", run_subcommand;
    "clean", clean_subcommand;]

let error fmt =
  Format.kfprintf (fun _ppf -> exit 1) Format.err_formatter fmt

let print_error ppf = function
  | Mandatory_option opt ->
    Format.fprintf ppf "Missing mandatory option %s" opt
  | Unexpected arg ->
    Format.fprintf ppf "Unexpected command line argument %s" arg
  | Problem_creating_save_directory s ->
    Format.fprintf ppf "Couldn't create save directory: %s" s

let print_error_benchmark ppf = function
  | Missing_file file ->
    Format.fprintf ppf "Missing file %s" file

let print_detect_config_error ppf = function
  | Parse_error loc ->
    Format.fprintf ppf "parse error %a" Loc.print_loc loc
  | Missing_config_field (file, field) ->
    Format.fprintf ppf "Missing field %s in file %s" field file
  | Duplicate_config_field (file, field) ->
    Format.fprintf ppf "Ducplicated field %s in file %s" field file
  | No_config_file ->
    Format.fprintf ppf "couldn't find a configuration file"
  | Not_ocaml_compiler_dir ->
    Format.fprintf ppf "couldn't find ocaml compilation directory"
  | No_compiler ->
    Format.fprintf ppf "couldn't find a working ocaml compiler"
  | No_timestamp ->
    Format.fprintf ppf "timestamp file is missing"
  | Missing_directory d ->
    Format.fprintf ppf "The directory %s doesn't exists" d

let print_errors ppf = function
  | Error e -> print_error ppf e
  | Benchmark.Error e -> print_error_benchmark ppf e
  | Detect_config.Error e -> print_detect_config_error ppf e
  | e ->
    let bt = Printexc.get_backtrace () in
    let exn = Printexc.to_string e in
    Format.fprintf ppf "Uncaugh exception %s:@.%s@." exn bt

let () =
  if Array.length Sys.argv < 2
  then
    error "usage: operf <command> [<args>]@.commands:@.%a"
      (fun ppf -> List.iter (fun (name, _) -> Format.fprintf ppf "    %s@." name))
      subcommands
  else
    let subcommand_name = Sys.argv.(1) in
    let subcommand =
      try Some (List.assoc subcommand_name subcommands) with
      | Not_found -> None in
    match subcommand with
    | None -> error "unknown subcommand %s@." subcommand_name
    | Some f ->
      try
        let (spec, annon_arg, usage, run) = f () in
        let usage = "usage: " ^ (Filename.basename Sys.executable_name) ^
                    " " ^ subcommand_name ^ " " ^ usage in
        Arg.parse_argv ~current:(ref 1) Sys.argv (Arg.align spec) annon_arg usage;
        run ()
      with
      | Arg.Help s ->
        print_endline s;
      | e ->
        Format.eprintf "Error: %a@." print_errors e;
        exit 1
