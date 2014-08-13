open Utils
open Detect_config
open Benchmark
open Builder

type error =
  | Mandatory_option of string
  | Unexpected of string
  | Problem_creating_save_directory of string
  | No_save_directory of string

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
    match Detect_config.find_operf_directory () with
    | None ->
       Format.printf "not found@."
    | Some operf_root_dir ->
       let operf_dir = Filename.concat operf_root_dir ".operf" in
       let sdir = Filename.concat operf_dir "micro" in
       Command.remove sdir;
       try
         Unix.rmdir operf_dir
       with _ -> ()
  in
  [],
  (fun _s -> ()),
  "\n\
   clean the .operf subdirectory",
  clean

type timestamped_result =
    { sr_timestamp: Detect_config.timestamp;
      sr_dir : Command.directory;
      sr_files : Command.file list }

type results_files =
    { res_name : string;
      res_dir : Command.directory;
      res : timestamped_result list }

let last_timestamped rf =
  let compare { sr_timestamp = ts1; _ } { sr_timestamp = ts2; _ } = compare ts1 ts2 in
  match List.sort compare rf.res with
  | [] -> None
  | h :: _ -> Some h

let print_timestamped_result ppf r =
  let rec print_files ppf files =
    let f file = Filename.chop_extension (Filename.basename file) in
    match files with
    | [] -> ()
    | [file] -> Format.fprintf ppf "%s" (f file)
    | file :: t -> Format.fprintf ppf "%s@ " (f file); print_files ppf t
  in
  Format.fprintf
    ppf "@[<h 2>%s:@ %a@]"
    r.sr_timestamp
    print_files r.sr_files

let print_result_files ppf r =
  let rec ptr ppf l =
    match l with
    | [] -> ()
    | [r] -> print_timestamped_result ppf r
    | r :: t ->
       Format.fprintf
         ppf "%a@ "
         print_timestamped_result r;
       ptr ppf t
  in
  Format.fprintf
    ppf "@[<v 2>%s@ %a@]" r.res_name
    ptr r.res

let load_result_list () =
  let timestamped_result dir =
    let sr_files =
      Array.to_list (Sys.readdir dir)
      |> List.filter (fun f -> Filename.check_suffix f ".result")
      |> List.map (Filename.concat dir)
      |> List.filter (fun v -> not (Sys.is_directory v))
    in
    { sr_timestamp = Filename.basename dir;
      sr_dir = dir;
      sr_files }
  in
  let res dir =
    let subdirs = Command.subdirectories dir in
    { res_name = Filename.basename dir;
      res_dir = dir;
      res = List.map timestamped_result subdirs }
  in
  match Detect_config.operf_home_directory () with
  | Err s -> raise (Error (No_save_directory s))
  | Ok home_dir ->
     let bench_named_dirs = Command.subdirectories home_dir in
     List.map res bench_named_dirs

let results_subcommand () =
  let l = ref [] in
  let results () =
    match !l with
    | [] ->
       let l = load_result_list () in
       List.iter (fun v -> Format.printf "%a@." print_result_files v) l
    | selected ->
       let l = load_result_list () in
       List.iter
         (fun v ->
          if List.mem v.res_name selected
          then
            match last_timestamped v with
            | None -> ()
            | Some res ->
               let results = Measurements.load_results Measurements.Cycles res.sr_files in
               Format.printf "@[<v 2>%s:@ " v.res_name;
               StringMap.iter
                 (fun bench_name res_map ->
                  Format.printf "@[<v 2>%s:@ " bench_name;
                  StringMap.iter
                    (fun fun_name result ->
                     Format.printf "%s: %f@ " fun_name result.Measurements.mean_value)
                    res_map;
                  Format.pp_close_box Format.std_formatter ();
                  Format.printf "@]")
                 results;
               Format.printf "@]@.")
         l;
       Format.printf "@."
  in
  [],
  (fun s -> l := s :: !l),
  "[<names>]\n\
   if no name provided, list recorded results, otherwise print last results",
  results

let load_all_results () =
  let aux map v =
    match last_timestamped v with
    | None -> map
    | Some res ->
      let results = Measurements.load_results Measurements.Cycles res.sr_files in
      StringMap.add v.res_name results map
  in
  List.fold_left aux StringMap.empty (load_result_list ())

let cut_pad width s =
  let len = String.length s in
  if len >= width
  then String.sub s 0 width
  else s ^ (String.make (width - len) ' ')

let print_compared_results ppf width name_width comp =
  let print ppf result_name map =
    Format.pp_print_string ppf (cut_pad name_width result_name);
    let print ppf _set_name map =
      let print ppf _function_name value =
        (match value with
         | None -> Format.pp_print_string ppf (String.make width ' ')
         | Some ratio -> Format.fprintf ppf "%*.2f" width ratio);
        Format.pp_print_string ppf " "
      in
      StringMap.iter (print ppf) map in
    StringMap.iter (print ppf) map;
    Format.fprintf ppf "@."
  in
  Format.pp_print_string ppf (String.make name_width ' ');
  StringMap.iter
    (fun _set_name sub -> StringMap.iter
        (fun sub _ ->
           Format.pp_print_string ppf (cut_pad width sub);
           Format.pp_print_string ppf " ")
        sub)
    (snd (StringMap.choose comp));
  Format.fprintf ppf "@.";
  StringMap.iter (print ppf) comp

let compare_subcommand () =
  let compare () =
    let result_map = load_all_results () in
    let _reference, comp = Measurements.compare_measurements result_map in
    let ppf = Format.std_formatter in
    let name_width = 10 in
    let width = 7 in
    print_compared_results ppf width name_width comp
  in
  [],
  (fun _s -> ()),
  "\n\
   do things...",
  compare

let subcommands =
  [ "init", init_subcommand;
    "build", build_subcommand;
    "list", list_subcommand;
    "run", run_subcommand;
    "clean", clean_subcommand;
    "results", results_subcommand;
    "compare", compare_subcommand ]

let error fmt =
  Format.kfprintf (fun _ppf -> exit 1) Format.err_formatter fmt

let print_error ppf = function
  | Mandatory_option opt ->
    Format.fprintf ppf "Missing mandatory option %s" opt
  | Unexpected arg ->
    Format.fprintf ppf "Unexpected command line argument %s" arg
  | Problem_creating_save_directory s ->
    Format.fprintf ppf "Couldn't create save directory: %s" s
  | No_save_directory s ->
     Format.fprintf ppf "Couldn't find save directory: %s" s

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

let print_measurements_error ppf = function
  | Measurements.Missing_field s ->
     Format.fprintf ppf "Missing file field %s" s

let print_errors ppf = function
  | Error e -> print_error ppf e
  | Benchmark.Error e -> print_error_benchmark ppf e
  | Detect_config.Error e -> print_detect_config_error ppf e
  | Measurements.Error e -> print_measurements_error ppf e
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
