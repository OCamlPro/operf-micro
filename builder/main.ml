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

let compile_arg = ref []
let set_compile_arg s = compile_arg := s :: !compile_arg
let compiler_arg_opt = ["--ccopt", Arg.String set_compile_arg, "s add ocamlopt argument"]

let do_build ocamlopt_arg =
  let config = Detect_config.load_operf_config_file () in
  let context = Detect_config.load_context config in
  let build_descr = load_build_descrs context in
  Detect_config.prepare_build context;
  let ocamlopt_arg = List.rev_map (fun s -> Command.A s) ocamlopt_arg in
  build_benchmarks context build_descr ocamlopt_arg

let build_subcommand () =
  compiler_arg_opt,
  (fun _s -> ()),
  "",
  (fun () -> do_build !compile_arg)

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

module Arg_opt = struct
  let time_quota = ref None
  let maximal_cost = ref None
  let different_values = ref None
  let set_quota = Arg.Float (fun v -> time_quota := Some v)
  let set_cost c = Arg.Unit (fun () -> maximal_cost := Some c)
  let set_different_values i = different_values := Some i

  let run_config_arg =
    [ "--time-quota", set_quota, "t time_quota";
      "-q", set_quota, " alias of --time-quota";
      "--different-values", Arg.Int set_different_values, "n number of different values on which functions are evaluated";
      "-n", Arg.Int set_different_values, " alias of --different-values";
      "--long", set_cost Long, " allow running long test";
      "--longer", set_cost Longer, " allow running longer test" ]


  let output_dir = ref None

  let output_dir_arg =
    [ "--output", set_string output_dir,
      " directory where .result files will be recorded";
      "-o", set_string output_dir, " same as --output" ]

  let make_run_config selected_sets =
    { maximal_cost = !maximal_cost;
      time_quota = !time_quota;
      different_values = !different_values;
      selected_sets }

  let bin_dir = ref None
  let bin_dir_arg = ["--bin-dir", set_string bin_dir, "p path to ocaml binary directory"]

  let extra_dir = ref None
  let extra_dir_arg = ["-I", set_string extra_dir, "p path to extra benchmarks directory"]

  let selected_sets = ref []
  let add_selected_set = Arg.String (fun s -> selected_sets := s :: !selected_sets)
  let selected_sets_arg = ["--selected", add_selected_set, "s run benchmark s. All are run if none is specified";
                           "-s" , add_selected_set, " alias of --selected"]

  let get_selected_sets () =
    match !selected_sets with
    | [] -> None
    | l -> Some l

end

let do_run output_dir rc =
  let config = Detect_config.load_operf_config_file () in
  let context = Detect_config.load_context config in
  let build_descr = load_build_descrs context in
  let m = run_benchmarks context rc build_descr in
  let output_dir =
    match output_dir with
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

let run_subcommand () =
  let selected_sets = ref [] in
  let run () =
    let selected_sets =
      match !selected_sets with
      | [] -> None
      | l -> Some l
    in
    do_run !Arg_opt.output_dir (Arg_opt.make_run_config selected_sets)
  in
  Arg_opt.run_config_arg @ Arg_opt.output_dir_arg,
  (fun s -> selected_sets := s :: !selected_sets),
  "",
  run

let do_init name extra_dir bin_dir =
  let init_dir =
    match bin_dir with
    | None ->
      Detect_config.initialize_in_compiler_dir name extra_dir
    | Some dir ->
      Detect_config.initialize_with_bin_dir name extra_dir dir
  in
  Format.printf "initialised in directory %s@." init_dir

let init_subcommand () =
  let name = ref None in
  Arg_opt.bin_dir_arg @
  Arg_opt.extra_dir_arg,
  (fun s ->
     match !name with
     | None -> name := Some s
     | Some _ -> raise (Error (Unexpected s))),
  "[<args>] <name> \n\
   initialise the .operf directory.\n",
  (fun () ->
     let name = get_opt !name
         (fun () -> raise (Error (Mandatory_option "name")))
     in
     do_init name !Arg_opt.extra_dir !Arg_opt.bin_dir)

let do_clean () =
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

let clean_subcommand () =
  [],
  (fun _s -> ()),
  "\n\
   clean the .operf subdirectory",
  do_clean

type timestamped_result =
    { sr_timestamp: Detect_config.timestamp;
      sr_dir : Command.directory;
      sr_files : Command.file list }

type results_files =
    { res_name : string;
      res_dir : Command.directory;
      res : timestamped_result list }

let last_timestamped rf =
  let compare { sr_timestamp = ts1; _ } { sr_timestamp = ts2; _ } = - (compare ts1 ts2) in
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

let do_results selected_names selected_sets =
  let is_selected_set s =
    match selected_sets with
    | None -> true
    | Some l -> List.mem s l
  in
  match selected_names with
  | [] ->
    let l = load_result_list () in
    List.iter (fun v -> Format.printf "%a@." print_result_files v) l
  | selected ->
    let l = load_result_list () in
    List.iter
      (fun v ->
         if List.mem v.res_name selected
         then
           (match last_timestamped v with
            | None -> ()
            | Some res ->
              let results = Measurements.load_results Measurements.Cycles res.sr_files in
              Format.printf "@[<v 2>%s %s:@ " v.res_name res.sr_timestamp;
              StringMap.iter
                (fun bench_name res_map ->
                   if is_selected_set bench_name
                   then begin
                     Format.printf "@[<v 2>%s:@ " bench_name;
                     StringMap.iter
                       (fun name -> function
                          | Measurements.Simple result ->
                            Format.printf "%s: %.2f@ "
                              name result.Measurements.mean_value
                          | Measurements.Group results ->
                            Format.printf "@[<v 2>group %s@ " name;
                            List.iter (fun (fun_name, result) ->
                              Format.printf "%s: %.2f@ "
                                fun_name result.Measurements.mean_value)
                              results;
                            Format.printf "@]@ ")
                       res_map;
                     Format.printf "@]@ "
                   end)
                results;
              Format.printf "@]@ "))
      l;
    Format.printf "@."

let results_subcommand () =
  let l = ref [] in
  Arg_opt.selected_sets_arg,
  (fun s -> l := s :: !l),
  "[<names>]\n\
   if no name provided, list recorded results, otherwise print last results",
  (fun () -> do_results !l (Arg_opt.get_selected_sets ()))

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
      let print ppf value =
        (match value with
         | None -> Format.pp_print_string ppf (String.make width ' ')
         | Some ratio -> Format.fprintf ppf "%*.2f" width ratio);
        Format.pp_print_string ppf " "
      in
      let print_group ppf _function_name = function
        | Measurements.Simple value -> print ppf value
        | Measurements.Group l -> List.iter (fun (_,value) -> print ppf value) l
      in
      StringMap.iter (print_group ppf) map in
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
   comparisons between runs",
  compare

let doall_subcommand () =
  let do_all name bin_dir extra_dir output_dir selected_sets compile_arg rc =
    do_clean ();
    do_init name extra_dir bin_dir;
    do_build compile_arg;
    do_run output_dir rc;
    do_results [name] selected_sets
  in
  let args = ref [] in
  Arg_opt.bin_dir_arg @
  Arg_opt.extra_dir_arg @
  Arg_opt.run_config_arg @
  Arg_opt.output_dir_arg @
  Arg_opt.selected_sets_arg @
  compiler_arg_opt,
  (fun s -> args := s :: !args),
  "[<args>] <name>\n\
   clean, initialise the .operf directory.\n",
  (fun () ->
     match !args with
     | [name] ->
       let selected_set = Arg_opt.get_selected_sets () in
       let rc = Arg_opt.make_run_config selected_set in
       do_all name !Arg_opt.bin_dir !Arg_opt.extra_dir !Arg_opt.output_dir selected_set !compile_arg
     | _ -> failwith "wrong number of arguments, expected: <name>")

let subcommands =
  [ "init", init_subcommand;
    "build", build_subcommand;
    "list", list_subcommand;
    "run", run_subcommand;
    "clean", clean_subcommand;
    "results", results_subcommand;
    "compare", compare_subcommand;
    "doall", doall_subcommand ]

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
    error "Usage: operf <command> [<args>]@.commands:@.%a"
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
        let usage = "Usage: " ^
                    (Filename.basename Sys.executable_name) ^ " " ^
                    subcommand_name ^ " " ^ usage in
        Arg.parse_argv ~current:(ref 1) Sys.argv (Arg.align spec) annon_arg usage;
        run ()
      with
      | Arg.Bad s
      | Arg.Help s ->
        print_endline s;
      | e ->
        Format.eprintf "Error: %a@." print_errors e;
        exit 1
