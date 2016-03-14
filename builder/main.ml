open Utils
open Detect_config
open Benchmark
open Builder

type error =
  | Mandatory_option of string
  | Unexpected of string
  | Problem_creating_save_directory of string
  | No_save_directory of string
  | Cannot_find_compiler

exception Error of error

let set_string r = Arg.String (fun v -> r := Some v)
let add_string r = Arg.String (fun v -> r := v :: !r)

let compile_arg = ref []
let compiler_arg_opt = ["--ccopt", add_string compile_arg, "s add ocamlopt argument"]

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
  (fun () ->
     let _failed_build = do_build !compile_arg in
     ())

let do_check extra_dir =
  let path =
    Filename.concat (Filename.get_temp_dir_name ())
      "operf-micro-check" in
  Utils.remove path;
  Unix.mkdir path 0o777;
  match Detect_config.find_ocaml_binary_path () with
  | None ->
    raise (Error Cannot_find_compiler)
  | Some (bin_dir:Utils.directory) ->
    let _ : Utils.directory =
      Detect_config.initialize_with_bin_dir
        ~path
        ~with_default_benchmarks:false
        "check" extra_dir bin_dir in
    let config = Detect_config.load_operf_config_file () ~path in
    let context = Detect_config.load_context config in
    let build_descr = load_build_descrs context in
    Detect_config.prepare_build context;
    let failed = build_benchmarks context build_descr [] in
    Utils.remove path;
    if failed then exit 1

let check_subcommand () =
  let r = ref [] in
  [],
  (fun s -> r := s :: !r),
  "[<paths>]\n\
   Typecheck benchmarks",
  (fun () -> do_check !r)

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
  let stabilize_gc = ref false

  let run_config_arg =
    [ "--time-quota", set_quota, "t time_quota";
      "-q", set_quota, " alias of --time-quota";
      "--different-values", Arg.Int set_different_values, "n number of different values on which functions are evaluated";
      "-n", Arg.Int set_different_values, " alias of --different-values";
      "--long", set_cost Long, " allow running long test";
      "--longer", set_cost Longer, " allow running longer test";
      "--stabilize-gc", Arg.Set stabilize_gc, " stabilize gc between runs"; ]


  let output_dir = ref None

  let output_dir_arg =
    [ "--output", set_string output_dir,
      " directory where .result files will be recorded";
      "-o", set_string output_dir, " same as --output" ]

  let make_run_config selected_sets =
    { maximal_cost = !maximal_cost;
      time_quota = !time_quota;
      different_values = !different_values;
      selected_sets;
      stabilize_gc = !stabilize_gc; }

  let bin_dir = ref None
  let bin_dir_arg = ["--bin-dir", set_string bin_dir, "p path to ocaml binary directory"]

  let extra_dir = ref []
  let extra_dir_arg = ["-I", add_string extra_dir, "p path to extra benchmarks directory"]

  let default_dir_flag = ref false
  let default_dir_arg = ["--default-dir", Arg.Set default_dir_flag, " use $(HOME)/.operf instead of cwd"]

  let more_info = ref false
  let more_info_arg = ["--more", Arg.Set more_info, " print min,max and standard error infos"]

  let standard_error = ref false
  let standard_error_arg = ["--std-error", Arg.Set standard_error, " print the standard error for each bench"]

  let with_fun = ref false
  let with_fun_arg = ["--with-fun", Arg.Set with_fun, " plot each functions of the bench"]

  let output_png = ref false
  let output_png_arg = ["--png", Arg.Set output_png, " choose png output"]

  let allocations = ref false
  let allocations_arg = ["--allocations", Arg.Set allocations, " show the number of allocations"]

  let selected_sets = ref []
  let add_selected_set = Arg.String (fun s -> selected_sets := s :: !selected_sets)
  let selected_sets_arg = ["--selected", add_selected_set, "s run benchmark s. All are run if none is specified";
                           "-s" , add_selected_set, " alias of --selected"]

  let with_default_benchmarks = ref true
  let without_defualt_bench_arg =
    ["--without-default", Arg.Clear with_default_benchmarks, " do not use the default benchmark set"]

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

let do_init name ~with_default_benchmarks extra_dir bin_dir default_dir_flag =
  let init_dir =
    match bin_dir with
    | None ->
      if default_dir_flag
      then
        (init_operf_default_dir ();
         Detect_config.initialize_in_compiler_dir
           ~with_default_benchmarks
           ~path:Utils.operf_default_dir name extra_dir)
      else
        Detect_config.initialize_in_compiler_dir
          ~with_default_benchmarks
          name extra_dir
    | Some dir ->
      if default_dir_flag
      then
        (init_operf_default_dir ();
         Detect_config.initialize_with_bin_dir
           ~with_default_benchmarks
           ~path:Utils.operf_default_dir name extra_dir dir)
      else
        Detect_config.initialize_with_bin_dir
          ~with_default_benchmarks
          name extra_dir dir
  in
  Format.printf "initialized in directory %s@." init_dir

let init_subcommand () =
  let name = ref None in
  Arg_opt.bin_dir_arg @
  Arg_opt.default_dir_arg @
  Arg_opt.without_defualt_bench_arg @
  Arg_opt.extra_dir_arg,
  (fun s ->
     match !name with
     | None -> name := Some s
     | Some _ -> raise (Error (Unexpected s))),
  "[<args>] <name> \n\
   initialize the .operf directory.\n",
  (fun () ->
     let name = get_opt !name
         (fun () -> raise (Error (Mandatory_option "name")))
    in
    do_init
      ~with_default_benchmarks:!Arg_opt.with_default_benchmarks
      name !Arg_opt.extra_dir !Arg_opt.bin_dir !Arg_opt.default_dir_flag)

let do_clean () =
  match Detect_config.find_operf_directory () with
  | None ->
    (match find_operf_directory ?path:(Some operf_default_dir) () with
     | None -> Format.printf "nothing to clean@."
     | Some operf_root_dir ->
       let operf_dir = Filename.concat operf_root_dir ".operf" in
       let sdir = Filename.concat operf_dir "micro" in
       Utils.remove sdir;
       try
         Unix.rmdir operf_dir
       with _ -> ())
  | Some operf_root_dir ->
    let operf_dir = Filename.concat operf_root_dir ".operf" in
    let sdir = Filename.concat operf_dir "micro" in
    Utils.remove sdir;
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
      sr_dir : Utils.directory;
      sr_files : Utils.file list }

type results_files =
    { res_name : string;
      res_dir : Utils.directory;
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

let load_result_list selected_run =
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
     let all_res = List.map res bench_named_dirs in
     if selected_run <> []
     then 
       let res_list = 
         List.filter (fun run -> List.mem run.res_name selected_run) all_res in
       if List.length res_list <> List.length selected_run
       then List.iter (fun run_name -> 
         if not (List.exists (fun res -> res.res_name = run_name) res_list)
         then Printf.printf "can't find %s run\n%!" run_name) selected_run;
       res_list
     else all_res

let format_group_parameter ppf { Measurements.group; parameter } =
  match parameter with
  | None -> Format.fprintf ppf "%s" group
  | Some i -> Format.fprintf ppf "%s %i" group i

let do_results selected_names allocations selected_sets more_info =
  let is_selected_set s =
    match selected_sets with
    | None -> true
    | Some l -> List.mem s l
  in
  let kind = if allocations then
      Measurements.Minor_allocated
    else
      Measurements.Cycles
  in
  match selected_names with
  | [] ->
    let l = load_result_list [] in
    List.iter (fun v -> Format.printf "%a@." print_result_files v) l
  | selected ->
    let l = load_result_list selected in
    List.iter
      (fun v -> match last_timestamped v with
         | None -> ()
         | Some res ->
           let results = Measurements.load_results kind res.sr_files in
           Format.printf "@[<v 2>%s %s:@ " v.res_name res.sr_timestamp;
           StringMap.iter
             (fun bench_name res_map ->
                if is_selected_set bench_name
                then begin
                  Format.printf "@[<v 2>%s:@ " bench_name;
                  Measurements.GroupParamMap.iter
                    (fun group_param -> function
                       | Measurements.Simple result ->
                         if more_info
                         then Format.printf "%a: %.2f min:%.2F max:%.2F standar_error:%.2F@ "
                             format_group_parameter group_param
                             result.Measurements.mean_value
                             (snd result.Measurements.min_value)
                             (snd result.Measurements.max_value)
                             result.Measurements.standard_error
                         else Format.printf "%a: %.2f@ "
                             format_group_parameter group_param
                             result.Measurements.mean_value
                       | Measurements.Group results ->
                         Format.printf "@[<v 2>group %a@ "
                           format_group_parameter group_param;
                         List.iter (fun (fun_name, result) ->
                           if more_info
                           then Format.printf "%s: %.2f min:%.2F max:%.2F standar_error:%.2F@ "
                             fun_name
                             result.Measurements.mean_value
                             (snd result.Measurements.min_value)
                             (snd result.Measurements.max_value)
                             result.Measurements.standard_error
                           else Format.printf "%s: %.2f@ "
                               fun_name result.Measurements.mean_value)
                           results;
                         Format.printf "@]@ ")
                    res_map;
                  Format.printf "@]@ "
                end)
             results;
           Format.printf "@]@ ")
      l;
    Format.printf "@."

let results_subcommand () =
  let l = ref [] in
  Arg_opt.selected_sets_arg @
  Arg_opt.allocations_arg @
  Arg_opt.more_info_arg,
  (fun s -> l := s :: !l),
  "[<names>]\n\
   if no name provided, list recorded results, otherwise print last results",
  (fun () -> do_results !l !Arg_opt.allocations (Arg_opt.get_selected_sets ()) !Arg_opt.more_info)

let load_all_results selected_run =
  let aux map v =
    match last_timestamped v with
    | None -> map
    | Some res ->
      let results = Measurements.load_results Measurements.Cycles res.sr_files in
      StringMap.add v.res_name results map
  in
  List.fold_left aux StringMap.empty (load_result_list selected_run)

let cut_pad width s =
  let len = String.length s in
  if len >= width
  then String.sub s 0 width
  else s ^ (String.make (width - len) ' ')

let sort_by_bench
    (comp:(float * float) option Measurements.group Measurements.GroupParamMap.t StringMap.t StringMap.t) =
  StringMap.fold (fun run_name set_bench res ->
    StringMap.fold (fun bench_name bench res ->
      Measurements.GroupParamMap.fold (fun { group = group_name; parameter } value res ->
        let param_name =
          match parameter with
          | None -> ""
          | Some i -> Printf.sprintf ".%i" i in
        match value with
        | Measurements.Simple v ->
          begin
            let full_name = bench_name ^ param_name in
            try
              let set = StringMap.find full_name res in
              let map = StringMap.add run_name v set in
              StringMap.add full_name map res
            with Not_found -> StringMap.add full_name (StringMap.singleton run_name v) res
          end
        | Measurements.Group l ->
          begin
            List.fold_left (fun res (fun_name, v) ->
              let full_name =
                Printf.sprintf "%s.%s.%s%s" bench_name group_name fun_name param_name
              in
              try
                let set = StringMap.find full_name res in
                let map = StringMap.add run_name v set in
                StringMap.add full_name map res
              with Not_found -> StringMap.add full_name (StringMap.singleton run_name v) res) res l
          end
      ) bench res
    ) set_bench res
  ) comp StringMap.empty

let print_compared_results ppf width name_width std_err_flag
    (comp:(float * float) option Measurements.group Measurements.GroupParamMap.t StringMap.t StringMap.t) =
  let print ppf value =
    (match value with
     | None -> Format.pp_print_string ppf (String.make name_width ' ')
     | Some (ratio, std_err) ->
       if std_err_flag
       then
         let std_err_str = Format.sprintf "(%.2F)" std_err in
         (Format.fprintf ppf "%.2f%s" ratio std_err_str;
          Format.pp_print_string
            ppf
            (String.make (name_width - (String.length std_err_str) - 4) ' '))
       else (Format.fprintf ppf "%.2f" ratio;
             Format.pp_print_string ppf (String.make (name_width - 4) ' ')));
    Format.pp_print_string ppf " " in
  Format.pp_print_string ppf (String.make (width + 1) ' ');
  StringMap.iter
    (fun run_name _ ->
       Format.pp_print_string ppf (cut_pad name_width run_name);
       Format.pp_print_string ppf " ")
    comp;
  Format.fprintf ppf "@.";
  let benchs = sort_by_bench comp in
  StringMap.iter (fun bench_name set_runs ->
    Format.pp_print_string ppf (cut_pad width bench_name);
    Format.pp_print_string ppf " ";
    StringMap.iter (fun _run_name value -> print ppf value) set_runs;
    Format.fprintf ppf "@."
  ) benchs

let compare_subcommand () =
  let selected_run = ref [] in
  let compare () =
    let result_map = load_all_results !selected_run in
    if (StringMap.cardinal result_map <> 0)
    then
      let _reference, comp = Measurements.compare_measurements result_map in
      let ppf = Format.std_formatter in
      let name_width = if !Arg_opt.standard_error then 12 else 8 in
      let width = 32 in
      print_compared_results ppf width name_width !Arg_opt.standard_error comp
  in
  Arg_opt.standard_error_arg,
  (fun s -> selected_run := s :: !selected_run),
  "[<names>]\n\
   comparisons between runs",
  compare

let replace_whitespace s =
  String.map (fun c ->
    match c with
    | ' ' | '\012' | '\n' | '\r'| '\t' -> '-'
    | _ -> c
  ) s

let get_results selected_run =
  let aux map v =
    match last_timestamped v with
    | None -> map
    | Some res ->
      let results = Measurements.get_results Measurements.Cycles res.sr_files in
      StringMap.add v.res_name results map
  in
  List.fold_left aux StringMap.empty (load_result_list selected_run)

let get_benchs
    (results:((int * float) list * Measurements.result) Measurements.group
         Measurements.GroupParamMap.t Utils.StringMap.t Utils.StringMap.t)
    selected_bench group_name with_fun =
  let test_group group = function
    | None -> true
    | Some s -> s = group in
  StringMap.fold (fun run_name res acc ->
    StringMap.fold (fun bench_name res acc ->
      if selected_bench = bench_name
      then
        Measurements.GroupParamMap.fold (fun { group = ssbench_name; parameter } res acc ->
          let display_name =
            match parameter with
            | None -> ssbench_name
            | Some i -> Printf.sprintf "%s.%i" ssbench_name i
          in
          if test_group ssbench_name group_name
          then match res with
            | Measurements.Simple res ->
              begin
                try
                  let set = StringMap.find display_name acc in
                  StringMap.add display_name
                    (StringMap.add run_name (StringMap.singleton display_name res) set) acc
                with Not_found ->
                  StringMap.add display_name
                    (StringMap.singleton run_name (StringMap.singleton display_name res)) acc
              end
            | Measurements.Group functions ->
              if with_fun
              then
                List.fold_left (fun acc (fun_name, res) ->
                  let full_name = Printf.sprintf "%s.%s" display_name fun_name in
                  try
                    let set = StringMap.find full_name acc in
                    StringMap.add full_name
                      (StringMap.add run_name (StringMap.singleton full_name res) set) acc
                  with Not_found ->
                    StringMap.add full_name (StringMap.singleton run_name (StringMap.singleton full_name res)) acc
                ) acc functions
              else
                begin
                  let group_set = List.fold_left (fun acc (fun_name, res) ->
                    StringMap.add fun_name res acc
                  ) StringMap.empty functions in
                  try
                    let run_set = StringMap.find display_name acc in
                    StringMap.add display_name (StringMap.add run_name group_set run_set) acc
                  with Not_found ->
                    StringMap.add display_name (StringMap.singleton run_name group_set) acc
                end
          else acc
        ) res acc
      else acc
    ) res acc
  ) results StringMap.empty

let dump_header oc plot_fn full_name =
  Printf.fprintf oc "set title %S\n" full_name;
  if !Arg_opt.output_png
  then
    begin
      let png_name = (Filename.chop_extension plot_fn) ^ ".png" in
      Printf.fprintf oc "set term png\n";
      Printf.fprintf oc "set terminal png size 1024,768\n";
      Printf.fprintf oc "set output %S\n" png_name;
    end;
  Printf.fprintf oc "set xlabel %S\n" "Number of runs";
  Printf.fprintf oc "set ylabel %S\n" "Cycles";
  Printf.fprintf oc "plot %S" plot_fn

let get_plot_dir () =
  try
    let config = Detect_config.load_operf_config_file () in
    let context = Detect_config.load_context config in
    Filename.concat context.operf_files_path "plot"
  with Utils.Error _ ->
    let path =
      Filename.concat (Filename.get_temp_dir_name ())
        "operf-micro-plot" in
    Utils.remove path;
    Unix.mkdir path 0o777;
    path

let dump_plot_data bench_name data =
  let plot_dir = get_plot_dir () in
  let cpt = ref 0 in
  StringMap.fold (fun sbench_name res list ->
    let full_name = Printf.sprintf "%s_%s" bench_name (replace_whitespace sbench_name) in
    let plot_file_name = full_name ^ ".data" in
    let plot_file = Filename.concat plot_dir plot_file_name in
    let script_file = Filename.concat plot_dir (full_name ^ ".plot") in
    let oc_plot = open_out plot_file in
    let oc_script = open_out script_file in
    cpt := 0;
    dump_header oc_script plot_file full_name;
    StringMap.iter (fun run_name res ->
      StringMap.iter (fun fun_name (list, res) ->
        let data_name = Printf.sprintf "%s.%s.%s" run_name full_name fun_name in
        (* Dump data in data file *)
        Printf.fprintf oc_plot "# %s [%i] \n" data_name (List.length list);
        List.iter (fun (i, f) -> Printf.fprintf oc_plot "%i %f\n" i f) (List.rev list);
        Printf.fprintf oc_plot "\n\n";
        (* Command to plot the data *)
        if (!cpt = 0)
        then
          Printf.fprintf oc_script
            " index %i ls %i title %S, \"\" index %i using 1:(%f*$1 + %f) ls %i w l title %S"
            !cpt (!cpt + 1) data_name !cpt
            res.Measurements.mean_value
            res.Measurements.constant
            (!cpt + 1) (data_name ^ " calculated")
        else
          Printf.fprintf oc_script
            ", \"\" index %i ls %i title %S, \"\" index %i using 1:(%f*$1 + %f) ls %i w l title %S"
            !cpt (!cpt + 1) data_name !cpt
            res.Measurements.mean_value
            res.Measurements.constant
            (!cpt + 1) (data_name ^ " calculated");
        incr cpt
      ) res
    ) res;
    close_out oc_script;
    close_out oc_plot;
    script_file::list
  ) data []

let open_plot list_file =
  if !Arg_opt.output_png
  then Printf.printf "Generating %i png(s)\n%!" (List.length list_file)
  else Printf.printf "Opening %i graph(s)\n%!" (List.length list_file);
  let command_list = List.map (fun file ->
    "gnuplot", [ Command.A "-persist"; Command.A file ], None) list_file in
  List.iteri (fun i c ->
    let cmd_str = Command.command_to_string c in
    let file = Filename.chop_extension (List.nth list_file i) in
    if !Arg_opt.output_png
    then Printf.printf "Generating %S\n%!" (file ^ ".png")
    else Printf.printf "Runnning %S\n%!" cmd_str;
    let output = Command.run_command ~use_path:true c in
    match output with
    | None -> ()
    | Some s -> Printf.eprintf "%s" s
  ) command_list

let plot_subcommand () =
  let selection = ref [] in
  let plot () =
    let selection = List.rev !selection in
    if (List.length selection) >= 1
    then
      let first_arg = List.hd selection in
      let bench_name, group_name = 
        if String.contains first_arg '.' 
        then
          let index = String.rindex first_arg '.' in
          let len_left = (String.length first_arg) - index - 1 in
          String.sub first_arg 0 index, 
          if len_left > 0 then 
            Some (String.sub first_arg (index + 1) ((String.length first_arg) - index - 1))
          else None
        else first_arg, None in
      let run_selection = List.tl selection in
      let result_map = get_results run_selection in
      let by_benchs = get_benchs result_map bench_name group_name !Arg_opt.with_fun in
      if (StringMap.cardinal by_benchs) <> 0
      then
        let list_file = dump_plot_data bench_name by_benchs in
        open_plot list_file
      else Printf.printf "Can't find %s bench\n" first_arg
    else Printf.printf "You need to specify a bench\n"
  in
  Arg_opt.with_fun_arg @
  Arg_opt.output_png_arg,
  (fun s -> selection := s :: !selection),
  "bench_name<.group_name> [<run_name>]\n\
   if no runs name is provided, plot for all run, otherwise plot the data on the given runs",
  plot

let doall_subcommand () =
  let do_all name bin_dir extra_dir default_dir_flag output_dir selected_sets compile_arg rc =
    do_clean ();
    do_init ~with_default_benchmarks:!Arg_opt.with_default_benchmarks
      name extra_dir bin_dir default_dir_flag;
    let _failed_build = do_build compile_arg in
    Utils.unlock ();
    do_run output_dir rc;
    Utils.unlock ();
    do_results [name] !Arg_opt.allocations selected_sets !Arg_opt.more_info
  in
  let args = ref [] in
  Arg_opt.bin_dir_arg @
  Arg_opt.extra_dir_arg @
  Arg_opt.default_dir_arg @
  Arg_opt.run_config_arg @
  Arg_opt.output_dir_arg @
  Arg_opt.selected_sets_arg @
  Arg_opt.more_info_arg @
  Arg_opt.allocations_arg @
  Arg_opt.without_defualt_bench_arg @
  compiler_arg_opt,
  (fun s -> args := s :: !args),
  "[<args>] <name>\n\
   clean, initialize the .operf directory.\n",
  (fun () ->
     match !args with
     | [name] ->
       let selected_set = Arg_opt.get_selected_sets () in
       let rc = Arg_opt.make_run_config selected_set in
       do_all name !Arg_opt.bin_dir !Arg_opt.extra_dir !Arg_opt.default_dir_flag !Arg_opt.output_dir selected_set !compile_arg rc
     | _ -> failwith "wrong number of arguments, expected: <name>")

let subcommands =
  [ "init", init_subcommand;
    "build", build_subcommand;
    "list", list_subcommand;
    "run", run_subcommand;
    "clean", clean_subcommand;
    "results", results_subcommand;
    "compare", compare_subcommand;
    "plot", plot_subcommand;
    "doall", doall_subcommand;
    "check", check_subcommand ]

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
  | Cannot_find_compiler ->
    Format.fprintf ppf "Couldn't find a compiler in the path"

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

let print_utils_error ppf = function
  | Error_home str ->
    Format.fprintf ppf "%s" str
  | Already_locked ->
    Format.fprintf ppf "%s is already use by another instance of operf-micro"
      Utils.operf_default_dir

let print_measurements_error ppf = function
  | Measurements.Missing_field s ->
     Format.fprintf ppf "Missing file field %s" s

let print_errors ppf = function
  | Error e -> print_error ppf e
  | Benchmark.Error e -> print_error_benchmark ppf e
  | Detect_config.Error e -> print_detect_config_error ppf e
  | Utils.Error e -> print_utils_error ppf e
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
        run ();
        if subcommand_name <> "init" then Utils.unlock ()
      with
      | Arg.Bad s
      | Arg.Help s ->
        print_endline s;
      | e ->
        Format.eprintf "Error: %a@." print_errors e;
        exit 1
