open Utils
open Command
open Detect_config
open Benchmark

let opt_libraries c b =
  List.map (fun l ->
      IF (Filename.concat c.stdlib_path (l ^ ".cmxa")))
    b.link

let byte_libraries c b =
  List.map (fun l ->
      IF (Filename.concat c.stdlib_path (l ^ ".cma")))
    b.link

let operf_files c l =
  List.map (fun f ->
      IF (Filename.concat c.operf_files_path f))
    l

let operf_built_files c l =
  List.map (fun f ->
      IF (Filename.concat c.operf_files_build_path f))
    l

let bench_files b =
  List.map (fun f ->
      IF (Filename.concat b.bench_path f))
    b.files

let opt_binary b =
  Filename.concat b.bench_path "benchmark.native"

let byte_binary b =
  Filename.concat b.bench_path "benchmark.byte"

let ocamlopt_command c other_args b =
  Detect_config.ocamlopt_command
    c
    ([ A "-g" ] @
     [ A "-o"; OF (opt_binary b) ] @
     (operf_built_files c [ "cycles.o" ]) @
     opt_libraries c b @
     [ A "-I"; ID c.operf_files_path ] @
     [ A "-I"; ID b.bench_path ] @
     other_args @
     (operf_built_files c [ "micro_bench_types.cmx" ]) @
     bench_files b @
     (operf_built_files c [ "time_stamp_counter.cmx"; "micro_bench_run.cmx" ]))
    None

let ocamlc_command c b =
  Detect_config.ocamlc_command
    c
    ([ A "-custom"; A "-o"; OF (byte_binary b) ] @
     (operf_built_files c [ "cycles.o" ]) @
     byte_libraries c b @
     [ A "-I"; ID c.operf_files_path ] @
     [ A "-I"; ID b.bench_path ] @
     (operf_built_files c [ "micro_bench_types.cmo" ]) @
     bench_files b @
     (operf_built_files c [ "time_stamp_counter.cmo"; "micro_bench_run.cmo" ]))
    None

let build_operf_file_command c native in_file =
  let comp_command =
    if native
    then Detect_config.ocamlopt_command c
    else Detect_config.ocamlc_command c
  in
  match in_file with
  | C, f ->
    (* circumvent problem with ocamlc ignoring -o with .c files *)
    let src = Filename.concat c.operf_files_path (source_filename in_file) in
    let dst = dest_filename ~native in_file in
    let dest_c_file = Filename.concat c.operf_files_build_path (f ^ ".c") in
    Command.copy_file src dest_c_file;
    comp_command
      [ A "-c";
        A "-I"; ID c.operf_files_build_path;
        A "-o"; OF (Filename.concat c.operf_files_build_path dst);
        IF dest_c_file ]
      (Some c.operf_files_build_path)
  | _ ->
    let src = source_filename in_file in
    let dst = dest_filename ~native in_file in
    comp_command
      [ A "-c";
        A "-I"; ID c.operf_files_build_path;
        A "-o"; OF (Filename.concat c.operf_files_build_path dst);
        IF (Filename.concat c.operf_files_path src) ]
      None

let build_operf_files c opt =
  let aux file =
    match build_operf_file_command c opt file with
    | None -> false
    | Some c ->
      run_command c <> None
  in
  List.for_all aux operf_source_files

let build_opt c b opt_arg =
  match ocamlopt_command c opt_arg b with
  | None -> false
  | Some command ->
    let r = run_command command in
    (r <> None && Sys.file_exists (opt_binary b))

let build_byte c b =
  match ocamlc_command c b with
  | None -> false
  | Some command ->
    let r = run_command command in
    (r <> None && Sys.file_exists (opt_binary b))

let build_benchmarks c l ocamlopt_arg =
  Printf.eprintf "building operf base files\n\n%!";
  let r_byte = build_operf_files c false in
  let r_opt = build_operf_files c true in
  if not (r_byte && r_opt)
  then
    (Printf.eprintf "couldn't build operf base files\n%!";
     exit 1)
  else
    let failed = ref false in
    List.iter (fun b ->
        if not (build_opt c b ocamlopt_arg)
        then begin
          Printf.eprintf "couldn't build %s\n%!" (opt_binary b);
          failed := true;
        end)
      l;
    !failed

let print_time_approximation rc b =
  let time_quota =
    match rc.time_quota with
    | None -> 10.0
    | Some t -> t in
  let different_values =
    match rc.different_values with
    | None -> float_of_int 1
    | Some t -> float_of_int t in
  let nbr_functions = float_of_int (Benchmark.benchmark_nbr_functions b) in
  let time = time_quota *. different_values *. nbr_functions in
  Printf.eprintf "Running %s benchmark (approx. %.1f secs)\n%!" b.bench_name time

let run_function_command context ~native rc b : (command * file) option =
  let cost_option =
    match rc.maximal_cost with
    | None -> []
    | Some Short -> []
    | Some Long -> [ A "--long" ]
    | Some Longer -> [ A "--longer" ] in
  let time_quota_option =
    match rc.time_quota with
    | None -> []
    | Some t -> [A "--time-quota"; A (string_of_float t)] in
  let different_values_option =
    match rc.different_values with
    | None -> []
    | Some t -> [A "--different-values"; A (string_of_int t)] in
  let stabilize_gc_option =
    if rc.stabilize_gc then
      [A "--stabilize-gc"]
    else
      []
  in
  let tmp = Filename.temp_file "result_" "" in
  let prog =
    if native
    then (benchmark_prog ~native b, Native)
    else (benchmark_prog ~native b, Bytecode)
  in
  print_time_approximation rc b;
  prepare_command context prog
    ([ A "-o"; OF tmp ] @
     cost_option @
     time_quota_option @
     different_values_option @
     stabilize_gc_option)
    None
  |> may_map (fun c -> c, tmp)

let run_benchmarks context rc l =
  filter_map (fun b ->
    let selected = match rc.selected_sets with
      | None -> true
      | Some l -> List.mem b.bench_name l in
    if selected
    then
      run_function_command context ~native:true rc b
      |> may_map (fun c ->
        match run_and_read c with
        | None -> b, []
        | Some r ->
          b, Measurements.read_measurement ~contents:r)
    else None)
    l

