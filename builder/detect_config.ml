open Utils
open Command

type config =
  { name : string;
    ocaml_bin_dir : directory;
    operf_root_dir : directory; }

type executable =
  | Bytecode
  | Native

type program = file * executable

type timestamp = string

type context =
  { config : config;
    mutable timestamp : timestamp option;
    stdlib_path : directory;
    ocamlrun_path : file option;
    ocamlc_path : program option;
    ocamlopt_path : program option;
    operf_files_path : directory;
    operf_files_build_path : directory }

type error =
  | Parse_error of Loc.t
  | Missing_config_field of file * string
  | Duplicate_config_field of file * string
  | No_config_file
  | Not_ocaml_compiler_dir
  | No_compiler
  | No_timestamp
  | Missing_directory of directory

exception Error of error

type source_file =
  | C
  | Mli
  | Ml

let operf_source_files =
  [ C,   "cycles";
    Mli, "micro_bench_types";
    Ml,  "micro_bench_types";
    Ml,  "time_stamp_counter";
    Ml,  "micro_bench_run" ]

let source_filename = function
  | C, f -> f ^ ".c"
  | Mli, f -> f ^ ".mli"
  | Ml, f -> f ^ ".ml"

let dest_filename ~native = function
  | C, f -> f ^ ".o"
  | Mli, f -> f ^ ".cmi"
  | Ml, f ->
    if native
    then f ^ ".cmx"
    else f ^ ".cmo"

let run_directory = Sys.getcwd ()
let executable_directory = Filename.dirname Sys.executable_name

let load_config_file (file_name:file) =
  let channel = open_in file_name in
  let lexbuf = Lexing.from_channel channel in
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <-
    { pos with Lexing.pos_fname = file_name };
  try
    Parser.file Lexer.token lexbuf
  with
  | Parsing.Parse_error ->
    let loc = Loc.curr lexbuf in
    raise (Error (Parse_error loc))
  | e -> raise e

let rec find_ancessor f path =
  if f path
  then Some path
  else
    let parent = Filename.dirname path in
    if parent = path
    then None (* root directory *)
    else find_ancessor f parent

let is_ocaml_building_directory path =
  let check_subdir d =
    let d' = Filename.concat path d in
    Sys.file_exists d' && Sys.is_directory d' in
  let check_file f =
    let f' = Filename.concat path f in
    Sys.file_exists f' && not (Sys.is_directory f') in
  Sys.is_directory path &&
  check_subdir "stdlib" &&
  check_subdir "byterun" &&
  check_file "configure" &&
  check_file "Makefile"

let find_ocaml_root_directory ?(path=run_directory) () =
  find_ancessor is_ocaml_building_directory path

let make_directory dir =
  try Unix.mkdir dir 0o777
  with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let home_directory () =
  try
    let d = Sys.getenv "HOME" in
    if Sys.file_exists d
    then
      if Sys.is_directory d
      then Ok d
      else Err (d ^ " is not a directory")
    else Err ("directory " ^ d ^ " doesn't exists")
  with Not_found -> Err "Environment variable HOME is not set"

let operf_home_directory () =
  get_and_make_subdir
    (fun () -> get_and_make_subdir home_directory ".operf")
    "micro"

let named_directory context =
  get_and_make_subdir operf_home_directory
    context.config.name

let save_directory context =
  match context.timestamp with
  | None -> raise (Error No_timestamp)
  | Some timestamp ->
    get_and_make_subdir (fun () -> named_directory context)
      timestamp

let operf_subdir path =
  Filename.concat path ".operf"
let micro_subdir path =
  Filename.concat (operf_subdir path) "micro"
let benchmarks_subdir path =
  Filename.concat (micro_subdir path) "benchmarks"
let config_file_name path =
  Filename.concat (micro_subdir path) "config"

let contains_operf_root_directory path =
  Sys.is_directory path &&
  Sys.file_exists (operf_subdir path) &&
  Sys.is_directory (operf_subdir path) &&
  Sys.file_exists (micro_subdir path) &&
  Sys.is_directory (micro_subdir path) &&
  Sys.file_exists (config_file_name path) &&
  not (Sys.is_directory (config_file_name path))

let find_operf_directory ?(path=run_directory) () =
  find_ancessor contains_operf_root_directory path

type config_file' =
  { name' : string option;
    ocaml_bin_dir' : directory option }

let empty_config_file' =
  { name' = None; ocaml_bin_dir' = None }


let micro_directory config =
  Filename.concat config.operf_root_dir "micro"
let build_directory config =
  Filename.concat (micro_directory config) "build"
let timestamp_file config =
  Filename.concat (micro_directory config) "timestamp"

let timestamp t =
  let tm = Unix.localtime t in
  let year = tm.Unix.tm_year + 1900 in
  Printf.sprintf "%i-%.2i-%.2i_%.2i-%.2i-%.2i"
    year
    tm.Unix.tm_mon
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

let write_timestamp context =
  let time = timestamp (Unix.time ()) in
  write_file (timestamp_file context.config) (fun ppf -> Format.pp_print_string ppf time)

let load_timestamp config =
  read_file (timestamp_file config)

let check_config_file dir filename = function
  | { name' = Some name; ocaml_bin_dir' = Some ocaml_bin_dir } ->
    { name;
      ocaml_bin_dir;
      operf_root_dir = operf_subdir dir; }
  | { name' = None; _ } ->
    raise (Error (Missing_config_field (filename, "name")))
  | { ocaml_bin_dir' = None; _ } ->
    raise (Error (Missing_config_field (filename, "ocaml_bin_dir")))

let config_to_file { operf_root_dir = _; ocaml_bin_dir; name } =
  let dict = [] in
  let dict = ("ocaml_bin_dir", Files.String ocaml_bin_dir) :: dict in
  let dict = ("name", Files.String name) :: dict in
  Files.Dict dict

let string = function
  | Files.String s -> s
  | _ -> failwith "parse error"

let parse_operf_config_file dir filename v =
  let ocaml_bin_dir v e =
    match e.ocaml_bin_dir' with
    | None -> { e with ocaml_bin_dir' = Some (string v) }
    | Some _ -> raise (Error (Duplicate_config_field (filename, "ocaml_bin_dir"))) in
  let config_name v e =
    match e.name' with
    | None -> { e with name' = Some (string v) }
    | Some _ -> raise (Error (Duplicate_config_field (filename, "name"))) in

  let fields e = function
    | "name", v -> config_name v e
    | "ocaml_bin_dir", v -> ocaml_bin_dir v e
    | s, _ -> failwith ("Unknown field: " ^ s)
  in
  let r =
    match v with
    | Files.Dict d ->
      List.fold_left fields empty_config_file' d
    | _ -> failwith ("parse error")
  in
  check_config_file dir filename r

let load_operf_config_file ?path () =
  match find_operf_directory ?path () with
  | None ->
    raise (Error (No_config_file))
  | Some path ->
    let c = config_file_name path in
    parse_operf_config_file path c (load_config_file c)

let copy_benchmark_files src_root dst_root =
  let src = Filename.concat src_root "benchmarks" in
  let dst = benchmarks_subdir dst_root in
  recursive_copy src dst

let copy_base_files src_root dst_root =
  List.iter (fun f ->
      let f = source_filename f in
      let src = Filename.concat src_root f in
      let dst = Filename.concat (micro_subdir dst_root) f in
      copy_file src dst)
    operf_source_files

let share_directory = Filename.concat Static_config.prefix "share"

let is_data_directory path =
  let check_subdir d =
    let d' = Filename.concat path d in
    Sys.file_exists d' && Sys.is_directory d' in
  let check_file f =
    let f' = Filename.concat path f in
    Sys.file_exists f' && not (Sys.is_directory f') in
  Sys.is_directory path &&
  check_subdir "benchmarks" &&
  List.for_all (fun f -> check_file (source_filename f))
    operf_source_files

let data_directory =
  match find_ancessor is_data_directory executable_directory with
  | None -> Filename.concat share_directory Static_config.name
  | Some s -> s

let write_initialise root_dir config =
  make_directory (operf_subdir root_dir);
  make_directory (micro_subdir root_dir);
  write_file (config_file_name root_dir)
    (fun ppf ->
       Files.print ppf (config_to_file config);
       Format.pp_print_newline ppf ());
  copy_benchmark_files data_directory root_dir;
  copy_base_files data_directory root_dir

let initialize_in_compiler_dir ?path name =
  let root_dir =
    match find_ocaml_root_directory ?path () with
    | None ->
      raise (Error Not_ocaml_compiler_dir)
    | Some d -> d
  in
  let config =
    { name = name;
      ocaml_bin_dir = root_dir;
      operf_root_dir = operf_subdir root_dir } in
  write_initialise root_dir config;
  root_dir

let initialize_with_bin_dir ?(path=run_directory) name ocaml_bin_dir =
  let root_dir = path in
  let ocaml_bin_dir =
    if Sys.file_exists ocaml_bin_dir
    then if Sys.is_directory ocaml_bin_dir
      then ocaml_bin_dir
      else Filename.dirname ocaml_bin_dir
    else raise (Error (Missing_directory ocaml_bin_dir))
  in
  let config =
    { name = name;
      ocaml_bin_dir;
      operf_root_dir = operf_subdir root_dir } in
  write_initialise root_dir config;
  root_dir

let bin_suffix =
  if Sys.os_type = "Unix"
  then ""
  else ".exe"

let find_bin config name =
  let bin s = Filename.concat config.ocaml_bin_dir (s^bin_suffix) in
  let opt = bin (name^".opt") in
  if Sys.file_exists opt
  then Some (opt, Native)
  else
    let byte = bin name in
    if Sys.file_exists byte
    then Some (byte, Bytecode)
    else None

let find_ocamlc config =
  find_bin config "ocamlc"

let find_ocamlopt config =
  find_bin config "ocamlopt"

let find_ocamlrun config =
  let bin_name = "ocamlrun"^bin_suffix in
  let bin d =
    Filename.concat
      (Filename.concat config.ocaml_bin_dir d)
      bin_name in
  let root_ocamlrun = Filename.concat config.ocaml_bin_dir bin_name in
  if Sys.file_exists root_ocamlrun
  then Some root_ocamlrun
  else
  if Sys.file_exists (bin "byterun")
  then Some (bin "byterun")
  else
  if Sys.file_exists (bin "boot")
  then Some (bin "boot")
  else None

let make_command ocamlrun (file, kind) command dir =
  match kind with
  | Native ->
    Some (file, command, dir)
  | Bytecode ->
    match ocamlrun with
    | None -> None
    | Some ocamlrun -> Some (ocamlrun, IF file :: command, dir)

let stdlib_path ocamlrun config =
  match find_ocamlc config, find_ocamlopt config with
  | None, None -> None
  | Some compiler, _
  | None, Some compiler ->
    match make_command ocamlrun compiler [A "-where"] None with
    | None -> None
    | Some c ->
      match run_command c with
      | None -> None
      | Some s -> Some (String.trim s)

let load_context config =
  let ocamlc_path = find_ocamlc config in
  let ocamlopt_path = find_ocamlopt config in
  let ocamlrun_path = find_ocamlrun config in
  match stdlib_path ocamlrun_path config with
  | None ->
    raise (Error No_compiler)
  | Some stdlib_path ->
    { config;
      stdlib_path;
      ocamlrun_path;
      ocamlc_path;
      ocamlopt_path;
      operf_files_path = micro_directory config;
      operf_files_build_path = build_directory config;
      timestamp = load_timestamp config }

let prepare_build context =
  let dir = build_directory context.config in
  write_timestamp context;
  context.timestamp <- load_timestamp context.config;
  make_directory dir

let ocamlopt_command context args dir =
  match context.ocamlopt_path with
  | None -> None
  | Some compiler ->
    make_command context.ocamlrun_path compiler args dir

let ocamlc_command context args dir =
  match context.ocamlc_path with
  | None -> None
  | Some compiler ->
    make_command context.ocamlrun_path compiler args dir

let prepare_command context (file, kind) args dir =
  make_command context.ocamlrun_path (file, kind) args dir
