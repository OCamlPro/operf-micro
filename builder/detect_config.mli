open Utils
open Command

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

val load_config_file : file -> Files.file

val find_ocaml_root_directory : ?path:directory -> unit -> directory option
val find_operf_directory : ?path:directory -> unit -> directory option

val load_operf_config_file : ?path:directory -> unit -> config

val initialize_in_compiler_dir :
  ?path:directory ->
  ?with_default_benchmarks:bool ->
  string -> directory list -> directory
val initialize_with_bin_dir :
  ?path:directory ->
  ?with_default_benchmarks:bool ->
  string -> directory list -> directory -> directory

val load_context : config -> context

val prepare_build : context -> unit

val ocamlopt_command : context -> command_part list -> directory option -> command option
val ocamlc_command : context -> command_part list -> directory option -> command option

val prepare_command : context -> program ->
  command_part list -> directory option -> command option

val find_ocaml_binary_path : unit -> directory option

type source_file =
  | C
  | Mli
  | Ml

val operf_source_files : (source_file * string) list

val source_filename : source_file * string -> file
val dest_filename : native:bool -> source_file * string -> file

val write_timestamp : context -> unit


val operf_home_directory : unit -> (directory, string) result
val save_directory : context -> (directory, string) result
