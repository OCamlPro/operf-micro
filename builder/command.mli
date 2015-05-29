open Utils

type file = string
type directory = string

type command_part =
  | IF of file (** input file *)
  | OF of file (** output file *)
  | ID of directory (** input directory *)
  | OD of directory (** output directory *)
  | A of string (** raw string *)

type command = file * command_part list * directory option

val command_to_string : command -> string

val run_command : ?use_path:bool -> command -> string option
(* returns stdout *)

val run_and_read : (command * file) -> string option

val run_and_read_lines : (command * file) -> string list option

val copy_file : file -> file -> unit

val recursive_extra_copy : file -> file -> unit

val recursive_copy : file -> file -> unit

val write_file : file -> (Format.formatter -> unit) -> unit

val read_file : file -> string option

val get_and_make_subdir : (unit -> (directory, directory) result) ->
  string -> (directory, string) result

val remove : directory -> unit

val subdirectories : directory -> directory list

val debug : bool
