
type file = string
type directory = string

type command_part =
  | IF of file (** input file *)
  | OF of file (** output file *)
  | ID of directory (** input directory *)
  | OD of directory (** output directory *)
  | A of string (** raw string *)

type command = file * command_part list * directory option

val run_command : command -> string option
(* returns stdout *)

val run_and_read : (command * file) -> string option

val run_and_read_lines : (command * file) -> string list option

val split : char -> string -> string list

val copy_file : file -> file -> unit

val recursive_copy : file -> file -> unit

val write_file : file -> (Format.formatter -> unit) -> unit

val read_file : file -> string option
