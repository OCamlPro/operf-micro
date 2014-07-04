
type file = string
type directory = string
type library = string

type benchmark =
  { bench_path : directory;
    files : file list;
    link: library list }

let empty_benchmark bench_path =
  { bench_path;
    files = [];
    link = [] }

let default_benchmark bench_path =
  { (empty_benchmark bench_path)
    with files = ["benchmark.ml"] }

type context =
  { stdlib_path : directory;
    operf_files_path : directory }

type command_part =
  | F of file
  | D of directory
  | A of string

type command = command_part list

let opt_libraries c b =
  List.map (fun l ->
      F (Filename.concat c.stdlib_path (l ^ ".cmxa")))
    b.link

let byte_libraries c b =
  List.map (fun l ->
      F (Filename.concat c.stdlib_path (l ^ ".cma")))
    b.link

let operf_files c l =
  List.map (fun f ->
      F (Filename.concat c.operf_files_path f))
    l

let bench_files b =
  List.map (fun f ->
      F (Filename.concat b.bench_path f))
    b.files

let benchmark_command b =
  F (Filename.concat b.bench_path "benchmark.native")

let list_function_command b =
  let tmp = Filename.temp_file "list_function" "" in
  [ benchmark_command b; A "-o"; F tmp; A "--raw-list" ], tmp

let ocamlopt_command c b =
  [ F "ocamlopt"; A "-o"; F "benchmark.native"] @
  (operf_files c [ "cycles.c" ]) @
  opt_libraries c b @
  (operf_files c [ "micro_bench_types.mli"; "micro_bench_types.ml" ]) @
  bench_files b @
  (operf_files c [ "time_stamp_counter.ml"; "micro_bench_run.ml" ])

let ocamlc_command c b =
  [ F "ocamlc"; A "-custom"; A "-o"; F "benchmark.byte"] @
  (operf_files c [ "cycles.c" ]) @
  byte_libraries c b @
  (operf_files c [ "micro_bench_types.mli"; "micro_bench_types.ml" ]) @
  bench_files b @
  (operf_files c [ "time_stamp_counter.ml"; "micro_bench_run.ml" ])

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

let command_to_string l =
  let l =
    List.map (function
        | F s -> Filename.quote s
        | D d -> Filename.quote d
        | A a -> a)
      l in
  String.concat " " l

let config_operf_path = "/home/chambart/OcamlPro/git/operf-micro/"
let config_stdlib_path = "/home/chambart/.opam/base/lib/ocaml"


type error =
  | Parse_error of Loc.t
  | Missing_file of file

exception Error of error

let print_error ppf = function
  | Parse_error loc ->
    Format.fprintf ppf "parse error %a" Loc.print_loc loc
  | Missing_file file ->
    Format.fprintf ppf "Missing file %s" file

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
    let channel = open_in file_name in
    let lexbuf = Lexing.from_channel channel in
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      { pos with Lexing.pos_fname = file_name };
    let result =
      try
        Parser.file Lexer.token lexbuf
      with
      | Parsing.Parse_error ->
        let loc = Loc.curr lexbuf in
        raise (Error (Parse_error loc))
      | e -> raise e
    in
    Some (parse_file d result)

let rec filter_map f = function
  | [] -> []
  | h :: t ->
    let t = filter_map f t in
    match f h with
    | None -> t
    | Some h -> h :: t

let load_build_descrs c =
  let build_dirs = subdirectories c.operf_files_path in
  let build_descrs = filter_map load_build_descr build_dirs in
  List.iter check_build_descr build_descrs;
  build_descrs

let input_all =
  let len = 1024 in
  let buf = String.create len in
  let rec aux ic b =
    let n = input ic buf 0 1024 in
    Buffer.add_substring b buf 0 n;
    if n = 1024
    then aux ic b
  in
  fun ic ->
    let b = Buffer.create 100 in
    aux ic b;
    Buffer.contents b

let run_and_read (c, out_file) =
  let c = command_to_string c in
  let r = Sys.command c in
  if r <> 0
  then None
  else
    let input = open_in out_file in
    let s = input_all input in
    let () = close_in input in
    Some s

let list_benchmarks b =
  List.map (fun b ->
      let c = list_function_command b in
      b, run_and_read c)
    b

let () =
  let operf_files_path =
    if Array.length Sys.argv > 1
    then Sys.argv.(1)
    else Filename.concat config_operf_path "benchmarks"
  in
  let c =
    { stdlib_path = config_stdlib_path;
      operf_files_path = operf_files_path } in
  let b =
    try load_build_descrs c
    with Error e ->
      Format.eprintf "Error: %a@." print_error e;
      exit 1
  in
  let () =
    List.iter (fun b ->
        Printf.printf "%s\n%!" (command_to_string (ocamlopt_command c b)))
      b
  in
  let l = list_benchmarks b in
  List.iter (fun (b, f) ->
      match f with
      | None ->
        Printf.printf "%s: FAILED\n%!" b.bench_path
      | Some f ->
        Printf.printf "%s:\n%s\n\n%!"
          b.bench_path
          f) l
