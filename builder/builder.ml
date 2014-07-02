
type file = string
type directory = string
type library = string

type benchmark =
  { files : file list;
    link: library list }

let empty_benchmark =
  { files = [];
    link = [] }

let default_benchmark =
  { empty_benchmark
    with files = ["benchmark.ml"] }

type context =
  { bench_path : directory;
    stdlib_path : directory;
    operf_files_path : directory }

let opt_libraries c b =
  List.map (fun l ->
      Filename.concat c.stdlib_path (l ^ ".cmxa"))
    b.link

let byte_libraries c b =
  List.map (fun l ->
      Filename.concat c.stdlib_path (l ^ ".cma"))
    b.link

let operf_files c l =
  List.map (fun f ->
      Filename.concat c.operf_files_path f)
    l

let bench_files c b =
  List.map (fun f ->
      Filename.concat c.bench_path f)
    b.files

let ocamlopt_command c b =
  [ "ocamlopt"; "-o"; "benchmark.native"] @
  (operf_files c [ "cycles.c" ]) @
  opt_libraries c b @
  (operf_files c [ "micro_bench_types.mli"; "micro_bench_types.ml" ]) @
  bench_files c b @
  (operf_files c [ "time_stamp_counter.ml"; "micro_bench_run.ml" ])

let ocamlc_command c b =
  [ "ocamlc"; "-custom"; "-o"; "benchmark.byte"] @
  (operf_files c [ "cycles.c" ]) @
  byte_libraries c b @
  (operf_files c [ "micro_bench_types.mli"; "micro_bench_types.ml" ]) @
  bench_files c b @
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

let parse_file v =
  let fields e = function
    | "link", v -> link v e
    | "files", v -> files v e
    | s, _ -> failwith ("Unknown field: " ^ s)
  in
  match v with
  | Files.Dict d ->
    List.fold_left fields empty_benchmark d
  | _ -> failwith ("parse error")

let parse_build_file file =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  parse_file (Parser.file Lexer.token lexbuf)

let () =
  let build_file = Sys.argv.(1) in
  let b = parse_build_file build_file in
  let c =
    { bench_path = Filename.basename build_file;
      stdlib_path = "/.../stdlib/";
      operf_files_path = "/.../operf_files/" }
  in
  Printf.printf "%s\n%!" (String.concat " " (ocamlopt_command c b))
