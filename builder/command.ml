open Utils

let debug = false

type file = string
type directory = string

type command_part =
  | IF of file (** input file *)
  | OF of file (** output file *)
  | ID of directory (** input directory *)
  | OD of directory (** output directory *)
  | A of string (** raw string *)

type command = file * command_part list * directory option

let command_to_string (prog, args, _exec_dir:command) =
  let prog = Filename.quote prog in
  let args =
    List.map (function
        | IF s -> Filename.quote s
        | OF s -> Filename.quote s
        | ID d -> Filename.quote d
        | OD d -> Filename.quote d
        | A a -> a)
      args in
  String.concat " " (prog::args)

let quote_command (prog, args, _exec_dir:command) =
  let args =
    List.map (function
        | IF s -> s
        | OF s -> s
        | ID d -> d
        | OD d -> d
        | A a -> a)
      args in
  prog, Array.of_list (prog :: args)

let check_file f =
  if not (Sys.file_exists f)
  then (Printf.eprintf "Missing file %s\n" f;
        false)
  else
  if Sys.is_directory f
  then (Printf.eprintf "%s is a directory\n" f;
        false)
  else true

let check_directory f =
  if not (Sys.file_exists f)
  then (Printf.eprintf "Missing directory %s\n" f;
        false)
  else
  if not (Sys.is_directory f)
  then (Printf.eprintf "%s is not a directory\n" f;
        false)
  else true

let check_command_in (_prog, args, exec_dir) =
  let aux = function
    | IF f -> check_file f
    | ID f -> check_directory f
    | _ -> true
  in
  let exec_dir =
    match exec_dir with
    | None -> true
    | Some d -> check_directory d in
  exec_dir && List.for_all aux args

let check_command_out (_prog, args, _exec_dir) =
  let aux = function
    | OF f -> check_file f
    | OD f -> check_directory f
    | _ -> true
  in
  List.for_all aux args

let input_all =
  let len = 1024 in
  let buf = Bytes.create len in
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

let input_all_file name =
  let ic = open_in name in
  let s = input_all ic in
  close_in ic;
  s

let make_tmp_file suffix =
  let name = Filename.temp_file "" suffix in
  name, Unix.openfile name [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o644

let run_command ?(use_path=false) (c:command) =
  let (prog, _,_) = c in
  if (not use_path) && (not (Sys.file_exists prog))
  then (Printf.eprintf "Missing program %s\n" prog;
        None)
  else
  if not (check_command_in c)
  then None
  else begin
    if debug
    then Printf.eprintf "Running %s\n\n%!" (command_to_string c);
    let prog, args = quote_command c in
    let stdout_name, fd_stdout = make_tmp_file ".out" in
    let stderr_name, fd_stderr = make_tmp_file ".err" in
    let () =
      let (_,_,exec_dir) = c in
      match exec_dir with
      | None -> ()
      | Some d -> Sys.chdir d
    in
    let pid = Unix.create_process prog args Unix.stdin fd_stdout fd_stderr in
    Unix.close fd_stdout;
    Unix.close fd_stderr;
    let rpid, status = Unix.waitpid [] pid in
    assert(rpid = pid);
    let read_stderr () = input_all_file stderr_name in
    match status with
    | Unix.WEXITED 0 ->
      if check_command_out c
      then Some (input_all_file stdout_name)
      else None
    | Unix.WEXITED n ->
      let () = Printf.eprintf "Command return code %i:\n  %s\n%s\n%!"
          n (command_to_string c) (read_stderr ()) in
      None
    | Unix.WSIGNALED n ->
      let () = Printf.eprintf "Command killed with signal %i:\n  %s\n%s\n%!"
          n (command_to_string c) (read_stderr ()) in
      None
    | Unix.WSTOPPED _n ->
      assert false
  end

let run_and_read (c, out_file) =
  match run_command c with
  | None -> None
  | Some _stdout ->
    let input = open_in out_file in
    let s = input_all input in
    let () = close_in input in
    let () = Sys.remove out_file in
    Some s

let run_and_read_lines c =
  match run_and_read c with
  | None -> None
  | Some f -> Some (lines f)

let copy_file =
  let buffer_size = 8192 in
  let buffer = Bytes.create buffer_size in

  fun input_name output_name ->
    let fd_in = Unix.openfile input_name [Unix.O_RDONLY] 0 in
    let fd_out = Unix.openfile output_name [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
    let rec loop () =
      let read = Unix.read fd_in buffer 0 buffer_size in
      if read <> 0
      then
        let _w : int = Unix.write fd_out buffer 0 read in
        loop ()
    in
    let close () =
      Unix.close fd_in;
      Unix.close fd_out
    in
    (try loop ()
     with e -> close (); raise e);
    close ()

let rec recursive_copy src dst =
  if Sys.file_exists dst
  then failwith ("file already exists: " ^ dst)
  else
    let stat = Unix.stat src in
    match stat.Unix.st_kind with
    | Unix.S_REG ->
      copy_file src dst
    | Unix.S_DIR ->
      Unix.mkdir dst 0o777;
      let handle = Unix.opendir src in
      begin try
          while true do
            let filename = Unix.readdir handle in
            match filename with
            | "." | ".." -> ()
            | _ ->
              let src = Filename.concat src filename in
              let dst = Filename.concat dst filename in
              recursive_copy src dst
          done
        with End_of_file -> ()
      end;
      Unix.closedir handle
    | _ ->
      Printf.eprintf "ignored file: %s@." src

let is_benchmark dir =
  let handle = Unix.opendir dir in
  let rec contains_bench_files handle =
    try
      let filename = Unix.readdir handle in
      match filename with
      | "benchmark.ml" | "benchmark.build" -> true
      | _ -> contains_bench_files handle
    with End_of_file -> false in
  let res = contains_bench_files handle in
  Unix.closedir handle;
  res

let rec recursive_extra_copy src dst =
  if not (Sys.file_exists src && Sys.is_directory src)
  then failwith ("Directory doen't exist: " ^ src)
  else
    if (is_benchmark src)
    then
      (* If the path is relative, convert it to an absolute one
         this allows to handle '.' for instance *)
      let abs_src =
        if Filename.is_relative src then
          Filename.concat (Sys.getcwd ()) src
        else
          src
      in
      let basename =
        let rec loop abs_src =
          let basename = Filename.basename abs_src in
          (* Chop trailing '.' *)
          if basename = Filename.current_dir_name then
            loop (Filename.dirname abs_src)
          else basename
        in
        loop abs_src
      in
      let dst = Filename.concat dst basename in
      recursive_copy abs_src dst
    else
      let handle = Unix.opendir src in
      begin try
          while true do
            let filename = Unix.readdir handle in
            match filename with
            | "." | ".." -> ()
            | _ ->
              let path = Filename.concat src filename in
              if Sys.is_directory path
              then recursive_extra_copy path dst
              else ()
          done
        with End_of_file -> ()
      end;
      Unix.closedir handle

let write_file filename f =
  let oc = open_out filename in
  let ppf = Format.formatter_of_out_channel oc in
  f ppf;
  Format.pp_print_flush ppf ();
  close_out oc

let read_file file =
  let stat = try Some (Unix.stat file) with _ -> None in
  match stat with
  | Some { Unix.st_kind = Unix.S_REG; _ } ->
    Some (input_all_file file)
  | _ -> None

let get_and_make_subdir f subdir =
  match f () with
  | Err e -> Err e
  | Ok d ->
    let sub = Filename.concat d subdir in
    if Sys.file_exists sub
    then
      if Sys.is_directory sub
      then Ok sub
      else Err (sub ^ " is not a directory")
    else
      try
        Unix.mkdir sub 0o777;
        Ok sub
      with
      | Unix.Unix_error (e,_,_) ->
        Err
          (Printf.sprintf "Error while mkdir %s: %s"
             sub (Unix.error_message e))

let rec remove file =
  if Sys.file_exists file
  then
    let stat = Unix.stat file in
    match stat.Unix.st_kind with
    | Unix.S_REG
    | Unix.S_LNK ->
       Unix.unlink file
    | Unix.S_DIR ->
       let handle = Unix.opendir file in
       begin try
           while true do
             let filename = Unix.readdir handle in
             match filename with
             | "." | ".." -> ()
             | _ ->
                remove (Filename.concat file filename)
           done
         with End_of_file -> ()
       end;
       Unix.closedir handle;
       begin try
           Unix.rmdir file
         with _ -> ()
       end
    | _ ->
       Printf.eprintf "ignored file: %s@." file

let subdirectories (d:directory) : directory list =
  let subdirectories =
    Array.to_list
      (Array.map (fun s -> Filename.concat d s)
         (Sys.readdir d)) in
  let subdirectories =
    List.filter Sys.is_directory subdirectories in
  subdirectories
