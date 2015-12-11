
module StringMap = Map.Make(String)

type file = string
type directory = string

type error =
  | Error_home of string
  | Already_locked

exception Error of error

let stringmap_of_list l =
  List.fold_left (fun map (key, value) ->
    StringMap.add key value map)
    StringMap.empty l

let get_opt v f =
  match v with
  | None -> f ()
  | Some v -> v

let may_map f = function
  | None -> None
  | Some v -> Some (f v)

let rec filter_map f = function
  | [] -> []
  | h :: t ->
    let t = filter_map f t in
    match f h with
    | None -> t
    | Some h -> h :: t

let ( |> ) x f = f x
let ( @@ ) f x = f x

type ('l, 'r) result =
  | Ok of 'l
  | Err of 'r

let split c s =
  let aux pos =
    let i = String.index_from s pos c in
    i, String.sub s pos (i - pos)
  in
  let rec loop pos acc =
    let r =
      try Some (aux pos) with
      | Not_found -> None
    in
    match r with
    | None ->
      let len = String.length s in
      let last_line = String.sub s pos (len - pos) in
      last_line :: acc
    | Some (pos, line) -> loop (pos+1) ( line :: acc )
  in
  List.rev (loop 0 [])

let lines s = split '\n' s

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

let mk_dir path =
  remove path;
  Unix.mkdir path 0o777

let make_directory dir =
  try Unix.mkdir dir 0o755
  with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let init_operf_default_dir () =
  let res = home_directory () in
  match res with
  | Ok home_dir ->
    let cache_dir = Filename.concat home_dir ".cache/operf/micro" in
    mk_dir cache_dir
  | Err msg -> raise (Error (Error_home msg))

let operf_default_dir =
  let res = home_directory () in
  match res with
  | Ok home_dir ->
    let cache_dir = Filename.concat home_dir ".cache/operf/micro" in
    cache_dir
  | Err msg -> raise (Error (Error_home msg))

let lock_path = Filename.concat operf_default_dir ".lock"

let mk_lock () =
  close_out (open_out lock_path)

let lock () =
  if Sys.file_exists lock_path
  then raise (Error Already_locked)
  else mk_lock ()

let unlock () =
  remove lock_path
