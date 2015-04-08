
module StringMap = Map.Make(String)

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
