
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
