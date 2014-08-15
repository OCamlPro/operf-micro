
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

(* module Core_compat = struct *)

(*   type ('l, 'r) r = *)
(*       ('l, 'r) result = *)
(*     | Ok of 'l *)
(*     | Err of 'r *)

(*   module Or_error = struct *)
(*     type 'a t = ('a, string) r *)
(*     let error_string s = Err s *)
(*   end *)

(*   exception Finish *)

(*   module Array = struct *)
(*     include ArrayLabels *)

(*     let for_all2_exn ~f a1 a2 = *)
(*       if Array.length a1 <> Array.length a2 *)
(*       then raise (Invalid_argument "array_for_all2"); *)
(*       try *)
(*         for i = 0 to Array.length a1 - 1 do *)
(*           if not (f a1.(i) a2.(i)) *)
(*           then raise Finish *)
(*         done; *)
(*         true *)
(*       with Finish -> false *)

(*     let create ~len v = create len v *)

(*     let exists ~f a = *)
(*       try *)
(*         for i = 0 to Array.length a - 1 do *)
(*           if f a.(i) then raise Finish *)
(*         done; *)
(*         false *)
(*       with Finish -> true *)

(*     let foldi ~f ~init a = *)
(*       let r = ref init in *)
(*       for i = 0 to Array.length a do *)
(*         r := f i !r a.(i); *)
(*       done; *)
(*       !r *)
(*   end *)

(*   module Float = struct *)
(*     type t = float *)
(*     let (<=) (x:float) y = x <= y *)
(*     let abs = abs_float *)
(*     let is_nan f = *)
(*       match classify_float f with *)
(*       | FP_nan -> true *)
(*       | _ -> false *)
(*     let neg_infinity = (-.infinity) *)
(*   end *)

(* end *)
