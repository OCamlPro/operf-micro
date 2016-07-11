
type 'a t = 'a option array

let length a = Array.length a

let make n = Array.make n None

let set (a:'a t) n (v:'a) = Array.set a n (Some v)

let get (a:'a t) n : 'a option = Array.get a n

let get_k (a:'a t) n none some =
  match Array.get a n with
  | None -> none ()
  | Some v -> some v

let iteri_sparse ~f a =
  Array.iteri (fun i v ->
      match v with
      | None -> ()
      | Some v -> f i v)
    a
