type elt =
  | Constant
  | Allocated of int

type 'a t = elt array

exception Null

let null : elt = Obj.magic Null

let empty_array : 'a t = Array.make 1 null

let make (n:int) : 'a t =
  (* The array contains n+1 elements: the first one is used to store the null as a reference *)
  Array.make (n+1) null

let length a = Array.length a - 1

let set (a:'a t) (n:int) (v:'a) : unit =
  Array.set a (n+1) (Obj.magic v : elt)
    [@@inline]

let clear (a:'a t) (n:int) : unit =
  Array.set a (n+1) (Array.unsafe_get a 0)

let unsafe_set (a:'a t) (n:int) (v:'a) : unit =
  Array.unsafe_set a (n+1) (Obj.magic v : elt)
    [@@inline]

let get (a:'a t) (n:int) : 'a option =
  let elt = Array.get a (n+1) in
  let null = Array.unsafe_get a 0 in
  if elt == null then
    None
  else
    Some (Obj.magic elt:'a)
      [@@inline]

let unsafe_get (a:'a t) (n:int) : 'a option =
  let elt = Array.unsafe_get a (n+1) in
  let null = Array.unsafe_get a 0 in
  if elt == null then
    None
  else
    Some (Obj.magic elt:'a)
      [@@inline]

let unsafe_get_some (a:'a t) (n:int) : 'a =
  let elt = Array.unsafe_get a (n+1) in
  (Obj.magic elt:'a)
    [@@inline]

let get_k (a:'a t) (n:int) (k_none:(unit -> 'b)) (k_some:('a -> 'b)) : 'b =
  let elt = Array.get a (n+1) in
  let null = Array.unsafe_get a 0 in
  if elt == null then
    k_none ()
  else
    k_some (Obj.magic elt:'a)
      [@@inline]

let unsafe_get_k (a:'a t) (n:int) (k_none:(unit -> 'b)) (k_some:('a -> 'b)) : 'b =
  let elt = Array.unsafe_get a (n+1) in
  let null = Array.unsafe_get a 0 in
  if elt == null then
    k_none ()
  else
    k_some (Obj.magic elt:'a)
      [@@inline]

let iteri_sparse ~(f:int -> 'a -> unit) (a:'a t) : unit =
  let null = Array.unsafe_get a 0 in
  for i = 1 to Array.length a - 1 do
    let elt = Array.unsafe_get a i in
    if elt != null then f (i-1) (Obj.magic elt:'a)
  done

let realloc (src:'a t) (n:int) =
  let null = Array.unsafe_get src 0 in
  let result = Array.make n null in
  let copy_lenght = min (Array.length src) (Array.length result) - 1 in
  Array.blit src 1 result 1 copy_lenght;
  result
