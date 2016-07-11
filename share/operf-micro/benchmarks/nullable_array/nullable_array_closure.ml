
(* This type tells the compiler that it is not a float, but can
   contain heap allocated values.

   Note that the correctness of this must be reasserted for each
   version of the compiler as new versions could specialise the array
   type further to its content. *)
type elt =
  | Constant
  | Allocated of int

type 'a t = elt array

(* A function is used to mark the empty fields. This is built as
   function returning an exception to ensure that no other function
   could be shared with this one. This ensures that the physical
   equality is meaningfull: No other value can be made physically
   equal to that one.

   This is a function instead of directly using the exception to
   ensure that marshalling behaves correctly: This won't work if we
   are marshalling between incompatible binaries, but sharing will
   be preserved otherwise.

   Not that this property also prevents this type from being correctly
   unmarshaled.

   There would be solutions by storing the null in the array as a
   reminder. *)
exception Null

let null : elt =
  Array.unsafe_get (Obj.magic (fun _ -> Null):elt array) 0

(* let null : elt = (Obj.magic (fun _ -> Null)) *)

let make (n:int) : 'a t =
  (* Null is not a floating point value, the array won't be allocated
     as a float array *)
  let a = Array.make n null in
  (* We mark the array with the closure tag such that the no naked
     pointer variant correctly recognize it as an out of heap pointer *)
  Obj.set_tag (Obj.repr a) Obj.closure_tag;
  a

let length a = Array.length a

let set (a:'a t) (n:int) (v:'a) : unit =
  Array.set a n (Obj.magic v : elt)
    [@@inline]

let get (a:'a t) (n:int) : 'a option =
  let elt = Array.get a n in
  if elt == null then
    None
  else
    Some (Obj.magic elt:'a)
      [@@inline]

let get_k (a:'a t) (n:int) (k_none:(unit -> 'b)) (k_some:('a -> 'b)) : 'b =
  let elt = Array.get a n in
  if elt == null then
    k_none ()
  else
    k_some (Obj.magic elt:'a)
      [@@inline]

let iteri_sparse ~(f:int -> 'a -> unit) (a:'a t) : unit =
  for i = 0 to Array.length a - 1 do
    let elt = Array.unsafe_get a i in
    if elt != null then f i (Obj.magic elt:'a)
  done
