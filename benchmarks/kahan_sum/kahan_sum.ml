

module Kahan : sig

  (** A more summation that keeps track of low order bits. *)
  type t

  (** [empty] initialize a summation. *)
  val empty : t

  val zero : t

  (** [update summation value] *)
  val update : t -> float -> t

  (** [update summation value] *)
  val ( + ) : t -> float -> t

  (** [sum summation] returns the sum from summation. *)
  val sum : t -> float

end = struct

  type t = { correction : float; sum : float }

  let empty = { correction = 0.; sum = 0. }

  let update t v =
    let x  = v -. t.correction in
    let ns = t.sum +. x in
    { correction = (ns -. t.sum) -. x
    ; sum = ns
    }

  let zero = empty

  let ( + ) = update

  let sum t = t.sum

end

let sum_with_array_fold a = Kahan.sum (Array.fold_left Kahan.update Kahan.empty a)

let sum_baseline a =
  let c = ref 0. in
  let s = ref 0. in
  for i = 0 to Array.length a - 1 do
    let x = a.(i) -. !c in
    let ns = !s +. x in
    c := (ns -. !s) -. x;
    s := ns;
  done;
  !s
