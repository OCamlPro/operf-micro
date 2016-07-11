module type Vector_base = sig
  type t
  type elt
  type index

  val fold_index : (index -> elt -> 'a) -> ('a -> index -> elt -> 'a) -> t -> 'a
  val fold_index_2 : (index -> elt -> elt -> 'a) -> ('a -> index -> elt -> elt -> 'a) -> t -> t -> 'a
  val map : (index -> elt -> elt) -> t -> t

end

module Vect2 = struct
  type t = float * float
  type elt = float
  type index = Fst | Snd

  let fold_index init f (fst, snd) =
    f (init Fst fst) Snd snd
  let fold_index_2 init f (fst1, snd1) (fst2, snd2) =
    f (init Fst fst1 fst2) Snd snd1 snd2
  let map f (fst, snd) =
    (f Fst fst, f Snd snd)
end

module Vect3 = struct
  type t = float * float * float
  type elt = float
  type index = Fst | Snd | Trd

  let fold_index init f (fst, snd, trd) =
    f (f (init Fst fst) Snd snd) Trd trd
  let fold_index_2 init f (fst1, snd1, trd1) (fst2, snd2, trd2) =
    f (f (init Fst fst1 fst2) Snd snd1 snd2) Trd trd1 trd2
  let map f (fst, snd, trd) =
    (f Fst fst, f Snd snd, f Trd trd)

end

module Vect2_record = struct
  type t = { x : float; y : float }
  type elt = float
  type index = X | Y

  let fold_index init f { x; y } =
    f (init X x) Y y
  let fold_index_2 init f v1 v2 =
    f (init X v1.x v2.x) Y v1.y v2.y
  let map f { x; y } =
    { x = f X x; y = f Y y}
end

module Vect3_record = struct
  type t = { x : float; y : float; z : float }
  type elt = float
  type index = X | Y | Z

  let fold_index init f { x; y; z } =
    f (f (init X x) Y y) Z z
  let fold_index_2 init f v1 v2 =
    f (f (init X v1.x v2.x) Y v1.y v2.y) Z v1.z v2.z
  let map f { x; y; z } =
    { x = f X x; y = f Y y; z = f Z z }
end

module Vect_array = struct
  type t = float array
  type elt = float
  type index = int

  let fold_index init f a =
    if Array.length a = 0 then invalid_arg "fold_index";
    let r = ref (init 0 (Array.unsafe_get a 0)) in
    for i = 1 to Array.length a - 1 do
      r := f !r i (Array.unsafe_get a i)
    done;
    !r

  let fold_index_2 init f v1 v2 =
    if Array.length v1 = 0 ||
       Array.length v2 = 0 ||
       Array.length v1 <> Array.length v2 then
      invalid_arg "fold_index";
    let r = ref (init 0 (Array.unsafe_get v1 0) (Array.unsafe_get v2 0)) in
    for i = 1 to Array.length v1 - 1 do
      r := f !r i (Array.unsafe_get v1 i) (Array.unsafe_get v2 i)
    done;
    !r

  let map = Array.mapi

end

module Vector_operations (V : Vector_base with type elt = float) = struct

  type elt = V.elt
  type t = V.t

  let norm v =
    let sum_sq =
      V.fold_index
        (fun _ elt -> elt *. elt)
        (fun acc _ elt -> acc +. elt *. elt)
        v
    in
    sqrt sum_sq

  let scale s v =
    V.map (fun _ x -> x *. s) v

  let dot v1 v2 =
    V.fold_index_2
      (fun _ f1 f2 -> f1 *. f2)
      (fun acc _ f1 f2 -> acc +. f1 *. f2) v1 v2

  let are_orthogonal v1 v2 =
    dot v1 v2 = 0. (* < epsilon ? *)

end
