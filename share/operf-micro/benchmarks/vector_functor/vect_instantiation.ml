open Vect_abstr

module IV2 = Vector_operations(Vect2)

module IV3 = Vector_operations(Vect3)

module IV2R = Vector_operations(Vect2_record)

module IV3R = Vector_operations(Vect3_record)

module IVA = Vector_operations(Vect_array)

module VR2 = struct
  type t = Vect2_record.t = { x : float; y : float }
  type elt = float

  let norm { x; y } = sqrt (x *. x +. y *. y)
  let scale s { x; y } = { x = x *. s; y = y *. s }
  let dot v1 v2 = v1.x *. v2.x +. v1.y *. v2.y
  let are_orthogonal v1 v2 = dot v1 v2 = 0.

end
