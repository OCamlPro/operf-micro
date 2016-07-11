let get a i = a.(i - 1)

let sum a =
  get a 1 +. get a 2 +. get a 3 +.
  get a 4 +. get a 5 +. get a 6 +.
  get a 7 +. get a 8 +. get a 9

let arr len =
  Array.init len (fun i -> Array.make 9 (float_of_int i))
