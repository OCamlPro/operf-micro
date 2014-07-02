type t =
  | Dict of (string * t) list
  | List of t list
  | String of string
  | Float of float

type file = t
