
type 'a t

val length : 'a t -> int

val make : int -> 'a t

val set : 'a t -> int -> 'a -> unit

val get : 'a t -> int -> 'a option

val get_k : 'a t -> int -> (unit -> 'b) -> ('a -> 'b) -> 'b

val iteri_sparse : f:(int -> 'a -> unit) -> 'a t -> unit
