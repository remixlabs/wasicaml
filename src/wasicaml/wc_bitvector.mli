type t

val empty : t
val set : int -> bool -> t -> t
val get : int -> t -> bool
val union : t -> t -> t
val elements : t -> int list
