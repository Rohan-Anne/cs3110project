(** chunk object *)
type t

(** [x chunk] returns the x coordinate of [chunk] *)
val x : t -> int

(** [y chunk] returns the y coordinate of [chunk] *)
val y : t -> int

(** [z chunk] returns the z coordinate of [chunk] *)
val z : t -> int

(** [index x y z] returns the flat array index for block [(x, y, z)] within a
    chunk *)
val index : int -> int -> int -> int

(** [get chunk x y z] returns the block at [(x, y, z)] within [chunk] *)
val get : t -> int -> int -> int -> Block.t

(** [set chunk x y z v] sets the block at [(x, y, z)] within [chunk] to [v] *)
val set : t -> int -> int -> int -> Block.t -> unit

(** [create ~x ~y ~z ~blocks] creates a chunk with the given coordinates and
    block array *)
val create : x:int -> y:int -> z:int -> blocks:Block.t array -> t
