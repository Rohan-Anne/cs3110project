(** block types *)
type block =
  | Air
  | Stone
  | Dirt
  | Grass

(** chunk object *)
type t = {
  x : int;  (** chunk x coordinate *)
  y : int;  (** chunk y coordinate *)
  z : int;  (** chunk z coordinate *)
  blocks : block array;  (** flat array of chunk_size^3 of block types *)
}

(** [index x y z] gets the index in the [blocks] array of [Chunk.t] of the block
    at the location (x, y, z) *)
let index x y z =
  x + (y * Config.chunk_size) + (z * Config.chunk_size * Config.chunk_size)

(** [get chunk x y z] gets the block type of the block located at (x, y, z) in
    [chunk] *)
let get chunk x y z = chunk.blocks.(index x y z)

(** [get chunk x y z v] sets the block type of the block located at (x, y, z) in
    [chunk] to [v]*)
let set chunk x y z v = chunk.blocks.(index x y z) <- v
