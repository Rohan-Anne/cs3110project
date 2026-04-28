(** world state: all loaded chunks keyed by chunk coordinate *)
type t

val create : unit -> t

(** [get_chunk world cx cy cz] returns [Some chunk] if the chunk at
    [(cx, cy, cz)] is loaded, or [None] otherwise *)
val get_chunk : t -> int -> int -> int -> Chunk.t option

(** gets the block at world coordinates [(x, y, z)], or [Air] if the chunk is
    not loaded *)
val get_block : t -> int -> int -> int -> Block.t

(** set the block at world coordinates [(x, y, z)] *)
val set_block : t -> int -> int -> int -> Block.t -> unit

(** generate a chunk at chunk coordinate [(cx, cy, cz)] and add it to [t] *)
val generate_chunk : t -> cx:int -> cy:int -> cz:int -> unit

(** [mesh_into world chunk pos_buf col_buf] writes mesh floats for [chunk] into
    [pos_buf] and [col_buf] starting at index 0, and returns the number of
    floats written; buffers must be at least [chunk_size^3 * 6 * 6 * 3] long *)
val mesh_into : t -> Chunk.t -> float array -> float array -> int

(** build vertex position and color arrays from a chunk for upload to the GPU *)
val mesh_chunk : t -> Chunk.t -> float array * float array

(** iterate over all loaded chunks *)
val iter : t -> (Chunk.t -> unit) -> unit
