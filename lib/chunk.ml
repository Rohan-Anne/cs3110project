type t = {
  x : int;  (** chunk x coordinate *)
  y : int;  (** chunk y coordinate *)
  z : int;  (** chunk z coordinate *)
  blocks : Block.t array;  (** flat array of chunk_size^3 block types *)
}

(* AF: [{x; y; z; blocks}] represents the cubic region of the voxel world whose
   chunk-grid corner is [(x, y, z)]. The block at local position [(bx, by, bz)]
   with [0 <= bx, by, bz < Config.chunk_size] is [blocks.(bx + by * chunk_size +
   bz * chunk_size * chunk_size)].

   RI: [Array.length blocks = Config.chunk_size * Config.chunk_size *
   Config.chunk_size]. *)

let x chunk = chunk.x
let y chunk = chunk.y
let z chunk = chunk.z

let index x y z =
  x + (y * Config.chunk_size) + (z * Config.chunk_size * Config.chunk_size)

let get chunk x y z = chunk.blocks.(index x y z)
let set chunk x y z v = chunk.blocks.(index x y z) <- v
let create ~x ~y ~z ~blocks = { x; y; z; blocks }
