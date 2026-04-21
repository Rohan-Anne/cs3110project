type t = {
  x : int;  (** chunk x coordinate *)
  y : int;  (** chunk y coordinate *)
  z : int;  (** chunk z coordinate *)
  blocks : Block.t array;  (** flat array of chunk_size^3 block types *)
}

let x chunk = chunk.x
let y chunk = chunk.y
let z chunk = chunk.z

let index x y z =
  x + (y * Config.chunk_size) + (z * Config.chunk_size * Config.chunk_size)

let get chunk x y z = chunk.blocks.(index x y z)
let set chunk x y z v = chunk.blocks.(index x y z) <- v

let create ~x ~y ~z ~blocks = { x; y; z; blocks }
