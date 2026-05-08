(** player state and per-frame update *)
type t

(** create a player in Survival mode at rest, holding Stone *)
val create : unit -> t

(** per-frame update: mode toggle, movement, crouching, block interaction,
    raycast, and selection outline. mutates [camera.pos] in place. *)
val update :
  t -> World.t -> Camera.t -> Input.t -> Chunk_manager.t -> dt:float -> unit

(** the block type currently selected for placement *)
val held_block : t -> Block.t

(** draw the wireframe outline around the targeted block, if any *)
val draw_selection : t -> unit

(** free the selection outline GPU buffer if allocated *)
val destroy : t -> unit
