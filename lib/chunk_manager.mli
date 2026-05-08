(** chunk buffer management and streaming *)
type t

(** [create ()] allocates the chunk buffer table and scratch arrays *)
val create : unit -> t

(** the GPU mesh table; passed to [World_renderer.draw] each frame *)
val bufs : t -> (int * int * int, Buffer.t) Hashtbl.t

(** remesh the chunk containing [(wx, wy, wz)] and any boundary-adjacent chunks;
    called after a block changes *)
val rebuild_affected : t -> World.t -> int -> int -> int -> unit

(** install one generated chunk into the world, mesh it, and remesh its loaded
    neighbours *)
val apply_result : t -> World.t -> int * int * int -> Block.t array -> unit

(** per-frame chunk streaming: unload distant chunks, drain worker results,
    request missing chunks in radius order *)
val update : t -> World.t -> Chunk_worker.t -> camera:Camera.t -> unit

(** block until the column under [camera] is fully loaded; call once at startup
*)
val wait_for_spawn : t -> World.t -> Chunk_worker.t -> camera:Camera.t -> unit

(** free all GPU buffers *)
val destroy : t -> unit
