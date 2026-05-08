(** renders the voxel world: chunk meshes with frustum culling *)
type t

(** compile the world shader *)
val create : unit -> t

(** draw all visible chunks in [chunk_bufs] using [mvp] for frustum culling and
    transform *)
val draw :
  t ->
  chunk_bufs:(int * int * int, Buffer.t) Hashtbl.t ->
  mvp:Math3d.mat4 ->
  unit

(** release GPU resources *)
val destroy : t -> unit
