(** axis-aligned bounding box *)
type aabb = {
  min : Math3d.vec3;
  max : Math3d.vec3;
}

(** [at_position pos] returns the player AABB centered at [pos], sized according
    to [Config.player_width] and [Config.player_height] *)
val at_position : Math3d.vec3 -> aabb

(** [move world box delta] sweeps [box] by [delta] against solid blocks in
    [world] and returns the largest movement that does not cause overlap. each
    axis is resolved independently so the player slides along walls *)
val move : World.t -> aabb -> Math3d.vec3 -> Math3d.vec3
