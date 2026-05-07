(** axis-aligned bounding box *)
type aabb = {
  min : Math3d.vec3;
  max : Math3d.vec3;
}

(** [at_position ?height pos] returns the player AABB centered at [pos], sized
    according to [Config.player_width] and [height] (defaults to
    [Config.player_height]) *)
val at_position : ?height:float -> Math3d.vec3 -> aabb

(** [has_ground_below world box] returns true if there is at least one solid
    block directly beneath [box] *)
val has_ground_below : World.t -> aabb -> bool

(** [move world box delta] sweeps [box] by [delta] against solid blocks in
    [world] and returns the largest movement that does not cause overlap. each
    axis is resolved independently so the player slides along walls *)
val move : World.t -> aabb -> Math3d.vec3 -> Math3d.vec3
