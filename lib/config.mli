(** normal movement speed *)
val move_speed : float

(** sprint movement speed *)
val sprint_speed : float

(** movement speed while crouching *)
val crouch_speed : float

(** player AABB height while crouching *)
val crouch_height : float

(** how sensitive the mouse is. converts an dx or dy from input and turns it
    into delta yaw/pitch *)
val mouse_sensitivity : float

(** maximum pitch, bounded in positive and negative direction *)
val pitch_limit : float

(** size of chunks, as cubes of chunk_size^3 *)
val chunk_size : int

(** field of vision in radians *)
val fov_y : float

(** near clipping plane distance *)
val near : float

(** far clipping plane distance *)
val far : float

(** player AABB width and depth in blocks *)
val player_width : float

(** player AABB height in blocks *)
val player_height : float

(** downward acceleration in blocks/s^2 *)
val gravity : float

(** initial vertical speed when jumping in blocks/s *)
val jump_velocity : float

(** how far the player can reach to break or place blocks, in world units *)
val max_reach : float

(** horizontal radius, in chunks, around the player to keep loaded *)
val render_distance : int

(** lowest chunk y-index that can contain non-air blocks *)
val chunk_y_min : int

(** highest chunk y-index that can contain non-air blocks *)
val chunk_y_max : int

(** maximum number of new chunks to generate (and mesh) per frame, used to
    cap the per-frame stall when streaming the world *)
val chunk_load_budget : int
