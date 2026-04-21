(** normal movement speed *)
val move_speed : float

(** sprint movement speed *)
val sprint_speed : float

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
