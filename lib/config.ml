(** normal movement speed *)
let move_speed = 2.0

(** sprint movement speed *)
let sprint_speed = 4.0

(** how sensitive the mouse is. converts an dx or dy from input and turns it
    into delta yaw/pitch *)
let mouse_sensitivity = 0.002

(** maximum pitch, bounded in positive and negative direction *)
let pitch_limit = 1.54

(** size of chunks, as cubes of chunk_size^3 *)
let chunk_size = 16

(** field of vision in radians *)
let fov_y = Float.pi /. 3.0

(** near clipping plane distance *)
let near = 0.1

(** far clipping plane distance *)
let far = 1000.0
