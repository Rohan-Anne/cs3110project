let move_speed = 4.0
let sprint_speed = 8.0
let crouch_speed = 2.0
let crouch_height = 1.5
let mouse_sensitivity = 0.002
let pitch_limit = 1.54
let chunk_size = 16
let fov_y = Float.pi /. 3.0
let near = 0.1
let far = 1000.0
let player_width = 0.6
let player_height = 1.8
let gravity = 20.0
let jump_velocity = 8.0
let max_reach = 5.0

(* horizontal radius (in chunks) around the player to keep loaded *)
let render_distance = 8

(* vertical chunk range. terrain heights peak around y=10, so [-2..1] covers
   everything that could contain solid blocks *)
let chunk_y_min = -2
let chunk_y_max = 1

(* maximum chunks to generate per frame to bound per-frame work *)
let chunk_load_budget = 4
