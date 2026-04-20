(** first-person camera with yaw/pitch orientation *)
type t = {
  mutable pos : Math3d.vec3;
  mutable yaw : float;  (** horizontal rotation in radians *)
  mutable pitch : float;
      (** vertical rotation in radians, clamped by [Config.pitch_limit] *)
}

(** [create ~pos ~yaw ~pitch] creates a camera with the specified [pos], [yaw],
    and [pitch] *)
val create : pos:Math3d.vec3 -> yaw:float -> pitch:float -> t

(** update yaw and pitch from a mouse delta, applying [sensitivity] and clamping
    pitch *)
val apply_mouse_look : t -> dx:float -> dy:float -> sensitivity:float -> unit

(** returns a movement vector scaled by speed, or zero if no keys are held *)
val movement_from_input :
  t ->
  Input.t ->
  move_speed:float ->
  sprint_speed:float ->
  dt:float ->
  Math3d.vec3

(** compute the view matrix for the current camera state *)
val view : t -> Math3d.mat4
