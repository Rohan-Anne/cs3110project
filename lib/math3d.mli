(** column-major 4x4 matrix stored as a flat 16-element array *)
type mat4 = float array

(** 3d vector *)
type vec3 = {
  x : float;
  y : float;
  z : float;
}

(** [vec3 x y z] creates a vec3 *)
val vec3 : float -> float -> float -> vec3

(** [add a b] adds the vectors [a] and [b] *)
val add : vec3 -> vec3 -> vec3

(** [sub a b] subtracts the vectors [a] and [b] *)
val sub : vec3 -> vec3 -> vec3

(** [scale v s] multiplies every component of [v] by the scalar [s] *)
val scale : vec3 -> float -> vec3

(* [length v] gets the length of [v] *)
val length : vec3 -> float

(** [normalize v] returns [v] scaled so that [length v = 0.] *)
val normalize : vec3 -> vec3

(* gets the 4x4 identity matrix *)
val identity : unit -> mat4

(** column-major multiply: result = a * b *)
val multiply : mat4 -> mat4 -> mat4

(** [translation x y z] creates the translation matrix, shifting the origin to
    [(x, y, z)] *)
val translation : x:float -> y:float -> z:float -> mat4

(** [rotation_x angle] creates the rotation matrix around the x-axis by [angle]
    radians *)
val rotation_x : float -> mat4

(** [rotation_y angle] creates the rotation matrix around the y-axis by [angle]
    radians *)
val rotation_y : float -> mat4

(** creates the standard OpenGL perspective matrix, [fov_y] in radians *)
val perspective :
  fov_y_radians:float -> aspect:float -> near:float -> far:float -> mat4

(** build a view matrix from position + yaw/pitch angles in radians *)
val view_from_camera : position:vec3 -> yaw:float -> pitch:float -> mat4
