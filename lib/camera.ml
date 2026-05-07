open Tsdl

type t = {
  mutable pos : Math3d.vec3;
  mutable yaw : float;
  mutable pitch : float;
}

(* AF: [{pos; yaw; pitch}] represents a first-person camera located at world
   position [pos], rotated horizontally by [yaw] radians around the Y-axis and
   vertically by [pitch] radians around the local X-axis (positive pitch looks
   up).

   RI: [pitch] is in the closed interval [[-Config.pitch_limit,
   Config.pitch_limit]]. *)

let create ~pos ~yaw ~pitch = { pos; yaw; pitch }

let apply_mouse_look t ~dx ~dy ~sensitivity =
  t.yaw <- mod_float (t.yaw -. (dx *. sensitivity)) (2.0 *. Float.pi);
  t.pitch <- t.pitch -. (dy *. sensitivity);
  t.pitch <-
    Float.max (-.Config.pitch_limit) (Float.min Config.pitch_limit t.pitch)

(** get the unit vector pointing forward relative to the camera *)
let forward t = Math3d.normalize (Math3d.vec3 (-.sin t.yaw) 0.0 (-.cos t.yaw))

(** get the unit vector pointing right, relative to the camera *)
let right t = Math3d.normalize (Math3d.vec3 (cos t.yaw) 0.0 (-.sin t.yaw))

(* returns a movement vector, scaled by speed, or zero *)
let movement_from_input t inp ~move_speed ~sprint_speed ~dt =
  let sprint = Input.is_down inp Sdl.Scancode.lctrl in
  let speed = (if sprint then sprint_speed else move_speed) *. dt in
  let fwd = forward t in
  let rgt = right t in
  (* up is constant, as there is no roll *)
  let up = Math3d.vec3 0.0 1.0 0.0 in
  let dir = ref (Math3d.vec3 0.0 0.0 0.0) in
  if Input.is_down inp Sdl.Scancode.w then dir := Math3d.add !dir fwd;
  if Input.is_down inp Sdl.Scancode.s then dir := Math3d.sub !dir fwd;
  if Input.is_down inp Sdl.Scancode.d then dir := Math3d.add !dir rgt;
  if Input.is_down inp Sdl.Scancode.a then dir := Math3d.sub !dir rgt;
  if Input.is_down inp Sdl.Scancode.space then dir := Math3d.add !dir up;
  if Input.is_down inp Sdl.Scancode.lshift then dir := Math3d.sub !dir up;
  if Math3d.length !dir > 0.0 then Math3d.scale (Math3d.normalize !dir) speed
  else !dir

let ground_movement_from_input t inp ~move_speed ~sprint_speed ~dt =
  let sprint = Input.is_down inp Sdl.Scancode.lctrl in
  let speed = (if sprint then sprint_speed else move_speed) *. dt in
  let fwd = forward t in
  let rgt = right t in
  let dir = ref (Math3d.vec3 0.0 0.0 0.0) in
  if Input.is_down inp Sdl.Scancode.w then dir := Math3d.add !dir fwd;
  if Input.is_down inp Sdl.Scancode.s then dir := Math3d.sub !dir fwd;
  if Input.is_down inp Sdl.Scancode.d then dir := Math3d.add !dir rgt;
  if Input.is_down inp Sdl.Scancode.a then dir := Math3d.sub !dir rgt;
  let flat = Math3d.vec3 !dir.x 0.0 !dir.z in
  if Math3d.length flat > 0.0 then Math3d.scale (Math3d.normalize flat) speed
  else flat

(** full 3D look direction including pitch *)
let look_dir t =
  let cp = cos t.pitch in
  Math3d.normalize
    (Math3d.vec3 (-.sin t.yaw *. cp) (sin t.pitch) (-.cos t.yaw *. cp))

let view t = Math3d.view_from_camera ~position:t.pos ~yaw:t.yaw ~pitch:t.pitch
