open Tsdl

(* clamp pitch just below ±π/2 to avoid gimbal-lock artifacts at vertical look
   extremes — 1.54 rad ≈ 88.2° *)
let pitch_limit = 1.54

type t = {
  mutable pos : Math3d.vec3;
  mutable yaw : float;
  mutable pitch : float;
}

let create ~pos ~yaw ~pitch = { pos; yaw; pitch }

let apply_mouse_look t ~dx ~dy ~sensitivity =
  t.yaw <- t.yaw -. (dx *. sensitivity);
  t.pitch <- t.pitch -. (dy *. sensitivity);
  t.pitch <- Float.max (-.pitch_limit) (Float.min pitch_limit t.pitch)

let forward t = Math3d.normalize (Math3d.vec3 (-.sin t.yaw) 0.0 (-.cos t.yaw))
let right t = Math3d.normalize (Math3d.vec3 (cos t.yaw) 0.0 (-.sin t.yaw))

(* returns a movement vector, scaled by speed, or zero *)
let movement_from_input t inp ~move_speed ~sprint_speed ~dt =
  let sprint = Input.is_down inp Sdl.Scancode.lctrl in
  let speed = (if sprint then sprint_speed else move_speed) *. dt in
  let fwd = forward t in
  let rgt = right t in
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

let view t = Math3d.view_from_camera ~position:t.pos ~yaw:t.yaw ~pitch:t.pitch
