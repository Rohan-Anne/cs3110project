(** Tests for the {!Config} module.

    {1 Module Overview}
    [Config] holds all global game constants: movement speeds, camera
    parameters, world geometry, player dimensions, and physics values. Every
    value is a module-level constant — there are no functions.

    {1 Invariants}
    - [move_speed] and [sprint_speed] are both positive, with
      [move_speed < sprint_speed].
    - [mouse_sensitivity] is positive.
    - [pitch_limit] is positive and strictly less than [π/2] (prevents the
      camera from flipping at the poles).
    - [chunk_size] is positive.
    - [fov_y] lies in the open interval [(0, π)].
    - [near] is positive and strictly less than [far].
    - [player_width] and [player_height] are both positive;
      [player_height > player_width] (taller than wide).
    - [gravity] and [jump_velocity] are both positive. *)

open OUnit2

let pi = Float.pi

(** Convenience: assert two floats are exactly equal (no epsilon needed for
    literal constants that are stored as-is). *)
let assert_feq ~msg expected actual =
  let eps = 1e-12 in
  if abs_float (expected -. actual) > eps then
    assert_failure
      (Printf.sprintf "%s: expected %.15g got %.15g" msg expected actual)

(* ------------------------------------------------------------------ *)
(*  {1 Exact-value tests}                                               *)
(* ------------------------------------------------------------------ *)

let test_chunk_size_value _ =
  assert_equal ~msg:"chunk_size = 16" 16 Config.chunk_size

let test_move_speed_value _ =
  assert_feq ~msg:"move_speed = 4.0" 4.0 Config.move_speed

let test_sprint_speed_value _ =
  assert_feq ~msg:"sprint_speed = 8.0" 8.0 Config.sprint_speed

let test_mouse_sensitivity_value _ =
  assert_feq ~msg:"mouse_sensitivity = 0.002" 0.002 Config.mouse_sensitivity

let test_pitch_limit_value _ =
  assert_feq ~msg:"pitch_limit = 1.54" 1.54 Config.pitch_limit

let test_fov_y_value _ = assert_feq ~msg:"fov_y = pi/3" (pi /. 3.0) Config.fov_y
let test_near_value _ = assert_feq ~msg:"near = 0.1" 0.1 Config.near
let test_far_value _ = assert_feq ~msg:"far = 1000.0" 1000.0 Config.far

let test_player_width_value _ =
  assert_feq ~msg:"player_width = 0.6" 0.6 Config.player_width

let test_player_height_value _ =
  assert_feq ~msg:"player_height = 1.8" 1.8 Config.player_height

let test_gravity_value _ = assert_feq ~msg:"gravity = 20.0" 20.0 Config.gravity

let test_jump_velocity_value _ =
  assert_feq ~msg:"jump_velocity = 8.0" 8.0 Config.jump_velocity

(* ------------------------------------------------------------------ *)
(*  {1 Sign invariants}                                                 *)
(* ------------------------------------------------------------------ *)

let test_chunk_size_positive _ =
  assert_bool "chunk_size > 0" (Config.chunk_size > 0)

let test_move_speed_positive _ =
  assert_bool "move_speed > 0" (Config.move_speed > 0.0)

let test_sprint_speed_positive _ =
  assert_bool "sprint_speed > 0" (Config.sprint_speed > 0.0)

let test_mouse_sensitivity_positive _ =
  assert_bool "mouse_sensitivity > 0" (Config.mouse_sensitivity > 0.0)

let test_pitch_limit_positive _ =
  assert_bool "pitch_limit > 0" (Config.pitch_limit > 0.0)

let test_near_positive _ = assert_bool "near > 0" (Config.near > 0.0)
let test_far_positive _ = assert_bool "far > 0" (Config.far > 0.0)

let test_player_width_positive _ =
  assert_bool "player_width > 0" (Config.player_width > 0.0)

let test_player_height_positive _ =
  assert_bool "player_height > 0" (Config.player_height > 0.0)

let test_gravity_positive _ = assert_bool "gravity > 0" (Config.gravity > 0.0)

let test_jump_velocity_positive _ =
  assert_bool "jump_velocity > 0" (Config.jump_velocity > 0.0)

(* ------------------------------------------------------------------ *)
(*  {1 Relational invariants}                                           *)
(* ------------------------------------------------------------------ *)

(** Sprint is strictly faster than normal walking. *)
let test_sprint_faster_than_move _ =
  assert_bool "sprint_speed > move_speed"
    (Config.sprint_speed > Config.move_speed)

(** The near clip plane must be closer than the far clip plane. *)
let test_near_less_than_far _ =
  assert_bool "near < far" (Config.near < Config.far)

(** The pitch limit must be below π/2 to prevent the camera from flipping. *)
let test_pitch_limit_below_half_pi _ =
  assert_bool "pitch_limit < pi/2" (Config.pitch_limit < pi /. 2.0)

(** FOV must be a sensible angle: in (0, π). *)
let test_fov_in_range _ =
  assert_bool "fov_y > 0" (Config.fov_y > 0.0);
  assert_bool "fov_y < pi" (Config.fov_y < pi)

(** The player is taller than they are wide (capsule invariant). *)
let test_player_taller_than_wide _ =
  assert_bool "player_height > player_width"
    (Config.player_height > Config.player_width)

(** Jump velocity must be enough to overcome at least one gravity step, i.e.
    jump_velocity > 0. (Covered by sign invariant, but restated here for
    clarity.) *)
let test_jump_overcomes_gravity_step _ =
  let dt = 1.0 /. 60.0 in
  (* one frame at 60 fps *)
  let vel_after_one_frame = Config.jump_velocity -. (Config.gravity *. dt) in
  assert_bool "can still move upward after first physics step"
    (vel_after_one_frame > 0.0)

let tests =
  "Config"
  >::: [
         (* Exact values *)
         "chunk_size_value" >:: test_chunk_size_value;
         "move_speed_value" >:: test_move_speed_value;
         "sprint_speed_value" >:: test_sprint_speed_value;
         "mouse_sensitivity_value" >:: test_mouse_sensitivity_value;
         "pitch_limit_value" >:: test_pitch_limit_value;
         "fov_y_value" >:: test_fov_y_value;
         "near_value" >:: test_near_value;
         "far_value" >:: test_far_value;
         "player_width_value" >:: test_player_width_value;
         "player_height_value" >:: test_player_height_value;
         "gravity_value" >:: test_gravity_value;
         "jump_velocity_value" >:: test_jump_velocity_value;
         (* Sign invariants *)
         "chunk_size_positive" >:: test_chunk_size_positive;
         "move_speed_positive" >:: test_move_speed_positive;
         "sprint_speed_positive" >:: test_sprint_speed_positive;
         "mouse_sensitivity_pos" >:: test_mouse_sensitivity_positive;
         "pitch_limit_positive" >:: test_pitch_limit_positive;
         "near_positive" >:: test_near_positive;
         "far_positive" >:: test_far_positive;
         "player_width_positive" >:: test_player_width_positive;
         "player_height_positive" >:: test_player_height_positive;
         "gravity_positive" >:: test_gravity_positive;
         "jump_velocity_positive" >:: test_jump_velocity_positive;
         (* Relational invariants *)
         "sprint_faster_than_move" >:: test_sprint_faster_than_move;
         "near_less_than_far" >:: test_near_less_than_far;
         "pitch_limit_below_half_pi" >:: test_pitch_limit_below_half_pi;
         "fov_in_range" >:: test_fov_in_range;
         "player_taller_than_wide" >:: test_player_taller_than_wide;
         "jump_overcomes_gravity" >:: test_jump_overcomes_gravity_step;
       ]

let _ = run_test_tt_main tests
