(** Tests for the {!Camera} module.

    {1 Module Overview}
    [Camera] represents a first-person camera in the 3-D world. It stores a
    mutable position, yaw (horizontal rotation), and pitch (vertical rotation).
    From these it computes a view matrix and converts keyboard / mouse input
    into movement vectors.

    {1 Invariants}
    {2 [create]}
    - The [pos], [yaw], and [pitch] fields are initialised exactly to the
      arguments supplied.

    {2 [apply_mouse_look]}
    - Yaw changes by [−dx × sensitivity] (left-handed: positive dx rotates
      right, i.e. decreasing yaw in a right-hand convention).
    - Pitch changes by [−dy × sensitivity].
    - Pitch is clamped to [[-Config.pitch_limit, Config.pitch_limit]] after
      every update; it never exceeds these bounds.
    - Zero mouse delta leaves yaw and pitch unchanged.
    - Multiple calls accumulate.

    {2 forward / right (internal, tested indirectly)}
    - At yaw = 0: forward = (0, 0, -1), right = (1, 0, 0).
    - [right] is always orthogonal to forward and lies in the XZ plane.

    {2 [movement_from_input]}
    - With no keys pressed the returned vector is (0, 0, 0).
    - With W pressed and yaw = 0, movement points in the -Z direction.
    - With D pressed and yaw = 0, movement points in the +X direction.
    - Sprint speed applies when Ctrl is held.
    - The returned vector has length ≤ speed × dt (normalised before scaling, so
      diagonal movement is not faster).

    {2 [ground_movement_from_input]}
    - Like [movement_from_input] but the y-component of the result is always 0
      (vertical velocity handled separately in survival mode).

    {2 [view]}
    - Returns a 16-element float array.
    - Distinct (pos, yaw, pitch) values produce distinct matrices. *)

open OUnit2
open Tsdl

let eps = 1e-5

(* ------------------------------------------------------------------ *)
(*  Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let assert_feq ?(eps = eps) ~msg expected actual =
  if abs_float (expected -. actual) > eps then
    assert_failure
      (Printf.sprintf "%s: expected %.8f got %.8f" msg expected actual)

let assert_v3 ?(eps = eps) ~msg (ex, ey, ez) (v : Math3d.vec3) =
  assert_feq ~eps ~msg:(msg ^ ".x") ex v.x;
  assert_feq ~eps ~msg:(msg ^ ".y") ey v.y;
  assert_feq ~eps ~msg:(msg ^ ".z") ez v.z

(** Create a camera at the origin with zero orientation by default. *)
let make_cam ?(x = 0.0) ?(y = 0.0) ?(z = 0.0) ?(yaw = 0.0) ?(pitch = 0.0) () =
  Camera.create ~pos:(Math3d.vec3 x y z) ~yaw ~pitch

(** Create a fresh [Input.t] with a specific key held down. *)
let input_with_key sc =
  let inp = Input.create () in
  Hashtbl.replace inp.keys_down sc true;
  inp

(** Fresh [Input.t] with no keys pressed. *)
let no_keys () = Input.create ()

(* ------------------------------------------------------------------ *)
(*  {1 create}                                                          *)
(* ------------------------------------------------------------------ *)

let test_create_pos _ =
  let c = make_cam ~x:1.0 ~y:2.0 ~z:3.0 () in
  assert_feq ~msg:"pos.x" 1.0 c.pos.x;
  assert_feq ~msg:"pos.y" 2.0 c.pos.y;
  assert_feq ~msg:"pos.z" 3.0 c.pos.z

let test_create_yaw _ =
  let c = make_cam ~yaw:0.5 () in
  assert_feq ~msg:"yaw" 0.5 c.yaw

let test_create_pitch _ =
  let c = make_cam ~pitch:0.3 () in
  assert_feq ~msg:"pitch" 0.3 c.pitch

let test_create_zero _ =
  let c = make_cam () in
  assert_feq ~msg:"x=0" 0.0 c.pos.x;
  assert_feq ~msg:"y=0" 0.0 c.pos.y;
  assert_feq ~msg:"z=0" 0.0 c.pos.z;
  assert_feq ~msg:"yaw=0" 0.0 c.yaw;
  assert_feq ~msg:"pitch=0" 0.0 c.pitch

(* ------------------------------------------------------------------ *)
(*  {1 apply_mouse_look}                                               *)
(* ------------------------------------------------------------------ *)

let test_mouse_yaw_changes _ =
  let c = make_cam () in
  Camera.apply_mouse_look c ~dx:1.0 ~dy:0.0 ~sensitivity:0.01;
  assert_bool "yaw changed" (abs_float c.yaw > 0.0)

let test_mouse_pitch_changes _ =
  let c = make_cam () in
  Camera.apply_mouse_look c ~dx:0.0 ~dy:1.0 ~sensitivity:0.01;
  assert_bool "pitch changed" (abs_float c.pitch > 0.0)

let test_mouse_zero_no_change _ =
  let c = make_cam ~yaw:0.7 ~pitch:0.3 () in
  Camera.apply_mouse_look c ~dx:0.0 ~dy:0.0 ~sensitivity:1.0;
  assert_feq ~msg:"yaw unchanged" 0.7 c.yaw;
  assert_feq ~msg:"pitch unchanged" 0.3 c.pitch

(** Positive dx decrements yaw (right-turn). *)
let test_mouse_yaw_direction _ =
  let c = make_cam ~yaw:0.0 () in
  Camera.apply_mouse_look c ~dx:1.0 ~dy:0.0 ~sensitivity:0.1;
  assert_bool "dx>0 → yaw<0" (c.yaw < 0.0)

(** Positive dy decrements pitch (look down). *)
let test_mouse_pitch_direction _ =
  let c = make_cam ~pitch:0.0 () in
  Camera.apply_mouse_look c ~dx:0.0 ~dy:1.0 ~sensitivity:0.1;
  assert_bool "dy>0 → pitch<0" (c.pitch < 0.0)

let test_yaw_accumulates _ =
  let c = make_cam () in
  Camera.apply_mouse_look c ~dx:1.0 ~dy:0.0 ~sensitivity:0.01;
  let y1 = c.yaw in
  Camera.apply_mouse_look c ~dx:1.0 ~dy:0.0 ~sensitivity:0.01;
  let y2 = c.yaw in
  assert_bool "yaw accumulates" (abs_float y2 > abs_float y1)

let test_sensitivity_scales_yaw _ =
  let c1 = make_cam () in
  let c2 = make_cam () in
  Camera.apply_mouse_look c1 ~dx:1.0 ~dy:0.0 ~sensitivity:0.01;
  Camera.apply_mouse_look c2 ~dx:1.0 ~dy:0.0 ~sensitivity:0.1;
  assert_bool "higher sensitivity → larger yaw change"
    (abs_float c2.yaw > abs_float c1.yaw)

(* ------------------------------------------------------------------ *)
(*  {1 Pitch clamping}                                                  *)
(* ------------------------------------------------------------------ *)

let test_pitch_clamp_upper _ =
  let c = make_cam () in
  Camera.apply_mouse_look c ~dx:0.0 ~dy:(-1000.0) ~sensitivity:1.0;
  assert_bool "pitch ≤ pitch_limit" (c.pitch <= Config.pitch_limit)

let test_pitch_clamp_lower _ =
  let c = make_cam () in
  Camera.apply_mouse_look c ~dx:0.0 ~dy:1000.0 ~sensitivity:1.0;
  assert_bool "pitch ≥ -pitch_limit" (c.pitch >= -.Config.pitch_limit)

(** Already at the limit — another push in the same direction keeps it clamped
    rather than exceeding it. *)
let test_pitch_stays_clamped _ =
  let c = make_cam ~pitch:Config.pitch_limit () in
  Camera.apply_mouse_look c ~dx:0.0 ~dy:(-100.0) ~sensitivity:1.0;
  assert_bool "pitch still ≤ pitch_limit" (c.pitch <= Config.pitch_limit)

(* ------------------------------------------------------------------ *)
(*  {1 movement_from_input — no keys → zero}                           *)
(* ------------------------------------------------------------------ *)

let test_movement_no_keys _ =
  let c = make_cam () in
  let m =
    Camera.movement_from_input c (no_keys ()) ~move_speed:4.0 ~sprint_speed:8.0
      ~dt:1.0
  in
  assert_v3 ~msg:"no-key movement" (0.0, 0.0, 0.0) m

let test_ground_movement_no_keys _ =
  let c = make_cam () in
  let m =
    Camera.ground_movement_from_input c (no_keys ()) ~move_speed:4.0
      ~sprint_speed:8.0 ~dt:1.0
  in
  assert_v3 ~msg:"no-key ground" (0.0, 0.0, 0.0) m

(* ------------------------------------------------------------------ *)
(*  {1 movement_from_input — directional keys at yaw = 0}             *)
(*                                                                      *)
(*  At yaw = 0:                                                         *)
(*    forward = normalize(-sin 0, 0, -cos 0) = (0, 0, -1)             *)
(*    right   = normalize( cos 0, 0, -sin 0) = (1, 0,  0)             *)
(* ------------------------------------------------------------------ *)

(** W key at yaw = 0: movement should be (0, 0, -speed*dt). *)
let test_movement_w_key _ =
  let c = make_cam ~yaw:0.0 () in
  let m =
    Camera.movement_from_input c
      (input_with_key Sdl.Scancode.w)
      ~move_speed:1.0 ~sprint_speed:2.0 ~dt:1.0
  in
  assert_feq ~msg:"W dx = 0" 0.0 m.x;
  assert_feq ~msg:"W dy = 0" 0.0 m.y;
  assert_feq ~msg:"W dz = -1" (-1.0) m.z

(** D key at yaw = 0: movement should be (speed*dt, 0, 0). *)
let test_movement_d_key _ =
  let c = make_cam ~yaw:0.0 () in
  let m =
    Camera.movement_from_input c
      (input_with_key Sdl.Scancode.d)
      ~move_speed:1.0 ~sprint_speed:2.0 ~dt:1.0
  in
  assert_feq ~msg:"D dx = 1" 1.0 m.x;
  assert_feq ~msg:"D dy = 0" 0.0 m.y;
  assert_feq ~msg:"D dz = 0" 0.0 m.z

(** A key at yaw = 0: movement should be (-speed*dt, 0, 0). *)
let test_movement_a_key _ =
  let c = make_cam ~yaw:0.0 () in
  let m =
    Camera.movement_from_input c
      (input_with_key Sdl.Scancode.a)
      ~move_speed:1.0 ~sprint_speed:2.0 ~dt:1.0
  in
  assert_feq ~msg:"A dx = -1" (-1.0) m.x;
  assert_feq ~msg:"A dy = 0" 0.0 m.y;
  assert_feq ~msg:"A dz = 0" 0.0 m.z

(** Space key: movement should be (0, speed*dt, 0) in creative mode. *)
let test_movement_space_key _ =
  let c = make_cam () in
  let m =
    Camera.movement_from_input c
      (input_with_key Sdl.Scancode.space)
      ~move_speed:1.0 ~sprint_speed:2.0 ~dt:1.0
  in
  assert_feq ~msg:"Space dy = 1" 1.0 m.y

(** Ctrl (sprint) doubles speed. *)
let test_movement_sprint _ =
  let c = make_cam ~yaw:0.0 () in
  let inp = input_with_key Sdl.Scancode.w in
  Hashtbl.replace inp.keys_down Sdl.Scancode.lctrl true;
  let m =
    Camera.movement_from_input c inp ~move_speed:1.0 ~sprint_speed:2.0 ~dt:1.0
  in
  assert_feq ~msg:"sprint dz = -2" (-2.0) m.z

(** Diagonal movement (W + D) has the same speed as cardinal movement — the
    direction vector is normalised before scaling. *)
let test_movement_diagonal_normalized _ =
  let c = make_cam ~yaw:0.0 () in
  let inp = input_with_key Sdl.Scancode.w in
  Hashtbl.replace inp.keys_down Sdl.Scancode.d true;
  let m =
    Camera.movement_from_input c inp ~move_speed:1.0 ~sprint_speed:2.0 ~dt:1.0
  in
  let len = Math3d.length m in
  assert_feq ~eps:1e-4 ~msg:"diagonal length = 1" 1.0 len

(* ------------------------------------------------------------------ *)
(*  {1 ground_movement_from_input — y is always 0}                     *)
(* ------------------------------------------------------------------ *)

let test_ground_movement_y_zero _ =
  let c = make_cam () in
  let inp = input_with_key Sdl.Scancode.w in
  let m =
    Camera.ground_movement_from_input c inp ~move_speed:1.0 ~sprint_speed:2.0
      ~dt:1.0
  in
  assert_feq ~msg:"ground y = 0" 0.0 m.y

let test_ground_movement_w _ =
  let c = make_cam ~yaw:0.0 () in
  let m =
    Camera.ground_movement_from_input c
      (input_with_key Sdl.Scancode.w)
      ~move_speed:1.0 ~sprint_speed:2.0 ~dt:1.0
  in
  assert_feq ~msg:"ground W dz = -1" (-1.0) m.z;
  assert_feq ~msg:"ground W dy = 0" 0.0 m.y

(* ------------------------------------------------------------------ *)
(*  {1 view}                                                            *)
(* ------------------------------------------------------------------ *)

let test_view_length _ =
  let m = Camera.view (make_cam ()) in
  assert_equal ~msg:"16 elements" 16 (Array.length m)

let test_view_differs_by_yaw _ =
  let m1 = Camera.view (make_cam ~yaw:0.0 ()) in
  let m2 = Camera.view (make_cam ~yaw:1.0 ()) in
  let same = Array.for_all2 (fun a b -> abs_float (a -. b) < eps) m1 m2 in
  assert_bool "yaw 0 ≠ yaw 1" (not same)

let test_view_differs_by_pitch _ =
  let m1 = Camera.view (make_cam ~pitch:0.0 ()) in
  let m2 = Camera.view (make_cam ~pitch:0.5 ()) in
  let same = Array.for_all2 (fun a b -> abs_float (a -. b) < eps) m1 m2 in
  assert_bool "pitch 0 ≠ pitch 0.5" (not same)

let test_view_differs_by_pos _ =
  let m1 = Camera.view (make_cam ~x:0.0 ()) in
  let m2 = Camera.view (make_cam ~x:5.0 ()) in
  let same = Array.for_all2 (fun a b -> abs_float (a -. b) < eps) m1 m2 in
  assert_bool "pos 0 ≠ pos 5" (not same)

let test_view_deterministic _ =
  let c = make_cam ~x:1.0 ~yaw:0.3 ~pitch:0.1 () in
  let m1 = Camera.view c in
  let m2 = Camera.view c in
  let same = Array.for_all2 (fun a b -> abs_float (a -. b) < eps) m1 m2 in
  assert_bool "view is deterministic" same

let tests =
  "Camera"
  >::: [
         (* create *)
         "create_pos" >:: test_create_pos;
         "create_yaw" >:: test_create_yaw;
         "create_pitch" >:: test_create_pitch;
         "create_zero" >:: test_create_zero;
         (* apply_mouse_look *)
         "mouse_yaw_changes" >:: test_mouse_yaw_changes;
         "mouse_pitch_changes" >:: test_mouse_pitch_changes;
         "mouse_zero_no_change" >:: test_mouse_zero_no_change;
         "mouse_yaw_direction" >:: test_mouse_yaw_direction;
         "mouse_pitch_direction" >:: test_mouse_pitch_direction;
         "yaw_accumulates" >:: test_yaw_accumulates;
         "sensitivity_scales" >:: test_sensitivity_scales_yaw;
         (* pitch clamp *)
         "pitch_clamp_upper" >:: test_pitch_clamp_upper;
         "pitch_clamp_lower" >:: test_pitch_clamp_lower;
         "pitch_stays_clamped" >:: test_pitch_stays_clamped;
         (* movement — no keys *)
         "movement_no_keys" >:: test_movement_no_keys;
         "ground_no_keys" >:: test_ground_movement_no_keys;
         (* movement — directional *)
         "movement_w" >:: test_movement_w_key;
         "movement_d" >:: test_movement_d_key;
         "movement_a" >:: test_movement_a_key;
         "movement_space" >:: test_movement_space_key;
         "movement_sprint" >:: test_movement_sprint;
         "movement_diagonal" >:: test_movement_diagonal_normalized;
         (* ground movement *)
         "ground_y_zero" >:: test_ground_movement_y_zero;
         "ground_w" >:: test_ground_movement_w;
         (* view *)
         "view_length" >:: test_view_length;
         "view_yaw" >:: test_view_differs_by_yaw;
         "view_pitch" >:: test_view_differs_by_pitch;
         "view_pos" >:: test_view_differs_by_pos;
         "view_deterministic" >:: test_view_deterministic;
       ]

let _ = run_test_tt_main tests
