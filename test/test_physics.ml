(** Tests for the {!Physics} module.

    {1 Module Overview}
    [Physics] provides axis-aligned bounding box (AABB) collision detection for
    the player. It constructs the player AABB from a position and sweeps it
    against solid blocks in the world, resolving each axis independently so the
    player can slide along walls.

    {1 Invariants}
    {2 [at_position]}
    - Width (x-extent) = [Config.player_width].
    - Height (y-extent) = [Config.player_height].
    - Depth (z-extent) = [Config.player_width].
    - The AABB is centered on [pos.x] and [pos.z].
    - Shifting [pos] by a vector [d] shifts both [min] and [max] by [d].
    - [min.x < max.x], [min.y < max.y], [min.z < max.z] always hold.

    {2 [move]}
    - In a world with no solid blocks, [move w box delta = delta] (movement is
      unrestricted).
    - Zero delta always returns zero: [move w box zero = zero].
    - The returned delta has the same sign as the input delta on each axis
      (collision can only reduce magnitude, never reverse direction).
    - After being blocked by a solid block, the returned delta is strictly
      smaller in magnitude than the requested delta but ≥ 0 in magnitude.
    - Each axis is resolved independently; blocking on one axis does not affect
      unobstructed axes.

    {1 Internal helpers (tested indirectly)}
    - [resolve_x / resolve_y / resolve_z]: each clips a single-axis displacement
      to avoid overlapping solid blocks. Their contracts are exercised through
      [move]. *)

open OUnit2

let eps = 1e-5

(* ------------------------------------------------------------------ *)
(*  Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let assert_feq ?(eps = eps) ~msg expected actual =
  if abs_float (expected -. actual) > eps then
    assert_failure
      (Printf.sprintf "%s: expected %.8f got %.8f" msg expected actual)

(** World with only a surface chunk loaded. *)
let surface_world () =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  w

(** Empty world (no chunks — all air). *)
let empty_world () = World.create ()

(* ------------------------------------------------------------------ *)
(*  {1 at_position — dimensions}                                        *)
(* ------------------------------------------------------------------ *)

let test_width _ =
  let box = Physics.at_position (Math3d.vec3 0.0 0.0 0.0) in
  assert_feq ~msg:"width = player_width" Config.player_width
    (box.max.x -. box.min.x)

let test_height _ =
  let box = Physics.at_position (Math3d.vec3 0.0 0.0 0.0) in
  assert_feq ~msg:"height = player_height" Config.player_height
    (box.max.y -. box.min.y)

let test_depth _ =
  let box = Physics.at_position (Math3d.vec3 0.0 0.0 0.0) in
  assert_feq ~msg:"depth = player_width" Config.player_width
    (box.max.z -. box.min.z)

(* ------------------------------------------------------------------ *)
(*  {1 at_position — centering}                                         *)
(* ------------------------------------------------------------------ *)

let test_centered_x _ =
  let pos = Math3d.vec3 5.0 0.0 0.0 in
  let box = Physics.at_position pos in
  let cx = (box.min.x +. box.max.x) /. 2.0 in
  assert_feq ~msg:"center x = pos.x" 5.0 cx

let test_centered_z _ =
  let pos = Math3d.vec3 0.0 0.0 3.0 in
  let box = Physics.at_position pos in
  let cz = (box.min.z +. box.max.z) /. 2.0 in
  assert_feq ~msg:"center z = pos.z" 3.0 cz

(* ------------------------------------------------------------------ *)
(*  {1 at_position — min < max invariant}                               *)
(* ------------------------------------------------------------------ *)

let test_min_lt_max _ =
  let positions =
    [
      Math3d.vec3 0.0 0.0 0.0;
      Math3d.vec3 10.0 20.0 (-5.0);
      Math3d.vec3 (-3.0) 0.5 100.0;
    ]
  in
  List.iter
    (fun pos ->
      let box = Physics.at_position pos in
      assert_bool "min.x < max.x" (box.min.x < box.max.x);
      assert_bool "min.y < max.y" (box.min.y < box.max.y);
      assert_bool "min.z < max.z" (box.min.z < box.max.z))
    positions

(* ------------------------------------------------------------------ *)
(*  {1 at_position — shift invariant}                                   *)
(* ------------------------------------------------------------------ *)

let test_shift _ =
  let p1 = Math3d.vec3 0.0 0.0 0.0 in
  let p2 = Math3d.vec3 10.0 5.0 (-3.0) in
  let b1 = Physics.at_position p1 in
  let b2 = Physics.at_position p2 in
  assert_feq ~msg:"shifted min.x" (b1.min.x +. 10.0) b2.min.x;
  assert_feq ~msg:"shifted min.y" (b1.min.y +. 5.0) b2.min.y;
  assert_feq ~msg:"shifted min.z" (b1.min.z -. 3.0) b2.min.z;
  assert_feq ~msg:"shifted max.x" (b1.max.x +. 10.0) b2.max.x;
  assert_feq ~msg:"shifted max.y" (b1.max.y +. 5.0) b2.max.y;
  assert_feq ~msg:"shifted max.z" (b1.max.z -. 3.0) b2.max.z

(* ------------------------------------------------------------------ *)
(*  {1 move — zero delta}                                               *)
(* ------------------------------------------------------------------ *)

let test_move_zero _ =
  let w = empty_world () in
  let box = Physics.at_position (Math3d.vec3 0.5 20.0 0.5) in
  let d = Physics.move w box (Math3d.vec3 0.0 0.0 0.0) in
  assert_feq ~msg:"dx = 0" 0.0 d.x;
  assert_feq ~msg:"dy = 0" 0.0 d.y;
  assert_feq ~msg:"dz = 0" 0.0 d.z

(* ------------------------------------------------------------------ *)
(*  {1 move — free movement in empty world}                             *)
(* ------------------------------------------------------------------ *)

(** In a world with no blocks, every requested displacement is returned
    unchanged. *)
let test_free_x _ =
  let w = empty_world () in
  let box = Physics.at_position (Math3d.vec3 0.5 50.0 0.5) in
  let d = Physics.move w box (Math3d.vec3 3.0 0.0 0.0) in
  assert_feq ~msg:"free +x" 3.0 d.x

let test_free_neg_x _ =
  let w = empty_world () in
  let box = Physics.at_position (Math3d.vec3 0.5 50.0 0.5) in
  let d = Physics.move w box (Math3d.vec3 (-3.0) 0.0 0.0) in
  assert_feq ~msg:"free -x" (-3.0) d.x

let test_free_y _ =
  let w = empty_world () in
  let box = Physics.at_position (Math3d.vec3 0.5 50.0 0.5) in
  let d = Physics.move w box (Math3d.vec3 0.0 (-2.0) 0.0) in
  assert_feq ~msg:"free fall" (-2.0) d.y

let test_free_pos_y _ =
  let w = empty_world () in
  let box = Physics.at_position (Math3d.vec3 0.5 50.0 0.5) in
  let d = Physics.move w box (Math3d.vec3 0.0 2.0 0.0) in
  assert_feq ~msg:"free up" 2.0 d.y

let test_free_z _ =
  let w = empty_world () in
  let box = Physics.at_position (Math3d.vec3 0.5 50.0 0.5) in
  let d = Physics.move w box (Math3d.vec3 0.0 0.0 2.5) in
  assert_feq ~msg:"free +z" 2.5 d.z

let test_free_neg_z _ =
  let w = empty_world () in
  let box = Physics.at_position (Math3d.vec3 0.5 50.0 0.5) in
  let d = Physics.move w box (Math3d.vec3 0.0 0.0 (-2.5)) in
  assert_feq ~msg:"free -z" (-2.5) d.z

(** Free movement in three axes simultaneously. *)
let test_free_all_axes _ =
  let w = empty_world () in
  let box = Physics.at_position (Math3d.vec3 0.5 50.0 0.5) in
  let delta = Math3d.vec3 1.0 (-2.0) 3.0 in
  let d = Physics.move w box delta in
  assert_feq ~msg:"dx" 1.0 d.x;
  assert_feq ~msg:"dy" (-2.0) d.y;
  assert_feq ~msg:"dz" 3.0 d.z

(* ------------------------------------------------------------------ *)
(*  {1 move — sign preservation}                                        *)
(* ------------------------------------------------------------------ *)

(** Collision can reduce the magnitude of a displacement but must never reverse
    its sign. *)
let test_sign_preserved _ =
  let w = surface_world () in
  (* Player high in the air — might be blocked by surface below, but delta.x and
     delta.z should not flip sign. *)
  let pos = Math3d.vec3 100.0 100.0 100.0 in
  let box = Physics.at_position pos in
  let deltas = [ Math3d.vec3 (-3.0) 0.0 0.0; Math3d.vec3 0.0 0.0 (-3.0) ] in
  List.iter
    (fun delta ->
      let d = Physics.move w box delta in
      assert_bool "dx sign" (d.x *. delta.x >= 0.0);
      assert_bool "dz sign" (d.z *. delta.z >= 0.0))
    deltas

(* ------------------------------------------------------------------ *)
(*  {1 move — blocked by solid block}                                   *)
(* ------------------------------------------------------------------ *)

(** Place a Stone floor at y = 10, put the player's feet clearly above it (min.y
    > 11), then request a downward movement of 5 blocks. The returned delta must
    be negative (the player does move down) but strictly greater than -5. *)
let test_blocked_downward _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  (* Blank the chunk to Air, then add a stone floor at y = 10. *)
  let n = Config.chunk_size in
  for bx = 0 to n - 1 do
    for bz = 0 to n - 1 do
      for by = 0 to n - 1 do
        World.set_block w bx by bz Block.Air
      done
    done
  done;
  for bx = 0 to 1 do
    for bz = 0 to 1 do
      World.set_block w bx 10 bz Block.Stone
    done
  done;
  (* Player at y=14.0: feet (min.y) at 14 - 1.6 = 12.4 — above stone top
     (y=11). *)
  let pos = Math3d.vec3 0.5 14.0 0.5 in
  let box = Physics.at_position pos in
  let d = Physics.move w box (Math3d.vec3 0.0 (-5.0) 0.0) in
  assert_bool "dy < 0 (player falls)" (d.y < 0.0);
  assert_bool "dy > -5 (clipped by stone)" (d.y > -5.0)

(** Unobstructed axes are not clipped when one axis is blocked. *)
let test_blocked_y_free_xz _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  let n = Config.chunk_size in
  for bx = 0 to n - 1 do
    for bz = 0 to n - 1 do
      for by = 0 to n - 1 do
        World.set_block w bx by bz Block.Air
      done
    done
  done;
  (* Stone platform at y=10 covering x=0..1, z=0..1. After dx=1.0 the player
     x-range becomes [1.2,1.8] so x=1 is checked. The stone at (1,10,0) is then
     in range for the y resolution. *)
  for bx = 0 to 1 do
    for bz = 0 to 1 do
      World.set_block w bx 10 bz Block.Stone
    done
  done;
  let pos = Math3d.vec3 0.5 14.0 0.5 in
  let box = Physics.at_position pos in
  let d = Physics.move w box (Math3d.vec3 1.0 (-5.0) 1.0) in
  (* x and z are unrestricted; only y is clipped *)
  assert_feq ~msg:"free dx" 1.0 d.x;
  assert_feq ~msg:"free dz" 1.0 d.z;
  assert_bool "clipped dy" (d.y > -5.0)

(* ------------------------------------------------------------------ *)
(*  {1 at_position — optional height parameter}                        *)
(* ------------------------------------------------------------------ *)

(** With a custom height of 3.0, the y-extent of the AABB equals 3.0. *)
let test_custom_height _ =
  let box = Physics.at_position ~height:3.0 (Math3d.vec3 0.0 0.0 0.0) in
  assert_feq ~msg:"custom height = 3.0" 3.0 (box.max.y -. box.min.y)

(** The x and z extents are still player_width regardless of custom height. *)
let test_custom_height_xy_unchanged _ =
  let box = Physics.at_position ~height:3.0 (Math3d.vec3 0.0 0.0 0.0) in
  assert_feq ~msg:"x-extent = player_width" Config.player_width
    (box.max.x -. box.min.x);
  assert_feq ~msg:"z-extent = player_width" Config.player_width
    (box.max.z -. box.min.z)

(* ------------------------------------------------------------------ *)
(*  {1 has_ground_below}                                               *)
(* ------------------------------------------------------------------ *)

(** In an empty world (no chunks) there is no solid block below. *)
let test_has_ground_below_empty _ =
  let w = empty_world () in
  let box = Physics.at_position (Math3d.vec3 0.5 5.0 0.5) in
  assert_bool "no ground in empty world"
    (not (Physics.has_ground_below w box))

(** When a solid block is placed directly below the AABB, [has_ground_below]
    returns true.

    [at_position (0.5, 5.0, 0.5)]:
      min.y = 5.0 − 1.6 = 3.4   →   by = floor(3.4 − ε) = 3
      bx  = floor((0.2+0.8)/2 − ε) = floor(0.4999) = 0
      bz  = floor(0.4999) = 0
    So the function checks block (0, 3, 0). *)
let test_has_ground_below_solid _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  let n = Config.chunk_size in
  for bx = 0 to n - 1 do
    for by = 0 to n - 1 do
      for bz = 0 to n - 1 do
        World.set_block w bx by bz Block.Air
      done
    done
  done;
  World.set_block w 0 3 0 Block.Stone;
  let box = Physics.at_position (Math3d.vec3 0.5 5.0 0.5) in
  assert_bool "ground detected below player" (Physics.has_ground_below w box)

(** After removing the block, [has_ground_below] reverts to false. *)
let test_has_ground_below_removed _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  let n = Config.chunk_size in
  for bx = 0 to n - 1 do
    for by = 0 to n - 1 do
      for bz = 0 to n - 1 do
        World.set_block w bx by bz Block.Air
      done
    done
  done;
  World.set_block w 0 3 0 Block.Stone;
  World.set_block w 0 3 0 Block.Air;
  let box = Physics.at_position (Math3d.vec3 0.5 5.0 0.5) in
  assert_bool "no ground after block removed"
    (not (Physics.has_ground_below w box))

(* ------------------------------------------------------------------ *)
(*  {1 move — blocked in +X and +Z}                                   *)
(* ------------------------------------------------------------------ *)

(** A stone wall at x = 5 clips a rightward (+X) delta. *)
let test_blocked_positive_x _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  let n = Config.chunk_size in
  for bx = 0 to n - 1 do
    for by = 0 to n - 1 do
      for bz = 0 to n - 1 do
        World.set_block w bx by bz Block.Air
      done
    done
  done;
  for by = 0 to n - 1 do
    for bz = 0 to n - 1 do
      World.set_block w 5 by bz Block.Stone
    done
  done;
  (* player at x=3.0: max.x = 3.3, stone wall starts at x=5 *)
  let pos = Math3d.vec3 3.0 8.0 0.5 in
  let box = Physics.at_position pos in
  let d = Physics.move w box (Math3d.vec3 10.0 0.0 0.0) in
  assert_bool "+x clipped by stone wall (dx < 10)" (d.x < 10.0);
  assert_bool "+x movement positive (dx > 0)" (d.x > 0.0)

(** A stone wall at z = 5 clips a forward (+Z) delta. *)
let test_blocked_positive_z _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  let n = Config.chunk_size in
  for bx = 0 to n - 1 do
    for by = 0 to n - 1 do
      for bz = 0 to n - 1 do
        World.set_block w bx by bz Block.Air
      done
    done
  done;
  for by = 0 to n - 1 do
    for bx = 0 to n - 1 do
      World.set_block w bx by 5 Block.Stone
    done
  done;
  let pos = Math3d.vec3 0.5 8.0 3.0 in
  let box = Physics.at_position pos in
  let d = Physics.move w box (Math3d.vec3 0.0 0.0 10.0) in
  assert_bool "+z clipped by stone wall (dz < 10)" (d.z < 10.0);
  assert_bool "+z movement positive (dz > 0)" (d.z > 0.0)

(* ------------------------------------------------------------------ *)
(*  QCheck property                                                     *)
(* ------------------------------------------------------------------ *)

(** In an empty world every requested displacement is returned unchanged. *)
let qcheck_free_movement_empty_world =
  let arb_delta = QCheck2.Gen.float_range (-5.0) 5.0 in
  QCheck2.Test.make ~name:"free_movement_in_empty_world" ~count:500
    (QCheck2.Gen.triple arb_delta arb_delta arb_delta) (fun (dx, dy, dz) ->
      let w = World.create () in
      let box = Physics.at_position (Math3d.vec3 0.5 50.0 0.5) in
      let d = Physics.move w box (Math3d.vec3 dx dy dz) in
      abs_float (d.x -. dx) < 1e-9
      && abs_float (d.y -. dy) < 1e-9
      && abs_float (d.z -. dz) < 1e-9)

let tests =
  "Physics"
  >::: [
         (* at_position dimensions *)
         "width" >:: test_width;
         "height" >:: test_height;
         "depth" >:: test_depth;
         (* centering *)
         "centered_x" >:: test_centered_x;
         "centered_z" >:: test_centered_z;
         (* min < max *)
         "min_lt_max" >:: test_min_lt_max;
         (* shift *)
         "shift" >:: test_shift;
         (* move: zero *)
         "move_zero" >:: test_move_zero;
         (* move: free *)
         "free_x" >:: test_free_x;
         "free_neg_x" >:: test_free_neg_x;
         "free_y" >:: test_free_y;
         "free_pos_y" >:: test_free_pos_y;
         "free_z" >:: test_free_z;
         "free_neg_z" >:: test_free_neg_z;
         "free_all_axes" >:: test_free_all_axes;
         (* sign *)
         "sign_preserved" >:: test_sign_preserved;
         (* collision *)
         "blocked_down" >:: test_blocked_downward;
         "blocked_y_free_xz" >:: test_blocked_y_free_xz;
         (* custom height *)
         "custom_height" >:: test_custom_height;
         "custom_height_xy" >:: test_custom_height_xy_unchanged;
         (* has_ground_below *)
         "ground_below_empty" >:: test_has_ground_below_empty;
         "ground_below_solid" >:: test_has_ground_below_solid;
         "ground_below_removed" >:: test_has_ground_below_removed;
         (* blocked +X and +Z *)
         "blocked_pos_x" >:: test_blocked_positive_x;
         "blocked_pos_z" >:: test_blocked_positive_z;
         (* qcheck *)
         QCheck_ounit.to_ounit2_test qcheck_free_movement_empty_world;
       ]

let _ = run_test_tt_main tests
