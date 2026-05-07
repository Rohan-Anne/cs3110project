(** Tests for the {!Frustum} module.

    {1 Module Overview}
    [Frustum] extracts six inward-facing planes from a model-view-projection
    matrix (Gribb-Hartmann method) and provides a conservative AABB-vs-frustum
    overlap test used to skip drawing chunks that lie outside the view.

    {1 Invariants under test}

    {2 [of_mvp]}
    - Defined for any [Math3d.mat4]; never raises.
    - For a non-degenerate MVP each extracted plane has a unit normal.
    - The result is opaque, so every other property is observed via
      [intersects_aabb].

    {2 [intersects_aabb]}
    - Conservative: if any part of an AABB is inside the frustum, the test
      returns [true]. False positives (AABB entirely outside but reported
      inside) are allowed near frustum corners and are measured rather than
      asserted-against.
    - For a standard OpenGL perspective × identity-view MVP (camera at the
      origin, looking down −Z):
      {ul
      {- An AABB straddling [(0, 0, −10)] (well inside view volume) is
         reported inside.}
      {- An AABB at [(0, 0, +10)] (behind the camera, past the near plane)
         is reported outside.}
      {- An AABB at [(0, 0, −10000)] (well past the far plane) is reported
         outside.}
      {- An AABB at [(10000, 0, −10)] (far off the right edge) is reported
         outside.}}
    - Containment property: any AABB that covers the camera-forward point
      [(0, 0, −10)] is reported inside (since it intersects the frustum).
    *)

open OUnit2

let pi = Float.pi

(* ------------------------------------------------------------------ *)
(*  Helpers                                                             *)
(* ------------------------------------------------------------------ *)

(** Standard test MVP: 60° vertical FOV, square aspect, near=0.1, far=100,
    camera at origin looking down −Z. With identity view, MVP = projection. *)
let std_mvp () =
  Math3d.perspective ~fov_y_radians:(pi /. 3.0) ~aspect:1.0 ~near:0.1
    ~far:100.0

(** Build an AABB centred at [(cx, cy, cz)] with the given half-extents. *)
let aabb_centered (cx, cy, cz) ~half =
  let mn = Math3d.vec3 (cx -. half) (cy -. half) (cz -. half) in
  let mx = Math3d.vec3 (cx +. half) (cy +. half) (cz +. half) in
  (mn, mx)

let test_of_mvp_does_not_raise _ =
  (* Even a singular matrix (all zeros) shouldn't raise; degenerate planes
     are allowed by the RI. *)
  let _ = Frustum.of_mvp (Array.make 16 0.0) in
  let _ = Frustum.of_mvp (std_mvp ()) in
  let _ = Frustum.of_mvp (Math3d.identity ()) in
  ()

(* ------------------------------------------------------------------ *)
(*  Concrete in/out cases for a standard perspective frustum            *)
(* ------------------------------------------------------------------ *)

let test_aabb_in_view_is_inside _ =
  let f = Frustum.of_mvp (std_mvp ()) in
  let mn, mx = aabb_centered (0.0, 0.0, -10.0) ~half:0.5 in
  assert_bool "AABB centred 10 units in front of the camera is visible"
    (Frustum.intersects_aabb f ~min:mn ~max:mx)

let test_aabb_behind_camera_is_outside _ =
  let f = Frustum.of_mvp (std_mvp ()) in
  let mn, mx = aabb_centered (0.0, 0.0, 10.0) ~half:0.5 in
  assert_bool "AABB behind the camera is culled"
    (not (Frustum.intersects_aabb f ~min:mn ~max:mx))

let test_aabb_past_far_is_outside _ =
  let f = Frustum.of_mvp (std_mvp ()) in
  let mn, mx = aabb_centered (0.0, 0.0, -10000.0) ~half:0.5 in
  assert_bool "AABB well past the far plane is culled"
    (not (Frustum.intersects_aabb f ~min:mn ~max:mx))

let test_aabb_far_off_axis_is_outside _ =
  let f = Frustum.of_mvp (std_mvp ()) in
  let mn, mx = aabb_centered (10000.0, 0.0, -10.0) ~half:0.5 in
  assert_bool "AABB 10 km off to the side is culled"
    (not (Frustum.intersects_aabb f ~min:mn ~max:mx));
  let mn, mx = aabb_centered (0.0, 10000.0, -10.0) ~half:0.5 in
  assert_bool "AABB 10 km above is culled"
    (not (Frustum.intersects_aabb f ~min:mn ~max:mx))

(** Containment: a point known to be inside the frustum forces any AABB
    covering it to be reported inside. *)
let test_aabb_containing_inside_point_is_inside _ =
  let f = Frustum.of_mvp (std_mvp ()) in
  let mn, mx = aabb_centered (0.0, 0.0, -10.0) ~half:50.0 in
  assert_bool "Big AABB containing the inside point is reported visible"
    (Frustum.intersects_aabb f ~min:mn ~max:mx)

(** Sanity check: degenerate min=max AABB at an interior point is treated
    as inside (the half-spaces all evaluate >= 0 at that point). *)
let test_point_aabb_inside _ =
  let f = Frustum.of_mvp (std_mvp ()) in
  let p = Math3d.vec3 0.0 0.0 (-10.0) in
  assert_bool "point AABB at interior is inside"
    (Frustum.intersects_aabb f ~min:p ~max:p)

(* qcheck tests *)

let arb_coord = QCheck2.Gen.float_range (-1000.0) 1000.0
let arb_half = QCheck2.Gen.float_range 0.5 50.0

(** Property: any AABB that contains the known-interior point [(0, 0, −10)]
    must be reported inside, regardless of how big or oddly placed it is.
    This catches sign errors in plane extraction and AABB orientation. *)
let qcheck_aabb_containing_known_point =
  QCheck2.Test.make ~name:"aabb_containing_in_view_point_is_inside"
    ~count:1000
    (QCheck2.Gen.quad arb_coord arb_coord arb_coord arb_half)
    (fun (ox, oy, oz, half) ->
      let f = Frustum.of_mvp (std_mvp ()) in
      let inside_pt_x = 0.0 and inside_pt_y = 0.0 and inside_pt_z = -10.0 in
      (* extend half-extents far enough that the AABB is guaranteed to cover
         the interior point regardless of the random offset *)
      let dx = abs_float (ox -. inside_pt_x) in
      let dy = abs_float (oy -. inside_pt_y) in
      let dz = abs_float (oz -. inside_pt_z) in
      let h = half +. Float.max dx (Float.max dy dz) in
      let mn = Math3d.vec3 (ox -. h) (oy -. h) (oz -. h) in
      let mx = Math3d.vec3 (ox +. h) (oy +. h) (oz +. h) in
      Frustum.intersects_aabb f ~min:mn ~max:mx)

(** Property: any AABB lying entirely on the wrong side of the near plane is
    culled. Camera looks down [−Z] and near = 0.1, so the AABB is placed at
    [z ≥ 1.0]. *)
let qcheck_far_behind_camera_is_culled =
  let arb_xy = QCheck2.Gen.float_range (-100.0) 100.0 in
  let arb_h = QCheck2.Gen.float_range 0.5 5.0 in
  QCheck2.Test.make ~name:"aabb_behind_near_plane_is_culled" ~count:1000
    (QCheck2.Gen.triple arb_xy arb_xy arb_h) (fun (cx, cy, half) ->
      let f = Frustum.of_mvp (std_mvp ()) in
      let cz = 1.0 +. half in
      let mn = Math3d.vec3 (cx -. half) (cy -. half) (cz -. half) in
      let mx = Math3d.vec3 (cx +. half) (cy +. half) (cz +. half) in
      not (Frustum.intersects_aabb f ~min:mn ~max:mx))

let tests =
  "Frustum"
  >::: [
         "of_mvp_does_not_raise" >:: test_of_mvp_does_not_raise;
         "aabb_in_view_inside" >:: test_aabb_in_view_is_inside;
         "aabb_behind_camera_outside" >:: test_aabb_behind_camera_is_outside;
         "aabb_past_far_outside" >:: test_aabb_past_far_is_outside;
         "aabb_far_off_axis_outside" >:: test_aabb_far_off_axis_is_outside;
         "aabb_containing_inside_point"
         >:: test_aabb_containing_inside_point_is_inside;
         "point_aabb_inside" >:: test_point_aabb_inside;
         (* qcheck tests *)
         QCheck_ounit.to_ounit2_test qcheck_aabb_containing_known_point;
         QCheck_ounit.to_ounit2_test qcheck_far_behind_camera_is_culled;
       ]

let _ = run_test_tt_main tests
