(** Tests for the {!Math3d} module.

    {1 Module Overview}
    [Math3d] provides the 3-D linear-algebra primitives used throughout the
    renderer and physics engine: [vec3] (3-component float vector) and [mat4]
    (4×4 column-major float matrix).

    {1 Invariants — vec3}
    - [add] is commutative: [add a b = add b a].
    - [add] has the zero vector as its identity: [add v zero = v].
    - [sub a a = zero].
    - [scale v 0.0 = zero]; [scale v 1.0 = v].
    - [length v ≥ 0] for all [v]; [length zero = 0].
    - [length (normalize v) = 1] when [v ≠ zero].
    - [normalize zero = zero] (degenerate case, no division by zero).
    - [normalize] is idempotent: [normalize (normalize v) = normalize v].

    {1 Invariants — mat4}
    - [identity] is the neutral element for [multiply]: [multiply I A = A] and
      [multiply A I = A].
    - [multiply] is associative (tested on concrete matrices).
    - [rotation_x 0.0 = identity]; [rotation_y 0.0 = identity].
    - [rotation_x (2π) ≈ identity]; [rotation_y (2π) ≈ identity].
    - [translation ~x ~y ~z] stores [x/y/z] in column 3 (indices 12–14).
    - [perspective] has [-1] at index 11 and [0] at index 15 (OpenGL
      convention).
    - [view_from_camera] is a 16-element array; distinct camera states produce
      distinct view matrices. *)

open OUnit2

let pi = Float.pi
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

let assert_m4 ?(eps = eps) ~msg (a : Math3d.mat4) (b : Math3d.mat4) =
  assert_equal ~msg:"length 16" 16 (Array.length a);
  assert_equal ~msg:"length 16" 16 (Array.length b);
  for i = 0 to 15 do
    assert_feq ~eps ~msg:(Printf.sprintf "%s[%d]" msg i) a.(i) b.(i)
  done

let zero3 = Math3d.vec3 0.0 0.0 0.0

(* ------------------------------------------------------------------ *)
(*  {1 vec3 — constructor}                                              *)
(* ------------------------------------------------------------------ *)

let test_vec3_fields _ =
  let v = Math3d.vec3 1.0 2.0 3.0 in
  assert_feq ~msg:"x" 1.0 v.x;
  assert_feq ~msg:"y" 2.0 v.y;
  assert_feq ~msg:"z" 3.0 v.z

let test_vec3_negatives _ =
  let v = Math3d.vec3 (-1.0) (-2.0) (-3.0) in
  assert_feq ~msg:"x" (-1.0) v.x;
  assert_feq ~msg:"y" (-2.0) v.y;
  assert_feq ~msg:"z" (-3.0) v.z

(* ------------------------------------------------------------------ *)
(*  {1 vec3 — add}                                                      *)
(* ------------------------------------------------------------------ *)

let test_add_basic _ =
  let a = Math3d.vec3 1.0 2.0 3.0 in
  let b = Math3d.vec3 4.0 5.0 6.0 in
  assert_v3 ~msg:"a+b" (5.0, 7.0, 9.0) (Math3d.add a b)

let test_add_commutative _ =
  let a = Math3d.vec3 1.5 (-2.0) 0.5 in
  let b = Math3d.vec3 3.0 4.0 (-1.0) in
  let ab = Math3d.add a b and ba = Math3d.add b a in
  assert_v3 ~msg:"commutativity" (ab.x, ab.y, ab.z) ba

let test_add_zero_identity _ =
  let a = Math3d.vec3 7.0 (-3.0) 5.0 in
  assert_v3 ~msg:"a+0=a" (a.x, a.y, a.z) (Math3d.add a zero3)

let test_add_negation _ =
  let a = Math3d.vec3 1.0 2.0 3.0 in
  let neg_a = Math3d.scale a (-1.0) in
  assert_v3 ~msg:"a + (-a) = 0" (0.0, 0.0, 0.0) (Math3d.add a neg_a)

(* ------------------------------------------------------------------ *)
(*  {1 vec3 — sub}                                                      *)
(* ------------------------------------------------------------------ *)

let test_sub_basic _ =
  let a = Math3d.vec3 5.0 7.0 9.0 in
  let b = Math3d.vec3 1.0 2.0 3.0 in
  assert_v3 ~msg:"a-b" (4.0, 5.0, 6.0) (Math3d.sub a b)

let test_sub_self_is_zero _ =
  let a = Math3d.vec3 3.0 (-1.0) 4.0 in
  assert_v3 ~msg:"a-a=0" (0.0, 0.0, 0.0) (Math3d.sub a a)

let test_sub_zero _ =
  let a = Math3d.vec3 3.0 (-1.0) 4.0 in
  assert_v3 ~msg:"a-0=a" (a.x, a.y, a.z) (Math3d.sub a zero3)

let test_add_sub_roundtrip _ =
  let a = Math3d.vec3 1.0 2.0 3.0 in
  let b = Math3d.vec3 4.0 5.0 6.0 in
  let r = Math3d.sub (Math3d.add a b) b in
  assert_v3 ~msg:"(a+b)-b=a" (a.x, a.y, a.z) r

(* ------------------------------------------------------------------ *)
(*  {1 vec3 — scale}                                                    *)
(* ------------------------------------------------------------------ *)

let test_scale_basic _ =
  let v = Math3d.vec3 1.0 2.0 3.0 in
  assert_v3 ~msg:"scale 3" (3.0, 6.0, 9.0) (Math3d.scale v 3.0)

let test_scale_zero _ =
  let v = Math3d.vec3 5.0 5.0 5.0 in
  assert_v3 ~msg:"scale 0" (0.0, 0.0, 0.0) (Math3d.scale v 0.0)

let test_scale_one_identity _ =
  let v = Math3d.vec3 1.5 (-2.5) 3.5 in
  assert_v3 ~msg:"scale 1" (v.x, v.y, v.z) (Math3d.scale v 1.0)

let test_scale_negate _ =
  let v = Math3d.vec3 1.0 2.0 3.0 in
  assert_v3 ~msg:"scale -1" (-1.0, -2.0, -3.0) (Math3d.scale v (-1.0))

let test_scale_negative _ =
  let v = Math3d.vec3 2.0 4.0 6.0 in
  assert_v3 ~msg:"scale 0.5" (1.0, 2.0, 3.0) (Math3d.scale v 0.5)

(* ------------------------------------------------------------------ *)
(*  {1 vec3 — length}                                                   *)
(* ------------------------------------------------------------------ *)

let test_length_zero_vec _ =
  assert_feq ~msg:"length zero" 0.0 (Math3d.length zero3)

let test_length_unit_x _ =
  assert_feq ~msg:"length (1,0,0)" 1.0 (Math3d.length (Math3d.vec3 1.0 0.0 0.0))

let test_length_pythagorean _ =
  assert_feq ~msg:"3-4-5" 5.0 (Math3d.length (Math3d.vec3 3.0 4.0 0.0))

let test_length_3d _ =
  (* |(1,2,3)| = sqrt(14) *)
  assert_feq ~msg:"sqrt14" (sqrt 14.0) (Math3d.length (Math3d.vec3 1.0 2.0 3.0))

let test_length_nonneg _ =
  let vecs =
    [
      Math3d.vec3 1.0 0.0 0.0;
      Math3d.vec3 (-1.0) 0.0 0.0;
      Math3d.vec3 3.0 4.0 0.0;
      Math3d.vec3 (-3.0) (-4.0) 0.0;
    ]
  in
  List.iter (fun v -> assert_bool "length >= 0" (Math3d.length v >= 0.0)) vecs

let test_length_scale _ =
  (* |s*v| = |s| * |v| *)
  let v = Math3d.vec3 1.0 2.0 3.0 in
  let s = 3.0 in
  assert_feq ~msg:"|s*v|=|s|*|v|"
    (abs_float s *. Math3d.length v)
    (Math3d.length (Math3d.scale v s))

(* ------------------------------------------------------------------ *)
(*  {1 vec3 — normalize}                                               *)
(* ------------------------------------------------------------------ *)

let test_normalize_unit_length _ =
  let v = Math3d.vec3 3.0 4.0 0.0 in
  assert_feq ~msg:"normalized length" 1.0 (Math3d.length (Math3d.normalize v))

let test_normalize_direction_x _ =
  let v = Math3d.vec3 7.0 0.0 0.0 in
  assert_v3 ~msg:"normalize x-axis" (1.0, 0.0, 0.0) (Math3d.normalize v)

let test_normalize_direction_diagonal _ =
  let v = Math3d.vec3 1.0 1.0 0.0 in
  let n = Math3d.normalize v in
  let r = 1.0 /. sqrt 2.0 in
  assert_feq ~msg:"x" r n.x;
  assert_feq ~msg:"y" r n.y;
  assert_feq ~msg:"z" 0.0 n.z

(** [normalize zero = zero] — no division by zero. *)
let test_normalize_zero_safe _ =
  let n = Math3d.normalize zero3 in
  assert_feq ~msg:"normalize(0) length = 0" 0.0 (Math3d.length n)

(** [normalize] is idempotent. *)
let test_normalize_idempotent _ =
  let v = Math3d.vec3 3.0 1.0 (-2.0) in
  let n1 = Math3d.normalize v in
  let n2 = Math3d.normalize n1 in
  assert_v3 ~msg:"idempotent" (n1.x, n1.y, n1.z) n2

(* ------------------------------------------------------------------ *)
(*  {1 mat4 — identity}                                                 *)
(* ------------------------------------------------------------------ *)

let test_identity_length _ =
  assert_equal ~msg:"16 elements" 16 (Array.length (Math3d.identity ()))

let test_identity_diagonal _ =
  let m = Math3d.identity () in
  List.iter
    (fun i -> assert_feq ~msg:(Printf.sprintf "diag[%d]" i) 1.0 m.(i))
    [ 0; 5; 10; 15 ]

let test_identity_off_diagonal _ =
  let m = Math3d.identity () in
  List.iter
    (fun i -> assert_feq ~msg:(Printf.sprintf "off[%d]" i) 0.0 m.(i))
    [ 1; 2; 3; 4; 6; 7; 8; 9; 11; 12; 13; 14 ]

(* ------------------------------------------------------------------ *)
(*  {1 mat4 — multiply}                                                 *)
(* ------------------------------------------------------------------ *)

let test_multiply_identity_left _ =
  let a =
    [| 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10.; 11.; 12.; 13.; 14.; 15.; 16. |]
  in
  assert_m4 ~msg:"I*A=A" a (Math3d.multiply (Math3d.identity ()) a)

let test_multiply_identity_right _ =
  let a =
    [| 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10.; 11.; 12.; 13.; 14.; 15.; 16. |]
  in
  assert_m4 ~msg:"A*I=A" a (Math3d.multiply a (Math3d.identity ()))

(** [multiply] is associative: [(A*B)*C = A*(B*C)]. *)
let test_multiply_associative _ =
  let rx = Math3d.rotation_x 0.3 in
  let ry = Math3d.rotation_y 0.7 in
  let t = Math3d.translation ~x:1.0 ~y:2.0 ~z:3.0 in
  let lhs = Math3d.multiply (Math3d.multiply rx ry) t in
  let rhs = Math3d.multiply rx (Math3d.multiply ry t) in
  assert_m4 ~eps:1e-4 ~msg:"(A*B)*C = A*(B*C)" lhs rhs

(* ------------------------------------------------------------------ *)
(*  {1 mat4 — translation}                                              *)
(* ------------------------------------------------------------------ *)

(** Translation stores [x/y/z] at indices 12–14 in column-major layout. *)
let test_translation_column3 _ =
  let m = Math3d.translation ~x:1.0 ~y:2.0 ~z:3.0 in
  assert_feq ~msg:"tx at [12]" 1.0 m.(12);
  assert_feq ~msg:"ty at [13]" 2.0 m.(13);
  assert_feq ~msg:"tz at [14]" 3.0 m.(14);
  assert_feq ~msg:"w  at [15]" 1.0 m.(15)

(** Upper-left 3×3 of a translation is the identity block. *)
let test_translation_identity_block _ =
  let m = Math3d.translation ~x:5.0 ~y:(-3.0) ~z:7.0 in
  assert_feq ~msg:"[0]" 1.0 m.(0);
  assert_feq ~msg:"[5]" 1.0 m.(5);
  assert_feq ~msg:"[10]" 1.0 m.(10)

(** Composing two translations equals translating by the sum. *)
let test_translation_compose _ =
  let t1 = Math3d.translation ~x:1.0 ~y:2.0 ~z:3.0 in
  let t2 = Math3d.translation ~x:4.0 ~y:5.0 ~z:6.0 in
  let composed = Math3d.multiply t1 t2 in
  let combined = Math3d.translation ~x:5.0 ~y:7.0 ~z:9.0 in
  assert_m4 ~msg:"T(a)*T(b)=T(a+b)" combined composed

(* ------------------------------------------------------------------ *)
(*  {1 mat4 — rotation_x}                                              *)
(* ------------------------------------------------------------------ *)

(** [rotation_x 0.0 = identity]. *)
let test_rotation_x_zero _ =
  assert_m4 ~msg:"rot_x(0)=I" (Math3d.identity ()) (Math3d.rotation_x 0.0)

(** [rotation_x (2π) ≈ identity]. *)
let test_rotation_x_full_cycle _ =
  assert_m4 ~eps:1e-5 ~msg:"rot_x(2pi)=I" (Math3d.identity ())
    (Math3d.rotation_x (2.0 *. pi))

(** At π/2: cos = 0, sin = 1. The column-major encoding places sin at index [6]
    and -sin at index [9]. *)
let test_rotation_x_halfpi _ =
  let m = Math3d.rotation_x (pi /. 2.0) in
  assert_feq ~msg:"c at [5]" 0.0 m.(5);
  assert_feq ~msg:"s at [6]" 1.0 m.(6);
  assert_feq ~msg:"-s at [9]" (-1.0) m.(9);
  assert_feq ~msg:"c at [10]" 0.0 m.(10)

(** Two half-rotations compose to a full rotation (identity). *)
let test_rotation_x_pi_twice _ =
  let half = Math3d.rotation_x pi in
  let full = Math3d.multiply half half in
  assert_m4 ~eps:1e-5 ~msg:"rot_x(pi)^2 = I" (Math3d.identity ()) full

(* ------------------------------------------------------------------ *)
(*  {1 mat4 — rotation_y}                                              *)
(* ------------------------------------------------------------------ *)

let test_rotation_y_zero _ =
  assert_m4 ~msg:"rot_y(0)=I" (Math3d.identity ()) (Math3d.rotation_y 0.0)

let test_rotation_y_full_cycle _ =
  assert_m4 ~eps:1e-5 ~msg:"rot_y(2pi)=I" (Math3d.identity ())
    (Math3d.rotation_y (2.0 *. pi))

(** At π/2: cos = 0, sin = 1. Column-major: -sin at [2], sin at [8]. *)
let test_rotation_y_halfpi _ =
  let m = Math3d.rotation_y (pi /. 2.0) in
  assert_feq ~msg:"c at [0]" 0.0 m.(0);
  assert_feq ~msg:"-s at [2]" (-1.0) m.(2);
  assert_feq ~msg:"s at [8]" 1.0 m.(8);
  assert_feq ~msg:"c at [10]" 0.0 m.(10)

let test_rotation_y_pi_twice _ =
  let half = Math3d.rotation_y pi in
  let full = Math3d.multiply half half in
  assert_m4 ~eps:1e-5 ~msg:"rot_y(pi)^2 = I" (Math3d.identity ()) full

(* ------------------------------------------------------------------ *)
(*  {1 mat4 — perspective}                                             *)
(* ------------------------------------------------------------------ *)

(** The projection matrix is 16 elements and obeys OpenGL conventions: index
    [11] = -1, index [15] = 0. *)
let test_perspective_shape _ =
  let m =
    Math3d.perspective ~fov_y_radians:(pi /. 3.0) ~aspect:1.0 ~near:0.1
      ~far:1000.0
  in
  assert_equal ~msg:"16 elements" 16 (Array.length m);
  assert_feq ~msg:"[11] = -1" (-1.0) m.(11);
  assert_feq ~msg:"[15] = 0" 0.0 m.(15)

(** With a square aspect ratio, the horizontal and vertical scale factors are
    equal: [m[0] = m[5]]. *)
let test_perspective_square_aspect _ =
  let m =
    Math3d.perspective ~fov_y_radians:(pi /. 2.0) ~aspect:1.0 ~near:0.1
      ~far:100.0
  in
  assert_feq ~msg:"m[0]=m[5]" m.(0) m.(5)

(** Halving the FOV should increase the zoom factor (m[5]). *)
let test_perspective_fov_effect _ =
  let m_narrow =
    Math3d.perspective ~fov_y_radians:(pi /. 6.0) ~aspect:1.0 ~near:0.1
      ~far:100.0
  in
  let m_wide =
    Math3d.perspective ~fov_y_radians:(pi /. 2.0) ~aspect:1.0 ~near:0.1
      ~far:100.0
  in
  assert_bool "narrower FOV → larger zoom factor" (m_narrow.(5) > m_wide.(5))

(* ------------------------------------------------------------------ *)
(*  {1 mat4 — view_from_camera}                                         *)
(* ------------------------------------------------------------------ *)

let test_view_length _ =
  let v =
    Math3d.view_from_camera ~position:(Math3d.vec3 0.0 0.0 0.0) ~yaw:0.0
      ~pitch:0.0
  in
  assert_equal ~msg:"16 elements" 16 (Array.length v)

(** Different yaw values produce different view matrices. *)
let test_view_differs_by_yaw _ =
  let make_view yaw =
    Math3d.view_from_camera ~position:(Math3d.vec3 0.0 0.0 0.0) ~yaw ~pitch:0.0
  in
  let m1 = make_view 0.0 and m2 = make_view 1.0 in
  let same = Array.for_all2 (fun a b -> abs_float (a -. b) < eps) m1 m2 in
  assert_bool "yaw 0 ≠ yaw 1" (not same)

(** Different pitch values produce different view matrices. *)
let test_view_differs_by_pitch _ =
  let make_view pitch =
    Math3d.view_from_camera ~position:(Math3d.vec3 0.0 0.0 0.0) ~yaw:0.0 ~pitch
  in
  let m1 = make_view 0.0 and m2 = make_view 0.5 in
  let same = Array.for_all2 (fun a b -> abs_float (a -. b) < eps) m1 m2 in
  assert_bool "pitch 0 ≠ pitch 0.5" (not same)

(** Translating the camera right shifts the world translation column. *)
let test_view_translation_component _ =
  let v =
    Math3d.view_from_camera ~position:(Math3d.vec3 1.0 0.0 0.0) ~yaw:0.0
      ~pitch:0.0
  in
  (* At yaw=pitch=0, rotation is identity, so tx at index 12 should be -1.0 *)
  assert_feq ~msg:"tx = -pos.x" (-1.0) v.(12)

let tests =
  "Math3d"
  >::: [
         (* vec3 constructor *)
         "vec3_fields" >:: test_vec3_fields;
         "vec3_negatives" >:: test_vec3_negatives;
         (* add *)
         "add_basic" >:: test_add_basic;
         "add_commutative" >:: test_add_commutative;
         "add_zero_identity" >:: test_add_zero_identity;
         "add_negation" >:: test_add_negation;
         (* sub *)
         "sub_basic" >:: test_sub_basic;
         "sub_self_is_zero" >:: test_sub_self_is_zero;
         "sub_zero" >:: test_sub_zero;
         "add_sub_roundtrip" >:: test_add_sub_roundtrip;
         (* scale *)
         "scale_basic" >:: test_scale_basic;
         "scale_zero" >:: test_scale_zero;
         "scale_one_identity" >:: test_scale_one_identity;
         "scale_negate" >:: test_scale_negate;
         "scale_half" >:: test_scale_negative;
         (* length *)
         "length_zero_vec" >:: test_length_zero_vec;
         "length_unit_x" >:: test_length_unit_x;
         "length_pythagorean" >:: test_length_pythagorean;
         "length_3d" >:: test_length_3d;
         "length_nonneg" >:: test_length_nonneg;
         "length_scale" >:: test_length_scale;
         (* normalize *)
         "normalize_unit" >:: test_normalize_unit_length;
         "normalize_x" >:: test_normalize_direction_x;
         "normalize_diagonal" >:: test_normalize_direction_diagonal;
         "normalize_zero" >:: test_normalize_zero_safe;
         "normalize_idempotent" >:: test_normalize_idempotent;
         (* identity *)
         "identity_length" >:: test_identity_length;
         "identity_diagonal" >:: test_identity_diagonal;
         "identity_off_diag" >:: test_identity_off_diagonal;
         (* multiply *)
         "multiply_id_left" >:: test_multiply_identity_left;
         "multiply_id_right" >:: test_multiply_identity_right;
         "multiply_associative" >:: test_multiply_associative;
         (* translation *)
         "translation_column3" >:: test_translation_column3;
         "translation_id_block" >:: test_translation_identity_block;
         "translation_compose" >:: test_translation_compose;
         (* rotation_x *)
         "rot_x_zero" >:: test_rotation_x_zero;
         "rot_x_full_cycle" >:: test_rotation_x_full_cycle;
         "rot_x_halfpi" >:: test_rotation_x_halfpi;
         "rot_x_pi_twice" >:: test_rotation_x_pi_twice;
         (* rotation_y *)
         "rot_y_zero" >:: test_rotation_y_zero;
         "rot_y_full_cycle" >:: test_rotation_y_full_cycle;
         "rot_y_halfpi" >:: test_rotation_y_halfpi;
         "rot_y_pi_twice" >:: test_rotation_y_pi_twice;
         (* perspective *)
         "perspective_shape" >:: test_perspective_shape;
         "perspective_square" >:: test_perspective_square_aspect;
         "perspective_fov" >:: test_perspective_fov_effect;
         (* view_from_camera *)
         "view_length" >:: test_view_length;
         "view_yaw" >:: test_view_differs_by_yaw;
         "view_pitch" >:: test_view_differs_by_pitch;
         "view_translation" >:: test_view_translation_component;
       ]

let _ = run_test_tt_main tests
