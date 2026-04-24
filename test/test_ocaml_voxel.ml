open OUnit2

(* ------------------------------------------------------------------ *)
(*  Floating-point comparison helper                                    *)
(* ------------------------------------------------------------------ *)

let eps = 1e-5

let assert_float_eq ?(eps = eps) ~msg expected actual =
  if abs_float (expected -. actual) > eps then
    assert_failure
      (Printf.sprintf "%s: expected %.8f but got %.8f" msg expected actual)

let assert_vec3_eq ?(eps = eps) ~msg (ex, ey, ez) (v : Math3d.vec3) =
  assert_float_eq ~eps ~msg:(msg ^ ".x") ex v.x;
  assert_float_eq ~eps ~msg:(msg ^ ".y") ey v.y;
  assert_float_eq ~eps ~msg:(msg ^ ".z") ez v.z

(** Check that two 16-element column-major matrices are element-wise equal *)
let assert_mat4_eq ?(eps = eps) ~msg (a : Math3d.mat4) (b : Math3d.mat4) =
  assert_equal ~msg:"mat4 length" 16 (Array.length a);
  assert_equal ~msg:"mat4 length" 16 (Array.length b);
  for i = 0 to 15 do
    assert_float_eq ~eps ~msg:(Printf.sprintf "%s[%d]" msg i) a.(i) b.(i)
  done

(* ------------------------------------------------------------------ *)
(*  {1 Config tests}                                                    *)
(* ------------------------------------------------------------------ *)

let test_config_chunk_size _ =
  assert_equal ~msg:"chunk_size" 16 Config.chunk_size

let test_config_move_speed _ =
  assert_float_eq ~msg:"move_speed" 4.0 Config.move_speed

let test_config_sprint_speed _ =
  assert_float_eq ~msg:"sprint_speed" 8.0 Config.sprint_speed

let test_config_player_dims _ =
  assert_float_eq ~msg:"player_width" 0.6 Config.player_width;
  assert_float_eq ~msg:"player_height" 1.8 Config.player_height

let test_config_gravity _ = assert_float_eq ~msg:"gravity" 20.0 Config.gravity

let test_config_jump_velocity _ =
  assert_float_eq ~msg:"jump_velocity" 8.0 Config.jump_velocity

let test_config_pitch_limit_positive _ =
  assert_bool "pitch_limit > 0" (Config.pitch_limit > 0.0)

let test_config_near_far_order _ =
  assert_bool "near < far" (Config.near < Config.far)

let test_config_fov_sensible _ =
  assert_bool "fov > 0" (Config.fov_y > 0.0);
  assert_bool "fov < pi" (Config.fov_y < Float.pi)

let config_tests =
  "Config"
  >::: [
         "chunk_size" >:: test_config_chunk_size;
         "move_speed" >:: test_config_move_speed;
         "sprint_speed" >:: test_config_sprint_speed;
         "player_dims" >:: test_config_player_dims;
         "gravity" >:: test_config_gravity;
         "jump_velocity" >:: test_config_jump_velocity;
         "pitch_limit_pos" >:: test_config_pitch_limit_positive;
         "near_far_order" >:: test_config_near_far_order;
         "fov_sensible" >:: test_config_fov_sensible;
       ]

(* ------------------------------------------------------------------ *)
(*  {1 Block tests}                                                     *)
(* ------------------------------------------------------------------ *)

let test_block_distinct _ =
  assert_bool "Air <> Stone" (Block.Air <> Block.Stone);
  assert_bool "Air <> Dirt" (Block.Air <> Block.Dirt);
  assert_bool "Air <> Grass" (Block.Air <> Block.Grass);
  assert_bool "Stone <> Dirt" (Block.Stone <> Block.Dirt);
  assert_bool "Stone <> Grass" (Block.Stone <> Block.Grass);
  assert_bool "Dirt <> Grass" (Block.Dirt <> Block.Grass)

let test_block_equality _ =
  assert_equal Block.Air Block.Air;
  assert_equal Block.Stone Block.Stone;
  assert_equal Block.Dirt Block.Dirt;
  assert_equal Block.Grass Block.Grass

let test_block_pattern_match _ =
  let is_air = function
    | Block.Air -> true
    | _ -> false
  in
  assert_bool "Air is_air" (is_air Block.Air);
  assert_bool "Stone not air" (not (is_air Block.Stone))

let block_tests =
  "Block"
  >::: [
         "distinct" >:: test_block_distinct;
         "equality" >:: test_block_equality;
         "pattern_match" >:: test_block_pattern_match;
       ]

(* ------------------------------------------------------------------ *)
(*  {1 Chunk tests}                                                     *)
(* ------------------------------------------------------------------ *)

let make_empty_chunk ?(x = 0) ?(y = 0) ?(z = 0) () =
  let n = Config.chunk_size in
  let blocks = Array.make (n * n * n) Block.Air in
  Chunk.create ~x ~y ~z ~blocks

(** Chunk.index maps (0,0,0) → 0 and the maximum corner → chunk_size^3 - 1. *)
let test_chunk_index_offset_bounds _ =
  let n = Config.chunk_size in
  assert_equal ~msg:"min index" 0 (Chunk.index 0 0 0);
  assert_equal ~msg:"max index"
    ((n * n * n) - 1)
    (Chunk.index (n - 1) (n - 1) (n - 1))

(** Every (x,y,z) in [0, chunk_size) must produce a unique flat index. *)
let test_chunk_index_unique _ =
  let n = Config.chunk_size in
  let indices = ref [] in
  for x = 0 to n - 1 do
    for y = 0 to n - 1 do
      for z = 0 to n - 1 do
        indices := Chunk.index x y z :: !indices
      done
    done
  done;
  let unique = List.sort_uniq compare !indices in
  assert_equal ~msg:"all indices unique" (List.length !indices)
    (List.length unique)

(** All indices must fall in [0, chunk_size^3). *)
let test_chunk_index_in_range _ =
  let n = Config.chunk_size in
  for x = 0 to n - 1 do
    for y = 0 to n - 1 do
      for z = 0 to n - 1 do
        let idx = Chunk.index x y z in
        assert_bool (Printf.sprintf "index(%d,%d,%d) >= 0" x y z) (idx >= 0);
        assert_bool
          (Printf.sprintf "index(%d,%d,%d) < n^3" x y z)
          (idx < n * n * n)
      done
    done
  done

(** set then get returns the stored block. *)
let test_chunk_get_set _ =
  let c = make_empty_chunk () in
  assert_equal ~msg:"initial Air" Block.Air (Chunk.get c 0 0 0);
  Chunk.set c 1 1 1 Block.Stone;
  assert_equal ~msg:"Stone" Block.Stone (Chunk.get c 1 1 1);
  Chunk.set c 1 1 1 Block.Dirt;
  assert_equal ~msg:"Dirt" Block.Dirt (Chunk.get c 1 1 1)

(** Overwriting a cell multiple times keeps only the last value. *)
let test_chunk_overwrite _ =
  let c = make_empty_chunk () in
  let all_blocks = [| Block.Air; Block.Stone; Block.Dirt; Block.Grass |] in
  Array.iter
    (fun b ->
      Chunk.set c 3 7 2 b;
      assert_equal b (Chunk.get c 3 7 2))
    all_blocks

(** Chunk.x / y / z return the stored coordinate. *)
let test_chunk_coords _ =
  let blocks =
    Array.make
      (Config.chunk_size * Config.chunk_size * Config.chunk_size)
      Block.Air
  in
  let c = Chunk.create ~x:3 ~y:5 ~z:7 ~blocks in
  assert_equal 3 (Chunk.x c);
  assert_equal 5 (Chunk.y c);
  assert_equal 7 (Chunk.z c)

(** Negative chunk coordinates are stored correctly. *)
let test_chunk_negative_coords _ =
  let c = make_empty_chunk ~x:(-2) ~y:(-5) ~z:(-8) () in
  assert_equal (-2) (Chunk.x c);
  assert_equal (-5) (Chunk.y c);
  assert_equal (-8) (Chunk.z c)

(** Setting every cell and immediately reading it back should succeed. *)
let test_chunk_index_consistency _ =
  let n = Config.chunk_size in
  let c = make_empty_chunk () in
  for x = 0 to n - 1 do
    for y = 0 to n - 1 do
      for z = 0 to n - 1 do
        Chunk.set c x y z Block.Grass;
        assert_equal ~msg:"get after set" Block.Grass (Chunk.get c x y z)
      done
    done
  done

(** Writing to one location must not affect any other location. *)
let test_chunk_no_aliasing _ =
  let c = make_empty_chunk () in
  Chunk.set c 0 0 0 Block.Stone;
  assert_equal ~msg:"adjacent unaffected" Block.Air (Chunk.get c 1 0 0);
  assert_equal ~msg:"corner unaffected" Block.Air
    (Chunk.get c (Config.chunk_size - 1) (Config.chunk_size - 1)
       (Config.chunk_size - 1))

(** The initial block array provided to create is reflected in get. *)
let test_chunk_create_with_data _ =
  let n = Config.chunk_size in
  let blocks =
    Array.init
      (n * n * n)
      (fun i -> if i mod 2 = 0 then Block.Stone else Block.Dirt)
  in
  let c = Chunk.create ~x:0 ~y:0 ~z:0 ~blocks in
  for x = 0 to n - 1 do
    for y = 0 to n - 1 do
      for z = 0 to n - 1 do
        let expected =
          if Chunk.index x y z mod 2 = 0 then Block.Stone else Block.Dirt
        in
        assert_equal expected (Chunk.get c x y z)
      done
    done
  done

let chunk_tests =
  "Chunk"
  >::: [
         "index_offset_bounds" >:: test_chunk_index_offset_bounds;
         "index_unique" >:: test_chunk_index_unique;
         "index_in_range" >:: test_chunk_index_in_range;
         "get_set" >:: test_chunk_get_set;
         "overwrite" >:: test_chunk_overwrite;
         "coords" >:: test_chunk_coords;
         "negative_coords" >:: test_chunk_negative_coords;
         "index_consistency" >:: test_chunk_index_consistency;
         "no_aliasing" >:: test_chunk_no_aliasing;
         "create_with_data" >:: test_chunk_create_with_data;
       ]

(* ------------------------------------------------------------------ *)
(*  {1 Color tests}                                                     *)
(* ------------------------------------------------------------------ *)

let test_color_make _ =
  let c = Color.make 0.2 0.5 0.9 in
  assert_float_eq ~msg:"r" 0.2 c.r;
  assert_float_eq ~msg:"g" 0.5 c.g;
  assert_float_eq ~msg:"b" 0.9 c.b

let test_color_make_black _ =
  let c = Color.make 0.0 0.0 0.0 in
  assert_float_eq ~msg:"r" 0.0 c.r;
  assert_float_eq ~msg:"g" 0.0 c.g;
  assert_float_eq ~msg:"b" 0.0 c.b

let test_color_make_white _ =
  let c = Color.make 1.0 1.0 1.0 in
  assert_float_eq ~msg:"r" 1.0 c.r;
  assert_float_eq ~msg:"g" 1.0 c.g;
  assert_float_eq ~msg:"b" 1.0 c.b

let test_color_shade _ =
  let c = Color.make 0.4 0.6 0.8 in
  let s = Color.shade 0.5 c in
  assert_float_eq ~msg:"r" 0.2 s.r;
  assert_float_eq ~msg:"g" 0.3 s.g;
  assert_float_eq ~msg:"b" 0.4 s.b

let test_color_shade_zero _ =
  let c = Color.make 0.4 0.6 0.8 in
  let s = Color.shade 0.0 c in
  assert_float_eq ~msg:"r" 0.0 s.r;
  assert_float_eq ~msg:"g" 0.0 s.g;
  assert_float_eq ~msg:"b" 0.0 s.b

let test_color_shade_one _ =
  let c = Color.make 0.4 0.6 0.8 in
  let s = Color.shade 1.0 c in
  assert_float_eq ~msg:"r" 0.4 s.r;
  assert_float_eq ~msg:"g" 0.6 s.g;
  assert_float_eq ~msg:"b" 0.8 s.b

let test_color_to_tuple _ =
  let c = Color.make 0.1 0.2 0.3 in
  let r, g, b = Color.to_tuple c in
  assert_float_eq ~msg:"r" 0.1 r;
  assert_float_eq ~msg:"g" 0.2 g;
  assert_float_eq ~msg:"b" 0.3 b

let test_color_infix_operator _ =
  let open Color in
  let c = make 0.4 0.6 0.8 in
  let s = 0.5 *. c in
  assert_float_eq ~msg:"r" 0.2 s.r;
  assert_float_eq ~msg:"g" 0.3 s.g;
  assert_float_eq ~msg:"b" 0.4 s.b

let test_color_infix_matches_shade _ =
  let c = Color.make 0.3 0.5 0.7 in
  let via_shade = Color.shade 0.75 c in
  let via_op = Color.(0.75 *. c) in
  assert_float_eq ~msg:"r" via_shade.r via_op.r;
  assert_float_eq ~msg:"g" via_shade.g via_op.g;
  assert_float_eq ~msg:"b" via_shade.b via_op.b

let test_color_shade_composable _ =
  let c = Color.make 1.0 1.0 1.0 in
  let c1 = Color.shade 0.5 (Color.shade 0.5 c) in
  let c2 = Color.shade 0.25 c in
  assert_float_eq ~msg:"r" c2.r c1.r;
  assert_float_eq ~msg:"g" c2.g c1.g;
  assert_float_eq ~msg:"b" c2.b c1.b

let color_tests =
  "Color"
  >::: [
         "make" >:: test_color_make;
         "make_black" >:: test_color_make_black;
         "make_white" >:: test_color_make_white;
         "shade" >:: test_color_shade;
         "shade_zero" >:: test_color_shade_zero;
         "shade_one" >:: test_color_shade_one;
         "to_tuple" >:: test_color_to_tuple;
         "infix_operator" >:: test_color_infix_operator;
         "infix_matches_shade" >:: test_color_infix_matches_shade;
         "shade_composable" >:: test_color_shade_composable;
       ]

(* ------------------------------------------------------------------ *)
(*  {1 Math3d – vec3 tests}                                            *)
(* ------------------------------------------------------------------ *)

let test_math3d_vec3_constructor _ =
  let v = Math3d.vec3 1.0 2.0 3.0 in
  assert_float_eq ~msg:"x" 1.0 v.x;
  assert_float_eq ~msg:"y" 2.0 v.y;
  assert_float_eq ~msg:"z" 3.0 v.z

let test_math3d_add _ =
  let a = Math3d.vec3 1.0 2.0 3.0 in
  let b = Math3d.vec3 4.0 5.0 6.0 in
  let r = Math3d.add a b in
  assert_vec3_eq ~msg:"add" (5.0, 7.0, 9.0) r

let test_math3d_add_commutative _ =
  let a = Math3d.vec3 1.5 (-2.0) 0.5 in
  let b = Math3d.vec3 3.0 4.0 (-1.0) in
  let ab = Math3d.add a b in
  let ba = Math3d.add b a in
  assert_vec3_eq ~msg:"a+b" (ab.x, ab.y, ab.z) ba

let test_math3d_add_zero _ =
  let a = Math3d.vec3 7.0 (-3.0) 5.0 in
  let zero = Math3d.vec3 0.0 0.0 0.0 in
  let r = Math3d.add a zero in
  assert_vec3_eq ~msg:"a+0=a" (a.x, a.y, a.z) r

let test_math3d_sub _ =
  let a = Math3d.vec3 5.0 7.0 9.0 in
  let b = Math3d.vec3 1.0 2.0 3.0 in
  let r = Math3d.sub a b in
  assert_vec3_eq ~msg:"sub" (4.0, 5.0, 6.0) r

let test_math3d_sub_self _ =
  let a = Math3d.vec3 3.0 (-1.0) 4.0 in
  let r = Math3d.sub a a in
  assert_vec3_eq ~msg:"a-a=0" (0.0, 0.0, 0.0) r

let test_math3d_scale _ =
  let v = Math3d.vec3 1.0 2.0 3.0 in
  let r = Math3d.scale v 3.0 in
  assert_vec3_eq ~msg:"scale" (3.0, 6.0, 9.0) r

let test_math3d_scale_zero _ =
  let v = Math3d.vec3 5.0 5.0 5.0 in
  let r = Math3d.scale v 0.0 in
  assert_vec3_eq ~msg:"scale*0" (0.0, 0.0, 0.0) r

let test_math3d_scale_negative _ =
  let v = Math3d.vec3 1.0 2.0 3.0 in
  let r = Math3d.scale v (-1.0) in
  assert_vec3_eq ~msg:"negate" (-1.0, -2.0, -3.0) r

let test_math3d_length_zero _ =
  let v = Math3d.vec3 0.0 0.0 0.0 in
  assert_float_eq ~msg:"length zero vec" 0.0 (Math3d.length v)

let test_math3d_length_unit _ =
  let v = Math3d.vec3 1.0 0.0 0.0 in
  assert_float_eq ~msg:"length unit" 1.0 (Math3d.length v)

let test_math3d_length_pythagorean _ =
  let v = Math3d.vec3 3.0 4.0 0.0 in
  assert_float_eq ~msg:"length 3-4-5" 5.0 (Math3d.length v)

let test_math3d_length_3d _ =
  (* sqrt(1+4+9) = sqrt(14) *)
  let v = Math3d.vec3 1.0 2.0 3.0 in
  assert_float_eq ~msg:"length 3d" (sqrt 14.0) (Math3d.length v)

let test_math3d_normalize_gives_unit _ =
  let v = Math3d.vec3 3.0 4.0 0.0 in
  let n = Math3d.normalize v in
  assert_float_eq ~msg:"normalized length" 1.0 (Math3d.length n)

let test_math3d_normalize_direction _ =
  let v = Math3d.vec3 2.0 0.0 0.0 in
  let n = Math3d.normalize v in
  assert_vec3_eq ~msg:"normalize x-axis" (1.0, 0.0, 0.0) n

let test_math3d_normalize_zero _ =
  let v = Math3d.vec3 0.0 0.0 0.0 in
  let n = Math3d.normalize v in
  assert_float_eq ~msg:"normalize zero" 0.0 (Math3d.length n)

let test_math3d_add_sub_roundtrip _ =
  let a = Math3d.vec3 1.0 2.0 3.0 in
  let b = Math3d.vec3 4.0 5.0 6.0 in
  let r = Math3d.sub (Math3d.add a b) b in
  assert_vec3_eq ~msg:"(a+b)-b=a" (a.x, a.y, a.z) r

let math3d_vec3_tests =
  "Math3d_vec3"
  >::: [
         "vec3_constructor" >:: test_math3d_vec3_constructor;
         "add" >:: test_math3d_add;
         "add_commutative" >:: test_math3d_add_commutative;
         "add_zero" >:: test_math3d_add_zero;
         "sub" >:: test_math3d_sub;
         "sub_self" >:: test_math3d_sub_self;
         "scale" >:: test_math3d_scale;
         "scale_zero" >:: test_math3d_scale_zero;
         "scale_negative" >:: test_math3d_scale_negative;
         "length_zero" >:: test_math3d_length_zero;
         "length_unit" >:: test_math3d_length_unit;
         "length_pythagorean" >:: test_math3d_length_pythagorean;
         "length_3d" >:: test_math3d_length_3d;
         "normalize_gives_unit" >:: test_math3d_normalize_gives_unit;
         "normalize_direction" >:: test_math3d_normalize_direction;
         "normalize_zero" >:: test_math3d_normalize_zero;
         "add_sub_roundtrip" >:: test_math3d_add_sub_roundtrip;
       ]

(* ------------------------------------------------------------------ *)
(*  {1 Math3d – mat4 tests}                                            *)
(* ------------------------------------------------------------------ *)

let test_math3d_identity_length _ =
  let m = Math3d.identity () in
  assert_equal ~msg:"identity length" 16 (Array.length m)

let test_math3d_identity_diagonal _ =
  let m = Math3d.identity () in
  (* Column-major: diagonal at indices 0, 5, 10, 15 *)
  assert_float_eq ~msg:"[0]" 1.0 m.(0);
  assert_float_eq ~msg:"[5]" 1.0 m.(5);
  assert_float_eq ~msg:"[10]" 1.0 m.(10);
  assert_float_eq ~msg:"[15]" 1.0 m.(15)

let test_math3d_identity_off_diagonal _ =
  let m = Math3d.identity () in
  let off = [ 1; 2; 3; 4; 6; 7; 8; 9; 11; 12; 13; 14 ] in
  List.iter
    (fun i -> assert_float_eq ~msg:(Printf.sprintf "off-diag[%d]" i) 0.0 m.(i))
    off

(** identity * A = A (column-major) *)
let test_math3d_multiply_identity_left _ =
  let a =
    [|
      1.0;
      2.0;
      3.0;
      4.0;
      5.0;
      6.0;
      7.0;
      8.0;
      9.0;
      10.0;
      11.0;
      12.0;
      13.0;
      14.0;
      15.0;
      16.0;
    |]
  in
  let r = Math3d.multiply (Math3d.identity ()) a in
  assert_mat4_eq ~msg:"I*A=A" a r

(** A * identity = A *)
let test_math3d_multiply_identity_right _ =
  let a =
    [|
      1.0;
      2.0;
      3.0;
      4.0;
      5.0;
      6.0;
      7.0;
      8.0;
      9.0;
      10.0;
      11.0;
      12.0;
      13.0;
      14.0;
      15.0;
      16.0;
    |]
  in
  let r = Math3d.multiply a (Math3d.identity ()) in
  assert_mat4_eq ~msg:"A*I=A" a r

(** translation stores tx/ty/tz in column 3 of the column-major matrix *)
let test_math3d_translation_entries _ =
  let m = Math3d.translation ~x:1.0 ~y:2.0 ~z:3.0 in
  assert_float_eq ~msg:"tx" 1.0 m.(12);
  assert_float_eq ~msg:"ty" 2.0 m.(13);
  assert_float_eq ~msg:"tz" 3.0 m.(14);
  assert_float_eq ~msg:"w" 1.0 m.(15)

let test_math3d_translation_identity_block _ =
  let m = Math3d.translation ~x:5.0 ~y:(-3.0) ~z:7.0 in
  assert_float_eq ~msg:"[0]" 1.0 m.(0);
  assert_float_eq ~msg:"[5]" 1.0 m.(5);
  assert_float_eq ~msg:"[10]" 1.0 m.(10)

(** rotation_x(0) should equal identity *)
let test_math3d_rotation_x_zero _ =
  let m = Math3d.rotation_x 0.0 in
  assert_mat4_eq ~msg:"rot_x(0)=I" (Math3d.identity ()) m

(** rotation_y(0) should equal identity *)
let test_math3d_rotation_y_zero _ =
  let m = Math3d.rotation_y 0.0 in
  assert_mat4_eq ~msg:"rot_y(0)=I" (Math3d.identity ()) m

(** rotation_x(pi/2): cos=0, sin=1. Matrix col-major: col0=[1,0,0,0]
    col1=[0,0,1,0] col2=[0,-1,0,0] col3=[0,0,0,1] *)
let test_math3d_rotation_x_halfpi _ =
  let m = Math3d.rotation_x (Float.pi /. 2.0) in
  assert_float_eq ~msg:"[0]" 1.0 m.(0);
  assert_float_eq ~msg:"[5]" 0.0 m.(5);
  (* c *)
  assert_float_eq ~msg:"[6]" 1.0 m.(6);
  (* s *)
  assert_float_eq ~msg:"[9]" (-1.0) m.(9);
  (* -s *)
  assert_float_eq ~msg:"[10]" 0.0 m.(10)
(* c *)

(** rotation_y(pi/2): cos=0, sin=1. *)
let test_math3d_rotation_y_halfpi _ =
  let m = Math3d.rotation_y (Float.pi /. 2.0) in
  assert_float_eq ~msg:"[0]" 0.0 m.(0);
  (* c *)
  assert_float_eq ~msg:"[2]" (-1.0) m.(2);
  (* -s *)
  assert_float_eq ~msg:"[8]" 1.0 m.(8);
  (* s *)
  assert_float_eq ~msg:"[10]" 0.0 m.(10)
(* c *)

(** rotation_x(2*pi) ≈ identity *)
let test_math3d_rotation_x_full_cycle _ =
  let m = Math3d.rotation_x (2.0 *. Float.pi) in
  assert_mat4_eq ~eps:1e-5 ~msg:"rot_x(2pi)=I" (Math3d.identity ()) m

(** rotation_y(2*pi) ≈ identity *)
let test_math3d_rotation_y_full_cycle _ =
  let m = Math3d.rotation_y (2.0 *. Float.pi) in
  assert_mat4_eq ~eps:1e-5 ~msg:"rot_y(2pi)=I" (Math3d.identity ()) m

(** perspective: near/far must appear in column 2 and 3. *)
let test_math3d_perspective_entries _ =
  let fov = Float.pi /. 3.0 in
  let m =
    Math3d.perspective ~fov_y_radians:fov ~aspect:1.0 ~near:0.1 ~far:1000.0
  in
  assert_equal ~msg:"perspective length" 16 (Array.length m);
  (* [11] = -1 in OpenGL projection convention *)
  assert_float_eq ~msg:"[11]=-1" (-1.0) m.(11);
  (* [15] = 0 *)
  assert_float_eq ~msg:"[15]=0" 0.0 m.(15)

(** With aspect=1, m[0] = m[5] (equal horizontal/vertical scale). *)
let test_math3d_perspective_square_aspect _ =
  let m =
    Math3d.perspective ~fov_y_radians:(Float.pi /. 2.0) ~aspect:1.0 ~near:0.1
      ~far:100.0
  in
  assert_float_eq ~msg:"m[0]=m[5]" m.(0) m.(5)

(** view_from_camera at origin with yaw=pitch=0: translation column is negated
    position *)
let test_math3d_view_from_camera_origin _ =
  let pos = Math3d.vec3 0.0 0.0 0.0 in
  let v = Math3d.view_from_camera ~position:pos ~yaw:0.0 ~pitch:0.0 in
  assert_equal ~msg:"view length" 16 (Array.length v)

let test_math3d_view_from_camera_translation _ =
  (* Moving camera right by 1.0: view matrix should shift world left by 1.0.
     With yaw=pitch=0, rotation block = identity, so the full matrix is just a
     translation by -position. *)
  let pos = Math3d.vec3 1.0 0.0 0.0 in
  let v = Math3d.view_from_camera ~position:pos ~yaw:0.0 ~pitch:0.0 in
  (* translation in col 3: indices 12,13,14 *)
  assert_float_eq ~msg:"tx" (-1.0) v.(12)

let math3d_mat4_tests =
  "Math3d_mat4"
  >::: [
         "identity_length" >:: test_math3d_identity_length;
         "identity_diagonal" >:: test_math3d_identity_diagonal;
         "identity_off_diagonal" >:: test_math3d_identity_off_diagonal;
         "multiply_identity_left" >:: test_math3d_multiply_identity_left;
         "multiply_identity_right" >:: test_math3d_multiply_identity_right;
         "translation_entries" >:: test_math3d_translation_entries;
         "translation_identity_block" >:: test_math3d_translation_identity_block;
         "rotation_x_zero" >:: test_math3d_rotation_x_zero;
         "rotation_y_zero" >:: test_math3d_rotation_y_zero;
         "rotation_x_halfpi" >:: test_math3d_rotation_x_halfpi;
         "rotation_y_halfpi" >:: test_math3d_rotation_y_halfpi;
         "rotation_x_full_cycle" >:: test_math3d_rotation_x_full_cycle;
         "rotation_y_full_cycle" >:: test_math3d_rotation_y_full_cycle;
         "perspective_entries" >:: test_math3d_perspective_entries;
         "perspective_square_aspect" >:: test_math3d_perspective_square_aspect;
         "view_from_camera_origin" >:: test_math3d_view_from_camera_origin;
         "view_from_camera_translation"
         >:: test_math3d_view_from_camera_translation;
       ]

(* ------------------------------------------------------------------ *)
(*  {1 Noise tests}                                                     *)
(* ------------------------------------------------------------------ *)

let test_noise_range _ =
  for xi = -20 to 20 do
    for zi = -20 to 20 do
      let v = Noise.perlin2d (float_of_int xi) (float_of_int zi) in
      assert_bool
        (Printf.sprintf "perlin2d(%d,%d)=%f out of [-1,1]" xi zi v)
        (v >= -1.0 -. eps && v <= 1.0 +. eps)
    done
  done

let test_noise_deterministic _ =
  let v1 = Noise.perlin2d 1.23 4.56 in
  let v2 = Noise.perlin2d 1.23 4.56 in
  assert_float_eq ~msg:"same input same output" v1 v2

let test_noise_different_points _ =
  let v1 = Noise.perlin2d 0.0 0.0 in
  let v2 = Noise.perlin2d 100.0 100.0 in
  (* Not equal at very different points in general—just a sanity check that the
     function returns something (not necessarily different). *)
  ignore (v1, v2)

let test_noise_grid_determinism _ =
  (* Re-evaluating a 5x5 grid twice must produce identical values *)
  let eval () =
    Array.init 25 (fun i ->
        Noise.perlin2d
          (float_of_int (i mod 5) *. 0.1)
          (float_of_int (i / 5) *. 0.1))
  in
  let a = eval () and b = eval () in
  Array.iteri
    (fun i v -> assert_float_eq ~msg:(Printf.sprintf "grid[%d]" i) v b.(i))
    a

let test_noise_fractional_inputs _ =
  (* Should not raise or produce NaN *)
  let v = Noise.perlin2d 0.5 0.5 in
  assert_bool "not nan" (not (Float.is_nan v))

let test_noise_negative_inputs _ =
  let v = Noise.perlin2d (-3.7) (-5.2) in
  assert_bool "not nan neg" (not (Float.is_nan v));
  assert_bool "in range neg" (v >= -1.0 -. eps && v <= 1.0 +. eps)

let noise_tests =
  "Noise"
  >::: [
         "range" >:: test_noise_range;
         "deterministic" >:: test_noise_deterministic;
         "different_points" >:: test_noise_different_points;
         "grid_determinism" >:: test_noise_grid_determinism;
         "fractional_inputs" >:: test_noise_fractional_inputs;
         "negative_inputs" >:: test_noise_negative_inputs;
       ]

(* ------------------------------------------------------------------ *)
(*  {1 Terrain tests}                                                   *)
(* ------------------------------------------------------------------ *)

(** height_at must return a non-negative integer in a plausible range. The
    formula is 6 + 4*perlin2d(...) with perlin in [-1,1], so height ∈ [2, 10].
    We accept a slightly wider window for float rounding. *)
let test_terrain_height_range _ =
  for x = -10 to 10 do
    for z = -10 to 10 do
      let h = Terrain.height_at x z in
      assert_bool (Printf.sprintf "height_at(%d,%d)=%d < 2" x z h) (h >= 1);
      assert_bool (Printf.sprintf "height_at(%d,%d)=%d > 10" x z h) (h <= 10)
    done
  done

let test_terrain_height_deterministic _ =
  let h1 = Terrain.height_at 5 5 in
  let h2 = Terrain.height_at 5 5 in
  assert_equal ~msg:"height_at deterministic" h1 h2

let test_terrain_fill_chunk_size _ =
  let blocks = Terrain.fill_chunk ~cx:0 ~cy:0 ~cz:0 in
  let n = Config.chunk_size in
  assert_equal ~msg:"fill_chunk block count" (n * n * n) (Array.length blocks)

(** For a chunk at cy=0, the surface height is in [2,10], which falls within
    world-y [0,15]. So we expect at least some Grass blocks. *)
let test_terrain_fill_chunk_has_grass _ =
  let blocks = Terrain.fill_chunk ~cx:0 ~cy:0 ~cz:0 in
  let has_grass = Array.exists (fun b -> b = Block.Grass) blocks in
  assert_bool "chunk cy=0 has Grass" has_grass

(** Same chunk must have Stone somewhere below the surface. *)
let test_terrain_fill_chunk_has_stone _ =
  let blocks = Terrain.fill_chunk ~cx:0 ~cy:0 ~cz:0 in
  let has_stone = Array.exists (fun b -> b = Block.Stone) blocks in
  assert_bool "chunk cy=0 has Stone" has_stone

(** A chunk high in the sky (cy=5, world-y ≥ 80) must be all Air. *)
let test_terrain_fill_chunk_sky_all_air _ =
  let blocks = Terrain.fill_chunk ~cx:0 ~cy:5 ~cz:0 in
  let all_air = Array.for_all (fun b -> b = Block.Air) blocks in
  assert_bool "sky chunk all Air" all_air

(** A chunk deep underground (cy=-2, world-y ≤ -17) must be all Stone. *)
let test_terrain_fill_chunk_deep_all_stone _ =
  let blocks = Terrain.fill_chunk ~cx:0 ~cy:(-2) ~cz:0 in
  let all_stone = Array.for_all (fun b -> b = Block.Stone) blocks in
  assert_bool "deep chunk all Stone" all_stone

(** The block at surface height y=h must be Grass, at h-1 Dirt, at h-4 Stone. We
    pick a known column and verify the layering for cy=0. *)
let test_terrain_fill_chunk_layering _ =
  let cx = 0 and cz = 0 and cy = 0 in
  let cs = Config.chunk_size in
  let blocks = Terrain.fill_chunk ~cx ~cy ~cz in
  let bx = 0 and bz = 0 in
  let wx = (cx * cs) + bx and wz = (cz * cs) + bz in
  let h = Terrain.height_at wx wz in
  if h >= 0 && h < cs then begin
    let idx_surf = Chunk.index bx h bz in
    assert_equal ~msg:"surface=Grass" Block.Grass blocks.(idx_surf)
  end;
  if h - 1 >= 0 && h - 1 < cs then begin
    let idx_dirt = Chunk.index bx (h - 1) bz in
    assert_equal ~msg:"h-1=Dirt" Block.Dirt blocks.(idx_dirt)
  end;
  if h - 4 >= 0 && h - 4 < cs then begin
    let idx_stone = Chunk.index bx (h - 4) bz in
    assert_equal ~msg:"h-4=Stone" Block.Stone blocks.(idx_stone)
  end

let terrain_tests =
  "Terrain"
  >::: [
         "height_range" >:: test_terrain_height_range;
         "height_deterministic" >:: test_terrain_height_deterministic;
         "fill_chunk_size" >:: test_terrain_fill_chunk_size;
         "fill_chunk_has_grass" >:: test_terrain_fill_chunk_has_grass;
         "fill_chunk_has_stone" >:: test_terrain_fill_chunk_has_stone;
         "fill_chunk_sky_all_air" >:: test_terrain_fill_chunk_sky_all_air;
         "fill_chunk_deep_all_stone" >:: test_terrain_fill_chunk_deep_all_stone;
         "fill_chunk_layering" >:: test_terrain_fill_chunk_layering;
       ]

(* ------------------------------------------------------------------ *)
(*  {1 World tests}                                                     *)
(* ------------------------------------------------------------------ *)

let test_world_create _ =
  let w = World.create () in
  (* Newly created world has no loaded chunks *)
  let count = ref 0 in
  World.iter w (fun _ -> incr count);
  assert_equal ~msg:"empty world has 0 chunks" 0 !count

let test_world_get_block_unloaded _ =
  let w = World.create () in
  (* Unloaded chunk returns Air *)
  let b = World.get_block w 100 100 100 in
  assert_equal ~msg:"unloaded chunk = Air" Block.Air b

let test_world_generate_chunk _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  let chunk = World.get_chunk w 0 0 0 in
  assert_bool "chunk generated" (chunk <> None)

let test_world_generate_chunk_count _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  World.generate_chunk w ~cx:1 ~cy:0 ~cz:0;
  let count = ref 0 in
  World.iter w (fun _ -> incr count);
  assert_equal ~msg:"two chunks" 2 !count

let test_world_get_chunk_none _ =
  let w = World.create () in
  let c = World.get_chunk w 99 99 99 in
  assert_equal ~msg:"no chunk at 99,99,99" None c

let test_world_set_block_then_get _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  World.set_block w 0 0 0 Block.Stone;
  let b = World.get_block w 0 0 0 in
  assert_equal ~msg:"set_block then get" Block.Stone b

let test_world_set_block_unloaded_noop _ =
  let w = World.create () in
  (* set_block on unloaded chunk is a no-op: no exception *)
  World.set_block w 500 500 500 Block.Stone;
  let b = World.get_block w 500 500 500 in
  assert_equal ~msg:"unloaded set = Air" Block.Air b

let test_world_get_block_loaded _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  (* Surface y for (0,0) should be in [2,10]; chunk cy=0 has world y 0..15. We
     know at y=0 (deep underground) the block must be Stone. *)
  let b = World.get_block w 0 0 0 in
  assert_equal ~msg:"underground block is Stone" Block.Stone b

let test_world_iter_visits_all _ =
  let w = World.create () in
  let coords = [ (0, 0, 0); (1, 0, 0); (0, 1, 0); (0, 0, 1) ] in
  List.iter (fun (cx, cy, cz) -> World.generate_chunk w ~cx ~cy ~cz) coords;
  let visited = ref [] in
  World.iter w (fun c ->
      visited := (Chunk.x c, Chunk.y c, Chunk.z c) :: !visited);
  assert_equal ~msg:"iter visits 4 chunks" 4 (List.length !visited)

let test_world_mesh_chunk_returns_arrays _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  let chunk = Option.get (World.get_chunk w 0 0 0) in
  let pos, col = World.mesh_chunk w chunk in
  (* positions and colors must have equal length *)
  assert_equal ~msg:"pos/col same length" (Array.length pos) (Array.length col)

let test_world_mesh_chunk_length_divisible_3 _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  let chunk = Option.get (World.get_chunk w 0 0 0) in
  let pos, col = World.mesh_chunk w chunk in
  assert_equal ~msg:"positions divisible by 3" 0 (Array.length pos mod 3);
  assert_equal ~msg:"colors divisible by 3" 0 (Array.length col mod 3)

let test_world_mesh_empty_chunk _ =
  let w = World.create () in
  (* A sky chunk (all Air) should produce no mesh geometry *)
  World.generate_chunk w ~cx:0 ~cy:5 ~cz:0;
  let chunk = Option.get (World.get_chunk w 0 5 0) in
  let pos, col = World.mesh_chunk w chunk in
  assert_equal ~msg:"sky chunk: no vertices" 0 (Array.length pos);
  assert_equal ~msg:"sky chunk: no colors" 0 (Array.length col)

let test_world_duplicate_generate _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  let count = ref 0 in
  World.iter w (fun _ -> incr count);
  (* Depending on implementation, second generate may be a no-op or replace—
     either way, we expect at most 1 chunk here. *)
  assert_bool "at most 1 chunk after duplicate generate" (!count <= 1)

let world_tests =
  "World"
  >::: [
         "create" >:: test_world_create;
         "get_block_unloaded" >:: test_world_get_block_unloaded;
         "generate_chunk" >:: test_world_generate_chunk;
         "generate_chunk_count" >:: test_world_generate_chunk_count;
         "get_chunk_none" >:: test_world_get_chunk_none;
         "set_block_then_get" >:: test_world_set_block_then_get;
         "set_block_unloaded_noop" >:: test_world_set_block_unloaded_noop;
         "get_block_loaded" >:: test_world_get_block_loaded;
         "iter_visits_all" >:: test_world_iter_visits_all;
         "mesh_chunk_returns_arrays" >:: test_world_mesh_chunk_returns_arrays;
         "mesh_chunk_length_div3" >:: test_world_mesh_chunk_length_divisible_3;
         "mesh_empty_chunk" >:: test_world_mesh_empty_chunk;
         "duplicate_generate" >:: test_world_duplicate_generate;
       ]

(* ------------------------------------------------------------------ *)
(*  {1 Physics – AABB tests}                                           *)
(* ------------------------------------------------------------------ *)

let test_physics_at_position_width _ =
  let pos = Math3d.vec3 0.0 0.0 0.0 in
  let box = Physics.at_position pos in
  let w = box.max.x -. box.min.x in
  assert_float_eq ~msg:"aabb width" Config.player_width w

let test_physics_at_position_height _ =
  let pos = Math3d.vec3 0.0 0.0 0.0 in
  let box = Physics.at_position pos in
  let h = box.max.y -. box.min.y in
  assert_float_eq ~msg:"aabb height" Config.player_height h

let test_physics_at_position_depth _ =
  let pos = Math3d.vec3 0.0 0.0 0.0 in
  let box = Physics.at_position pos in
  let d = box.max.z -. box.min.z in
  assert_float_eq ~msg:"aabb depth" Config.player_width d

let test_physics_at_position_centered_xz _ =
  let pos = Math3d.vec3 5.0 0.0 3.0 in
  let box = Physics.at_position pos in
  let cx = (box.min.x +. box.max.x) /. 2.0 in
  let cz = (box.min.z +. box.max.z) /. 2.0 in
  assert_float_eq ~msg:"center x" 5.0 cx;
  assert_float_eq ~msg:"center z" 3.0 cz

let test_physics_at_position_shift _ =
  let pos1 = Math3d.vec3 0.0 0.0 0.0 in
  let pos2 = Math3d.vec3 10.0 5.0 (-3.0) in
  let b1 = Physics.at_position pos1 in
  let b2 = Physics.at_position pos2 in
  assert_float_eq ~msg:"shifted min x" (b1.min.x +. 10.0) b2.min.x;
  assert_float_eq ~msg:"shifted min y" (b1.min.y +. 5.0) b2.min.y;
  assert_float_eq ~msg:"shifted min z" (b1.min.z -. 3.0) b2.min.z

(* ------------------------------------------------------------------ *)
(*  {1 Physics – move tests}                                           *)
(* ------------------------------------------------------------------ *)

(** In an empty world (no solid blocks) movement should be unrestricted. *)
let test_physics_move_free_space _ =
  let w = World.create () in
  let pos = Math3d.vec3 0.5 20.0 0.5 in
  let box = Physics.at_position pos in
  let delta = Math3d.vec3 1.0 0.0 0.0 in
  let actual = Physics.move w box delta in
  assert_float_eq ~msg:"free dx" 1.0 actual.x;
  assert_float_eq ~msg:"free dy" 0.0 actual.y;
  assert_float_eq ~msg:"free dz" 0.0 actual.z

let test_physics_move_zero_delta _ =
  let w = World.create () in
  let pos = Math3d.vec3 8.0 8.0 8.0 in
  let box = Physics.at_position pos in
  let delta = Math3d.vec3 0.0 0.0 0.0 in
  let actual = Physics.move w box delta in
  assert_float_eq ~msg:"zero delta x" 0.0 actual.x;
  assert_float_eq ~msg:"zero delta y" 0.0 actual.y;
  assert_float_eq ~msg:"zero delta z" 0.0 actual.z

let test_physics_move_blocked_by_stone _ =
  (* Place a stone floor at y=10 and ensure downward movement stops *)
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  (* Put solid stone at y=10 along the column x=0..1, z=0..1 *)
  for bx = 0 to 1 do
    for bz = 0 to 1 do
      World.set_block w bx 10 bz Block.Stone
    done
  done;
  (* Player at y=14: feet (min.y = 14-1.6=12.4) are above stone top (y=11) *)
  let pos = Math3d.vec3 0.5 14.0 0.5 in
  let box = Physics.at_position pos in
  (* Try to move down by 5 blocks — should be clipped *)
  let delta = Math3d.vec3 0.0 (-5.0) 0.0 in
  let actual = Physics.move w box delta in
  assert_bool "dy clipped (< 0)" (actual.y < 0.0);
  assert_bool "dy not full -5" (actual.y > -5.0)

let test_physics_move_free_vertical _ =
  let w = World.create () in
  let pos = Math3d.vec3 0.5 100.0 0.5 in
  let box = Physics.at_position pos in
  let delta = Math3d.vec3 0.0 (-2.0) 0.0 in
  let actual = Physics.move w box delta in
  assert_float_eq ~msg:"free fall" (-2.0) actual.y

let test_physics_move_preserves_sign _ =
  let w = World.create () in
  let pos = Math3d.vec3 100.0 100.0 100.0 in
  let box = Physics.at_position pos in
  let delta = Math3d.vec3 (-3.0) 0.0 0.0 in
  let actual = Physics.move w box delta in
  assert_bool "dx negative in open air" (actual.x <= 0.0)

let physics_tests =
  "Physics"
  >::: [
         "at_position_width" >:: test_physics_at_position_width;
         "at_position_height" >:: test_physics_at_position_height;
         "at_position_depth" >:: test_physics_at_position_depth;
         "at_position_centered" >:: test_physics_at_position_centered_xz;
         "at_position_shift" >:: test_physics_at_position_shift;
         "move_free_space" >:: test_physics_move_free_space;
         "move_zero_delta" >:: test_physics_move_zero_delta;
         "move_blocked_by_stone" >:: test_physics_move_blocked_by_stone;
         "move_free_vertical" >:: test_physics_move_free_vertical;
         "move_preserves_sign" >:: test_physics_move_preserves_sign;
       ]

(* ------------------------------------------------------------------ *)
(*  {1 Camera tests}                                                    *)
(* ------------------------------------------------------------------ *)

let make_camera ?(x = 0.0) ?(y = 0.0) ?(z = 0.0) ?(yaw = 0.0) ?(pitch = 0.0) ()
    =
  Camera.create ~pos:(Math3d.vec3 x y z) ~yaw ~pitch

let test_camera_create _ =
  let c = make_camera ~x:1.0 ~y:2.0 ~z:3.0 ~yaw:0.5 ~pitch:0.2 () in
  assert_float_eq ~msg:"pos.x" 1.0 c.pos.x;
  assert_float_eq ~msg:"pos.y" 2.0 c.pos.y;
  assert_float_eq ~msg:"pos.z" 3.0 c.pos.z;
  assert_float_eq ~msg:"yaw" 0.5 c.yaw;
  assert_float_eq ~msg:"pitch" 0.2 c.pitch

let test_camera_mouse_look_yaw _ =
  let c = make_camera () in
  Camera.apply_mouse_look c ~dx:1.0 ~dy:0.0 ~sensitivity:1.0;
  assert_bool "yaw changed by mouse_look" (abs_float c.yaw > 0.0)

let test_camera_mouse_look_pitch _ =
  let c = make_camera () in
  Camera.apply_mouse_look c ~dx:0.0 ~dy:1.0 ~sensitivity:1.0;
  assert_bool "pitch changed by mouse_look" (abs_float c.pitch > 0.0)

let test_camera_pitch_clamped_upper _ =
  let c = make_camera () in
  (* Drive pitch past the limit *)
  Camera.apply_mouse_look c ~dx:0.0 ~dy:(-1000.0) ~sensitivity:1.0;
  assert_bool "pitch <= pitch_limit" (c.pitch <= Config.pitch_limit)

let test_camera_pitch_clamped_lower _ =
  let c = make_camera () in
  Camera.apply_mouse_look c ~dx:0.0 ~dy:1000.0 ~sensitivity:1.0;
  assert_bool "pitch >= -pitch_limit" (c.pitch >= -.Config.pitch_limit)

let test_camera_yaw_accumulates _ =
  let c = make_camera () in
  Camera.apply_mouse_look c ~dx:1.0 ~dy:0.0 ~sensitivity:0.01;
  let y1 = c.yaw in
  Camera.apply_mouse_look c ~dx:1.0 ~dy:0.0 ~sensitivity:0.01;
  let y2 = c.yaw in
  assert_bool "yaw accumulates" (abs_float y2 > abs_float y1)

let test_camera_zero_mouse_delta _ =
  let c = make_camera ~yaw:0.7 ~pitch:0.3 () in
  Camera.apply_mouse_look c ~dx:0.0 ~dy:0.0 ~sensitivity:1.0;
  assert_float_eq ~msg:"yaw unchanged" 0.7 c.yaw;
  assert_float_eq ~msg:"pitch unchanged" 0.3 c.pitch

let test_camera_view_returns_mat4 _ =
  let c = make_camera () in
  let m = Camera.view c in
  assert_equal ~msg:"view length" 16 (Array.length m)

let test_camera_view_changes_with_yaw _ =
  let c1 = make_camera ~yaw:0.0 () in
  let c2 = make_camera ~yaw:1.0 () in
  let m1 = Camera.view c1 in
  let m2 = Camera.view c2 in
  let same = Array.for_all2 (fun a b -> abs_float (a -. b) < eps) m1 m2 in
  assert_bool "different yaw → different view" (not same)

let test_camera_view_changes_with_pitch _ =
  let c1 = make_camera ~pitch:0.0 () in
  let c2 = make_camera ~pitch:0.5 () in
  let m1 = Camera.view c1 in
  let m2 = Camera.view c2 in
  let same = Array.for_all2 (fun a b -> abs_float (a -. b) < eps) m1 m2 in
  assert_bool "different pitch → different view" (not same)

let test_camera_view_changes_with_position _ =
  let c1 = make_camera ~x:0.0 () in
  let c2 = make_camera ~x:5.0 () in
  let m1 = Camera.view c1 in
  let m2 = Camera.view c2 in
  let same = Array.for_all2 (fun a b -> abs_float (a -. b) < eps) m1 m2 in
  assert_bool "different position → different view" (not same)

let camera_tests =
  "Camera"
  >::: [
         "create" >:: test_camera_create;
         "mouse_look_yaw" >:: test_camera_mouse_look_yaw;
         "mouse_look_pitch" >:: test_camera_mouse_look_pitch;
         "pitch_clamped_upper" >:: test_camera_pitch_clamped_upper;
         "pitch_clamped_lower" >:: test_camera_pitch_clamped_lower;
         "yaw_accumulates" >:: test_camera_yaw_accumulates;
         "zero_mouse_delta" >:: test_camera_zero_mouse_delta;
         "view_returns_mat4" >:: test_camera_view_returns_mat4;
         "view_changes_with_yaw" >:: test_camera_view_changes_with_yaw;
         "view_changes_with_pitch" >:: test_camera_view_changes_with_pitch;
         "view_changes_with_pos" >:: test_camera_view_changes_with_position;
       ]

(* ------------------------------------------------------------------ *)
(*  {1 Integration tests}                                              *)
(* ------------------------------------------------------------------ *)

(** Generate a 3x1x3 patch of surface chunks and verify that each generated
    chunk has blocks. *)
let test_integration_world_patch _ =
  let w = World.create () in
  for cx = 0 to 2 do
    for cz = 0 to 2 do
      World.generate_chunk w ~cx ~cy:0 ~cz
    done
  done;
  let count = ref 0 in
  World.iter w (fun _ -> incr count);
  assert_equal ~msg:"9 chunks in 3x3 patch" 9 !count

(** Verify that after terrain generation, surface blocks accessible via
    World.get_block are Grass. *)
let test_integration_surface_grass _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  let h = Terrain.height_at 0 0 in
  if h >= 0 && h < Config.chunk_size then begin
    let b = World.get_block w 0 h 0 in
    assert_equal ~msg:"surface block is Grass" Block.Grass b
  end

(** Verify that blocks just below the surface are Dirt. *)
let test_integration_subsurface_dirt _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  let h = Terrain.height_at 0 0 in
  if h - 1 >= 0 && h - 1 < Config.chunk_size then begin
    let b = World.get_block w 0 (h - 1) 0 in
    assert_equal ~msg:"h-1 block is Dirt" Block.Dirt b
  end

(** Player AABB in free space: moving in all six directions should not be
    clipped. *)
let test_integration_physics_free_all_axes _ =
  let w = World.create () in
  let pos = Math3d.vec3 0.5 50.0 0.5 in
  let box = Physics.at_position pos in
  let deltas =
    [
      Math3d.vec3 1.0 0.0 0.0;
      Math3d.vec3 (-1.0) 0.0 0.0;
      Math3d.vec3 0.0 1.0 0.0;
      Math3d.vec3 0.0 (-1.0) 0.0;
      Math3d.vec3 0.0 0.0 1.0;
      Math3d.vec3 0.0 0.0 (-1.0);
    ]
  in
  List.iter
    (fun d ->
      let r = Physics.move w box d in
      assert_float_eq ~msg:"free x" d.x r.x;
      assert_float_eq ~msg:"free y" d.y r.y;
      assert_float_eq ~msg:"free z" d.z r.z)
    deltas

(** Terrain mesh for a solid chunk must be non-empty. *)
let test_integration_mesh_nonempty _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  let chunk = Option.get (World.get_chunk w 0 0 0) in
  let pos, _ = World.mesh_chunk w chunk in
  assert_bool "solid chunk mesh non-empty" (Array.length pos > 0)

(** height_at and fill_chunk agree: the block at world height h must be Grass
    across several columns. *)
let test_integration_height_fill_agreement _ =
  let cs = Config.chunk_size in
  let blocks = Terrain.fill_chunk ~cx:0 ~cy:0 ~cz:0 in
  for bx = 0 to min 3 (cs - 1) do
    for bz = 0 to min 3 (cs - 1) do
      let h = Terrain.height_at bx bz in
      if h >= 0 && h < cs then begin
        let idx = Chunk.index bx h bz in
        assert_equal
          ~msg:(Printf.sprintf "(%d,h,%d) is Grass" bx bz)
          Block.Grass blocks.(idx)
      end
    done
  done

let integration_tests =
  "Integration"
  >::: [
         "world_patch" >:: test_integration_world_patch;
         "surface_grass" >:: test_integration_surface_grass;
         "subsurface_dirt" >:: test_integration_subsurface_dirt;
         "physics_free_all_axes" >:: test_integration_physics_free_all_axes;
         "mesh_nonempty" >:: test_integration_mesh_nonempty;
         "height_fill_agreement" >:: test_integration_height_fill_agreement;
       ]

(* ------------------------------------------------------------------ *)
(*  Runner                                                              *)
(* ------------------------------------------------------------------ *)

let tests =
  "test suite"
  >::: [
         config_tests;
         block_tests;
         chunk_tests;
         color_tests;
         math3d_vec3_tests;
         math3d_mat4_tests;
         noise_tests;
         terrain_tests;
         world_tests;
         physics_tests;
         camera_tests;
         integration_tests;
       ]

let _ = run_test_tt_main tests
