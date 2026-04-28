(** Integration tests for the ocaml-voxel project.

    {1 Purpose}
    These tests exercise cross-module interactions that cannot be covered by
    per-module unit tests. In particular they verify that:

    - [Terrain] and [World] produce consistent data (the blocks accessible
      through the world agree with what [Terrain.fill_chunk] and
      [Terrain.height_at] specify).
    - [Physics] interacts correctly with a fully-populated [World]: collision
      detection works against terrain-generated geometry.
    - [Camera] and [Math3d] compose correctly: [Camera.view] delegates to
      [Math3d.view_from_camera] and the result has the expected structure.
    - A multi-chunk world patch generates the right number of chunks and each
      chunk is accessible.

    {1 Modules that require a display context}
    [Buffer], [Gl_utils], [Shader], and [Window] all need a live OpenGL / SDL
    context. They are intentionally not covered here; correctness for those
    modules is verified by running the full game. *)

open OUnit2

let eps = 1e-5

let assert_feq ?(eps = eps) ~msg expected actual =
  if abs_float (expected -. actual) > eps then
    assert_failure
      (Printf.sprintf "%s: expected %.8f got %.8f" msg expected actual)

(* ------------------------------------------------------------------ *)
(*  {1 Terrain ↔ World consistency}                                     *)
(* ------------------------------------------------------------------ *)

(** After generating a chunk, the block at the surface height returned by
    [height_at] must be [Grass] when accessed via [World.get_block]. *)
let test_terrain_world_surface_grass _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  let cs = Config.chunk_size in
  let h = Terrain.height_at 0 0 in
  if h >= 0 && h < cs then
    assert_equal ~msg:"surface via world = Grass" Block.Grass
      (World.get_block w 0 h 0)

(** One block below the surface must be [Dirt]. *)
let test_terrain_world_subsurface_dirt _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  let cs = Config.chunk_size in
  let h = Terrain.height_at 0 0 in
  if h - 1 >= 0 && h - 1 < cs then
    assert_equal ~msg:"h-1 via world = Dirt" Block.Dirt
      (World.get_block w 0 (h - 1) 0)

(** [fill_chunk] and [World.get_block] agree on the entire surface layer for a
    selection of columns in chunk (0,0,0). *)
let test_terrain_world_fill_agreement _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  let cs = Config.chunk_size in
  for bx = 0 to min 3 (cs - 1) do
    for bz = 0 to min 3 (cs - 1) do
      let h = Terrain.height_at bx bz in
      if h >= 0 && h < cs then begin
        let expected = Block.Grass in
        let actual = World.get_block w bx h bz in
        assert_equal
          ~msg:(Printf.sprintf "surface at (%d,%d)" bx bz)
          expected actual
      end
    done
  done

(* ------------------------------------------------------------------ *)
(*  {1 Physics ↔ World: player blocked by terrain}                     *)
(* ------------------------------------------------------------------ *)

(** In an empty (no-chunk) world, the player falls freely: movement is
    unrestricted on all six axes. *)
let test_physics_free_in_empty_world _ =
  let w = World.create () in
  let pos = Math3d.vec3 0.5 50.0 0.5 in
  let box = Physics.at_position pos in
  let deltas =
    [
      Math3d.vec3 1.0 0.0 0.0;
      Math3d.vec3 0.0 (-1.0) 0.0;
      Math3d.vec3 0.0 0.0 1.0;
    ]
  in
  List.iter
    (fun d ->
      let r = Physics.move w box d in
      assert_feq ~msg:"free x" d.x r.x;
      assert_feq ~msg:"free y" d.y r.y;
      assert_feq ~msg:"free z" d.z r.z)
    deltas

(** A player standing on terrain cannot fall through the surface. *)
let test_physics_blocked_by_terrain _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  (* Surface height at column (0,0). *)
  let h = Terrain.height_at 0 0 in
  (* Position player 5 blocks above the surface with feet well clear. *)
  let pos_y = float_of_int h +. 5.0 in
  let pos = Math3d.vec3 0.5 pos_y 0.5 in
  let box = Physics.at_position pos in
  let d = Physics.move w box (Math3d.vec3 0.0 (-10.0) 0.0) in
  assert_bool "player clipped by terrain (dy < 0)" (d.y < 0.0);
  assert_bool "player did not fall through (dy > -10)" (d.y > -10.0)

(* ------------------------------------------------------------------ *)
(*  {1 Camera ↔ Math3d: view matrix structure}                         *)
(* ------------------------------------------------------------------ *)

(** [Camera.view] must return a 16-element array (delegates to
    [Math3d.view_from_camera]). *)
let test_camera_view_is_mat4 _ =
  let c = Camera.create ~pos:(Math3d.vec3 0.0 0.0 0.0) ~yaw:0.0 ~pitch:0.0 in
  assert_equal ~msg:"view length" 16 (Array.length (Camera.view c))

(** At yaw = pitch = 0 and position = origin, [Camera.view] should match
    [Math3d.view_from_camera] called with the same arguments. *)
let test_camera_view_matches_math3d _ =
  let pos = Math3d.vec3 1.0 2.0 3.0 in
  let yaw = 0.5 in
  let pitch = 0.3 in
  let c = Camera.create ~pos ~yaw ~pitch in
  let from_cam = Camera.view c in
  let from_m3d = Math3d.view_from_camera ~position:pos ~yaw ~pitch in
  Array.iteri
    (fun i v -> assert_feq ~msg:(Printf.sprintf "view[%d]" i) from_m3d.(i) v)
    from_cam

(* ------------------------------------------------------------------ *)
(*  {1 Multi-chunk world patch}                                          *)
(* ------------------------------------------------------------------ *)

(** Generating a 3×1×3 patch of chunks populates exactly 9 entries. *)
let test_world_patch_count _ =
  let w = World.create () in
  for cx = 0 to 2 do
    for cz = 0 to 2 do
      World.generate_chunk w ~cx ~cy:0 ~cz
    done
  done;
  let count = ref 0 in
  World.iter w (fun _ -> incr count);
  assert_equal ~msg:"3×3 patch = 9 chunks" 9 !count

(** Every chunk in the patch is accessible via [get_chunk]. *)
let test_world_patch_accessible _ =
  let w = World.create () in
  let coords = [ (0, 0, 0); (1, 0, 0); (2, 0, 0); (0, 0, 1); (1, 0, 1) ] in
  List.iter (fun (cx, cy, cz) -> World.generate_chunk w ~cx ~cy ~cz) coords;
  List.iter
    (fun (cx, cy, cz) ->
      assert_bool
        (Printf.sprintf "chunk (%d,%d,%d) present" cx cy cz)
        (World.get_chunk w cx cy cz <> None))
    coords

(** Mesh generation on a 3×1×3 patch produces non-empty geometry. *)
let test_world_patch_mesh _ =
  let w = World.create () in
  for cx = 0 to 2 do
    for cz = 0 to 2 do
      World.generate_chunk w ~cx ~cy:0 ~cz
    done
  done;
  let total_verts = ref 0 in
  World.iter w (fun chunk ->
      let pos, _ = World.mesh_chunk w chunk in
      total_verts := !total_verts + Array.length pos);
  assert_bool "patch mesh non-empty" (!total_verts > 0)

(* ------------------------------------------------------------------ *)
(*  {1 Noise → Terrain → World pipeline}                               *)
(* ------------------------------------------------------------------ *)

(** The height returned by [height_at] must be in the expected range for every
    column in the chunk at (0,0,0), and the corresponding world block must be
    [Grass]. *)
let test_noise_terrain_world_pipeline _ =
  let w = World.create () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  let cs = Config.chunk_size in
  for bx = 0 to cs - 1 do
    for bz = 0 to cs - 1 do
      let h = Terrain.height_at bx bz in
      assert_bool (Printf.sprintf "h(%d,%d) >= 1" bx bz) (h >= 1);
      assert_bool (Printf.sprintf "h(%d,%d) <= 10" bx bz) (h <= 10);
      if h >= 0 && h < cs then
        assert_equal
          ~msg:(Printf.sprintf "pipeline: surface (%d,%d)" bx bz)
          Block.Grass
          (World.get_block w bx h bz)
    done
  done

let tests =
  "Integration"
  >::: [
         (* Terrain ↔ World *)
         "surface_grass" >:: test_terrain_world_surface_grass;
         "subsurface_dirt" >:: test_terrain_world_subsurface_dirt;
         "fill_agreement" >:: test_terrain_world_fill_agreement;
         (* Physics ↔ World *)
         "free_empty_world" >:: test_physics_free_in_empty_world;
         "blocked_by_terrain" >:: test_physics_blocked_by_terrain;
         (* Camera ↔ Math3d *)
         "view_is_mat4" >:: test_camera_view_is_mat4;
         "view_matches_math3d" >:: test_camera_view_matches_math3d;
         (* Multi-chunk *)
         "patch_count" >:: test_world_patch_count;
         "patch_accessible" >:: test_world_patch_accessible;
         "patch_mesh" >:: test_world_patch_mesh;
         (* Pipeline *)
         "noise_terrain_world" >:: test_noise_terrain_world_pipeline;
       ]

let _ = run_test_tt_main tests
