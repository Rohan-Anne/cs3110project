(** Tests for the {!World} module.

    {1 Module Overview}
    [World] maintains a hash-table of loaded chunks keyed by [(cx, cy, cz)]
    chunk coordinates. It converts between world-space block coordinates and
    chunk/local coordinates, delegates terrain generation to
    [Terrain.fill_chunk], and constructs triangle meshes for rendering.

    {1 Invariants}
    {2 Coordinate mapping — [coord_to_chunk] (internal)}
    - For any integer [x], [coord_to_chunk x] returns [(cx, bx)] such that
      [cx * chunk_size + bx = x] and [0 ≤ bx < chunk_size].
    - Negative coordinates are handled via positive modulo:
      [x = -1 → (cx = -1, bx = chunk_size - 1)].

    {2 Unloaded chunk}
    - [get_block] on an unloaded chunk coordinate returns [Air].
    - [set_block] on an unloaded chunk is a no-op (no exception).
    - [get_chunk] returns [None] for an unloaded coordinate.

    {2 [generate_chunk]}
    - After calling [generate_chunk w ~cx ~cy ~cz], [get_chunk w cx cy cz]
      returns [Some chunk].
    - The chunk has the correct [(cx, cy, cz)] coordinates.
    - Re-generating the same chunk (using [Hashtbl.replace]) replaces the
      existing entry; the iteration count does not grow.

    {2 [get_block] / [set_block]}
    - [set_block] followed by [get_block] at the same coordinates returns the
      written block (when the chunk is loaded).
    - Works correctly across chunk boundaries and with negative coordinates.

    {2 [mesh_chunk]}
    - Returns two arrays of equal length, both divisible by 3.
    - An all-Air chunk produces empty arrays.
    - A single isolated block produces 6 faces × 6 vertices × 3 floats = 108
      elements in each array.
    - All color values in the color array lie in [0, 1].

    {2 [iter]}
    - Visits every generated chunk exactly once.

    {2 [add_chunk] / [remove_chunk]}
    - [add_chunk] inserts a chunk under its declared coordinates and is
      observable via [get_chunk].
    - Adding a chunk at a coordinate that already has one replaces the existing
      entry; the loaded count does not grow.
    - [remove_chunk] removes the chunk; subsequent [get_chunk] returns [None]
      and [get_block] reverts to [Air].
    - [remove_chunk] on an unloaded coordinate is a no-op (no exception). *)

open OUnit2

let cs = Config.chunk_size

(* ------------------------------------------------------------------ *)
(*  Helpers                                                             *)
(* ------------------------------------------------------------------ *)

(** printer for [Block.t] values. *)
let pp_block = function
  | Block.Air -> "Air"
  | Block.Stone -> "Stone"
  | Block.Dirt -> "Dirt"
  | Block.Grass -> "Grass"

(** printer for [(int * int * int)] coords. *)
let pp_xyz (x, y, z) = Printf.sprintf "(%d, %d, %d)" x y z

(** printer for a list of int triples, used by iter coverage tests. *)
let pp_xyz_list lst = "[" ^ String.concat "; " (List.map pp_xyz lst) ^ "]"

(** printer for [Chunk.t option] - we only need to see whether it's None. *)
let pp_chunk_opt = function
  | None -> "None"
  | Some _ -> "Some _"

let fresh () = World.create ()

let with_chunk ?(cx = 0) ?(cy = 0) ?(cz = 0) w =
  World.generate_chunk w ~cx ~cy ~cz;
  w

(* ------------------------------------------------------------------ *)
(*  {1 Empty world}                                                     *)
(* ------------------------------------------------------------------ *)

let test_create_empty _ =
  let w = fresh () in
  let count = ref 0 in
  World.iter w (fun _ -> incr count);
  assert_equal ~printer:string_of_int ~msg:"no chunks initially" 0 !count

(* ------------------------------------------------------------------ *)
(*  {1 Unloaded chunk behaviour}                                        *)
(* ------------------------------------------------------------------ *)

let test_get_block_unloaded _ =
  let w = fresh () in
  assert_equal ~printer:pp_block ~msg:"unloaded → Air" Block.Air
    (World.get_block w 100 100 100)

let test_get_chunk_none _ =
  let w = fresh () in
  assert_equal ~printer:pp_chunk_opt ~msg:"get_chunk on empty → None" None
    (World.get_chunk w 9 9 9)

let test_set_block_unloaded_noop _ =
  let w = fresh () in
  (* Must not raise and the result should still be Air. *)
  World.set_block w 500 500 500 Block.Stone;
  assert_equal ~printer:pp_block ~msg:"still Air after no-op set" Block.Air
    (World.get_block w 500 500 500)

(* ------------------------------------------------------------------ *)
(*  {1 generate_chunk}                                                  *)
(* ------------------------------------------------------------------ *)

let test_generate_chunk_loads _ =
  let w = fresh () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  assert_bool "chunk present" (World.get_chunk w 0 0 0 <> None)

let test_generate_chunk_coords _ =
  let w = fresh () in
  World.generate_chunk w ~cx:3 ~cy:(-1) ~cz:2;
  match World.get_chunk w 3 (-1) 2 with
  | None -> assert_failure "chunk not found"
  | Some c ->
      assert_equal ~printer:string_of_int 3 (Chunk.x c);
      assert_equal ~printer:string_of_int (-1) (Chunk.y c);
      assert_equal ~printer:string_of_int 2 (Chunk.z c)

let test_generate_multiple_chunks _ =
  let w = fresh () in
  let coords = [ (0, 0, 0); (1, 0, 0); (0, 1, 0); (0, 0, 1) ] in
  List.iter (fun (cx, cy, cz) -> World.generate_chunk w ~cx ~cy ~cz) coords;
  let count = ref 0 in
  World.iter w (fun _ -> incr count);
  assert_equal ~printer:string_of_int ~msg:"4 chunks loaded" 4 !count

let test_generate_duplicate_no_growth _ =
  let w = fresh () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  let count = ref 0 in
  World.iter w (fun _ -> incr count);
  assert_bool "at most 1 chunk after duplicate" (!count <= 1)

(* ------------------------------------------------------------------ *)
(*  {1 get_block / set_block}                                           *)
(* ------------------------------------------------------------------ *)

let test_set_then_get _ =
  let w = with_chunk (fresh ()) in
  World.set_block w 0 0 0 Block.Stone;
  assert_equal ~printer:pp_block ~msg:"get after set" Block.Stone
    (World.get_block w 0 0 0)

let test_set_get_all_types _ =
  let w = with_chunk (fresh ()) in
  List.iter
    (fun b ->
      World.set_block w 0 0 0 b;
      assert_equal ~printer:pp_block ~msg:"round-trip" b
        (World.get_block w 0 0 0))
    [ Block.Stone; Block.Dirt; Block.Grass; Block.Air ]

(** Underground block at y = 0 should be Stone (terrain invariant). *)
let test_get_block_underground _ =
  let w = with_chunk (fresh ()) in
  assert_equal ~printer:pp_block ~msg:"underground = Stone" Block.Stone
    (World.get_block w 0 0 0)

(** Block at the chunk boundary (world x = chunk_size - 1) is accessible. *)
let test_get_block_chunk_boundary _ =
  let w = with_chunk (fresh ()) in
  let x = cs - 1 in
  (* Just confirm no exception is raised. *)
  let _ = World.get_block w x 0 0 in
  ()

(* ------------------------------------------------------------------ *)
(*  {1 Coordinate mapping — negative / cross-chunk}                    *)
(*  Verified via round-trip: generate both chunks, set and get.        *)
(* ------------------------------------------------------------------ *)

(** World x = -1 maps to chunk cx = -1, local bx = cs-1. Generate that chunk and
    verify set/get. *)
let test_negative_world_coord _ =
  let w = fresh () in
  World.generate_chunk w ~cx:(-1) ~cy:0 ~cz:0;
  World.set_block w (-1) 0 0 Block.Grass;
  assert_equal ~printer:pp_block ~msg:"get at x=-1" Block.Grass
    (World.get_block w (-1) 0 0)

(** World x = cs maps to chunk cx = 1, local bx = 0. *)
let test_cross_chunk_boundary _ =
  let w = fresh () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  World.generate_chunk w ~cx:1 ~cy:0 ~cz:0;
  World.set_block w cs 0 0 Block.Dirt;
  assert_equal ~printer:pp_block ~msg:"get at x=cs" Block.Dirt
    (World.get_block w cs 0 0)

(** x = -cs maps to chunk cx = -1, local bx = 0. *)
let test_negative_chunk_boundary _ =
  let w = fresh () in
  World.generate_chunk w ~cx:(-1) ~cy:0 ~cz:0;
  World.set_block w (-cs) 0 0 Block.Stone;
  assert_equal ~printer:pp_block ~msg:"get at x=-cs" Block.Stone
    (World.get_block w (-cs) 0 0)

(* ------------------------------------------------------------------ *)
(*  {1 iter}                                                            *)
(* ------------------------------------------------------------------ *)

let test_iter_count _ =
  let w = fresh () in
  for cx = 0 to 2 do
    World.generate_chunk w ~cx ~cy:0 ~cz:0
  done;
  let count = ref 0 in
  World.iter w (fun _ -> incr count);
  assert_equal ~printer:string_of_int ~msg:"3 chunks" 3 !count

let test_iter_visits_correct_coords _ =
  let w = fresh () in
  let expected = [ (0, 0, 0); (1, 0, 0); (0, 1, 0) ] in
  List.iter (fun (cx, cy, cz) -> World.generate_chunk w ~cx ~cy ~cz) expected;
  let found = ref [] in
  World.iter w (fun c -> found := (Chunk.x c, Chunk.y c, Chunk.z c) :: !found);
  let sorted_found = List.sort compare !found in
  let sorted_expected = List.sort compare expected in
  assert_equal ~printer:pp_xyz_list ~msg:"iter visits all coords"
    sorted_expected sorted_found

(* ------------------------------------------------------------------ *)
(*  {1 mesh_chunk}                                                      *)
(* ------------------------------------------------------------------ *)

let test_mesh_equal_lengths _ =
  let w = with_chunk (fresh ()) in
  let chunk = Option.get (World.get_chunk w 0 0 0) in
  let pos, col = World.mesh_chunk w chunk in
  assert_equal ~printer:string_of_int ~msg:"same length" (Array.length pos)
    (Array.length col)

let test_mesh_divisible_by_3 _ =
  let w = with_chunk (fresh ()) in
  let chunk = Option.get (World.get_chunk w 0 0 0) in
  let pos, col = World.mesh_chunk w chunk in
  assert_equal ~printer:string_of_int ~msg:"pos % 3 = 0" 0
    (Array.length pos mod 3);
  assert_equal ~printer:string_of_int ~msg:"col % 3 = 0" 0
    (Array.length col mod 3)

(** An all-Air chunk produces zero vertices. *)
let test_mesh_sky_empty _ =
  let w = fresh () in
  World.generate_chunk w ~cx:0 ~cy:5 ~cz:0;
  let chunk = Option.get (World.get_chunk w 0 5 0) in
  let pos, col = World.mesh_chunk w chunk in
  assert_equal ~printer:string_of_int ~msg:"sky pos empty" 0 (Array.length pos);
  assert_equal ~printer:string_of_int ~msg:"sky col empty" 0 (Array.length col)

(** A non-empty chunk has a non-empty mesh. *)
let test_mesh_nonempty _ =
  let w = with_chunk (fresh ()) in
  let chunk = Option.get (World.get_chunk w 0 0 0) in
  let pos, _ = World.mesh_chunk w chunk in
  assert_bool "surface chunk → non-empty mesh" (Array.length pos > 0)

(** An isolated single block exposes all 6 faces. 6 faces × 6 vertices × 3
    floats = 108. *)
let test_mesh_single_block_face_count _ =
  let w = fresh () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  (* Clear the whole chunk to Air, then set exactly one block. *)
  for bx = 0 to cs - 1 do
    for by = 0 to cs - 1 do
      for bz = 0 to cs - 1 do
        World.set_block w bx by bz Block.Air
      done
    done
  done;
  World.set_block w 0 0 0 Block.Stone;
  let chunk = Option.get (World.get_chunk w 0 0 0) in
  let pos, col = World.mesh_chunk w chunk in
  assert_equal ~printer:string_of_int ~msg:"6 faces × 6 verts × 3 floats = 108"
    108 (Array.length pos);
  assert_equal ~printer:string_of_int ~msg:"colors same count" 108
    (Array.length col)

(** All colour component values are in [0, 1]. *)
let test_mesh_color_in_range _ =
  let w = with_chunk (fresh ()) in
  let chunk = Option.get (World.get_chunk w 0 0 0) in
  let _, col = World.mesh_chunk w chunk in
  Array.iteri
    (fun i v ->
      assert_bool
        (Printf.sprintf "color[%d]=%f in [0,1]" i v)
        (v >= 0.0 && v <= 1.0))
    col

(* ------------------------------------------------------------------ *)
(*  {1 add_chunk / remove_chunk}                                        *)
(* ------------------------------------------------------------------ *)

let test_add_chunk_inserts _ =
  let w = fresh () in
  let blocks = Array.make (cs * cs * cs) Block.Stone in
  let c = Chunk.create ~x:2 ~y:0 ~z:(-3) ~blocks in
  World.add_chunk w c;
  match World.get_chunk w 2 0 (-3) with
  | None -> assert_failure "add_chunk did not insert"
  | Some c' ->
      assert_equal ~printer:string_of_int 2 (Chunk.x c');
      assert_equal ~printer:string_of_int 0 (Chunk.y c');
      assert_equal ~printer:string_of_int (-3) (Chunk.z c')

let test_add_chunk_replaces _ =
  let w = fresh () in
  let stone = Array.make (cs * cs * cs) Block.Stone in
  let dirt = Array.make (cs * cs * cs) Block.Dirt in
  World.add_chunk w (Chunk.create ~x:0 ~y:0 ~z:0 ~blocks:stone);
  World.add_chunk w (Chunk.create ~x:0 ~y:0 ~z:0 ~blocks:dirt);
  let count = ref 0 in
  World.iter w (fun _ -> incr count);
  assert_equal ~printer:string_of_int ~msg:"replace, not duplicate" 1 !count;
  assert_equal ~printer:pp_block ~msg:"second chunk wins" Block.Dirt
    (World.get_block w 0 0 0)

let test_remove_chunk _ =
  let w = with_chunk (fresh ()) in
  assert_bool "chunk loaded before remove" (World.get_chunk w 0 0 0 <> None);
  World.remove_chunk w 0 0 0;
  assert_equal ~printer:pp_chunk_opt ~msg:"get_chunk → None after remove" None
    (World.get_chunk w 0 0 0);
  assert_equal ~printer:pp_block ~msg:"get_block → Air after remove" Block.Air
    (World.get_block w 0 0 0)

let test_remove_missing_noop _ =
  let w = fresh () in
  (* Must not raise. *)
  World.remove_chunk w 42 42 42;
  let count = ref 0 in
  World.iter w (fun _ -> incr count);
  assert_equal ~printer:string_of_int ~msg:"empty stays empty" 0 !count

let test_add_then_remove_iter _ =
  let w = fresh () in
  let blocks = Array.make (cs * cs * cs) Block.Air in
  World.add_chunk w (Chunk.create ~x:1 ~y:0 ~z:0 ~blocks);
  World.add_chunk w (Chunk.create ~x:2 ~y:0 ~z:0 ~blocks);
  World.remove_chunk w 1 0 0;
  let found = ref [] in
  World.iter w (fun c -> found := (Chunk.x c, Chunk.y c, Chunk.z c) :: !found);
  assert_equal ~printer:pp_xyz_list ~msg:"only chunk 2 remains"
    [ (2, 0, 0) ]
    !found

(* ------------------------------------------------------------------ *)
(*  {1 raycast}                                                        *)
(* ------------------------------------------------------------------ *)

(** printer for the raycast result option. *)
let pp_raycast = function
  | None -> "None"
  | Some (bx, by, bz, nx, ny, nz) ->
      Printf.sprintf "Some (%d,%d,%d) normal (%d,%d,%d)" bx by bz nx ny nz

(** Helper: fresh chunk cleared to all Air, then optionally a single
    Stone block set. Returns the world with cx=cy=cz=0 loaded. *)
let world_with_stone bx by bz =
  let w = fresh () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  for x = 0 to cs - 1 do
    for y = 0 to cs - 1 do
      for z = 0 to cs - 1 do
        World.set_block w x y z Block.Air
      done
    done
  done;
  World.set_block w bx by bz Block.Stone;
  w

(** In an empty world (no chunks) [raycast] always returns [None]. *)
let test_raycast_none_empty _ =
  let w = fresh () in
  let origin = Math3d.vec3 0.5 0.5 (-5.0) in
  let dir = Math3d.vec3 0.0 0.0 1.0 in
  assert_equal ~printer:pp_raycast ~msg:"empty world → None" None
    (World.raycast w ~origin ~dir ~max_dist:100.0)

(** A ray aimed along +Z at a block directly ahead must hit that block. *)
let test_raycast_hits_block _ =
  let w = world_with_stone 0 5 8 in
  (* origin in front of the block along -Z, traveling +Z *)
  let origin = Math3d.vec3 0.5 5.5 (-2.0) in
  let dir = Math3d.vec3 0.0 0.0 1.0 in
  match World.raycast w ~origin ~dir ~max_dist:20.0 with
  | None -> assert_failure "expected raycast hit, got None"
  | Some (rx, ry, rz, _, _, _) ->
      assert_equal ~printer:string_of_int ~msg:"hit bx" 0 rx;
      assert_equal ~printer:string_of_int ~msg:"hit by" 5 ry;
      assert_equal ~printer:string_of_int ~msg:"hit bz" 8 rz

(** When [max_dist] is smaller than the distance to the block, [raycast]
    returns [None]. *)
let test_raycast_max_dist _ =
  let w = world_with_stone 0 5 8 in
  (* block is ~10 units away; max_dist = 5 should miss it *)
  let origin = Math3d.vec3 0.5 5.5 (-2.0) in
  let dir = Math3d.vec3 0.0 0.0 1.0 in
  assert_equal ~printer:pp_raycast ~msg:"max_dist stops traversal" None
    (World.raycast w ~origin ~dir ~max_dist:5.0)

(** A ray traveling in +Z hits a block on its -Z face; the face normal must
    be (0, 0, -1). *)
let test_raycast_normal_neg_z _ =
  let w = world_with_stone 0 5 8 in
  let origin = Math3d.vec3 0.5 5.5 (-2.0) in
  let dir = Math3d.vec3 0.0 0.0 1.0 in
  match World.raycast w ~origin ~dir ~max_dist:20.0 with
  | None -> assert_failure "expected hit"
  | Some (_, _, _, nx, ny, nz) ->
      assert_equal ~printer:string_of_int ~msg:"+Z ray: nx = 0" 0 nx;
      assert_equal ~printer:string_of_int ~msg:"+Z ray: ny = 0" 0 ny;
      assert_equal ~printer:string_of_int ~msg:"+Z ray: nz = -1" (-1) nz

(** A ray descending along -Y hits a block on its +Y face; normal = (0,1,0). *)
let test_raycast_normal_from_above _ =
  let w = world_with_stone 0 5 0 in
  let origin = Math3d.vec3 0.5 13.0 0.5 in
  let dir = Math3d.vec3 0.0 (-1.0) 0.0 in
  match World.raycast w ~origin ~dir ~max_dist:20.0 with
  | None -> assert_failure "expected hit from above"
  | Some (_, _, _, nx, ny, nz) ->
      assert_equal ~printer:string_of_int ~msg:"-Y ray: nx = 0" 0 nx;
      assert_equal ~printer:string_of_int ~msg:"-Y ray: ny = 1" 1 ny;
      assert_equal ~printer:string_of_int ~msg:"-Y ray: nz = 0" 0 nz

(** A ray aimed away from the block misses it entirely. *)
let test_raycast_miss _ =
  let w = world_with_stone 0 5 8 in
  (* ray pointing -Z, block is in +Z direction: no hit *)
  let origin = Math3d.vec3 0.5 5.5 (-2.0) in
  let dir = Math3d.vec3 0.0 0.0 (-1.0) in
  assert_equal ~printer:pp_raycast ~msg:"ray pointing away → None" None
    (World.raycast w ~origin ~dir ~max_dist:20.0)

(* ------------------------------------------------------------------ *)
(*  QCheck property                                                     *)
(* ------------------------------------------------------------------ *)

(** In a world with no chunks, [raycast] always returns [None] regardless
    of origin and direction. *)
let qcheck_raycast_empty_none =
  let arb_float = QCheck2.Gen.float_range (-100.0) 100.0 in
  let arb_dir = QCheck2.Gen.float_range (-1.0) 1.0 in
  QCheck2.Test.make ~name:"raycast_empty_world_none" ~count:500
    (QCheck2.Gen.pair
       (QCheck2.Gen.triple arb_float arb_float arb_float)
       (QCheck2.Gen.triple arb_dir arb_dir arb_dir))
    (fun ((ox, oy, oz), (dx, dy, dz)) ->
      let len = sqrt ((dx *. dx) +. (dy *. dy) +. (dz *. dz)) in
      (* skip degenerate zero-direction vectors *)
      if len < 1e-9 then true
      else
        let w = World.create () in
        let origin = Math3d.vec3 ox oy oz in
        let dir = Math3d.vec3 dx dy dz in
        World.raycast w ~origin ~dir ~max_dist:1000.0 = None)

let tests =
  "World"
  >::: [
         (* Empty world *)
         "create_empty" >:: test_create_empty;
         (* Unloaded *)
         "get_block_unloaded" >:: test_get_block_unloaded;
         "get_chunk_none" >:: test_get_chunk_none;
         "set_block_unloaded" >:: test_set_block_unloaded_noop;
         (* generate_chunk *)
         "generate_loads" >:: test_generate_chunk_loads;
         "generate_coords" >:: test_generate_chunk_coords;
         "generate_multiple" >:: test_generate_multiple_chunks;
         "generate_duplicate" >:: test_generate_duplicate_no_growth;
         (* get/set block *)
         "set_then_get" >:: test_set_then_get;
         "set_get_all_types" >:: test_set_get_all_types;
         "get_underground" >:: test_get_block_underground;
         "get_boundary" >:: test_get_block_chunk_boundary;
         (* Coordinate mapping *)
         "neg_world_coord" >:: test_negative_world_coord;
         "cross_chunk" >:: test_cross_chunk_boundary;
         "neg_chunk_boundary" >:: test_negative_chunk_boundary;
         (* iter *)
         "iter_count" >:: test_iter_count;
         "iter_coords" >:: test_iter_visits_correct_coords;
         (* mesh_chunk *)
         "mesh_equal_lengths" >:: test_mesh_equal_lengths;
         "mesh_div_3" >:: test_mesh_divisible_by_3;
         "mesh_sky_empty" >:: test_mesh_sky_empty;
         "mesh_nonempty" >:: test_mesh_nonempty;
         "mesh_single_block" >:: test_mesh_single_block_face_count;
         "mesh_color_range" >:: test_mesh_color_in_range;
         (* add_chunk / remove_chunk *)
         "add_chunk_inserts" >:: test_add_chunk_inserts;
         "add_chunk_replaces" >:: test_add_chunk_replaces;
         "remove_chunk" >:: test_remove_chunk;
         "remove_missing_noop" >:: test_remove_missing_noop;
         "add_then_remove_iter" >:: test_add_then_remove_iter;
         (* raycast *)
         "raycast_none_empty" >:: test_raycast_none_empty;
         "raycast_hits_block" >:: test_raycast_hits_block;
         "raycast_max_dist" >:: test_raycast_max_dist;
         "raycast_normal_neg_z" >:: test_raycast_normal_neg_z;
         "raycast_normal_from_above" >:: test_raycast_normal_from_above;
         "raycast_miss" >:: test_raycast_miss;
         (* qcheck *)
         QCheck_ounit.to_ounit2_test qcheck_raycast_empty_none;
       ]

let _ = run_test_tt_main tests
