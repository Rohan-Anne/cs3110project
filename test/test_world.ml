(** Tests for the {!World} module.

    {1 Module Overview}
    [World] maintains a hash-table of loaded chunks keyed by
    [(cx, cy, cz)] chunk coordinates.  It converts between world-space
    block coordinates and chunk/local coordinates, delegates terrain
    generation to [Terrain.fill_chunk], and constructs triangle meshes
    for rendering.

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
    - [set_block] followed by [get_block] at the same coordinates returns
      the written block (when the chunk is loaded).
    - Works correctly across chunk boundaries and with negative coordinates.

    {2 [mesh_chunk]}
    - Returns two arrays of equal length, both divisible by 3.
    - An all-Air chunk produces empty arrays.
    - A single isolated block produces 6 faces × 6 vertices × 3 floats =
      108 elements in each array.
    - All color values in the color array lie in [0, 1].

    {2 [iter]}
    - Visits every generated chunk exactly once. *)

open OUnit2

let cs = Config.chunk_size

(* ------------------------------------------------------------------ *)
(*  Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let fresh () = World.create ()

let with_chunk ?(cx = 0) ?(cy = 0) ?(cz = 0) w =
  World.generate_chunk w ~cx ~cy ~cz; w

(* ------------------------------------------------------------------ *)
(*  {1 Empty world}                                                     *)
(* ------------------------------------------------------------------ *)

let test_create_empty _ =
  let w = fresh () in
  let count = ref 0 in
  World.iter w (fun _ -> incr count);
  assert_equal ~msg:"no chunks initially" 0 !count

(* ------------------------------------------------------------------ *)
(*  {1 Unloaded chunk behaviour}                                        *)
(* ------------------------------------------------------------------ *)

let test_get_block_unloaded _ =
  let w = fresh () in
  assert_equal ~msg:"unloaded → Air" Block.Air (World.get_block w 100 100 100)

let test_get_chunk_none _ =
  let w = fresh () in
  assert_equal ~msg:"get_chunk on empty → None" None (World.get_chunk w 9 9 9)

let test_set_block_unloaded_noop _ =
  let w = fresh () in
  (* Must not raise and the result should still be Air. *)
  World.set_block w 500 500 500 Block.Stone;
  assert_equal ~msg:"still Air after no-op set" Block.Air
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
    assert_equal 3  (Chunk.x c);
    assert_equal (-1) (Chunk.y c);
    assert_equal 2  (Chunk.z c)

let test_generate_multiple_chunks _ =
  let w = fresh () in
  let coords = [(0,0,0);(1,0,0);(0,1,0);(0,0,1)] in
  List.iter (fun (cx,cy,cz) -> World.generate_chunk w ~cx ~cy ~cz) coords;
  let count = ref 0 in
  World.iter w (fun _ -> incr count);
  assert_equal ~msg:"4 chunks loaded" 4 !count

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
  assert_equal ~msg:"get after set" Block.Stone (World.get_block w 0 0 0)

let test_set_get_all_types _ =
  let w = with_chunk (fresh ()) in
  List.iter (fun b ->
    World.set_block w 0 0 0 b;
    assert_equal ~msg:"round-trip" b (World.get_block w 0 0 0)
  ) [ Block.Stone; Block.Dirt; Block.Grass; Block.Air ]

(** Underground block at y = 0 should be Stone (terrain invariant). *)
let test_get_block_underground _ =
  let w = with_chunk (fresh ()) in
  assert_equal ~msg:"underground = Stone" Block.Stone (World.get_block w 0 0 0)

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

(** World x = -1 maps to chunk cx = -1, local bx = cs-1.
    Generate that chunk and verify set/get. *)
let test_negative_world_coord _ =
  let w = fresh () in
  World.generate_chunk w ~cx:(-1) ~cy:0 ~cz:0;
  World.set_block w (-1) 0 0 Block.Grass;
  assert_equal ~msg:"get at x=-1" Block.Grass (World.get_block w (-1) 0 0)

(** World x = cs maps to chunk cx = 1, local bx = 0. *)
let test_cross_chunk_boundary _ =
  let w = fresh () in
  World.generate_chunk w ~cx:0 ~cy:0 ~cz:0;
  World.generate_chunk w ~cx:1 ~cy:0 ~cz:0;
  World.set_block w cs 0 0 Block.Dirt;
  assert_equal ~msg:"get at x=cs" Block.Dirt (World.get_block w cs 0 0)

(** x = -cs maps to chunk cx = -1, local bx = 0. *)
let test_negative_chunk_boundary _ =
  let w = fresh () in
  World.generate_chunk w ~cx:(-1) ~cy:0 ~cz:0;
  World.set_block w (-cs) 0 0 Block.Stone;
  assert_equal ~msg:"get at x=-cs" Block.Stone (World.get_block w (-cs) 0 0)

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
  assert_equal ~msg:"3 chunks" 3 !count

let test_iter_visits_correct_coords _ =
  let w = fresh () in
  let expected = [(0,0,0);(1,0,0);(0,1,0)] in
  List.iter (fun (cx,cy,cz) -> World.generate_chunk w ~cx ~cy ~cz) expected;
  let found = ref [] in
  World.iter w (fun c ->
    found := (Chunk.x c, Chunk.y c, Chunk.z c) :: !found);
  let sorted_found    = List.sort compare !found in
  let sorted_expected = List.sort compare expected in
  assert_equal ~msg:"iter visits all coords" sorted_expected sorted_found

(* ------------------------------------------------------------------ *)
(*  {1 mesh_chunk}                                                      *)
(* ------------------------------------------------------------------ *)

let test_mesh_equal_lengths _ =
  let w = with_chunk (fresh ()) in
  let chunk = Option.get (World.get_chunk w 0 0 0) in
  let (pos, col) = World.mesh_chunk w chunk in
  assert_equal ~msg:"same length" (Array.length pos) (Array.length col)

let test_mesh_divisible_by_3 _ =
  let w = with_chunk (fresh ()) in
  let chunk = Option.get (World.get_chunk w 0 0 0) in
  let (pos, col) = World.mesh_chunk w chunk in
  assert_equal ~msg:"pos % 3 = 0" 0 (Array.length pos mod 3);
  assert_equal ~msg:"col % 3 = 0" 0 (Array.length col mod 3)

(** An all-Air chunk produces zero vertices. *)
let test_mesh_sky_empty _ =
  let w = fresh () in
  World.generate_chunk w ~cx:0 ~cy:5 ~cz:0;
  let chunk = Option.get (World.get_chunk w 0 5 0) in
  let (pos, col) = World.mesh_chunk w chunk in
  assert_equal ~msg:"sky pos empty" 0 (Array.length pos);
  assert_equal ~msg:"sky col empty" 0 (Array.length col)

(** A non-empty chunk has a non-empty mesh. *)
let test_mesh_nonempty _ =
  let w = with_chunk (fresh ()) in
  let chunk = Option.get (World.get_chunk w 0 0 0) in
  let (pos, _) = World.mesh_chunk w chunk in
  assert_bool "surface chunk → non-empty mesh" (Array.length pos > 0)

(** An isolated single block exposes all 6 faces.
    6 faces × 6 vertices × 3 floats = 108. *)
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
  let (pos, col) = World.mesh_chunk w chunk in
  assert_equal ~msg:"6 faces × 6 verts × 3 floats = 108" 108 (Array.length pos);
  assert_equal ~msg:"colors same count" 108 (Array.length col)

(** All colour component values are in [0, 1]. *)
let test_mesh_color_in_range _ =
  let w = with_chunk (fresh ()) in
  let chunk = Option.get (World.get_chunk w 0 0 0) in
  let (_, col) = World.mesh_chunk w chunk in
  Array.iteri (fun i v ->
    assert_bool
      (Printf.sprintf "color[%d]=%f in [0,1]" i v)
      (v >= 0.0 && v <= 1.0)
  ) col

let tests =
  "World"
  >::: [
    (* Empty world *)
    "create_empty"           >:: test_create_empty;
    (* Unloaded *)
    "get_block_unloaded"     >:: test_get_block_unloaded;
    "get_chunk_none"         >:: test_get_chunk_none;
    "set_block_unloaded"     >:: test_set_block_unloaded_noop;
    (* generate_chunk *)
    "generate_loads"         >:: test_generate_chunk_loads;
    "generate_coords"        >:: test_generate_chunk_coords;
    "generate_multiple"      >:: test_generate_multiple_chunks;
    "generate_duplicate"     >:: test_generate_duplicate_no_growth;
    (* get/set block *)
    "set_then_get"           >:: test_set_then_get;
    "set_get_all_types"      >:: test_set_get_all_types;
    "get_underground"        >:: test_get_block_underground;
    "get_boundary"           >:: test_get_block_chunk_boundary;
    (* Coordinate mapping *)
    "neg_world_coord"        >:: test_negative_world_coord;
    "cross_chunk"            >:: test_cross_chunk_boundary;
    "neg_chunk_boundary"     >:: test_negative_chunk_boundary;
    (* iter *)
    "iter_count"             >:: test_iter_count;
    "iter_coords"            >:: test_iter_visits_correct_coords;
    (* mesh_chunk *)
    "mesh_equal_lengths"     >:: test_mesh_equal_lengths;
    "mesh_div_3"             >:: test_mesh_divisible_by_3;
    "mesh_sky_empty"         >:: test_mesh_sky_empty;
    "mesh_nonempty"          >:: test_mesh_nonempty;
    "mesh_single_block"      >:: test_mesh_single_block_face_count;
    "mesh_color_range"       >:: test_mesh_color_in_range;
  ]

let _ = run_test_tt_main tests
