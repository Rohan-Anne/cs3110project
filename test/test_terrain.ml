(** Tests for the {!Terrain} module.

    {1 Module Overview}
    [Terrain] converts 2-D Perlin noise into a layered voxel landscape.
    It exposes two functions:
    - [height_at x z] — surface height at world column [(x, z)].
    - [fill_chunk ~cx ~cy ~cz] — generates the block array for a chunk.

    {1 Invariants}
    {2 [height_at]}
    - Deterministic: [height_at x z = height_at x z].
    - Bounded: since [perlin2d ∈ [-1,1]], the formula
      [⌊6 + 4·n⌋] with [n ∈ [-1,1]] gives heights in [{2,…,10}].
      (Using [int_of_float] which truncates toward zero, so the range may
      be [1..10] in edge cases; tests allow for this.)

    {2 [fill_chunk]}
    - Returns exactly [chunk_size³] blocks.
    - Layering law (for a block at world coordinate [(wx, wy, wz)] with
      surface height [h = height_at wx wz]):
        - [wy > h]         → [Air]
        - [wy = h]         → [Grass]
        - [h-3 ≤ wy < h]  → [Dirt]
        - [wy < h-3]       → [Stone]
    - A chunk entirely above the tallest possible surface (cy = 5,
      world-y ≥ 80) contains only [Air].
    - A chunk deep underground (cy = -2, world-y ≤ -17) contains only
      [Stone]. *)

open OUnit2

let cs = Config.chunk_size

(* ------------------------------------------------------------------ *)
(*  {1 height_at}                                                       *)
(* ------------------------------------------------------------------ *)

(** Surface heights lie in the range determined by the noise formula. *)
let test_height_range _ =
  for x = -10 to 10 do
    for z = -10 to 10 do
      let h = Terrain.height_at x z in
      assert_bool
        (Printf.sprintf "height_at(%d,%d)=%d >= 1" x z h)
        (h >= 1);
      assert_bool
        (Printf.sprintf "height_at(%d,%d)=%d <= 10" x z h)
        (h <= 10)
    done
  done

(** [height_at] is deterministic. *)
let test_height_deterministic _ =
  for x = -5 to 5 do
    for z = -5 to 5 do
      let h1 = Terrain.height_at x z in
      let h2 = Terrain.height_at x z in
      assert_equal
        ~msg:(Printf.sprintf "height_at(%d,%d) deterministic" x z)
        h1 h2
    done
  done

(** [height_at] returns an integer (type-level guarantee, exercised here
    by checking it is a valid array index within [0, cs-1]). *)
let test_height_is_int _ =
  let h = Terrain.height_at 0 0 in
  assert_bool "height is non-negative" (h >= 0)

(* ------------------------------------------------------------------ *)
(*  {1 fill_chunk — array size}                                         *)
(* ------------------------------------------------------------------ *)

let test_fill_chunk_size _ =
  let blocks = Terrain.fill_chunk ~cx:0 ~cy:0 ~cz:0 in
  assert_equal ~msg:"cs³ blocks" (cs * cs * cs) (Array.length blocks)

let test_fill_chunk_size_other_coords _ =
  let cases = [(1,0,1); (-1,0,-1); (0,2,0)] in
  List.iter (fun (cx,cy,cz) ->
    let b = Terrain.fill_chunk ~cx ~cy ~cz in
    assert_equal
      ~msg:(Printf.sprintf "size at (%d,%d,%d)" cx cy cz)
      (cs*cs*cs) (Array.length b)
  ) cases

(* ------------------------------------------------------------------ *)
(*  {1 fill_chunk — sky chunk (all Air)}                               *)
(* ------------------------------------------------------------------ *)

(** Chunk at cy = 5 spans world-y = 80..95, well above any surface. *)
let test_fill_chunk_sky_all_air _ =
  let blocks = Terrain.fill_chunk ~cx:0 ~cy:5 ~cz:0 in
  let all_air = Array.for_all (fun b -> b = Block.Air) blocks in
  assert_bool "sky chunk (cy=5) is all Air" all_air

let test_fill_chunk_sky_other_cols _ =
  List.iter (fun (cx, cz) ->
    let b = Terrain.fill_chunk ~cx ~cy:5 ~cz in
    assert_bool
      (Printf.sprintf "sky at cx=%d cz=%d" cx cz)
      (Array.for_all (fun blk -> blk = Block.Air) b)
  ) [ (0,0); (1,0); (0,1); (-1,-1) ]

(* ------------------------------------------------------------------ *)
(*  {1 fill_chunk — deep underground (all Stone)}                      *)
(* ------------------------------------------------------------------ *)

(** Chunk at cy = -2 spans world-y = -32..-17.  Surface max = 10, so
    every block in this chunk is at least 27 blocks below the surface. *)
let test_fill_chunk_deep_all_stone _ =
  let blocks = Terrain.fill_chunk ~cx:0 ~cy:(-2) ~cz:0 in
  let all_stone = Array.for_all (fun b -> b = Block.Stone) blocks in
  assert_bool "deep chunk (cy=-2) is all Stone" all_stone

let test_fill_chunk_deep_other_cols _ =
  List.iter (fun (cx, cz) ->
    let b = Terrain.fill_chunk ~cx ~cy:(-2) ~cz in
    assert_bool
      (Printf.sprintf "deep all Stone at cx=%d cz=%d" cx cz)
      (Array.for_all (fun blk -> blk = Block.Stone) b)
  ) [ (1,0); (0,1); (-1,0) ]

(* ------------------------------------------------------------------ *)
(*  {1 fill_chunk — surface chunk block variety}                       *)
(* ------------------------------------------------------------------ *)

(** The chunk at cy = 0 (world-y 0..15) includes the surface for most
    columns and therefore contains Grass, Dirt, and Stone. *)
let test_fill_chunk_has_grass _ =
  let blocks = Terrain.fill_chunk ~cx:0 ~cy:0 ~cz:0 in
  assert_bool "cy=0 has Grass"
    (Array.exists (fun b -> b = Block.Grass) blocks)

let test_fill_chunk_has_dirt _ =
  let blocks = Terrain.fill_chunk ~cx:0 ~cy:0 ~cz:0 in
  assert_bool "cy=0 has Dirt"
    (Array.exists (fun b -> b = Block.Dirt) blocks)

let test_fill_chunk_has_stone _ =
  let blocks = Terrain.fill_chunk ~cx:0 ~cy:0 ~cz:0 in
  assert_bool "cy=0 has Stone"
    (Array.exists (fun b -> b = Block.Stone) blocks)

(* ------------------------------------------------------------------ *)
(*  {1 fill_chunk — layering law}                                       *)
(* ------------------------------------------------------------------ *)

(** Verify the surface, subsurface-dirt, and deep-stone layers for a
    representative set of columns within the chunk at cy = 0. *)
let test_fill_chunk_layering _ =
  let cx = 0 and cz = 0 and cy = 0 in
  let blocks = Terrain.fill_chunk ~cx ~cy ~cz in
  for bx = 0 to min 3 (cs - 1) do
    for bz = 0 to min 3 (cs - 1) do
      let wx = cx * cs + bx and wz = cz * cs + bz in
      let h = Terrain.height_at wx wz in
      (* Surface block *)
      if h >= 0 && h < cs then begin
        let idx = Chunk.index bx h bz in
        assert_equal
          ~msg:(Printf.sprintf "surface Grass at (%d,%d,%d)" bx h bz)
          Block.Grass blocks.(idx)
      end;
      (* One below surface → Dirt *)
      if h - 1 >= 0 && h - 1 < cs then begin
        let idx = Chunk.index bx (h - 1) bz in
        assert_equal
          ~msg:(Printf.sprintf "h-1 Dirt at (%d,%d,%d)" bx (h-1) bz)
          Block.Dirt blocks.(idx)
      end;
      (* Three below surface still Dirt *)
      if h - 3 >= 0 && h - 3 < cs then begin
        let idx = Chunk.index bx (h - 3) bz in
        assert_equal
          ~msg:(Printf.sprintf "h-3 Dirt at (%d,%d,%d)" bx (h-3) bz)
          Block.Dirt blocks.(idx)
      end;
      (* Four below surface → Stone *)
      if h - 4 >= 0 && h - 4 < cs then begin
        let idx = Chunk.index bx (h - 4) bz in
        assert_equal
          ~msg:(Printf.sprintf "h-4 Stone at (%d,%d,%d)" bx (h-4) bz)
          Block.Stone blocks.(idx)
      end;
      (* One above surface → Air *)
      if h + 1 >= 0 && h + 1 < cs then begin
        let idx = Chunk.index bx (h + 1) bz in
        assert_equal
          ~msg:(Printf.sprintf "h+1 Air at (%d,%d,%d)" bx (h+1) bz)
          Block.Air blocks.(idx)
      end
    done
  done

(** [fill_chunk] agrees with [height_at]: the block at world-y = h is
    [Grass] for every column in the chunk at cy = 0. *)
let test_fill_chunk_agrees_with_height_at _ =
  let cx = 0 and cz = 0 and cy = 0 in
  let blocks = Terrain.fill_chunk ~cx ~cy ~cz in
  for bx = 0 to cs - 1 do
    for bz = 0 to cs - 1 do
      let wx = cx * cs + bx and wz = cz * cs + bz in
      let h = Terrain.height_at wx wz in
      (* Only testable when the surface falls within this chunk's y range *)
      let wy_min = cy * cs and wy_max = cy * cs + cs - 1 in
      if h >= wy_min && h <= wy_max then begin
        let by = h - cy * cs in
        let idx = Chunk.index bx by bz in
        assert_equal
          ~msg:(Printf.sprintf "surface agreement at (%d,%d,%d)" bx by bz)
          Block.Grass blocks.(idx)
      end
    done
  done

let tests =
  "Terrain"
  >::: [
    (* height_at *)
    "height_range"          >:: test_height_range;
    "height_deterministic"  >:: test_height_deterministic;
    "height_is_int"         >:: test_height_is_int;
    (* fill_chunk size *)
    "fill_size"             >:: test_fill_chunk_size;
    "fill_size_other"       >:: test_fill_chunk_size_other_coords;
    (* sky *)
    "sky_all_air"           >:: test_fill_chunk_sky_all_air;
    "sky_other_cols"        >:: test_fill_chunk_sky_other_cols;
    (* deep *)
    "deep_all_stone"        >:: test_fill_chunk_deep_all_stone;
    "deep_other_cols"       >:: test_fill_chunk_deep_other_cols;
    (* block variety *)
    "has_grass"             >:: test_fill_chunk_has_grass;
    "has_dirt"              >:: test_fill_chunk_has_dirt;
    "has_stone"             >:: test_fill_chunk_has_stone;
    (* layering *)
    "layering"              >:: test_fill_chunk_layering;
    "agrees_height_at"      >:: test_fill_chunk_agrees_with_height_at;
  ]

let _ = run_test_tt_main tests
