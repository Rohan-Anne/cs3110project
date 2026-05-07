(** Tests for the {!Chunk} module.

    {1 Module Overview}
    A [Chunk] is a [chunk_size × chunk_size × chunk_size] cube of blocks stored
    in a flat array. Chunks are the fundamental unit of world storage; the world
    is a hash-map from chunk coordinates to [Chunk.t].

    {1 Invariants}
    {2 Indexing}
    - [Chunk.index] is a bijection from [{0,…,cs-1}³ → {0,…,cs³-1}] where
      [cs = Config.chunk_size].
    - [index 0 0 0 = 0].
    - [index (cs-1) (cs-1) (cs-1) = cs³ - 1].
    - The strides satisfy: Δx = 1, Δy = cs, Δz = cs². Concretely:
      [index x y z = x + y*cs + z*cs²].

    {2 Storage (get / set)}
    - [get c x y z] after [set c x y z v] returns [v] (round-trip).
    - Writing to one cell does not alter any other cell (no aliasing).
    - Multiple writes to the same cell; only the last value survives.
    - A freshly-created chunk has all blocks set to whatever value was passed in
      the [blocks] array (typically [Air]).

    {2 Coordinates}
    - [Chunk.x], [Chunk.y], [Chunk.z] return the chunk's world-grid coordinates
      as stored by [create]. *)

open OUnit2

let cs = Config.chunk_size

(* ------------------------------------------------------------------ *)
(*  Helpers                                                             *)
(* ------------------------------------------------------------------ *)

(** Create a chunk at world-grid position [(x,y,z)] filled entirely with
    [Block.Air]. *)
let make_air_chunk ?(x = 0) ?(y = 0) ?(z = 0) () =
  Chunk.create ~x ~y ~z ~blocks:(Array.make (cs * cs * cs) Block.Air)

(* ------------------------------------------------------------------ *)
(*  {1 Indexing invariants}                                             *)
(* ------------------------------------------------------------------ *)

(** [index 0 0 0 = 0]. *)
let test_index_origin _ =
  assert_equal ~msg:"index(0,0,0) = 0" 0 (Chunk.index 0 0 0)

(** [index (cs-1) (cs-1) (cs-1) = cs³ - 1]. *)
let test_index_max_corner _ =
  let expected = (cs * cs * cs) - 1 in
  let actual = Chunk.index (cs - 1) (cs - 1) (cs - 1) in
  assert_equal ~msg:"index at max corner" expected actual

(** The x-stride is 1: [index (x+1) y z - index x y z = 1]. *)
let test_index_x_stride _ =
  for y = 0 to cs - 1 do
    for z = 0 to cs - 1 do
      let diff = Chunk.index 1 y z - Chunk.index 0 y z in
      assert_equal ~msg:(Printf.sprintf "x-stride at y=%d z=%d" y z) 1 diff
    done
  done

(** The y-stride is [cs]: [index x (y+1) z - index x y z = cs]. *)
let test_index_y_stride _ =
  for x = 0 to cs - 1 do
    for z = 0 to cs - 1 do
      let diff = Chunk.index x 1 z - Chunk.index x 0 z in
      assert_equal ~msg:(Printf.sprintf "y-stride at x=%d z=%d" x z) cs diff
    done
  done

(** The z-stride is [cs²]: [index x y (z+1) - index x y z = cs²]. *)
let test_index_z_stride _ =
  for x = 0 to cs - 1 do
    for y = 0 to cs - 1 do
      let diff = Chunk.index x y 1 - Chunk.index x y 0 in
      assert_equal
        ~msg:(Printf.sprintf "z-stride at x=%d y=%d" x y)
        (cs * cs) diff
    done
  done

(** [index] is a bijection: all [cs³] triples produce distinct indices. *)
let test_index_bijection _ =
  let indices = Array.init (cs * cs * cs) (fun _ -> -1) in
  let k = ref 0 in
  for x = 0 to cs - 1 do
    for y = 0 to cs - 1 do
      for z = 0 to cs - 1 do
        indices.(!k) <- Chunk.index x y z;
        incr k
      done
    done
  done;
  let sorted = Array.copy indices in
  Array.sort compare sorted;
  (* After sorting, unique = all elements present once each *)
  let unique = Array.of_list (List.sort_uniq compare (Array.to_list indices)) in
  assert_equal ~msg:"bijection: all indices unique"
    (cs * cs * cs)
    (Array.length unique)

(** Every index produced by [index] lies in [0, cs³). *)
let test_index_in_range _ =
  let upper = cs * cs * cs in
  for x = 0 to cs - 1 do
    for y = 0 to cs - 1 do
      for z = 0 to cs - 1 do
        let i = Chunk.index x y z in
        assert_bool (Printf.sprintf "index(%d,%d,%d) >= 0" x y z) (i >= 0);
        assert_bool (Printf.sprintf "index(%d,%d,%d) < cs^3" x y z) (i < upper)
      done
    done
  done

(** Verify the explicit formula [index x y z = x + y*cs + z*cs²]. *)
let test_index_formula _ =
  for x = 0 to cs - 1 do
    for y = 0 to cs - 1 do
      for z = 0 to cs - 1 do
        let expected = x + (y * cs) + (z * cs * cs) in
        assert_equal
          ~msg:(Printf.sprintf "formula at (%d,%d,%d)" x y z)
          expected (Chunk.index x y z)
      done
    done
  done

(* ------------------------------------------------------------------ *)
(*  {1 Storage: get / set round-trip}                                  *)
(* ------------------------------------------------------------------ *)

(** A freshly created chunk filled with [Air] returns [Air] everywhere. *)
let test_initial_all_air _ =
  let c = make_air_chunk () in
  for x = 0 to cs - 1 do
    for y = 0 to cs - 1 do
      for z = 0 to cs - 1 do
        assert_equal
          ~msg:(Printf.sprintf "initial Air at (%d,%d,%d)" x y z)
          Block.Air (Chunk.get c x y z)
      done
    done
  done

(** [set] then [get] at the same cell returns the written value. *)
let test_set_then_get _ =
  let c = make_air_chunk () in
  let cases =
    [
      (0, 0, 0, Block.Stone);
      (1, 2, 3, Block.Dirt);
      (cs - 1, cs - 1, cs - 1, Block.Grass);
      (0, cs - 1, 0, Block.Stone);
    ]
  in
  List.iter
    (fun (x, y, z, b) ->
      Chunk.set c x y z b;
      assert_equal
        ~msg:(Printf.sprintf "set/get at (%d,%d,%d)" x y z)
        b (Chunk.get c x y z))
    cases

(** Writing all four block kinds to the same cell cycles correctly. *)
let test_overwrite _ =
  let c = make_air_chunk () in
  List.iter
    (fun b ->
      Chunk.set c 5 5 5 b;
      assert_equal ~msg:"overwrite" b (Chunk.get c 5 5 5))
    [ Block.Stone; Block.Dirt; Block.Grass; Block.Air ]

(** Setting every cell and immediately reading it back must succeed. *)
let test_set_get_all_cells _ =
  let c = make_air_chunk () in
  for x = 0 to cs - 1 do
    for y = 0 to cs - 1 do
      for z = 0 to cs - 1 do
        Chunk.set c x y z Block.Grass;
        assert_equal
          ~msg:(Printf.sprintf "get after set at (%d,%d,%d)" x y z)
          Block.Grass (Chunk.get c x y z)
      done
    done
  done

(* ------------------------------------------------------------------ *)
(*  {1 No-aliasing invariant}                                          *)
(*  Writing to one cell must leave all other cells untouched.          *)
(* ------------------------------------------------------------------ *)

(** Writing to (0,0,0) must not affect any adjacent cell. *)
let test_no_aliasing_adjacent _ =
  let c = make_air_chunk () in
  Chunk.set c 0 0 0 Block.Stone;
  assert_equal ~msg:"(1,0,0) unaffected" Block.Air (Chunk.get c 1 0 0);
  assert_equal ~msg:"(0,1,0) unaffected" Block.Air (Chunk.get c 0 1 0);
  assert_equal ~msg:"(0,0,1) unaffected" Block.Air (Chunk.get c 0 0 1)

(** Writing to one corner must leave the opposite corner unchanged. *)
let test_no_aliasing_corners _ =
  let c = make_air_chunk () in
  Chunk.set c 0 0 0 Block.Dirt;
  let far = cs - 1 in
  assert_equal ~msg:"far corner unaffected" Block.Air (Chunk.get c far far far)

(** When every cell has a unique block written (by alternating Stone/Dirt),
    reading back confirms no value leaked into neighbouring cells. *)
let test_no_aliasing_full_grid _ =
  let c = make_air_chunk () in
  (* Write a checkerboard: Stone if (x+y+z) is even, Dirt otherwise. *)
  for x = 0 to cs - 1 do
    for y = 0 to cs - 1 do
      for z = 0 to cs - 1 do
        let b = if (x + y + z) mod 2 = 0 then Block.Stone else Block.Dirt in
        Chunk.set c x y z b
      done
    done
  done;
  for x = 0 to cs - 1 do
    for y = 0 to cs - 1 do
      for z = 0 to cs - 1 do
        let expected =
          if (x + y + z) mod 2 = 0 then Block.Stone else Block.Dirt
        in
        assert_equal
          ~msg:(Printf.sprintf "checkerboard at (%d,%d,%d)" x y z)
          expected (Chunk.get c x y z)
      done
    done
  done

(* ------------------------------------------------------------------ *)
(*  {1 Coordinate accessors}                                           *)
(* ------------------------------------------------------------------ *)

(** Positive, zero, and negative chunk coordinates are all stored faithfully by
    [create] and returned unchanged by [x/y/z]. *)
let test_coords_positive _ =
  let c =
    Chunk.create ~x:3 ~y:5 ~z:7 ~blocks:(Array.make (cs * cs * cs) Block.Air)
  in
  assert_equal 3 (Chunk.x c);
  assert_equal 5 (Chunk.y c);
  assert_equal 7 (Chunk.z c)

let test_coords_zero _ =
  let c = make_air_chunk () in
  assert_equal 0 (Chunk.x c);
  assert_equal 0 (Chunk.y c);
  assert_equal 0 (Chunk.z c)

let test_coords_negative _ =
  let c = make_air_chunk ~x:(-4) ~y:(-1) ~z:(-9) () in
  assert_equal (-4) (Chunk.x c);
  assert_equal (-1) (Chunk.y c);
  assert_equal (-9) (Chunk.z c)

(** Coordinates are independent: changing one does not affect the others. *)
let test_coords_independent _ =
  let c =
    Chunk.create ~x:10 ~y:20 ~z:30 ~blocks:(Array.make (cs * cs * cs) Block.Air)
  in
  assert_equal 10 (Chunk.x c);
  assert_equal 20 (Chunk.y c);
  assert_equal 30 (Chunk.z c)

(* ------------------------------------------------------------------ *)
(*  {1 create with pre-filled block array}                             *)
(* ------------------------------------------------------------------ *)

(** A chunk created with a custom block array reflects those values. *)
let test_create_with_data _ =
  let n = cs * cs * cs in
  let blocks =
    Array.init n (fun i -> if i mod 2 = 0 then Block.Stone else Block.Dirt)
  in
  let c = Chunk.create ~x:0 ~y:0 ~z:0 ~blocks in
  for x = 0 to cs - 1 do
    for y = 0 to cs - 1 do
      for z = 0 to cs - 1 do
        let i = Chunk.index x y z in
        let expected = if i mod 2 = 0 then Block.Stone else Block.Dirt in
        assert_equal
          ~msg:(Printf.sprintf "data at (%d,%d,%d)" x y z)
          expected (Chunk.get c x y z)
      done
    done
  done

let tests =
  "Chunk"
  >::: [
         (* Indexing *)
         "index_origin" >:: test_index_origin;
         "index_max_corner" >:: test_index_max_corner;
         "index_x_stride" >:: test_index_x_stride;
         "index_y_stride" >:: test_index_y_stride;
         "index_z_stride" >:: test_index_z_stride;
         "index_bijection" >:: test_index_bijection;
         "index_in_range" >:: test_index_in_range;
         "index_formula" >:: test_index_formula;
         (* Storage *)
         "initial_all_air" >:: test_initial_all_air;
         "set_then_get" >:: test_set_then_get;
         "overwrite" >:: test_overwrite;
         "set_get_all_cells" >:: test_set_get_all_cells;
         (* No-aliasing *)
         "no_aliasing_adj" >:: test_no_aliasing_adjacent;
         "no_aliasing_corners" >:: test_no_aliasing_corners;
         "no_aliasing_grid" >:: test_no_aliasing_full_grid;
         (* Coordinates *)
         "coords_positive" >:: test_coords_positive;
         "coords_zero" >:: test_coords_zero;
         "coords_negative" >:: test_coords_negative;
         "coords_independent" >:: test_coords_independent;
         (* create *)
         "create_with_data" >:: test_create_with_data;
       ]

let _ = run_test_tt_main tests
