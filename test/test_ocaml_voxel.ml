open OUnit2

(** {1 Chunk tests} *)

(** Chunk.index maps 3D coords to a flat array index. (0,0,0) should map to 0,
    and the max coord (chunk_size-1, chunk_size-1, chunk_size-1) should map to
    chunk_size^3 - 1. *)
let test_chunk_index_offset_bounds _ =
  let open Config in
  let min_idx = Chunk.index 0 0 0 in
  let max_idx =
    Chunk.index (chunk_size - 1) (chunk_size - 1) (chunk_size - 1)
  in
  assert_equal ~msg:"min index" 0 min_idx;
  assert_equal ~msg:"max index"
    ((chunk_size * chunk_size * chunk_size) - 1)
    max_idx

(** Chunk.index should give a unique index for each (x,y,z) triple in
    the valid range [0, chunk_size). No two coords should collide. *)
let test_chunk_index_unique _ =
  let open Config in
  let indices = ref [] in
  for x = 0 to chunk_size - 1 do
    for y = 0 to chunk_size - 1 do
      for z = 0 to chunk_size - 1 do
        indices := Chunk.index x y z :: !indices
      done
    done
  done;
  let unique = List.sort_uniq compare !indices in
  assert_equal ~msg:"all indices unique" (List.length !indices)
    (List.length unique)

(** Chunk.get reads the block at a given coord; Chunk.set writes it. Setting a
    block and then getting it should return the same value. *)
let test_chunk_get_set _ =
  let blocks =
    Array.make
      (Config.chunk_size * Config.chunk_size * Config.chunk_size)
      Block.Air
  in
  let c = Chunk.create ~x:1 ~y:2 ~z:3 ~blocks in
  assert_equal ~msg:"initial block is Air" Block.Air (Chunk.get c 0 0 0);
  Chunk.set c 1 1 1 Block.Stone;
  assert_equal ~msg:"block set to Stone" Block.Stone (Chunk.get c 1 1 1);
  Chunk.set c 1 1 1 Block.Dirt;
  assert_equal ~msg:"block set to Dirt" Block.Dirt (Chunk.get c 1 1 1)

(** Chunk.x, Chunk.y, Chunk.z should return the chunk's stored coordinates. *)
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

(** Chunk.index, get, and set must be consistent: for every valid (x,y,z) in the
    chunk, setting a block and immediately getting it returns that value. *)
let test_chunk_index_consistency _ =
  let blocks =
    Array.make
      (Config.chunk_size * Config.chunk_size * Config.chunk_size)
      Block.Air
  in
  let c = Chunk.create ~x:0 ~y:0 ~z:0 ~blocks in
  for x = 0 to Config.chunk_size - 1 do
    for y = 0 to Config.chunk_size - 1 do
      for z = 0 to Config.chunk_size - 1 do
        Chunk.set c x y z Block.Grass;
        assert_equal ~msg:"get after set" Block.Grass (Chunk.get c x y z)
      done
    done
  done

let chunk_tests =
  "Chunk"
  >::: [
         "index_offset_bounds" >:: test_chunk_index_offset_bounds;
         "index_unique" >:: test_chunk_index_unique;
         "get_set" >:: test_chunk_get_set;
         "coords" >:: test_chunk_coords;
         "index_consistency" >:: test_chunk_index_consistency;
       ]

let tests = "test suite" >::: [ chunk_tests ]
let _ = run_test_tt_main tests
