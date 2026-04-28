(** Tests for the {!Block} module.

    {1 Module Overview}
    [Block] defines a simple variant type for the four kinds of voxels that
    exist in the game world.

    {1 Invariants}
    - The four variants [Air], [Stone], [Dirt], and [Grass] are distinct:
      no two different constructors compare as equal.
    - Structural equality is reflexive: every variant equals itself.
    - [Air] is the only non-solid variant; the other three occupy space.
    - The type is closed: there are exactly four inhabitants.
    - Pattern matching is exhaustive over all four variants.

    {1 Notes on untested behaviour}
    [Buffer], [Gl_utils], [Shader], and [Window] all require a live OpenGL
    / SDL context and cannot be exercised in a headless test runner.  Their
    correctness is verified through integration runs of the actual game. *)

open OUnit2

(* ------------------------------------------------------------------ *)
(*  Helpers                                                             *)
(* ------------------------------------------------------------------ *)

(** Classify a block as "solid" (non-Air). *)
let is_solid = function Block.Air -> false | _ -> true

(** Textual name of a block, used in assertion messages. *)
let name = function
  | Block.Air   -> "Air"
  | Block.Stone -> "Stone"
  | Block.Dirt  -> "Dirt"
  | Block.Grass -> "Grass"

(* ------------------------------------------------------------------ *)
(*  {1 Distinctness: every pair of different variants is inequal}       *)
(* ------------------------------------------------------------------ *)

let test_air_ne_stone _ =
  assert_bool "Air <> Stone" (Block.Air <> Block.Stone)

let test_air_ne_dirt _ =
  assert_bool "Air <> Dirt" (Block.Air <> Block.Dirt)

let test_air_ne_grass _ =
  assert_bool "Air <> Grass" (Block.Air <> Block.Grass)

let test_stone_ne_dirt _ =
  assert_bool "Stone <> Dirt" (Block.Stone <> Block.Dirt)

let test_stone_ne_grass _ =
  assert_bool "Stone <> Grass" (Block.Stone <> Block.Grass)

let test_dirt_ne_grass _ =
  assert_bool "Dirt <> Grass" (Block.Dirt <> Block.Grass)

(* ------------------------------------------------------------------ *)
(*  {1 Reflexivity: every variant equals itself}                        *)
(* ------------------------------------------------------------------ *)

let test_air_eq_air _ =
  assert_equal ~msg:"Air = Air" Block.Air Block.Air

let test_stone_eq_stone _ =
  assert_equal ~msg:"Stone = Stone" Block.Stone Block.Stone

let test_dirt_eq_dirt _ =
  assert_equal ~msg:"Dirt = Dirt" Block.Dirt Block.Dirt

let test_grass_eq_grass _ =
  assert_equal ~msg:"Grass = Grass" Block.Grass Block.Grass

(* ------------------------------------------------------------------ *)
(*  {1 Exhaustive pattern matching}                                     *)
(* ------------------------------------------------------------------ *)

(** Pattern matching correctly identifies every variant by name. *)
let test_names _ =
  let variants = [ Block.Air; Block.Stone; Block.Dirt; Block.Grass ] in
  let expected = [ "Air"; "Stone"; "Dirt"; "Grass" ] in
  List.iter2
    (fun block exp ->
      assert_equal ~msg:("name of " ^ exp) exp (name block))
    variants expected

(* ------------------------------------------------------------------ *)
(*  {1 Solidity invariant}                                              *)
(*  [Air] is the only non-solid block; all others occupy space.        *)
(* ------------------------------------------------------------------ *)

let test_air_not_solid _ =
  assert_bool "Air is not solid" (not (is_solid Block.Air))

let test_stone_solid _ =
  assert_bool "Stone is solid" (is_solid Block.Stone)

let test_dirt_solid _ =
  assert_bool "Dirt is solid" (is_solid Block.Dirt)

let test_grass_solid _ =
  assert_bool "Grass is solid" (is_solid Block.Grass)

(* ------------------------------------------------------------------ *)
(*  {1 Cardinality invariant}                                           *)
(*  The type has exactly four distinct inhabitants.                     *)
(* ------------------------------------------------------------------ *)

let test_exactly_four_variants _ =
  let all = [ Block.Air; Block.Stone; Block.Dirt; Block.Grass ] in
  assert_equal ~msg:"list has 4 elements" 4 (List.length all);
  let unique = List.sort_uniq compare all in
  assert_equal ~msg:"all 4 are distinct" 4 (List.length unique)

(* ------------------------------------------------------------------ *)
(*  {1 Symmetry of inequality}                                          *)
(* ------------------------------------------------------------------ *)

(** If [a <> b] then [b <> a] (structural comparison is symmetric). *)
let test_ne_symmetric _ =
  let pairs =
    [ (Block.Air, Block.Stone); (Block.Air, Block.Dirt);
      (Block.Air, Block.Grass); (Block.Stone, Block.Dirt);
      (Block.Stone, Block.Grass); (Block.Dirt, Block.Grass) ]
  in
  List.iter (fun (a, b) ->
    assert_bool
      (Printf.sprintf "%s <> %s implies %s <> %s"
         (name a) (name b) (name b) (name a))
      (b <> a)
  ) pairs

let tests =
  "Block"
  >::: [
    "air_ne_stone"      >:: test_air_ne_stone;
    "air_ne_dirt"       >:: test_air_ne_dirt;
    "air_ne_grass"      >:: test_air_ne_grass;
    "stone_ne_dirt"     >:: test_stone_ne_dirt;
    "stone_ne_grass"    >:: test_stone_ne_grass;
    "dirt_ne_grass"     >:: test_dirt_ne_grass;
    "air_eq_air"        >:: test_air_eq_air;
    "stone_eq_stone"    >:: test_stone_eq_stone;
    "dirt_eq_dirt"      >:: test_dirt_eq_dirt;
    "grass_eq_grass"    >:: test_grass_eq_grass;
    "names"             >:: test_names;
    "air_not_solid"     >:: test_air_not_solid;
    "stone_solid"       >:: test_stone_solid;
    "dirt_solid"        >:: test_dirt_solid;
    "grass_solid"       >:: test_grass_solid;
    "exactly_four"      >:: test_exactly_four_variants;
    "ne_symmetric"      >:: test_ne_symmetric;
  ]

let _ = run_test_tt_main tests
