(** Tests for the {!Input} module.

    {1 Module Overview}
    [Input] maintains per-frame input state: quit flag, window-resize
    flag, accumulated mouse deltas, and a hash-table of currently-held
    keys indexed by SDL scancode.

    {1 What is and is not testable}
    [Input.poll] drains the SDL event queue and therefore requires a
    running SDL context; it is exercised only during integration testing
    of the actual game.

    The remaining public API — [create] and [is_down] — makes no SDL
    calls and can be tested freely.

    {1 Invariants}
    {2 [create]}
    - [quit = false], [resized = false] on a fresh value.
    - [mouse_dx = 0.0], [mouse_dy = 0.0] on a fresh value.
    - [is_down inp sc = false] for every scancode on a fresh value
      (the hash-table is empty).

    {2 [is_down]}
    - Returns [false] when the scancode is absent from the table.
    - Returns [true] after [Hashtbl.replace keys_down sc true].
    - Returns [false] after [Hashtbl.replace keys_down sc false].
    - Does not raise for arbitrary scancode values. *)

open OUnit2
open Tsdl

(* ------------------------------------------------------------------ *)
(*  {1 create — initial state}                                          *)
(* ------------------------------------------------------------------ *)

let test_create_quit_false _ =
  let inp = Input.create () in
  assert_bool "quit = false" (not inp.quit)

let test_create_resized_false _ =
  let inp = Input.create () in
  assert_bool "resized = false" (not inp.resized)

let test_create_mouse_dx_zero _ =
  let inp = Input.create () in
  if abs_float inp.mouse_dx > 1e-10 then
    assert_failure
      (Printf.sprintf "mouse_dx should be 0.0, got %.10f" inp.mouse_dx)

let test_create_mouse_dy_zero _ =
  let inp = Input.create () in
  if abs_float inp.mouse_dy > 1e-10 then
    assert_failure
      (Printf.sprintf "mouse_dy should be 0.0, got %.10f" inp.mouse_dy)

(* ------------------------------------------------------------------ *)
(*  {1 is_down — fresh state}                                           *)
(*  The hash-table is empty so every key reports false.                 *)
(* ------------------------------------------------------------------ *)

let test_is_down_fresh_w _ =
  let inp = Input.create () in
  assert_bool "W not down initially" (not (Input.is_down inp Sdl.Scancode.w))

let test_is_down_fresh_space _ =
  let inp = Input.create () in
  assert_bool "Space not down initially"
    (not (Input.is_down inp Sdl.Scancode.space))

let test_is_down_fresh_escape _ =
  let inp = Input.create () in
  assert_bool "Escape not down initially"
    (not (Input.is_down inp Sdl.Scancode.escape))

let test_is_down_fresh_arbitrary _ =
  let inp = Input.create () in
  (* Check a representative sample of scancodes. *)
  let keys = [
    Sdl.Scancode.a; Sdl.Scancode.s; Sdl.Scancode.d;
    Sdl.Scancode.lctrl; Sdl.Scancode.lshift;
  ] in
  List.iter (fun sc ->
    assert_bool "key not down on fresh input" (not (Input.is_down inp sc))
  ) keys

(* ------------------------------------------------------------------ *)
(*  {1 is_down — after manual key insertion}                            *)
(*  The [keys_down] field is publicly visible, allowing direct          *)
(*  manipulation in tests.                                              *)
(* ------------------------------------------------------------------ *)

let test_is_down_after_set_true _ =
  let inp = Input.create () in
  Hashtbl.replace inp.keys_down Sdl.Scancode.w true;
  assert_bool "W down after insert" (Input.is_down inp Sdl.Scancode.w)

let test_is_down_after_set_false _ =
  let inp = Input.create () in
  Hashtbl.replace inp.keys_down Sdl.Scancode.w true;
  Hashtbl.replace inp.keys_down Sdl.Scancode.w false;
  assert_bool "W up after unset" (not (Input.is_down inp Sdl.Scancode.w))

(** Setting one key does not affect other keys. *)
let test_is_down_no_aliasing _ =
  let inp = Input.create () in
  Hashtbl.replace inp.keys_down Sdl.Scancode.w true;
  assert_bool "A unaffected" (not (Input.is_down inp Sdl.Scancode.a));
  assert_bool "S unaffected" (not (Input.is_down inp Sdl.Scancode.s))

(** Multiple keys can be held simultaneously. *)
let test_is_down_multiple_keys _ =
  let inp = Input.create () in
  Hashtbl.replace inp.keys_down Sdl.Scancode.w     true;
  Hashtbl.replace inp.keys_down Sdl.Scancode.lctrl true;
  assert_bool "W down"    (Input.is_down inp Sdl.Scancode.w);
  assert_bool "Ctrl down" (Input.is_down inp Sdl.Scancode.lctrl)

(** Re-inserting the same key as true keeps it true. *)
let test_is_down_idempotent_true _ =
  let inp = Input.create () in
  Hashtbl.replace inp.keys_down Sdl.Scancode.d true;
  Hashtbl.replace inp.keys_down Sdl.Scancode.d true;
  assert_bool "D still down" (Input.is_down inp Sdl.Scancode.d)

(** Releasing (set false) after multiple presses results in false. *)
let test_is_down_release_after_press _ =
  let inp = Input.create () in
  Hashtbl.replace inp.keys_down Sdl.Scancode.space true;
  Hashtbl.replace inp.keys_down Sdl.Scancode.space true;
  Hashtbl.replace inp.keys_down Sdl.Scancode.space false;
  assert_bool "Space released" (not (Input.is_down inp Sdl.Scancode.space))

let tests =
  "Input"
  >::: [
    (* create *)
    "quit_false"          >:: test_create_quit_false;
    "resized_false"       >:: test_create_resized_false;
    "mouse_dx_zero"       >:: test_create_mouse_dx_zero;
    "mouse_dy_zero"       >:: test_create_mouse_dy_zero;
    (* is_down on fresh state *)
    "is_down_w_fresh"     >:: test_is_down_fresh_w;
    "is_down_space_fresh" >:: test_is_down_fresh_space;
    "is_down_esc_fresh"   >:: test_is_down_fresh_escape;
    "is_down_sample"      >:: test_is_down_fresh_arbitrary;
    (* is_down after mutation *)
    "is_down_set_true"    >:: test_is_down_after_set_true;
    "is_down_set_false"   >:: test_is_down_after_set_false;
    "is_down_no_alias"    >:: test_is_down_no_aliasing;
    "is_down_multi"       >:: test_is_down_multiple_keys;
    "is_down_idempotent"  >:: test_is_down_idempotent_true;
    "is_down_release"     >:: test_is_down_release_after_press;
  ]

let _ = run_test_tt_main tests
