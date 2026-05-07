(** Tests for the {!Color} module.

    {1 Module Overview}
    [Color] provides a simple RGB triple with components in [0.0, 1.0] and a
    small algebra for constructing and scaling colours.

    {1 Invariants}
    {2 [make]}
    - [make r g b] stores [r], [g], [b] in the [r], [g], [b] fields without
      modification.

    {2 [shade]}
    - [shade 1.0 c = c] (identity element).
    - [shade 0.0 c = make 0.0 0.0 0.0] (annihilator).
    - [shade s (shade t c) = shade (s *. t) c] (multiplicative composability).
    - [shade] distributes over components independently.

    {2 [to_tuple]}
    - [(r, g, b) = to_tuple (make r g b)] (lossless round-trip).

    {2 Infix [( *. )]}
    - [s *. c = shade s c] for all [s] and [c].

    {1 Notes}
    Components outside [0,1] are not clamped by the implementation; the caller
    is responsible for supplying sensible values. *)

open OUnit2

let eps = 1e-10

let assert_feq ?(eps = eps) ~msg expected actual =
  if abs_float (expected -. actual) > eps then
    assert_failure
      (Printf.sprintf "%s: expected %.10f got %.10f" msg expected actual)

let assert_color_eq ?(eps = eps) ~msg (er, eg, eb) (c : Color.t) =
  assert_feq ~eps ~msg:(msg ^ ".r") er c.r;
  assert_feq ~eps ~msg:(msg ^ ".g") eg c.g;
  assert_feq ~eps ~msg:(msg ^ ".b") eb c.b

(* ------------------------------------------------------------------ *)
(*  {1 make}                                                            *)
(* ------------------------------------------------------------------ *)

let test_make_stores_components _ =
  let c = Color.make 0.25 0.50 0.75 in
  assert_feq ~msg:"r" 0.25 c.r;
  assert_feq ~msg:"g" 0.50 c.g;
  assert_feq ~msg:"b" 0.75 c.b

let test_make_black _ =
  assert_color_eq ~msg:"black" (0.0, 0.0, 0.0) (Color.make 0.0 0.0 0.0)

let test_make_white _ =
  assert_color_eq ~msg:"white" (1.0, 1.0, 1.0) (Color.make 1.0 1.0 1.0)

let test_make_red _ =
  let c = Color.make 1.0 0.0 0.0 in
  assert_feq ~msg:"r" 1.0 c.r;
  assert_feq ~msg:"g" 0.0 c.g;
  assert_feq ~msg:"b" 0.0 c.b

let test_make_asymmetric _ =
  (* Confirm that swapping components actually matters. *)
  let c1 = Color.make 0.1 0.2 0.3 in
  let c2 = Color.make 0.3 0.2 0.1 in
  assert_bool "r fields differ" (abs_float (c1.r -. c2.r) > eps);
  assert_bool "b fields differ" (abs_float (c1.b -. c2.b) > eps)

(* ------------------------------------------------------------------ *)
(*  {1 shade}                                                           *)
(* ------------------------------------------------------------------ *)

(** [shade 1.0 c = c] — identity. *)
let test_shade_identity _ =
  let c = Color.make 0.3 0.6 0.9 in
  let s = Color.shade 1.0 c in
  assert_color_eq ~msg:"shade 1.0" (c.r, c.g, c.b) s

(** [shade 0.0 c = black] — zero annihilator. *)
let test_shade_zero _ =
  let c = Color.make 0.3 0.6 0.9 in
  let s = Color.shade 0.0 c in
  assert_color_eq ~msg:"shade 0.0" (0.0, 0.0, 0.0) s

(** [shade 0.5 c] halves every component. *)
let test_shade_half _ =
  let c = Color.make 0.4 0.6 0.8 in
  let s = Color.shade 0.5 c in
  assert_feq ~msg:"r" 0.2 s.r;
  assert_feq ~msg:"g" 0.3 s.g;
  assert_feq ~msg:"b" 0.4 s.b

(** [shade] scales each component independently. *)
let test_shade_components_independent _ =
  let c = Color.make 0.1 0.5 0.9 in
  let s = Color.shade 2.0 c in
  assert_feq ~msg:"r doubled" 0.2 s.r;
  assert_feq ~msg:"g doubled" 1.0 s.g;
  assert_feq ~msg:"b doubled" 1.8 s.b

(** [shade s (shade t c) = shade (s*t) c] — composability. *)
let test_shade_compose _ =
  let c = Color.make 0.8 0.6 0.4 in
  let c1 = Color.shade 0.5 (Color.shade 0.5 c) in
  let c2 = Color.shade 0.25 c in
  assert_color_eq ~msg:"shade(0.5, shade(0.5, c))" (c2.r, c2.g, c2.b) c1

(** Multiple shades accumulate multiplicatively. *)
let test_shade_triple_compose _ =
  let c = Color.make 1.0 1.0 1.0 in
  let c1 = Color.shade 0.5 (Color.shade 0.5 (Color.shade 0.5 c)) in
  let c2 = Color.shade 0.125 c in
  assert_color_eq ~msg:"shade^3(0.5)" (c2.r, c2.g, c2.b) c1

(** [shade s black = black] for any scalar [s]. *)
let test_shade_black_invariant _ =
  List.iter
    (fun s ->
      let black = Color.make 0.0 0.0 0.0 in
      let result = Color.shade s black in
      assert_color_eq
        ~msg:(Printf.sprintf "shade %.1f black" s)
        (0.0, 0.0, 0.0) result)
    [ 0.0; 0.5; 1.0; 2.0; 10.0 ]

(* ------------------------------------------------------------------ *)
(*  {1 to_tuple}                                                        *)
(* ------------------------------------------------------------------ *)

(** Round-trip: [to_tuple (make r g b) = (r, g, b)]. *)
let test_to_tuple_roundtrip _ =
  let r, g, b = (0.12, 0.34, 0.56) in
  let r', g', b' = Color.to_tuple (Color.make r g b) in
  assert_feq ~msg:"r" r r';
  assert_feq ~msg:"g" g g';
  assert_feq ~msg:"b" b b'

let test_to_tuple_black _ =
  let r, g, b = Color.to_tuple (Color.make 0.0 0.0 0.0) in
  assert_feq ~msg:"r" 0.0 r;
  assert_feq ~msg:"g" 0.0 g;
  assert_feq ~msg:"b" 0.0 b

let test_to_tuple_white _ =
  let r, g, b = Color.to_tuple (Color.make 1.0 1.0 1.0) in
  assert_feq ~msg:"r" 1.0 r;
  assert_feq ~msg:"g" 1.0 g;
  assert_feq ~msg:"b" 1.0 b

(** Destructuring preserves field order — first element is r, not g or b. *)
let test_to_tuple_order _ =
  let r, g, b = Color.to_tuple (Color.make 0.1 0.5 0.9) in
  assert_feq ~msg:"first is r" 0.1 r;
  assert_feq ~msg:"second is g" 0.5 g;
  assert_feq ~msg:"third is b" 0.9 b

(* ------------------------------------------------------------------ *)
(*  {1 Infix operator [( *. )]}                                        *)
(* ------------------------------------------------------------------ *)

(** [s *. c = shade s c] for a representative sample of (s, c) pairs. *)
let test_infix_equals_shade _ =
  let open Color in
  let cases =
    [
      (0.5, make 0.4 0.6 0.8);
      (0.0, make 1.0 1.0 1.0);
      (1.0, make 0.3 0.3 0.3);
      (2.0, make 0.1 0.2 0.3);
    ]
  in
  List.iter
    (fun (s, c) ->
      let via_op = s *. c in
      let via_shade = shade s c in
      assert_feq ~msg:"r" via_shade.r via_op.r;
      assert_feq ~msg:"g" via_shade.g via_op.g;
      assert_feq ~msg:"b" via_shade.b via_op.b)
    cases

(** Associativity-like property: [(s *. (t *. c)) = (s *. t) *. c] (since
    [*. = shade] and shade composes multiplicatively). *)
let test_infix_compose _ =
  let open Color in
  let c = make 1.0 0.8 0.6 in
  let s = 0.5 and t = 0.4 in
  let lhs = s *. (t *. c) in
  let factor = Stdlib.( *. ) s t in
  let rhs = shade factor c in
  assert_feq ~msg:"r" rhs.r lhs.r;
  assert_feq ~msg:"g" rhs.g lhs.g;
  assert_feq ~msg:"b" rhs.b lhs.b

let tests =
  "Color"
  >::: [
         (* make *)
         "make_stores" >:: test_make_stores_components;
         "make_black" >:: test_make_black;
         "make_white" >:: test_make_white;
         "make_red" >:: test_make_red;
         "make_asymmetric" >:: test_make_asymmetric;
         (* shade *)
         "shade_identity" >:: test_shade_identity;
         "shade_zero" >:: test_shade_zero;
         "shade_half" >:: test_shade_half;
         "shade_independent" >:: test_shade_components_independent;
         "shade_compose" >:: test_shade_compose;
         "shade_triple_compose" >:: test_shade_triple_compose;
         "shade_black" >:: test_shade_black_invariant;
         (* to_tuple *)
         "to_tuple_roundtrip" >:: test_to_tuple_roundtrip;
         "to_tuple_black" >:: test_to_tuple_black;
         "to_tuple_white" >:: test_to_tuple_white;
         "to_tuple_order" >:: test_to_tuple_order;
         (* infix *)
         "infix_equals_shade" >:: test_infix_equals_shade;
         "infix_compose" >:: test_infix_compose;
       ]

let _ = run_test_tt_main tests
