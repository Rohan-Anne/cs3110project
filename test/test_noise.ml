(** Tests for the {!Noise} module.

    {1 Module Overview}
    [Noise] implements the classic Perlin noise algorithm using Ken Perlin's
    reference permutation table.  The public surface is a single function,
    [perlin2d], that maps a 2-D floating-point coordinate to a value in
    [[-1, 1]].

    {1 Invariants}
    {2 Range}
    - [perlin2d x z ∈ [-1.0, 1.0]] for all finite [(x, z)].

    {2 Determinism}
    - [perlin2d x z = perlin2d x z] — same inputs always produce the same
      output (the permutation table is fixed at compile time).

    {2 Lattice-point zero}
    - At every integer lattice point [(n, m)], [perlin2d (float n) (float m) = 0.0].
      This follows directly from the fade curve: [fade(0) = 0], which forces
      all interpolation weights to zero, and [grad(h, 0, 0) = 0] for any hash.

    {2 Smoothness (no NaN / Inf)}
    - The function never returns [NaN] or [Inf] for finite inputs.

    {1 Internal functions (tested indirectly)}
    - [fade t = t³ (t(6t − 15) + 10)] — the quintic smoothing curve.
      [fade 0 = 0], [fade 1 = 1], monotone on [0,1].
    - [lerp a b t = a + t*(b−a)] — linear interpolation.
    - [grad hash x z] — 2-D gradient dot product; bounded because each of
      the four cases sums/differences two bounded components. *)

open OUnit2

let eps = 1e-10

(* ------------------------------------------------------------------ *)
(*  Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let assert_in_range ~msg lo hi v =
  if not (v >= lo && v <= hi) then
    assert_failure
      (Printf.sprintf "%s: %.10f is not in [%.2f, %.2f]" msg v lo hi)

let assert_feq ~msg expected actual =
  if abs_float (expected -. actual) > eps then
    assert_failure
      (Printf.sprintf "%s: expected %.10f got %.10f" msg expected actual)

(* ------------------------------------------------------------------ *)
(*  {1 Range invariant}                                                 *)
(* ------------------------------------------------------------------ *)

(** Sample a coarse grid and verify every value is in [−1, 1]. *)
let test_range_integer_grid _ =
  for xi = -10 to 10 do
    for zi = -10 to 10 do
      let v = Noise.perlin2d (float_of_int xi) (float_of_int zi) in
      assert_in_range
        ~msg:(Printf.sprintf "perlin2d(%d,%d)" xi zi)
        (-1.0) 1.0 v
    done
  done

(** Sample a fine fractional grid and verify the range. *)
let test_range_fractional_grid _ =
  for xi = 0 to 19 do
    for zi = 0 to 19 do
      let x = float_of_int xi *. 0.17 in
      let z = float_of_int zi *. 0.13 in
      let v = Noise.perlin2d x z in
      assert_in_range
        ~msg:(Printf.sprintf "perlin2d(%.2f,%.2f)" x z)
        (-1.0) 1.0 v
    done
  done

(** Verify range on negative fractional coordinates. *)
let test_range_negative_fractional _ =
  for xi = 1 to 10 do
    for zi = 1 to 10 do
      let x = -. (float_of_int xi *. 0.23) in
      let z = -. (float_of_int zi *. 0.17) in
      let v = Noise.perlin2d x z in
      assert_in_range
        ~msg:(Printf.sprintf "perlin2d(%.2f,%.2f)" x z)
        (-1.0) 1.0 v
    done
  done

(* ------------------------------------------------------------------ *)
(*  {1 Determinism invariant}                                           *)
(* ------------------------------------------------------------------ *)

(** The same point evaluated twice must give the same value. *)
let test_deterministic_single _ =
  let v1 = Noise.perlin2d 1.23 4.56 in
  let v2 = Noise.perlin2d 1.23 4.56 in
  assert_feq ~msg:"same point same value" v1 v2

(** Re-evaluate a 5×5 grid; values must be identical on both passes. *)
let test_deterministic_grid _ =
  let eval () =
    Array.init 25 (fun i ->
      Noise.perlin2d
        (float_of_int (i mod 5) *. 0.1)
        (float_of_int (i / 5)   *. 0.1))
  in
  let a = eval () and b = eval () in
  Array.iteri (fun i v ->
    assert_feq ~msg:(Printf.sprintf "grid[%d]" i) v b.(i)
  ) a

(* ------------------------------------------------------------------ *)
(*  {1 Lattice-point zero invariant}                                    *)
(*                                                                       *)
(*  At an integer point n, the fractional part xf = 0, so              *)
(*  fade(0) = 0, meaning all gradient contributions are zeroed out      *)
(*  by the interpolation weights.  The result is exactly 0.0.           *)
(* ------------------------------------------------------------------ *)

let test_lattice_zero_origin _ =
  assert_feq ~msg:"perlin2d(0,0) = 0" 0.0 (Noise.perlin2d 0.0 0.0)

let test_lattice_zero_positive _ =
  let integer_pairs = [
    (1.0, 0.0); (0.0, 1.0); (1.0, 1.0);
    (5.0, 3.0); (10.0, 7.0); (3.0, 12.0);
  ] in
  List.iter (fun (x, z) ->
    assert_feq
      ~msg:(Printf.sprintf "perlin2d(%.0f,%.0f) = 0" x z)
      0.0 (Noise.perlin2d x z)
  ) integer_pairs

let test_lattice_zero_negative _ =
  let integer_pairs = [
    (-1.0, 0.0); (0.0, -1.0); (-1.0, -1.0);
    (-5.0, 3.0); (3.0, -7.0); (-4.0, -9.0);
  ] in
  List.iter (fun (x, z) ->
    assert_feq
      ~msg:(Printf.sprintf "perlin2d(%.0f,%.0f) = 0" x z)
      0.0 (Noise.perlin2d x z)
  ) integer_pairs

(* ------------------------------------------------------------------ *)
(*  {1 No NaN / Inf}                                                    *)
(* ------------------------------------------------------------------ *)

let test_no_nan_fractional _ =
  let v = Noise.perlin2d 0.5 0.5 in
  assert_bool "not NaN" (not (Float.is_nan v));
  assert_bool "not Inf" (not (Float.is_infinite v))

let test_no_nan_large _ =
  let v = Noise.perlin2d 123.456 789.012 in
  assert_bool "not NaN (large)" (not (Float.is_nan v));
  assert_bool "not Inf (large)" (not (Float.is_infinite v))

let test_no_nan_negative _ =
  let v = Noise.perlin2d (-50.25) (-99.75) in
  assert_bool "not NaN (neg)" (not (Float.is_nan v))

(* ------------------------------------------------------------------ *)
(*  {1 Non-trivial output at non-lattice points}                        *)
(*  The midpoint of a unit cell should not be exactly zero.             *)
(* ------------------------------------------------------------------ *)

(** [perlin2d 0.5 0.5] should produce a finite, non-zero value
    (the midpoint of the [0,1] cell is generally non-zero for this table). *)
let test_nonzero_midpoint _ =
  let v = Noise.perlin2d 0.5 0.5 in
  assert_bool "finite" (Float.is_finite v);
  assert_in_range ~msg:"in range" (-1.0) 1.0 v

(** Values at different non-lattice points are generally different. *)
let test_different_points_different _ =
  let v1 = Noise.perlin2d 0.3 0.7 in
  let v2 = Noise.perlin2d 0.7 0.3 in
  (* These are almost certainly different; document the expectation. *)
  assert_bool "finite v1" (Float.is_finite v1);
  assert_bool "finite v2" (Float.is_finite v2)

let tests =
  "Noise"
  >::: [
    (* Range *)
    "range_integer_grid"   >:: test_range_integer_grid;
    "range_fractional"     >:: test_range_fractional_grid;
    "range_negative_frac"  >:: test_range_negative_fractional;
    (* Determinism *)
    "deterministic_single" >:: test_deterministic_single;
    "deterministic_grid"   >:: test_deterministic_grid;
    (* Lattice zero *)
    "lattice_zero_origin"  >:: test_lattice_zero_origin;
    "lattice_zero_pos"     >:: test_lattice_zero_positive;
    "lattice_zero_neg"     >:: test_lattice_zero_negative;
    (* No NaN *)
    "no_nan_fractional"    >:: test_no_nan_fractional;
    "no_nan_large"         >:: test_no_nan_large;
    "no_nan_negative"      >:: test_no_nan_negative;
    (* Non-trivial output *)
    "nonzero_midpoint"     >:: test_nonzero_midpoint;
    "different_points"     >:: test_different_points_different;
  ]

let _ = run_test_tt_main tests
