type plane = {
  a : float;
  b : float;
  c : float;
  d : float;
}

type t = plane array

(* AF: A [t] value [planes] represents the view frustum as the intersection of
        the half-spaces defined by [planes.(0)..planes.(5)]. For each plane
        [{a; b; c; d}], a point [(x, y, z)] is inside the half-space when
        [a*x + b*y + c*z + d >= 0]. The 6 planes are, in order: left, right,
        bottom, top, near, far.
   RI: [Array.length planes = 6] and for every plane [(a, b, c)] is a unit
        vector (or all zero, in the degenerate case of a singular MVP). *)

let normalize { a; b; c; d } =
  let len = sqrt ((a *. a) +. (b *. b) +. (c *. c)) in
  if len = 0.0 then { a; b; c; d }
  else { a = a /. len; b = b /. len; c = c /. len; d = d /. len }

(* row [r] of the column-major matrix [m] *)
let row m r = (m.(r), m.(r + 4), m.(r + 8), m.(r + 12))

let plus_ (a1, b1, c1, d1) (a2, b2, c2, d2) =
  { a = a1 +. a2; b = b1 +. b2; c = c1 +. c2; d = d1 +. d2 }

let minus_ (a1, b1, c1, d1) (a2, b2, c2, d2) =
  { a = a1 -. a2; b = b1 -. b2; c = c1 -. c2; d = d1 -. d2 }

let of_mvp m =
  let r0 = row m 0 in
  let r1 = row m 1 in
  let r2 = row m 2 in
  let r3 = row m 3 in
  [|
    normalize (plus_ r3 r0);
    (* left   *)
    normalize (minus_ r3 r0);
    (* right  *)
    normalize (plus_ r3 r1);
    (* bottom *)
    normalize (minus_ r3 r1);
    (* top    *)
    normalize (plus_ r3 r2);
    (* near   *)
    normalize (minus_ r3 r2);
    (* far    *)
  |]

let intersects_aabb planes ~min ~max =
  let outside = ref false in
  let i = ref 0 in
  while (not !outside) && !i < 6 do
    let p = planes.(!i) in
    (* p-vertex: corner of AABB farthest along plane normal. If even that
       corner is on the negative side, the whole AABB is outside this plane. *)
    let px = if p.a >= 0.0 then max.Math3d.x else min.Math3d.x in
    let py = if p.b >= 0.0 then max.Math3d.y else min.Math3d.y in
    let pz = if p.c >= 0.0 then max.Math3d.z else min.Math3d.z in
    if (p.a *. px) +. (p.b *. py) +. (p.c *. pz) +. p.d < 0.0 then
      outside := true;
    incr i
  done;
  not !outside
