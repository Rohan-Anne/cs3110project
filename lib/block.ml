type t =
  | Air
  | Stone
  | Dirt
  | Grass

(* AF: [Air] represents an empty block (no solid material). [Stone], [Dirt], and
   [Grass] each represent a solid block of that material.

   RI: None — every value of type [t] is a valid block kind. *)
