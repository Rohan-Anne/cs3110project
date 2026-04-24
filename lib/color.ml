(** rgb color with components in [0, 1] *)
type t = {
  r : float;
  g : float;
  b : float;
}

(* AF: [{r; g; b}] represents the RGB color with red channel [r], green
        channel [g], and blue channel [b], each a linear intensity in [0, 1].
   RI: [0.0 <= r <= 1.0], [0.0 <= g <= 1.0], [0.0 <= b <= 1.0]. *)

let make r g b = { r; g; b }
let shade s c = { r = c.r *. s; g = c.g *. s; b = c.b *. s }
let to_tuple c = (c.r, c.g, c.b)
let ( *. ) = shade
