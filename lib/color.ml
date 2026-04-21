(** rgb color with components in [0, 1] *)
type t = {
  r : float;
  g : float;
  b : float;
}

let make r g b = { r; g; b }
let shade s c = { r = c.r *. s; g = c.g *. s; b = c.b *. s }
let to_tuple c = (c.r, c.g, c.b)
let ( *. ) = shade
