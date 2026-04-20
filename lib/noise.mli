(** [perlin2d x z] returns a value in [-1, 1] at world column [(x, z)] based on
    the perlin noise algorithm *)
val perlin2d : float -> float -> float
