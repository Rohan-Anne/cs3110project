(** [height_at x z] returns the surface height at world column [(x, z)] in
    blocks, determined by the noise function. this is used for spawning the
    player at the right y level *)
val height_at : int -> int -> int

(** [fill_chunk ~cx ~cy ~cz] generates and returns the block array for the chunk
    at coordinates [(cx, cy, cz)], with blocks assigned based on the depth below
    the surface. surface = grass, dirt a few blocks below, stone below that, and
    air above *)
val fill_chunk : cx:int -> cy:int -> cz:int -> Block.t array
