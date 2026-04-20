(** GPU vertex buffer: a VAO paired with position and color VBOs *)
type t

(** [create ~positions ~colors] uploads the position and color arrays to the
    GPU. [positions] and [colors] must contain triples of values, so their
    length must be a multiple of 3 *)
val create : positions:float array -> colors:float array -> t

(** bind the VAO and draw all triangles *)
val draw : t -> unit

(** free all GPU objects *)
val destroy : t -> unit
