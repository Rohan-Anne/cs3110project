(** rgb color with components in [0, 1] *)
type t = {
  r : float;
  g : float;
  b : float;
}
(** [make r g b] makes a color with [r], [g], [b] *)

val make : float -> float -> float -> t

(** [shade s c] scales all components by [s] *)
val shade : float -> t -> t

(** [to_tuple c] flattens a color to a float triple *)
val to_tuple : t -> float * float * float

(** [s *. c] is [shade s c] *)
val ( *. ) : float -> t -> t
