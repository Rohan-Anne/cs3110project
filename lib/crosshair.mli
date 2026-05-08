(** 2D crosshair overlay rendered in screen space *)
type t

(** upload geometry to the GPU and compile the crosshair shader *)
val create : unit -> t

(** draw the crosshair at the center of the screen; [aspect] is width/height *)
val draw : t -> float -> unit

(** release GPU resources *)
val destroy : t -> unit
