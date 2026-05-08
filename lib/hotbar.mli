(** 2D hotbar overlay rendered in screen space *)
type t

(** upload geometry to the GPU and compile the hotbar shader *)
val create : unit -> t

(** draw the hotbar with [held] block highlighted; [aspect] is width/height *)
val draw : t -> held:Block.t -> aspect:float -> unit

(** release GPU resources *)
val destroy : t -> unit
