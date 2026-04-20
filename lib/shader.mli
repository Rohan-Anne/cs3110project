(** compiled and linked GPU program *)
type t

(** compile vertex and fragment stages from GLSL source and link them into a
    program *)
val create : vertex_source:string -> fragment_source:string -> t

(** bind this program for subsequent draw calls *)
val use : t -> unit

(** upload a 4x4 matrix to a named uniform *)
val set_uniform_mat4 : t -> string -> Math3d.mat4 -> unit

(** delete the GPU program *)
val destroy : t -> unit
