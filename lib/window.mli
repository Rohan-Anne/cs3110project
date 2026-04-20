open Tsdl

(** SDL window with an associated OpenGLcontext *)
type t = {
  window : Sdl.window;  (** the SDL window *)
  gl_context : Sdl.gl_context;  (** the OpenGL context *)
}

(** initialize SDL, create a window and GL context, configure vsync and viewport
*)
val create : title:string -> w:int -> h:int -> t

(** present the back buffer, called once per frame *)
val swap : t -> unit

(** sync the GL viewport size to the current drawable size, called after resize
    event *)
val update_viewport : t -> unit

(** delete the GL context, destroy the window, and shut down SDL *)
val destroy : t -> unit
