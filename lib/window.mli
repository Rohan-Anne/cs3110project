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

(** clear the color and depth buffers, called at the start of each frame *)
val clear : unit -> unit

(** enable depth testing; call once after window creation *)
val enable_depth_test : unit -> unit

(** current time in milliseconds as a float *)
val ticks : unit -> float

(** drawable framebuffer size in pixels; may differ from window size on HiDPI *)
val drawable_size : t -> int * int

(** enable or disable relative (captured) mouse mode *)
val capture_mouse : bool -> unit

(** delete the GL context, destroy the window, and shut down SDL *)
val destroy : t -> unit
