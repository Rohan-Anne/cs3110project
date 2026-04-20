open Tsdl

(** per-frame input state. fields are reset at the start of each [poll] call *)
type t = {
  mutable quit : bool;
      (** true once a quit event or Escape is received. need to immediately end
          game *)
  mutable resized : bool;  (** true if the window was resized this frame *)
  mutable mouse_dx : float;
      (** accumulated horizontal mouse delta this frame *)
  mutable mouse_dy : float;  (** accumulated vertical mouse delta this frame *)
  keys_down : (Sdl.scancode, bool) Hashtbl.t;
}

(** create an input state with all fields at zero/false *)
val create : unit -> t

(** returns true if [sc] is currently held *)
val is_down : t -> Sdl.scancode -> bool

(** drain the SDL event queue, updating [t] in place *)
val poll : t -> unit
