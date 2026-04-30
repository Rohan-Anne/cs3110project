open Tsdl

(** storing state for various input events *)
type t = {
  mutable quit : bool;
  mutable resized : bool;
  mutable mouse_dx : float;
  mutable mouse_dy : float;
  keys_down : (Sdl.scancode, bool) Hashtbl.t;
}

let create () =
  {
    quit = false;
    resized = false;
    mouse_dx = 0.0;
    mouse_dy = 0.0;
    keys_down = Hashtbl.create 64;
  }

let is_down inp sc =
  Option.value ~default:false (Hashtbl.find_opt inp.keys_down sc)

(** reset the input state *)
let reset input =
  input.mouse_dx <- 0.0;
  input.mouse_dy <- 0.0;
  input.resized <- false

let poll input =
  (* first reset everything *)
  reset input;
  (* then poll for event *)
  let e = Sdl.Event.create () in
  while Sdl.poll_event (Some e) do
    match Sdl.Event.(enum (get e typ)) with
    (* on window close *)
    | `Quit -> input.quit <- true
    (* on key down *)
    | `Key_down ->
        let sc = Sdl.Event.(get e keyboard_scancode) in
        Hashtbl.replace input.keys_down sc true;
        if sc = Sdl.Scancode.escape then input.quit <- true
    (* on key up *)
    | `Key_up ->
        let sc = Sdl.Event.(get e keyboard_scancode) in
        Hashtbl.replace input.keys_down sc false
    (* on mouse movement *)
    | `Mouse_motion ->
        input.mouse_dx <-
          input.mouse_dx +. Float.of_int Sdl.Event.(get e mouse_motion_xrel);
        input.mouse_dy <-
          input.mouse_dy +. Float.of_int Sdl.Event.(get e mouse_motion_yrel)
    (* on window resize event *)
    | `Window_event
      when Sdl.Event.(get e window_event_id)
           = Sdl.Event.window_event_size_changed -> input.resized <- true
    | _ -> ()
  done
