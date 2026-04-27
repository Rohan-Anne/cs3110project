open Tsdl
open Tgl3

(** util function to check for an Ok or failwith an error message *)
let check msg = function
  | Error (`Msg e) -> failwith (Printf.sprintf "%s: %s" msg e)
  | Ok v -> v

type t = {
  window : Sdl.window;
  gl_context : Sdl.gl_context;
}

let create ~title ~w ~h =
  (* init sdl video: handles video creation and event input *)
  check "SDL init" (Sdl.init Sdl.Init.video);

  (* set attributes before creating the window *)
  check "GL profile"
    (Sdl.gl_set_attribute Sdl.Gl.context_profile_mask
       Sdl.Gl.context_profile_core);
  check "GL major" (Sdl.gl_set_attribute Sdl.Gl.context_major_version 3);
  check "GL minor" (Sdl.gl_set_attribute Sdl.Gl.context_minor_version 3);
  check "GL depth" (Sdl.gl_set_attribute Sdl.Gl.depth_size 24);
  check "GL dblbuf" (Sdl.gl_set_attribute Sdl.Gl.doublebuffer 1);
  (* use hidpi *)
  let flags = Sdl.Window.(opengl + resizable + allow_highdpi) in
  let window =
    check "create_window"
      (Sdl.create_window title ~x:Sdl.Window.pos_centered
         ~y:Sdl.Window.pos_centered ~w ~h flags)
  in
  let gl_context = check "gl_create_context" (Sdl.gl_create_context window) in
  check "gl_make_current" (Sdl.gl_make_current window gl_context);
  (* Enable vsync when the platform supports it. Some WSLg/Mesa setups reject
     this even after creating a valid OpenGL context. *)
  (match Sdl.gl_set_swap_interval 1 with
  | Ok () -> ()
  | Error (`Msg e) -> prerr_endline ("warning: vsync disabled: " ^ e));
  (* handle window size properly, bc hidpi means window size differs *)
  let drawable_w, drawable_h = Sdl.gl_get_drawable_size window in
  Gl.viewport 0 0 drawable_w drawable_h;
  Gl.clear_color 0.1 0.1 0.1 1.0;
  { window; gl_context }

let swap t = Sdl.gl_swap_window t.window

let update_viewport t =
  let w, h = Sdl.gl_get_drawable_size t.window in
  Gl.viewport 0 0 w h

let destroy t =
  Sdl.gl_delete_context t.gl_context;
  Sdl.destroy_window t.window;
  Sdl.quit ()
