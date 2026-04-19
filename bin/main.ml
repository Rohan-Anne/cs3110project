open Tsdl
open Tgl3

let vertex_shader_source = [%blob "shaders/vertex.vert"]
let fragment_shader_source = [%blob "shaders/fragment.frag"]
let triangle_positions = [| 0.0; 0.5; 0.0; -0.5; -0.5; 0.0; 0.5; -0.5; 0.0 |]
let triangle_colors = [| 1.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 1.0 |]

let () =
  let win = Window.create ~title:"ocaml-voxel" ~w:800 ~h:600 in
  let shader =
    Shader.create ~vertex_source:vertex_shader_source
      ~fragment_source:fragment_shader_source
  in
  let buf =
    Buffer.create ~positions:triangle_positions ~colors:triangle_colors
  in
  (* reusable event record *)
  let event = Sdl.Event.create () in
  let running = ref true in
  while !running do
    while Sdl.poll_event (Some event) do
      match Sdl.Event.(get event typ |> enum) with
      | `Quit -> running := false
      | `Key_down
        when Sdl.Event.(get event keyboard_scancode) = Sdl.Scancode.escape ->
          running := false
      | `Window_event
        when Sdl.Event.(get event window_event_id)
             = Sdl.Event.window_event_size_changed -> Window.update_viewport win
      | _ -> ()
    done;
    Gl.clear Gl.color_buffer_bit;
    Shader.use shader;
    Buffer.draw buf;
    Window.swap win
  done;
  Buffer.destroy buf;
  Shader.destroy shader;
  Window.destroy win
