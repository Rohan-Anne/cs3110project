open Tsdl
open Tgl3

let vertex_shader_source = [%blob "shaders/vertex.vert"]
let fragment_shader_source = [%blob "shaders/fragment.frag"]

let cube_positions =
  [|
    (* front *)
    -0.5; -0.5;  0.5;   0.5; -0.5;  0.5;   0.5;  0.5;  0.5;
    -0.5; -0.5;  0.5;   0.5;  0.5;  0.5;  -0.5;  0.5;  0.5;
    (* back *)
     0.5; -0.5; -0.5;  -0.5; -0.5; -0.5;  -0.5;  0.5; -0.5;
     0.5; -0.5; -0.5;  -0.5;  0.5; -0.5;   0.5;  0.5; -0.5;
    (* left *)
    -0.5; -0.5; -0.5;  -0.5; -0.5;  0.5;  -0.5;  0.5;  0.5;
    -0.5; -0.5; -0.5;  -0.5;  0.5;  0.5;  -0.5;  0.5; -0.5;
    (* right *)
     0.5; -0.5;  0.5;   0.5; -0.5; -0.5;   0.5;  0.5; -0.5;
     0.5; -0.5;  0.5;   0.5;  0.5; -0.5;   0.5;  0.5;  0.5;
    (* top *)
    -0.5;  0.5;  0.5;   0.5;  0.5;  0.5;   0.5;  0.5; -0.5;
    -0.5;  0.5;  0.5;   0.5;  0.5; -0.5;  -0.5;  0.5; -0.5;
    (* bottom *)
    -0.5; -0.5; -0.5;   0.5; -0.5; -0.5;   0.5; -0.5;  0.5;
    -0.5; -0.5; -0.5;   0.5; -0.5;  0.5;  -0.5; -0.5;  0.5;
  |] [@ocamlformat "disable"]

let cube_colors =
  let face c = Array.init 18 (fun i -> c.(i mod 3)) in
  Array.concat
    [
      face [| 1.0; 0.4; 0.4 |]; (* front:  red *)
      face [| 0.4; 1.0; 1.0 |]; (* back:   cyan *)
      face [| 0.4; 1.0; 0.4 |]; (* left:   green *)
      face [| 1.0; 0.4; 1.0 |]; (* right:  magenta *)
      face [| 0.4; 0.4; 1.0 |]; (* top:    blue *)
      face [| 1.0; 1.0; 0.4 |]; (* bottom: yellow *)
    ] [@ocamlformat "disable"]

let () =
  let win = Window.create ~title:"ocaml-voxel" ~w:800 ~h:600 in
  let shader =
    Shader.create ~vertex_source:vertex_shader_source
      ~fragment_source:fragment_shader_source
  in
  let buf = Buffer.create ~positions:cube_positions ~colors:cube_colors in
  Gl.enable Gl.depth_test;
  let input = Input.create () in
  let camera =
    Camera.create ~pos:(Math3d.vec3 0.0 0.0 2.0) ~yaw:0.0 ~pitch:0.0
  in
  (* capture mouse input to use mouse to look *)
  ignore (Sdl.set_relative_mouse_mode true);
  let prev_ticks = ref (Sdl.get_ticks ()) in
  while not input.quit do
    let now = Sdl.get_ticks () in
    let dt = Int32.to_float (Int32.sub now !prev_ticks) /. 1000.0 in
    prev_ticks := now;
    Input.poll input;
    if input.resized then Window.update_viewport win;
    Camera.apply_mouse_look camera ~dx:input.mouse_dx ~dy:input.mouse_dy
      ~sensitivity:0.002;
    let move =
      Camera.movement_from_input camera input ~move_speed:2. ~sprint_speed:4.
        ~dt
    in
    camera.pos <- Math3d.add camera.pos move;
    let w, h = Sdl.gl_get_drawable_size win.window in
    let aspect = float_of_int w /. float_of_int h in
    let proj =
      Math3d.perspective ~fov_y_radians:(Float.pi /. 3.0) ~aspect ~near:0.1
        ~far:100.0
    in
    let mvp = Math3d.multiply proj (Camera.view camera) in
    Gl.clear Gl.(color_buffer_bit lor depth_buffer_bit);
    Shader.use shader;
    Shader.set_uniform_mat4 shader "mvp" mvp;
    Buffer.draw buf;
    Window.swap win
  done;
  Buffer.destroy buf;
  Shader.destroy shader;
  Window.destroy win
