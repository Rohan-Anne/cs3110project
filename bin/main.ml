open Tsdl
open Tgl3

let vertex_shader_source = [%blob "shaders/vertex.vert"]
let fragment_shader_source = [%blob "shaders/fragment.frag"]

let build_world_buffer () =
  let world = World.create () in
  (* generate a 1x1x1 region of chunks. will make this bigger later, so keep the
     loop format *)
  for cx = 0 to 0 do
    for cy = 0 to 0 do
      for cz = 0 to 0 do
        World.generate_chunk world ~cx ~cy ~cz
      done
    done
  done;
  let all_positions = ref [||] in
  let all_colors = ref [||] in
  World.iter world (fun chunk ->
      let positions, colors = World.mesh_chunk world chunk in
      all_positions := Array.append !all_positions positions;
      all_colors := Array.append !all_colors colors);
  Buffer.create ~positions:!all_positions ~colors:!all_colors

let () =
  let win = Window.create ~title:"ocaml-voxel" ~w:800 ~h:600 in
  let shader =
    Shader.create ~vertex_source:vertex_shader_source
      ~fragment_source:fragment_shader_source
  in
  let buf = build_world_buffer () in
  Gl.enable Gl.depth_test;
  let input = Input.create () in
  let camera =
    Camera.create ~pos:(Math3d.vec3 0.0 5.0 10.0) ~yaw:0.0 ~pitch:(-0.3)
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
      ~sensitivity:Config.mouse_sensitivity;
    let move =
      Camera.movement_from_input camera input ~move_speed:Config.move_speed
        ~sprint_speed:Config.sprint_speed ~dt
    in
    camera.pos <- Math3d.add camera.pos move;
    let w, h = Sdl.gl_get_drawable_size win.window in
    let aspect = float_of_int w /. float_of_int h in
    let proj =
      Math3d.perspective ~fov_y_radians:Config.fov_y ~aspect ~near:Config.near
        ~far:Config.far
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
