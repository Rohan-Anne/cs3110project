open Tsdl
open Tgl3

let vertex_shader_source = [%blob "shaders/vertex.vert"]
let fragment_shader_source = [%blob "shaders/fragment.frag"]

type game_mode = Creative | Survival

let build_world () =
  let world = World.create () in
  for cx = 0 to 0 do
    for cy = 0 to 0 do
      for cz = 0 to 0 do
        World.generate_chunk world ~cx ~cy ~cz
      done
    done
  done;
  world

let build_world_buffer world =
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
  let world = build_world () in
  let buf = build_world_buffer world in
  Gl.enable Gl.depth_test;
  let input = Input.create () in
  (* spawn above the center of the chunk *)
  let camera =
    Camera.create ~pos:(Math3d.vec3 8.0 10.0 8.0) ~yaw:0.0 ~pitch:(-0.3)
  in
  ignore (Sdl.set_relative_mouse_mode true);
  let mode = ref Survival in
  let vel_y = ref 0.0 in
  let on_ground = ref false in
  let prev_space = ref false in
  let prev_g = ref false in
  let prev_ticks = ref (Sdl.get_ticks ()) in
  while not input.quit do
    let now = Sdl.get_ticks () in
    let dt =
      Float.min 0.05
        (Int32.to_float (Int32.sub now !prev_ticks) /. 1000.0)
    in
    prev_ticks := now;
    Input.poll input;
    if input.resized then Window.update_viewport win;
    Camera.apply_mouse_look camera ~dx:input.mouse_dx ~dy:input.mouse_dy
      ~sensitivity:Config.mouse_sensitivity;
    (* G toggles between creative and survival *)
    let g_now = Input.is_down input Sdl.Scancode.g in
    if g_now && not !prev_g then begin
      mode := (match !mode with Creative -> Survival | Survival -> Creative);
      vel_y := 0.0
    end;
    prev_g := g_now;
    let space_now = Input.is_down input Sdl.Scancode.space in
    (match !mode with
    | Creative ->
      let move =
        Camera.movement_from_input camera input ~move_speed:Config.move_speed
          ~sprint_speed:Config.sprint_speed ~dt
      in
      camera.pos <- Math3d.add camera.pos move
    | Survival ->
      (* gravity accumulates downward each frame *)
      vel_y := !vel_y -. Config.gravity *. dt;
      (* jump on rising edge of space, only when on the ground *)
      if space_now && not !prev_space && !on_ground then
        vel_y := Config.jump_velocity;
      let horiz =
        Camera.ground_movement_from_input camera input
          ~move_speed:Config.move_speed ~sprint_speed:Config.sprint_speed ~dt
      in
      let delta = Math3d.vec3 horiz.x (!vel_y *. dt) horiz.z in
      let box = Physics.at_position camera.pos in
      let actual = Physics.move world box delta in
      (* detect floor collision: wanted to go down but were blocked *)
      if delta.y < -. 1e-6 && actual.y > delta.y +. 1e-6 then begin
        on_ground := true;
        vel_y := 0.0
      end else begin
        (* detect ceiling collision: wanted to go up but were blocked *)
        if delta.y > 1e-6 && actual.y < delta.y -. 1e-6 then vel_y := 0.0;
        on_ground := false
      end;
      camera.pos <- Math3d.add camera.pos actual);
    prev_space := space_now;
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
