open Tsdl
open Tgl3

let vertex_shader_source = [%blob "shaders/vertex.vert"]
let fragment_shader_source = [%blob "shaders/fragment.frag"]
let crosshair_vertex_source = [%blob "shaders/crosshair.vert"]
let crosshair_fragment_source = [%blob "shaders/crosshair.frag"]
let hotbar_vertex_source = [%blob "shaders/hotbar.vert"]
let hotbar_fragment_source = [%blob "shaders/hotbar.frag"]

type hotbar = { hb_vao : int; hb_vbo : int; hb_shader : Shader.t }

let block_display_color = function
  | Block.Stone -> (0.5, 0.5, 0.5)
  | Block.Dirt -> (0.55, 0.35, 0.15)
  | Block.Grass -> (0.3, 0.7, 0.2)
  | Block.Air -> (0.0, 0.0, 0.0)

let hotbar_quad_append buf x0 y0 x1 y1 r g b =
  Array.append buf
    [| x0; y0; r; g; b; x1; y0; r; g; b; x1; y1; r; g; b;
       x0; y0; r; g; b; x1; y1; r; g; b; x0; y1; r; g; b |]

let hotbar_verts held =
  let slot_s = 0.08 and gap = 0.01 and outline_pad = 0.007 in
  let y_bot = -0.92 and y_top = -0.84 in
  let slots = [| Block.Stone; Block.Dirt; Block.Grass |] in
  let n = Array.length slots in
  let total_x = float_of_int n *. slot_s +. float_of_int (n - 1) *. gap in
  let start_x = -. total_x /. 2.0 in
  let buf = ref [||] in
  Array.iteri (fun i block ->
      let x0 = start_x +. float_of_int i *. (slot_s +. gap) in
      let x1 = x0 +. slot_s in
      if block = held then
        buf :=
          hotbar_quad_append !buf
            (x0 -. outline_pad) (y_bot -. outline_pad)
            (x1 +. outline_pad) (y_top +. outline_pad)
            1.0 1.0 0.9;
      let r, g, b = block_display_color block in
      buf := hotbar_quad_append !buf x0 y_bot x1 y_top r g b)
    slots;
  !buf

let create_hotbar () =
  let vao = Gl_utils.get_int (Gl.gen_vertex_arrays 1) in
  Gl.bind_vertex_array vao;
  let vbo = Gl_utils.get_int (Gl.gen_buffers 1) in
  Gl.bind_buffer Gl.array_buffer vbo;
  let stride = 5 * 4 in
  Gl.enable_vertex_attrib_array 0;
  Gl.vertex_attrib_pointer 0 2 Gl.float false stride (`Offset 0);
  Gl.enable_vertex_attrib_array 1;
  Gl.vertex_attrib_pointer 1 3 Gl.float false stride (`Offset (2 * 4));
  Gl.bind_vertex_array 0;
  Gl.bind_buffer Gl.array_buffer 0;
  let hb_shader =
    Shader.create ~vertex_source:hotbar_vertex_source
      ~fragment_source:hotbar_fragment_source
  in
  { hb_vao = vao; hb_vbo = vbo; hb_shader }

let draw_hotbar hb ~held ~aspect =
  let verts = hotbar_verts held in
  let n_verts = Array.length verts / 5 in
  Gl.disable Gl.depth_test;
  Gl.bind_vertex_array hb.hb_vao;
  Gl.bind_buffer Gl.array_buffer hb.hb_vbo;
  let data = Gl_utils.float32_array verts in
  Gl.buffer_data Gl.array_buffer (Gl.bigarray_byte_size data) (Some data)
    Gl.dynamic_draw;
  Shader.use hb.hb_shader;
  Shader.set_uniform_float hb.hb_shader "aspect" aspect;
  Gl.draw_arrays Gl.triangles 0 n_verts;
  Gl.bind_vertex_array 0;
  Gl.bind_buffer Gl.array_buffer 0;
  Gl.enable Gl.depth_test

let destroy_hotbar hb =
  Gl_utils.with_int (Gl.delete_vertex_arrays 1) hb.hb_vao;
  Gl_utils.with_int (Gl.delete_buffers 1) hb.hb_vbo;
  Shader.destroy hb.hb_shader

type game_mode =
  | Creative
  | Survival

let build_world () =
  let world = World.create () in
  for cx = -5 to 5 do
    for cy = -2 to 1 do
      (* y is smaller *)
      for cz = -5 to 5 do
        World.generate_chunk world ~cx ~cy ~cz
      done
    done
  done;
  world

let rebuild_chunk world chunk_bufs cx cy cz =
  let key = (cx, cy, cz) in
  (match Hashtbl.find_opt chunk_bufs key with
  | Some buf ->
      Buffer.destroy buf;
      Hashtbl.remove chunk_bufs key
  | None -> ());
  match World.get_chunk world cx cy cz with
  | None -> ()
  | Some chunk ->
      let cs = Config.chunk_size in
      let max_floats = cs * cs * cs * 6 * 6 * 3 in
      let pos_buf = Array.create_float max_floats in
      let col_buf = Array.create_float max_floats in
      let n = World.mesh_into world chunk pos_buf col_buf in
      if n > 0 then
        Hashtbl.replace chunk_bufs key
          (Buffer.create ~positions:(Array.sub pos_buf 0 n)
             ~colors:(Array.sub col_buf 0 n))

(* Remesh the chunk containing (wx,wy,wz) and any adjacent chunks whose face
   visibility may be affected by a block on the chunk boundary. *)
let rebuild_affected_chunks world chunk_bufs wx wy wz =
  let cs = Config.chunk_size in
  let coord_to_chunk x =
    let local = ((x mod cs) + cs) mod cs in
    ((x - local) / cs, local)
  in
  let cx, lx = coord_to_chunk wx in
  let cy, ly = coord_to_chunk wy in
  let cz, lz = coord_to_chunk wz in
  rebuild_chunk world chunk_bufs cx cy cz;
  if lx = 0 then rebuild_chunk world chunk_bufs (cx - 1) cy cz;
  if lx = cs - 1 then rebuild_chunk world chunk_bufs (cx + 1) cy cz;
  if ly = 0 then rebuild_chunk world chunk_bufs cx (cy - 1) cz;
  if ly = cs - 1 then rebuild_chunk world chunk_bufs cx (cy + 1) cz;
  if lz = 0 then rebuild_chunk world chunk_bufs cx cy (cz - 1);
  if lz = cs - 1 then rebuild_chunk world chunk_bufs cx cy (cz + 1)

let build_chunk_buffers world =
  let cs = Config.chunk_size in
  let max_floats = cs * cs * cs * 6 * 6 * 3 in
  let pos_scratch = Array.create_float max_floats in
  let col_scratch = Array.create_float max_floats in
  let tbl : (int * int * int, Buffer.t) Hashtbl.t = Hashtbl.create 64 in
  World.iter world (fun chunk ->
      let n = World.mesh_into world chunk pos_scratch col_scratch in
      if n > 0 then begin
        let key = (Chunk.x chunk, Chunk.y chunk, Chunk.z chunk) in
        Hashtbl.add tbl key
          (Buffer.create
             ~positions:(Array.sub pos_scratch 0 n)
             ~colors:(Array.sub col_scratch 0 n))
      end);
  tbl

let () =
  let win = Window.create ~title:"ocaml-voxel" ~w:800 ~h:600 in
  let shader =
    Shader.create ~vertex_source:vertex_shader_source
      ~fragment_source:fragment_shader_source
  in
  let world = build_world () in
  let chunk_bufs = build_chunk_buffers world in
  let crosshair =
    Crosshair.create ~vertex_source:crosshair_vertex_source
      ~fragment_source:crosshair_fragment_source
  in
  let hotbar = create_hotbar () in
  Gl.enable Gl.depth_test;
  let input = Input.create () in
  (* spawn above the center of the chunk *)
  let camera =
    Camera.create ~pos:(Math3d.vec3 8.0 10.0 8.0) ~yaw:0.0 ~pitch:(-0.3)
  in
  (match Sdl.set_relative_mouse_mode true with
  | Ok () -> ()
  | Error (`Msg e) -> prerr_endline ("warning: relative mouse mode failed: " ^ e));
  let mode = ref Survival in
  let vel_y = ref 0.0 in
  let on_ground = ref false in
  let prev_space = ref false in
  let prev_g = ref false in
  let prev_mouse_left = ref false in
  let prev_mouse_right = ref false in
  (* block type the player will place; cycle with 1/2/3 *)
  let held_block = ref Block.Stone in
  let prev_ticks = ref (Sdl.get_ticks ()) in
  while not input.quit do
    let now = Sdl.get_ticks () in
    let dt =
      Float.min 0.05 (Int32.to_float (Int32.sub now !prev_ticks) /. 1000.0)
    in
    prev_ticks := now;
    Input.poll input;
    if input.resized then Window.update_viewport win;
    Camera.apply_mouse_look camera ~dx:input.mouse_dx ~dy:input.mouse_dy
      ~sensitivity:Config.mouse_sensitivity;
    (* G toggles between creative and survival *)
    let g_now = Input.is_down input Sdl.Scancode.g in
    if g_now && not !prev_g then begin
      (mode :=
         match !mode with
         | Creative -> Survival
         | Survival -> Creative);
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
        vel_y := !vel_y -. (Config.gravity *. dt);
        (* jump on rising edge of space, only when on the ground *)
        if space_now && (not !prev_space) && !on_ground then
          vel_y := Config.jump_velocity;
        let horiz =
          Camera.ground_movement_from_input camera input
            ~move_speed:Config.move_speed ~sprint_speed:Config.sprint_speed ~dt
        in
        let delta = Math3d.vec3 horiz.x (!vel_y *. dt) horiz.z in
        let box = Physics.at_position camera.pos in
        let actual = Physics.move world box delta in
        (* detect floor collision: wanted to go down but were blocked *)
        if delta.y < -1e-6 && actual.y > delta.y +. 1e-6 then begin
          on_ground := true;
          vel_y := 0.0
        end
        else begin
          (* detect ceiling collision: wanted to go up but were blocked *)
          if delta.y > 1e-6 && actual.y < delta.y -. 1e-6 then vel_y := 0.0;
          on_ground := false
        end;
        camera.pos <- Math3d.add camera.pos actual);
    prev_space := space_now;
    (* block type selection: keys 1/2/3 *)
    if Input.is_down input Sdl.Scancode.k1 then held_block := Block.Stone;
    if Input.is_down input Sdl.Scancode.k2 then held_block := Block.Dirt;
    if Input.is_down input Sdl.Scancode.k3 then held_block := Block.Grass;
    (* block interaction via raycast *)
    let ml_now = input.mouse_left in
    let mr_now = input.mouse_right in
    let target =
      World.raycast world ~origin:camera.pos ~dir:(Camera.look_dir camera)
        ~max_dist:Config.max_reach
    in
    (* left click: break block on rising edge *)
    if ml_now && not !prev_mouse_left then
      begin match target with
      | Some (bx, by, bz, _, _, _) ->
          World.set_block world bx by bz Block.Air;
          rebuild_affected_chunks world chunk_bufs bx by bz
      | None -> ()
      end;
    (* right click: place block on rising edge, on the face normal *)
    if mr_now && not !prev_mouse_right then
      begin match target with
      | Some (bx, by, bz, nx, ny, nz) ->
          let px = bx + nx and py = by + ny and pz = bz + nz in
          (* don't place inside the player *)
          let box = Physics.at_position camera.pos in
          let block_min_x = Float.of_int px
          and block_max_x = Float.of_int (px + 1) in
          let block_min_y = Float.of_int py
          and block_max_y = Float.of_int (py + 1) in
          let block_min_z = Float.of_int pz
          and block_max_z = Float.of_int (pz + 1) in
          let overlaps =
            box.min.x < block_max_x && box.max.x > block_min_x
            && box.min.y < block_max_y && box.max.y > block_min_y
            && box.min.z < block_max_z && box.max.z > block_min_z
          in
          if (not overlaps) && World.get_block world px py pz = Block.Air then begin
            World.set_block world px py pz !held_block;
            rebuild_affected_chunks world chunk_bufs px py pz
          end
      | None -> ()
      end;
    prev_mouse_left := ml_now;
    prev_mouse_right := mr_now;
    let w, h = Sdl.gl_get_drawable_size win.window in
    let aspect = Float.of_int w /. Float.of_int (max 1 h) in
    let proj =
      Math3d.perspective ~fov_y_radians:Config.fov_y ~aspect ~near:Config.near
        ~far:Config.far
    in
    let mvp = Math3d.multiply proj (Camera.view camera) in
    Gl.clear Gl.(color_buffer_bit lor depth_buffer_bit);
    Shader.use shader;
    Shader.set_uniform_mat4 shader "mvp" mvp;
    Hashtbl.iter (fun _ buf -> Buffer.draw buf) chunk_bufs;
    Crosshair.draw crosshair aspect;
    draw_hotbar hotbar ~held:!held_block ~aspect;
    Window.swap win
  done;
  Hashtbl.iter (fun _ buf -> Buffer.destroy buf) chunk_bufs;
  Crosshair.destroy crosshair;
  destroy_hotbar hotbar;
  Shader.destroy shader;
  Window.destroy win
