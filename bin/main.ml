open Tsdl
open Tgl3

let vertex_shader_source = [%blob "shaders/vertex.vert"]
let fragment_shader_source = [%blob "shaders/fragment.frag"]
let crosshair_vertex_source = [%blob "shaders/crosshair.vert"]
let crosshair_fragment_source = [%blob "shaders/crosshair.frag"]
let hotbar_vertex_source = [%blob "shaders/hotbar.vert"]
let hotbar_fragment_source = [%blob "shaders/hotbar.frag"]

type hotbar = {
  hb_vao : int;
  hb_vbo : int;
  hb_shader : Shader.t;
}

let block_display_color = function
  | Block.Stone -> (0.5, 0.5, 0.5)
  | Block.Dirt -> (0.55, 0.35, 0.15)
  | Block.Grass -> (0.3, 0.7, 0.2)
  | Block.Air -> (0.0, 0.0, 0.0)

let hotbar_quad_append buf x0 y0 x1 y1 r g b =
  Array.append buf
    [| x0; y0; r; g; b; x1; y0; r; g; b; x1; y1; r; g; b;
       x0; y0; r; g; b; x1; y1; r; g; b; x0; y1; r; g; b |]
    [@@ocamlformat "disable"]

let hotbar_verts held =
  let slot_s = 0.08 and gap = 0.01 and outline_pad = 0.007 in
  let y_bot = -0.92 and y_top = -0.84 in
  let slots = [| Block.Stone; Block.Dirt; Block.Grass |] in
  let n = Array.length slots in
  let total_x = (float_of_int n *. slot_s) +. (float_of_int (n - 1) *. gap) in
  let start_x = -.total_x /. 2.0 in
  let buf = ref [||] in
  Array.iteri
    (fun i block ->
      let x0 = start_x +. (float_of_int i *. (slot_s +. gap)) in
      let x1 = x0 +. slot_s in
      if block = held then
        buf :=
          hotbar_quad_append !buf (x0 -. outline_pad) (y_bot -. outline_pad)
            (x1 +. outline_pad) (y_top +. outline_pad) 1.0 1.0 0.9;
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
  Gl.buffer_data Gl.array_buffer
    (Gl.bigarray_byte_size data)
    (Some data) Gl.dynamic_draw;
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

(* mesh chunk [(cx, cy, cz)] using shared [scratch_pos]/[scratch_col] arrays,
   destroy the prior GPU buffer if any, and upload the new one. If the chunk has
   no visible faces, leaves [chunk_bufs] without an entry. *)
let remesh_chunk world chunk_bufs ~scratch_pos ~scratch_col cx cy cz =
  let key = (cx, cy, cz) in
  (match Hashtbl.find_opt chunk_bufs key with
  | Some buf ->
      Buffer.destroy buf;
      Hashtbl.remove chunk_bufs key
  | None -> ());
  match World.get_chunk world cx cy cz with
  | None -> ()
  | Some chunk ->
      let n = World.mesh_into world chunk scratch_pos scratch_col in
      if n > 0 then
        Hashtbl.replace chunk_bufs key
          (Buffer.create
             ~positions:(Array.sub scratch_pos 0 n)
             ~colors:(Array.sub scratch_col 0 n))

(* Remesh the chunk containing (wx,wy,wz) and any adjacent chunks whose face
   visibility may be affected by a block on the chunk boundary. *)
let rebuild_affected_chunks world chunk_bufs ~scratch_pos ~scratch_col wx wy wz
    =
  let cs = Config.chunk_size in
  let coord_to_chunk x =
    let local = ((x mod cs) + cs) mod cs in
    ((x - local) / cs, local)
  in
  let cx, lx = coord_to_chunk wx in
  let cy, ly = coord_to_chunk wy in
  let cz, lz = coord_to_chunk wz in
  let mesh = remesh_chunk world chunk_bufs ~scratch_pos ~scratch_col in
  mesh cx cy cz;
  if lx = 0 then mesh (cx - 1) cy cz;
  if lx = cs - 1 then mesh (cx + 1) cy cz;
  if ly = 0 then mesh cx (cy - 1) cz;
  if ly = cs - 1 then mesh cx (cy + 1) cz;
  if lz = 0 then mesh cx cy (cz - 1);
  if lz = cs - 1 then mesh cx cy (cz + 1)

let neighbours cx cy cz =
  [
    (cx - 1, cy, cz);
    (cx + 1, cy, cz);
    (cx, cy - 1, cz);
    (cx, cy + 1, cz);
    (cx, cy, cz - 1);
    (cx, cy, cz + 1);
  ]

(* Apply one finished chunk-generation result: install the new chunk into the
   world, mesh it, and remesh any already-loaded neighbours whose boundary faces
   may have changed. *)
let apply_chunk_result world chunk_bufs ~scratch_pos ~scratch_col (cx, cy, cz)
    blocks =
  let chunk = Chunk.create ~x:cx ~y:cy ~z:cz ~blocks in
  World.add_chunk world chunk;
  let mesh = remesh_chunk world chunk_bufs ~scratch_pos ~scratch_col in
  mesh cx cy cz;
  List.iter
    (fun (ncx, ncy, ncz) ->
      if World.get_chunk world ncx ncy ncz <> None then mesh ncx ncy ncz)
    (neighbours cx cy cz)

(* Stream chunks around the player. Each frame: 1. Unload chunks outside the
   horizontal render distance and remesh their still-loaded neighbours. 2. Drain
   up to [Config.chunk_load_budget] finished results from the worker, applying
   each one (or dropping it if the player has since moved out of range). 3. Scan
   for missing chunks inside the render radius and request the worker to
   generate them, in order of distance (closest first). *)
let update_chunks world chunk_bufs worker ~camera ~scratch_pos ~scratch_col =
  let cs = Config.chunk_size in
  let cs_f = Float.of_int cs in
  let rd = Config.render_distance in
  let player_cx = Float.to_int (Float.floor (camera.Camera.pos.x /. cs_f)) in
  let player_cz = Float.to_int (Float.floor (camera.Camera.pos.z /. cs_f)) in
  let mesh = remesh_chunk world chunk_bufs ~scratch_pos ~scratch_col in
  (* 1. unload *)
  let to_unload = ref [] in
  World.iter world (fun chunk ->
      let cx = Chunk.x chunk and cy = Chunk.y chunk and cz = Chunk.z chunk in
      let dx = cx - player_cx and dz = cz - player_cz in
      if (dx * dx) + (dz * dz) > (rd + 2) * (rd + 2) then
        to_unload := (cx, cy, cz) :: !to_unload);
  List.iter
    (fun (cx, cy, cz) ->
      (match Hashtbl.find_opt chunk_bufs (cx, cy, cz) with
      | Some buf ->
          Buffer.destroy buf;
          Hashtbl.remove chunk_bufs (cx, cy, cz)
      | None -> ());
      World.remove_chunk world cx cy cz;
      List.iter
        (fun (ncx, ncy, ncz) ->
          if World.get_chunk world ncx ncy ncz <> None then mesh ncx ncy ncz)
        (neighbours cx cy cz))
    !to_unload;
  (* 2. drain finished generation results *)
  let still_in_range cx cy cz =
    let dx = cx - player_cx and dz = cz - player_cz in
    abs dx <= rd
    && abs dz <= rd
    && cy >= Config.chunk_y_min && cy <= Config.chunk_y_max
  in
  let rec drain n =
    if n <= 0 then ()
    else
      match Chunk_worker.poll worker with
      | None -> ()
      | Some ((cx, cy, cz), blocks) ->
          if still_in_range cx cy cz && World.get_chunk world cx cy cz = None
          then begin
            apply_chunk_result world chunk_bufs ~scratch_pos ~scratch_col
              (cx, cy, cz) blocks;
            drain (n - 1)
          end
          else
            (* result is no longer wanted; drop it but keep draining the same
               budget so a stale result doesn't cost us a real load *)
            drain n
  in
  drain Config.chunk_load_budget;
  (* 3. request missing chunks inside the radius, closest first *)
  let missing = ref [] in
  for cx = player_cx - rd to player_cx + rd do
    for cz = player_cz - rd to player_cz + rd do
      let dx = cx - player_cx and dz = cz - player_cz in
      let d2 = (dx * dx) + (dz * dz) in
      if d2 <= rd * rd then
        for cy = Config.chunk_y_min to Config.chunk_y_max do
          if
            World.get_chunk world cx cy cz = None
            && not (Chunk_worker.pending worker (cx, cy, cz))
          then missing := (d2, cx, cy, cz) :: !missing
        done
    done
  done;
  let sorted =
    List.sort (fun (a, _, _, _) (b, _, _, _) -> compare a b) !missing
  in
  List.iter
    (fun (_, cx, cy, cz) -> Chunk_worker.request worker (cx, cy, cz))
    sorted

let () =
  let win = Window.create ~title:"ocaml-voxel" ~w:800 ~h:600 in
  let shader =
    Shader.create ~vertex_source:vertex_shader_source
      ~fragment_source:fragment_shader_source
  in
  let world = World.create () in
  let chunk_bufs : (int * int * int, Buffer.t) Hashtbl.t = Hashtbl.create 256 in
  (* shared scratch arrays for meshing; sized for the worst case (every block
     solid with every face exposed) so they never need to grow *)
  let cs = Config.chunk_size in
  let max_floats = cs * cs * cs * 6 * 6 * 3 in
  let scratch_pos = Array.create_float max_floats in
  let scratch_col = Array.create_float max_floats in
  let worker = Chunk_worker.create () in
  let crosshair =
    Crosshair.create ~vertex_source:crosshair_vertex_source
      ~fragment_source:crosshair_fragment_source
  in
  let hotbar = create_hotbar () in
  let selection_buf : Buffer.t option ref = ref None in
  Gl.enable Gl.depth_test;
  let input = Input.create () in
  (* spawn above the centre of the world; high enough to never spawn inside
     terrain, then update_chunks below loads the column underneath us *)
  let camera =
    Camera.create ~pos:(Math3d.vec3 8.0 30.0 8.0) ~yaw:0.0 ~pitch:(-0.3)
  in
  let player_chunk_loaded () =
    let player_cx =
      Float.to_int (Float.floor (camera.pos.x /. Float.of_int cs))
    in
    let player_cz =
      Float.to_int (Float.floor (camera.pos.z /. Float.of_int cs))
    in
    let any = ref false in
    for cy = Config.chunk_y_min to Config.chunk_y_max do
      if World.get_chunk world player_cx cy player_cz <> None then any := true
    done;
    !any
  in
  (* request chunks around the spawn position, then block on poll_blocking until
     the column under the player is ready, so the first physics step doesn't
     fall through nothing *)
  update_chunks world chunk_bufs worker ~camera ~scratch_pos ~scratch_col;
  while not (player_chunk_loaded ()) do
    match Chunk_worker.poll_blocking worker with
    | None -> ()
    | Some ((cx, cy, cz), blocks) ->
        apply_chunk_result world chunk_bufs ~scratch_pos ~scratch_col
          (cx, cy, cz) blocks
  done;
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
  let is_crouching = ref false in
  let place_cooldown = ref 0.0 in
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
    (* crouching: only active in Survival; lshift held *)
    let was_crouching = !is_crouching in
    (match !mode with
    | Creative -> is_crouching := false
    | Survival ->
        let want_crouch = Input.is_down input Sdl.Scancode.lshift in
        if want_crouch && not was_crouching then begin
          (* lower camera eye to match crouched height *)
          let feet_y = camera.pos.y -. (Config.player_height -. 0.2) in
          camera.pos <-
            { camera.pos with y = feet_y +. (Config.crouch_height -. 0.2) };
          is_crouching := true
        end
        else if (not want_crouch) && was_crouching then begin
          (* try to stand: check headroom by sweeping upward *)
          let delta_y = Config.player_height -. Config.crouch_height in
          let crouched_box =
            Physics.at_position ~height:Config.crouch_height camera.pos
          in
          let actual =
            Physics.move world crouched_box (Math3d.vec3 0.0 delta_y 0.0)
          in
          if actual.y >= delta_y -. 1e-6 then begin
            let feet_y = camera.pos.y -. (Config.crouch_height -. 0.2) in
            camera.pos <-
              { camera.pos with y = feet_y +. (Config.player_height -. 0.2) };
            is_crouching := false
          end
          else is_crouching := true
        end
        else is_crouching := was_crouching);
    let eff_height =
      if !is_crouching then Config.crouch_height else Config.player_height
    in
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
        (* jump on rising edge of space, only when on the ground and not
           crouching *)
        if space_now && (not !prev_space) && !on_ground && not !is_crouching
        then vel_y := Config.jump_velocity;
        let eff_move_speed =
          if !is_crouching then Config.crouch_speed else Config.move_speed
        in
        let horiz =
          Camera.ground_movement_from_input camera input
            ~move_speed:eff_move_speed ~sprint_speed:Config.sprint_speed ~dt
        in
        let delta = Math3d.vec3 horiz.x (!vel_y *. dt) horiz.z in
        let box = Physics.at_position ~height:eff_height camera.pos in
        let actual = Physics.move world box delta in
        (* sneak: prevent walking off edges. Scans the full player width on the
           movement axis but only the center column on the stationary axis, so
           corners don't let you slip off while the full-width scan gives enough
           overhang (~0.3 blocks) for the DDA to see the side face when
           bridging. *)
        let actual =
          if !is_crouching && !on_ground then begin
            let ifloor f = Float.to_int (floor f) in
            let feet_y = camera.pos.y -. (eff_height -. 0.2) in
            let by = ifloor (feet_y -. 1e-3) in
            let hw = Config.player_width /. 2.0 in
            let has_ground_x cx_test cz_cur =
              let bz = ifloor cz_cur in
              let bx0 = ifloor (cx_test -. hw) in
              let bx1 = ifloor (cx_test +. hw -. 1e-6) in
              let found = ref false in
              for bx = bx0 to bx1 do
                if World.get_block world bx by bz <> Block.Air then
                  found := true
              done;
              !found
            in
            let has_ground_z cx_cur cz_test =
              let bx = ifloor cx_cur in
              let bz0 = ifloor (cz_test -. hw) in
              let bz1 = ifloor (cz_test +. hw -. 1e-6) in
              let found = ref false in
              for bz = bz0 to bz1 do
                if World.get_block world bx by bz <> Block.Air then
                  found := true
              done;
              !found
            in
            let safe_x = has_ground_x (camera.pos.x +. actual.x) camera.pos.z in
            let safe_z = has_ground_z camera.pos.x (camera.pos.z +. actual.z) in
            Math3d.vec3
              (if safe_x then actual.x else 0.0)
              actual.y
              (if safe_z then actual.z else 0.0)
          end
          else actual
        in
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
          rebuild_affected_chunks world chunk_bufs ~scratch_pos ~scratch_col bx
            by bz
      | None -> ()
      end;
    (* right click: place block on rising edge OR hold with cooldown *)
    place_cooldown := Float.max 0.0 (!place_cooldown -. dt);
    let try_place () =
      match target with
      | Some (bx, by, bz, nx, ny, nz) ->
          let px = bx + nx and py = by + ny and pz = bz + nz in
          let box = Physics.at_position ~height:eff_height camera.pos in
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
            rebuild_affected_chunks world chunk_bufs ~scratch_pos ~scratch_col
              px py pz
          end
      | None -> ()
    in
    if mr_now && not !prev_mouse_right then begin
      try_place ();
      place_cooldown := 0.5
    end
    else if mr_now && !place_cooldown = 0.0 then begin
      try_place ();
      place_cooldown := 0.2
    end;
    prev_mouse_left := ml_now;
    prev_mouse_right := mr_now;
    update_chunks world chunk_bufs worker ~camera ~scratch_pos ~scratch_col;
    (* update selection outline buffer for the targeted block *)
    (match (target, !selection_buf) with
    | None, Some b ->
        Buffer.destroy b;
        selection_buf := None
    | None, None -> ()
    | Some (bx, by, bz, _, _, _), existing ->
        let eps = 0.002 in
        let x0 = Float.of_int bx -. eps
        and x1 = Float.of_int bx +. 1.0 +. eps in
        let y0 = Float.of_int by -. eps
        and y1 = Float.of_int by +. 1.0 +. eps in
        let z0 = Float.of_int bz -. eps
        and z1 = Float.of_int bz +. 1.0 +. eps in
        let positions =
          [|
            x0;
            y0;
            z0;
            x1;
            y0;
            z0;
            x1;
            y0;
            z0;
            x1;
            y0;
            z1;
            x1;
            y0;
            z1;
            x0;
            y0;
            z1;
            x0;
            y0;
            z1;
            x0;
            y0;
            z0;
            x0;
            y1;
            z0;
            x1;
            y1;
            z0;
            x1;
            y1;
            z0;
            x1;
            y1;
            z1;
            x1;
            y1;
            z1;
            x0;
            y1;
            z1;
            x0;
            y1;
            z1;
            x0;
            y1;
            z0;
            x0;
            y0;
            z0;
            x0;
            y1;
            z0;
            x1;
            y0;
            z0;
            x1;
            y1;
            z0;
            x1;
            y0;
            z1;
            x1;
            y1;
            z1;
            x0;
            y0;
            z1;
            x0;
            y1;
            z1;
          |]
        in
        let colors = Array.make (Array.length positions) 0.0 in
        selection_buf :=
          Some
            (match existing with
            | None -> Buffer.create ~positions ~colors
            | Some b -> Buffer.update b ~positions ~colors));
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
    let frustum = Frustum.of_mvp mvp in
    let cs_f = Float.of_int Config.chunk_size in
    Hashtbl.iter
      (fun (cx, cy, cz) buf ->
        let bmin =
          Math3d.vec3
            (Float.of_int cx *. cs_f)
            (Float.of_int cy *. cs_f)
            (Float.of_int cz *. cs_f)
        in
        let bmax =
          Math3d.vec3 (bmin.x +. cs_f) (bmin.y +. cs_f) (bmin.z +. cs_f)
        in
        if Frustum.intersects_aabb frustum ~min:bmin ~max:bmax then
          Buffer.draw buf)
      chunk_bufs;
    (match !selection_buf with
    | None -> ()
    | Some buf -> Buffer.draw_lines buf);
    Crosshair.draw crosshair aspect;
    draw_hotbar hotbar ~held:!held_block ~aspect;
    Window.swap win
  done;
  Chunk_worker.destroy worker;
  Hashtbl.iter (fun _ buf -> Buffer.destroy buf) chunk_bufs;
  (match !selection_buf with
  | None -> ()
  | Some buf -> Buffer.destroy buf);
  Crosshair.destroy crosshair;
  destroy_hotbar hotbar;
  Shader.destroy shader;
  Window.destroy win
