(** player state and per-frame update *)

open Tsdl

type game_mode =
  | Creative
  | Survival

type t = {
  mutable mode : game_mode;
  mutable vel_y : float;
  mutable on_ground : bool;
  mutable is_crouching : bool;
  mutable held_block : Block.t;
  mutable place_cooldown : float;
  mutable prev_space : bool;
  mutable prev_g : bool;
  mutable prev_mouse_left : bool;
  mutable prev_mouse_right : bool;
  mutable selection_buf : Buffer.t option;
}

(* AF: [{mode; vel_y; on_ground; is_crouching; held_block; place_cooldown;
   prev_space; prev_g; prev_mouse_left; prev_mouse_right; selection_buf}]
   represents the full mutable state of the local player. [mode] is the current
   game mode. [vel_y] is the vertical velocity in blocks/s (survival only).
   [on_ground] is true when the player was blocked moving downward last frame.
   [is_crouching] tracks crouch state. [held_block] is the block type used for
   placement. [place_cooldown] is the remaining seconds before another
   auto-place fires. [prev_*] are the prior-frame values of the corresponding
   inputs, used to detect rising edges. [selection_buf] is the wireframe outline
   around the targeted block, or None when no block is targeted.

   RI: [vel_y] is finite. [place_cooldown >= 0]. *)

let create () =
  {
    mode = Survival;
    vel_y = 0.0;
    on_ground = false;
    is_crouching = false;
    held_block = Block.Stone;
    place_cooldown = 0.0;
    prev_space = false;
    prev_g = false;
    prev_mouse_left = false;
    prev_mouse_right = false;
    selection_buf = None;
  }

let held_block t = t.held_block

let draw_selection t =
  match t.selection_buf with
  | None -> ()
  | Some buf -> Buffer.draw_lines buf

let update t world camera input chunk_manager ~dt =
  (* G toggles between creative and survival *)
  let g_now = Input.is_down input Sdl.Scancode.g in
  if g_now && not t.prev_g then begin
    t.mode <-
      (match t.mode with
      | Creative -> Survival
      | Survival -> Creative);
    t.vel_y <- 0.0
  end;
  t.prev_g <- g_now;
  (* crouching: only active in Survival; lshift held *)
  let was_crouching = t.is_crouching in
  begin match t.mode with
  | Creative -> t.is_crouching <- false
  | Survival ->
      let want_crouch = Input.is_down input Sdl.Scancode.lshift in
      if want_crouch && not was_crouching then begin
        (* lower camera eye to match crouched height *)
        let feet_y = camera.Camera.pos.y -. (Config.player_height -. 0.2) in
        camera.Camera.pos <-
          { camera.Camera.pos with y = feet_y +. (Config.crouch_height -. 0.2) };
        t.is_crouching <- true
      end
      else if (not want_crouch) && was_crouching then begin
        (* try to stand: check headroom by sweeping upward *)
        let delta_y = Config.player_height -. Config.crouch_height in
        let crouched_box =
          Physics.at_position ~height:Config.crouch_height camera.Camera.pos
        in
        let actual =
          Physics.move world crouched_box (Math3d.vec3 0.0 delta_y 0.0)
        in
        if actual.y >= delta_y -. 1e-6 then begin
          let feet_y = camera.Camera.pos.y -. (Config.crouch_height -. 0.2) in
          camera.Camera.pos <-
            {
              camera.Camera.pos with
              y = feet_y +. (Config.player_height -. 0.2);
            };
          t.is_crouching <- false
        end
        else t.is_crouching <- true
      end
      else t.is_crouching <- was_crouching
  end;
  let eff_height =
    if t.is_crouching then Config.crouch_height else Config.player_height
  in
  let space_now = Input.is_down input Sdl.Scancode.space in
  (match t.mode with
  | Creative ->
      let move =
        Camera.movement_from_input camera input ~move_speed:Config.move_speed
          ~sprint_speed:Config.sprint_speed ~dt
      in
      camera.Camera.pos <- Math3d.add camera.Camera.pos move
  | Survival ->
      (* gravity accumulates downward each frame *)
      t.vel_y <- t.vel_y -. (Config.gravity *. dt);
      (* jump on rising edge of space, only when on the ground and not
         crouching *)
      if space_now && (not t.prev_space) && t.on_ground && not t.is_crouching
      then t.vel_y <- Config.jump_velocity;
      let eff_move_speed =
        if t.is_crouching then Config.crouch_speed else Config.move_speed
      in
      let horiz =
        Camera.ground_movement_from_input camera input
          ~move_speed:eff_move_speed ~sprint_speed:Config.sprint_speed ~dt
      in
      let delta = Math3d.vec3 horiz.x (t.vel_y *. dt) horiz.z in
      let box = Physics.at_position ~height:eff_height camera.Camera.pos in
      let actual = Physics.move world box delta in
      (* sneak: prevent walking off edges. Scans the full player width on the
         movement axis but only the center column on the stationary axis, so
         corners don't let you slip off while the full-width scan gives enough
         overhang (~0.3 blocks) for the DDA to see the side face when
         bridging. *)
      let actual =
        if t.is_crouching && t.on_ground then begin
          let ifloor f = Float.to_int (floor f) in
          let feet_y = camera.Camera.pos.y -. (eff_height -. 0.2) in
          let by = ifloor (feet_y -. 1e-3) in
          let hw = Config.player_width /. 2.0 in
          let has_ground_x cx_test cz_cur =
            let bz = ifloor cz_cur in
            let bx0 = ifloor (cx_test -. hw) in
            let bx1 = ifloor (cx_test +. hw -. 1e-6) in
            let found = ref false in
            for bx = bx0 to bx1 do
              if World.get_block world bx by bz <> Block.Air then found := true
            done;
            !found
          in
          let has_ground_z cx_cur cz_test =
            let bx = ifloor cx_cur in
            let bz0 = ifloor (cz_test -. hw) in
            let bz1 = ifloor (cz_test +. hw -. 1e-6) in
            let found = ref false in
            for bz = bz0 to bz1 do
              if World.get_block world bx by bz <> Block.Air then found := true
            done;
            !found
          in
          let safe_x =
            has_ground_x (camera.Camera.pos.x +. actual.x) camera.Camera.pos.z
          in
          let safe_z =
            has_ground_z camera.Camera.pos.x (camera.Camera.pos.z +. actual.z)
          in
          Math3d.vec3
            (if safe_x then actual.x else 0.0)
            actual.y
            (if safe_z then actual.z else 0.0)
        end
        else actual
      in
      (* detect floor collision: wanted to go down but were blocked *)
      if delta.y < -1e-6 && actual.y > delta.y +. 1e-6 then begin
        t.on_ground <- true;
        t.vel_y <- 0.0
      end
      else begin
        (* detect ceiling collision: wanted to go up but were blocked *)
        if delta.y > 1e-6 && actual.y < delta.y -. 1e-6 then t.vel_y <- 0.0;
        t.on_ground <- false
      end;
      camera.Camera.pos <- Math3d.add camera.Camera.pos actual);
  t.prev_space <- space_now;
  (* block type selection: keys 1/2/3 *)
  if Input.is_down input Sdl.Scancode.k1 then t.held_block <- Block.Stone;
  if Input.is_down input Sdl.Scancode.k2 then t.held_block <- Block.Dirt;
  if Input.is_down input Sdl.Scancode.k3 then t.held_block <- Block.Grass;
  (* block interaction via raycast *)
  let ml_now = input.mouse_left in
  let mr_now = input.mouse_right in
  let target =
    World.raycast world ~origin:camera.Camera.pos ~dir:(Camera.look_dir camera)
      ~max_dist:Config.max_reach
  in
  (* left click: break block on rising edge *)
  if ml_now && not t.prev_mouse_left then
    begin match target with
    | Some (bx, by, bz, _, _, _) ->
        World.set_block world bx by bz Block.Air;
        Chunk_manager.rebuild_affected chunk_manager world bx by bz
    | None -> ()
    end;
  (* right click: place block on rising edge OR hold with cooldown *)
  t.place_cooldown <- Float.max 0.0 (t.place_cooldown -. dt);
  let try_place () =
    match target with
    | Some (bx, by, bz, nx, ny, nz) ->
        let px = bx + nx and py = by + ny and pz = bz + nz in
        let box = Physics.at_position ~height:eff_height camera.Camera.pos in
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
          World.set_block world px py pz t.held_block;
          Chunk_manager.rebuild_affected chunk_manager world px py pz
        end
    | None -> ()
  in
  if mr_now && not t.prev_mouse_right then begin
    try_place ();
    t.place_cooldown <- 0.5
  end
  else if mr_now && t.place_cooldown = 0.0 then begin
    try_place ();
    t.place_cooldown <- 0.2
  end;
  t.prev_mouse_left <- ml_now;
  t.prev_mouse_right <- mr_now;
  (* update selection outline buffer for the targeted block *)
  match (target, t.selection_buf) with
  | None, Some b ->
      Buffer.destroy b;
      t.selection_buf <- None
  | None, None -> ()
  | Some (bx, by, bz, _, _, _), existing ->
      let eps = 0.002 in
      let x0 = Float.of_int bx -. eps and x1 = Float.of_int bx +. 1.0 +. eps in
      let y0 = Float.of_int by -. eps and y1 = Float.of_int by +. 1.0 +. eps in
      let z0 = Float.of_int bz -. eps and z1 = Float.of_int bz +. 1.0 +. eps in
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
      t.selection_buf <-
        Some
          (match existing with
          | None -> Buffer.create ~positions ~colors
          | Some b -> Buffer.update b ~positions ~colors)

let destroy t =
  match t.selection_buf with
  | None -> ()
  | Some buf -> Buffer.destroy buf
