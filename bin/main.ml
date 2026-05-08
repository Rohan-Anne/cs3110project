let () =
  let win = Window.create ~title:"ocaml-voxel" ~w:800 ~h:600 in
  let world_renderer = World_renderer.create () in
  let world = World.create () in
  let chunks = Chunk_manager.create () in
  let worker = Chunk_worker.create () in
  let crosshair = Crosshair.create () in
  let hotbar = Hotbar.create () in
  let player = Player.create () in
  Window.enable_depth_test ();
  let input = Input.create () in
  (* spawn above the centre of the world; high enough to never spawn inside
     terrain *)
  let camera =
    Camera.create ~pos:(Math3d.vec3 8.0 30.0 8.0) ~yaw:0.0 ~pitch:(-0.3)
  in
  Chunk_manager.wait_for_spawn chunks world worker ~camera;
  Window.capture_mouse true;
  let prev_ticks = ref (Window.ticks ()) in
  while not input.quit do
    let now = Window.ticks () in
    let dt = Float.min 0.05 ((now -. !prev_ticks) /. 1000.0) in
    prev_ticks := now;
    Input.poll input;
    if input.resized then Window.update_viewport win;
    Camera.apply_mouse_look camera ~dx:input.mouse_dx ~dy:input.mouse_dy
      ~sensitivity:Config.mouse_sensitivity;
    Player.update player world camera input chunks ~dt;
    Chunk_manager.update chunks world worker ~camera;
    let w, h = Window.drawable_size win in
    let aspect = Float.of_int w /. Float.of_int (max 1 h) in
    let mvp =
      Math3d.multiply
        (Math3d.perspective ~fov_y_radians:Config.fov_y ~aspect
           ~near:Config.near ~far:Config.far)
        (Camera.view camera)
    in
    Window.clear ();
    World_renderer.draw world_renderer
      ~chunk_bufs:(Chunk_manager.bufs chunks)
      ~mvp;
    Player.draw_selection player;
    Crosshair.draw crosshair aspect;
    Hotbar.draw hotbar ~held:(Player.held_block player) ~aspect;
    Window.swap win
  done;
  Chunk_worker.destroy worker;
  Chunk_manager.destroy chunks;
  Player.destroy player;
  Crosshair.destroy crosshair;
  Hotbar.destroy hotbar;
  World_renderer.destroy world_renderer;
  Window.destroy win
