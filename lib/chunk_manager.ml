(** chunk buffer management and streaming *)

type t = {
  chunk_bufs : (int * int * int, Buffer.t) Hashtbl.t;
  scratch_pos : float array;
  scratch_col : float array;
}

(* AF: [{chunk_bufs; scratch_pos; scratch_col}] owns the GPU mesh for every
   loaded chunk. [chunk_bufs] maps chunk coordinates to their uploaded Buffer.t.
   [scratch_pos] and [scratch_col] are reusable float arrays large enough for
   the meshing worst case (every block solid, every face exposed).

   RI: every value in [chunk_bufs] is a live GPU Buffer.t. [scratch_pos] and
   [scratch_col] have length [chunk_size^3 * 6 * 6 * 3]. *)

let create () =
  let cs = Config.chunk_size in
  let max_floats = cs * cs * cs * 6 * 6 * 3 in
  {
    chunk_bufs = Hashtbl.create 256;
    scratch_pos = Array.create_float max_floats;
    scratch_col = Array.create_float max_floats;
  }

let bufs t = t.chunk_bufs

(* mesh chunk [(cx, cy, cz)] using [t]'s scratch arrays, destroy the prior GPU
   buffer if any, and upload the new one. if the chunk has no visible faces,
   leaves [chunk_bufs] without an entry. *)
let remesh_chunk t world cx cy cz =
  let key = (cx, cy, cz) in
  (match Hashtbl.find_opt t.chunk_bufs key with
  | Some buf ->
      Buffer.destroy buf;
      Hashtbl.remove t.chunk_bufs key
  | None -> ());
  match World.get_chunk world cx cy cz with
  | None -> ()
  | Some chunk ->
      let n = World.mesh_into world chunk t.scratch_pos t.scratch_col in
      if n > 0 then
        Hashtbl.replace t.chunk_bufs key
          (Buffer.create
             ~positions:(Array.sub t.scratch_pos 0 n)
             ~colors:(Array.sub t.scratch_col 0 n))

let neighbours cx cy cz =
  [
    (cx - 1, cy, cz);
    (cx + 1, cy, cz);
    (cx, cy - 1, cz);
    (cx, cy + 1, cz);
    (cx, cy, cz - 1);
    (cx, cy, cz + 1);
  ]

(* Remesh the chunk containing (wx,wy,wz) and any adjacent chunks whose face
   visibility may be affected by a block on the chunk boundary. *)
let rebuild_affected t world wx wy wz =
  let cs = Config.chunk_size in
  let coord_to_chunk x =
    let local = ((x mod cs) + cs) mod cs in
    ((x - local) / cs, local)
  in
  let cx, lx = coord_to_chunk wx in
  let cy, ly = coord_to_chunk wy in
  let cz, lz = coord_to_chunk wz in
  let mesh = remesh_chunk t world in
  mesh cx cy cz;
  if lx = 0 then mesh (cx - 1) cy cz;
  if lx = cs - 1 then mesh (cx + 1) cy cz;
  if ly = 0 then mesh cx (cy - 1) cz;
  if ly = cs - 1 then mesh cx (cy + 1) cz;
  if lz = 0 then mesh cx cy (cz - 1);
  if lz = cs - 1 then mesh cx cy (cz + 1)

(* Apply one finished chunk-generation result: install the new chunk into the
   world, mesh it, and remesh any already-loaded neighbours whose boundary faces
   may have changed. *)
let apply_result t world (cx, cy, cz) blocks =
  let chunk = Chunk.create ~x:cx ~y:cy ~z:cz ~blocks in
  World.add_chunk world chunk;
  let mesh = remesh_chunk t world in
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
let update t world worker ~camera =
  let cs = Config.chunk_size in
  let cs_f = Float.of_int cs in
  let rd = Config.render_distance in
  let player_cx = Float.to_int (Float.floor (camera.Camera.pos.x /. cs_f)) in
  let player_cz = Float.to_int (Float.floor (camera.Camera.pos.z /. cs_f)) in
  let mesh = remesh_chunk t world in
  (* 1. unload *)
  let to_unload = ref [] in
  World.iter world (fun chunk ->
      let cx = Chunk.x chunk and cy = Chunk.y chunk and cz = Chunk.z chunk in
      let dx = cx - player_cx and dz = cz - player_cz in
      if (dx * dx) + (dz * dz) > (rd + 2) * (rd + 2) then
        to_unload := (cx, cy, cz) :: !to_unload);
  List.iter
    (fun (cx, cy, cz) ->
      (match Hashtbl.find_opt t.chunk_bufs (cx, cy, cz) with
      | Some buf ->
          Buffer.destroy buf;
          Hashtbl.remove t.chunk_bufs (cx, cy, cz)
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
            apply_result t world (cx, cy, cz) blocks;
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

let wait_for_spawn t world worker ~camera =
  (* request chunks around the spawn position, then block on poll_blocking until
     the column under the player is ready, so the first physics step doesn't
     fall through nothing *)
  update t world worker ~camera;
  let cs_f = Float.of_int Config.chunk_size in
  let player_chunk_loaded () =
    let player_cx = Float.to_int (Float.floor (camera.Camera.pos.x /. cs_f)) in
    let player_cz = Float.to_int (Float.floor (camera.Camera.pos.z /. cs_f)) in
    let any = ref false in
    for cy = Config.chunk_y_min to Config.chunk_y_max do
      if World.get_chunk world player_cx cy player_cz <> None then any := true
    done;
    !any
  in
  while not (player_chunk_loaded ()) do
    match Chunk_worker.poll_blocking worker with
    | None -> ()
    | Some ((cx, cy, cz), blocks) -> apply_result t world (cx, cy, cz) blocks
  done

let destroy t = Hashtbl.iter (fun _ buf -> Buffer.destroy buf) t.chunk_bufs
