type t = { chunks : (int * int * int, Chunk.t) Hashtbl.t }

(* AF: [{chunks}] represents the set of all loaded chunks. For each binding
   [(cx, cy, cz) -> chunk] in the table, the chunk covers world blocks with X in
   [cx*chunk_size, (cx+1)*chunk_size - 1], and likewise for Y and Z. Any block
   whose chunk is absent from the table is [Air]. RI: For every [(cx, cy, cz) ->
   chunk] in [chunks], [Chunk.x chunk = cx], [Chunk.y chunk = cy], [Chunk.z
   chunk = cz]. No two bindings share the same key. *)

let create () = { chunks = Hashtbl.create 64 }
let get_chunk world cx cy cz = Hashtbl.find_opt world.chunks (cx, cy, cz)

(* floor division + positive modulo for potentially negative world coords *)
let coord_to_chunk x =
  let cs = Config.chunk_size in
  let local = ((x mod cs) + cs) mod cs in
  ((x - local) / cs, local)

let get_block world x y z =
  let cx, bx = coord_to_chunk x in
  let cy, by = coord_to_chunk y in
  let cz, bz = coord_to_chunk z in
  match get_chunk world cx cy cz with
  | None -> Block.Air
  | Some c -> Chunk.get c bx by bz

let set_block world x y z block =
  let cx, bx = coord_to_chunk x in
  let cy, by = coord_to_chunk y in
  let cz, bz = coord_to_chunk z in
  match get_chunk world cx cy cz with
  | None -> ()
  | Some c -> Chunk.set c bx by bz block

let generate_chunk world ~cx ~cy ~cz =
  let blocks = Terrain.fill_chunk ~cx ~cy ~cz in
  let chunk = Chunk.create ~x:cx ~y:cy ~z:cz ~blocks in
  Hashtbl.replace world.chunks (cx, cy, cz) chunk

let add_chunk world chunk =
  let key = (Chunk.x chunk, Chunk.y chunk, Chunk.z chunk) in
  Hashtbl.replace world.chunks key chunk

let remove_chunk world cx cy cz = Hashtbl.remove world.chunks (cx, cy, cz)

let block_color = function
  | Block.Grass -> Color.make 0.2 0.6 0.1
  | Block.Dirt -> Color.make 0.55 0.35 0.15
  | Block.Stone -> Color.make 0.45 0.45 0.5
  | Block.Air -> assert false

(* writes faces for [chunk] into [pos_buf]/[col_buf] starting at index 0,
   returns the number of floats written *)
let mesh_into world chunk pos_buf col_buf =
  let cs = Config.chunk_size in
  let n = ref 0 in
  let wx = Chunk.x chunk * cs in
  let wy = Chunk.y chunk * cs in
  let wz = Chunk.z chunk * cs in
  let write x y z r g b =
    let i = !n in
    pos_buf.(i) <- x;
    pos_buf.(i + 1) <- y;
    pos_buf.(i + 2) <- z;
    col_buf.(i) <- r;
    col_buf.(i + 1) <- g;
    col_buf.(i + 2) <- b;
    n := i + 3
  in
  let add_face c (ax, ay, az) (bx, by, bz) (cx, cy, cz) (dx, dy, dz) =
    let r, g, b = Color.to_tuple c in
    write ax ay az r g b;
    write bx by bz r g b;
    write cx cy cz r g b;
    write ax ay az r g b;
    write cx cy cz r g b;
    write dx dy dz r g b
  in
  (* use direct chunk access for intra-chunk neighbors, hashtable only at
     edges *)
  let air_at bx by bz =
    if bx >= 0 && bx < cs && by >= 0 && by < cs && bz >= 0 && bz < cs then
      Chunk.get chunk bx by bz = Block.Air
    else get_block world (wx + bx) (wy + by) (wz + bz) = Block.Air
  in
  for bx = 0 to cs - 1 do
    for by = 0 to cs - 1 do
      for bz = 0 to cs - 1 do
        let block = Chunk.get chunk bx by bz in
        if block <> Block.Air then begin
          let fx = Float.of_int (wx + bx) in
          let fy = Float.of_int (wy + by) in
          let fz = Float.of_int (wz + bz) in
          let c = block_color block in
          (* positive x face *)
          if air_at (bx + 1) by bz then
            add_face (Color.shade 0.6 c)
              (fx +. 1., fy, fz)
              (fx +. 1., fy +. 1., fz)
              (fx +. 1., fy +. 1., fz +. 1.)
              (fx +. 1., fy, fz +. 1.);
          (* negative x face *)
          if air_at (bx - 1) by bz then
            add_face (Color.shade 0.7 c)
              (fx, fy, fz +. 1.)
              (fx, fy +. 1., fz +. 1.)
              (fx, fy +. 1., fz)
              (fx, fy, fz);
          (* positive y face (top) *)
          if air_at bx (by + 1) bz then
            add_face c
              (fx, fy +. 1., fz)
              (fx +. 1., fy +. 1., fz)
              (fx +. 1., fy +. 1., fz +. 1.)
              (fx, fy +. 1., fz +. 1.);
          (* negative y face (bottom) *)
          if air_at bx (by - 1) bz then
            add_face (Color.shade 0.5 c)
              (fx, fy, fz +. 1.)
              (fx +. 1., fy, fz +. 1.)
              (fx +. 1., fy, fz)
              (fx, fy, fz);
          (* positive z face *)
          if air_at bx by (bz + 1) then
            add_face (Color.shade 0.9 c)
              (fx +. 1., fy, fz +. 1.)
              (fx, fy, fz +. 1.)
              (fx, fy +. 1., fz +. 1.)
              (fx +. 1., fy +. 1., fz +. 1.);
          (* negative z face *)
          if air_at bx by (bz - 1) then
            add_face (Color.shade 0.8 c) (fx, fy, fz)
              (fx +. 1., fy, fz)
              (fx +. 1., fy +. 1., fz)
              (fx, fy +. 1., fz)
        end
      done
    done
  done;
  !n

let mesh_chunk world chunk =
  let cs = Config.chunk_size in
  (* worst case: every block solid with every face exposed *)
  let max_floats = cs * cs * cs * 6 * 6 * 3 in
  let pos_buf = Array.create_float max_floats in
  let col_buf = Array.create_float max_floats in
  let used = mesh_into world chunk pos_buf col_buf in
  (Array.sub pos_buf 0 used, Array.sub col_buf 0 used)

let iter world f = Hashtbl.iter (fun _ c -> f c) world.chunks

(* DDA voxel raycast. Returns Some (bx, by, bz, nx, ny, nz) where (bx,by,bz) is
   the first solid block hit and (nx,ny,nz) is the face normal pointing back
   toward the ray origin. Returns None if no block within max_dist. *)
let raycast world ~origin ~dir ~max_dist =
  let open Math3d in
  let ifloor f = Float.to_int (floor f) in
  let bx = ref (ifloor origin.x) in
  let by = ref (ifloor origin.y) in
  let bz = ref (ifloor origin.z) in
  let sx = if dir.x >= 0.0 then 1 else -1 in
  let sy = if dir.y >= 0.0 then 1 else -1 in
  let sz = if dir.z >= 0.0 then 1 else -1 in
  let t_max_x =
    ref
      (if Float.abs dir.x < 1e-9 then Float.infinity
       else if dir.x > 0.0 then (Float.of_int (!bx + 1) -. origin.x) /. dir.x
       else (Float.of_int !bx -. origin.x) /. dir.x)
  in
  let t_max_y =
    ref
      (if Float.abs dir.y < 1e-9 then Float.infinity
       else if dir.y > 0.0 then (Float.of_int (!by + 1) -. origin.y) /. dir.y
       else (Float.of_int !by -. origin.y) /. dir.y)
  in
  let t_max_z =
    ref
      (if Float.abs dir.z < 1e-9 then Float.infinity
       else if dir.z > 0.0 then (Float.of_int (!bz + 1) -. origin.z) /. dir.z
       else (Float.of_int !bz -. origin.z) /. dir.z)
  in
  let td_x =
    if Float.abs dir.x < 1e-9 then Float.infinity else Float.abs (1.0 /. dir.x)
  in
  let td_y =
    if Float.abs dir.y < 1e-9 then Float.infinity else Float.abs (1.0 /. dir.y)
  in
  let td_z =
    if Float.abs dir.z < 1e-9 then Float.infinity else Float.abs (1.0 /. dir.z)
  in
  let nx = ref 0 and ny = ref 0 and nz = ref 0 in
  let result = ref None in
  while
    !result = None
    && Float.min !t_max_x (Float.min !t_max_y !t_max_z) <= max_dist
  do
    if !t_max_x <= !t_max_y && !t_max_x <= !t_max_z then begin
      bx := !bx + sx;
      t_max_x := !t_max_x +. td_x;
      nx := -sx;
      ny := 0;
      nz := 0
    end
    else if !t_max_y <= !t_max_z then begin
      by := !by + sy;
      t_max_y := !t_max_y +. td_y;
      nx := 0;
      ny := -sy;
      nz := 0
    end
    else begin
      bz := !bz + sz;
      t_max_z := !t_max_z +. td_z;
      nx := 0;
      ny := 0;
      nz := -sz
    end;
    if get_block world !bx !by !bz <> Block.Air then
      result := Some (!bx, !by, !bz, !nx, !ny, !nz)
  done;
  !result
