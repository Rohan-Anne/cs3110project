type t = { chunks : (int * int * int, Chunk.t) Hashtbl.t }

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
