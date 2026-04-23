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
  | Block.Air -> Color.make 0. 0. 0.

let add_face positions colors c (ax, ay, az) (bx, by, bz) (cx, cy, cz)
    (dx, dy, dz) =
  let r, g, b = Color.to_tuple c in
  let v x y z =
    positions := [| x; y; z |] :: !positions;
    colors := [| r; g; b |] :: !colors
  in
  v ax ay az;
  v bx by bz;
  v cx cy cz;
  v ax ay az;
  v cx cy cz;
  v dx dy dz

let mesh_chunk world chunk =
  let positions = ref [] in
  let colors = ref [] in
  let wx = Chunk.x chunk * Config.chunk_size in
  let wy = Chunk.y chunk * Config.chunk_size in
  let wz = Chunk.z chunk * Config.chunk_size in
  for bx = 0 to Config.chunk_size - 1 do
    for by = 0 to Config.chunk_size - 1 do
      for bz = 0 to Config.chunk_size - 1 do
        let block = Chunk.get chunk bx by bz in
        if block <> Block.Air then (
          let x = wx + bx in
          let y = wy + by in
          let z = wz + bz in
          let fx = float x in
          let fy = float y in
          let fz = float z in
          let c = block_color block in
          (* positive x face *)
          if get_block world (x + 1) y z = Block.Air then
            add_face positions colors (Color.shade 0.6 c)
              (fx +. 1., fy, fz)
              (fx +. 1., fy +. 1., fz)
              (fx +. 1., fy +. 1., fz +. 1.)
              (fx +. 1., fy, fz +. 1.);
          (* negative x face *)
          if get_block world (x - 1) y z = Block.Air then
            add_face positions colors (Color.shade 0.7 c)
              (fx, fy, fz +. 1.)
              (fx, fy +. 1., fz +. 1.)
              (fx, fy +. 1., fz)
              (fx, fy, fz);
          (* positive y face (top) *)
          if get_block world x (y + 1) z = Block.Air then
            add_face positions colors c
              (fx, fy +. 1., fz)
              (fx +. 1., fy +. 1., fz)
              (fx +. 1., fy +. 1., fz +. 1.)
              (fx, fy +. 1., fz +. 1.);
          (* negative y face (bottom) *)
          if get_block world x (y - 1) z = Block.Air then
            add_face positions colors (Color.shade 0.5 c)
              (fx, fy, fz +. 1.)
              (fx +. 1., fy, fz +. 1.)
              (fx +. 1., fy, fz)
              (fx, fy, fz);
          (* positive z face *)
          if get_block world x y (z + 1) = Block.Air then
            add_face positions colors (Color.shade 0.9 c)
              (fx +. 1., fy, fz +. 1.)
              (fx, fy, fz +. 1.)
              (fx, fy +. 1., fz +. 1.)
              (fx +. 1., fy +. 1., fz +. 1.);
          (* negative z face *)
          if get_block world x y (z - 1) = Block.Air then
            add_face positions colors (Color.shade 0.8 c) (fx, fy, fz)
              (fx +. 1., fy, fz)
              (fx +. 1., fy +. 1., fz)
              (fx, fy +. 1., fz))
      done
    done
  done;
  (Array.concat (List.rev !positions), Array.concat (List.rev !colors))

let iter world f = Hashtbl.iter (fun _ c -> f c) world.chunks
