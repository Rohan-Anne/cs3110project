type t = {
  chunks : (int * int * int, Chunk.t) Hashtbl.t;
}

let create () = { chunks = Hashtbl.create 64 }

let get_chunk world cx cy cz = Hashtbl.find_opt world.chunks (cx, cy, cz)

(* floor division + positive modulo for potentially negative world coords *)
let coord_to_chunk x =
  let cs = Config.chunk_size in
  if x >= 0 then (x / cs, x mod cs)
  else (- ((-x + cs - 1) / cs), ((x mod cs) + cs) mod cs)

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
  | Block.Grass -> (0.2, 0.6, 0.1)
  | Block.Dirt -> (0.55, 0.35, 0.15)
  | Block.Stone -> (0.45, 0.45, 0.5)
  | Block.Air -> (0., 0., 0.)

let add_face positions colors fx fy fz r g b
    (v1x, v1y, v1z) (v2x, v2y, v2z) (v3x, v3y, v3z) (v4x, v4y, v4z) =
  let add_tri x1 y1 z1 =
    positions := Array.append !positions [| x1; y1; z1 |];
    colors := Array.append !colors [| r; g; b |]
  in
  add_tri fx fy fz;
  add_tri (fx +. v1x) (fy +. v1y) (fz +. v1z);
  add_tri (fx +. v2x) (fy +. v2y) (fz +. v2z);
  add_tri fx fy fz;
  add_tri (fx +. v2x) (fy +. v2y) (fz +. v2z);
  add_tri (fx +. v3x) (fy +. v3y) (fz +. v3z);
  add_tri fx fy fz;
  add_tri (fx +. v3x) (fy +. v3y) (fz +. v3z);
  add_tri (fx +. v4x) (fy +. v4y) (fz +. v4z);
  add_tri fx fy fz;
  add_tri (fx +. v4x) (fy +. v4y) (fz +. v4z);
  add_tri (fx +. v1x) (fy +. v1y) (fz +. v1z)

let mesh_chunk world chunk =
  let positions = ref [||] in
  let colors = ref [||] in
  let wx = Chunk.x chunk * Config.chunk_size in
  let wy = Chunk.y chunk * Config.chunk_size in
  let wz = Chunk.z chunk * Config.chunk_size in
  for bx = 0 to Config.chunk_size - 1 do
    for by = 0 to Config.chunk_size - 1 do
      for bz = 0 to Config.chunk_size - 1 do
        let block = Chunk.get chunk bx by bz in
        if block <> Block.Air then
          let x = wx + bx in
          let y = wy + by in
          let z = wz + bz in
          let fx = float x in
          let fy = float y in
          let fz = float z in
          let r, g, b = block_color block in
          (* positive x face *)
          if get_block world (x + 1) y z = Block.Air then
            add_face positions colors (fx +. 1.) fy fz (r *. 0.7) (g *. 0.7) (b *. 0.7)
              (0., 1., 0.) (0., 0., 1.) (0., 1., 1.) (0., 0., 0.);
          (* negative x face *)
          if get_block world (x - 1) y z = Block.Air then
            add_face positions colors fx fy fz r g b
              (0., 1., 0.) (0., 0., 1.) (0., 1., 1.) (0., 0., 0.);
          (* positive y face (top) *)
          if get_block world x (y + 1) z = Block.Air then
            add_face positions colors fx (fy +. 1.) fz r g b
              (1., 0., 1.) (1., 0., 0.) (1., 1., 0.) (0., 0., 0.);
          (* negative y face (bottom) *)
          if get_block world x (y - 1) z = Block.Air then
            add_face positions colors fx fy fz (r *. 0.6) (g *. 0.6) (b *. 0.6)
              (1., 0., 1.) (1., 0., 0.) (1., 1., 0.) (0., 0., 0.);
          (* positive z face *)
          if get_block world x y (z + 1) = Block.Air then
            add_face positions colors fx fy (fz +. 1.) (r *. 0.85) (g *. 0.85) (b *. 0.85)
              (1., 1., 0.) (1., 0., 0.) (0., 0., 0.) (0., 1., 0.);
          (* negative z face *)
          if get_block world x y (z - 1) = Block.Air then
            add_face positions colors fx fy fz r g b
              (1., 1., 0.) (1., 0., 0.) (0., 0., 0.) (0., 1., 0.)
      done
    done
  done;
  (!positions, !colors)

let iter world f = Hashtbl.iter (fun _ c -> f c) world.chunks
