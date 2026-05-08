let height_at x z =
  let x = Float.of_int x and z = Float.of_int z in
  (* roughness for larger hills *)
  let amp =
    let roughness = (Noise.perlin2d (x *. 0.03) z +. 1.0) /. 2.0 in
    roughness *. roughness
  in
  let detail = Noise.perlin2d (x *. 0.05) (z *. 0.05) in
  Float.to_int (6.0 +. (15.0 *. amp *. detail))

let fill_chunk ~cx ~cy ~cz =
  let cs = Config.chunk_size in
  let blocks = Array.make (cs * cs * cs) Block.Air in
  for bx = 0 to cs - 1 do
    for bz = 0 to cs - 1 do
      let wx = (cx * cs) + bx in
      let wz = (cz * cs) + bz in
      let h = height_at wx wz in
      for by = 0 to cs - 1 do
        let wy = (cy * cs) + by in
        let block =
          if wy > h then Block.Air
          else if wy = h then Block.Grass
          else if wy >= h - 3 then Block.Dirt
          else Block.Stone
        in
        blocks.(Chunk.index bx by bz) <- block
      done
    done
  done;
  blocks
