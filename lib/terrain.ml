let height_at x z =
  let scale = 0.05 in
  let n = Noise.perlin2d (float_of_int x *. scale) (float_of_int z *. scale) in
  int_of_float (6.0 +. 4.0 *. n)

let fill_chunk ~cx ~cy ~cz =
  let cs = Config.chunk_size in
  let blocks = Array.make (cs * cs * cs) Block.Air in
  for bx = 0 to cs - 1 do
    for bz = 0 to cs - 1 do
      let wx = cx * cs + bx in
      let wz = cz * cs + bz in
      let h = height_at wx wz in
      for by = 0 to cs - 1 do
        let wy = cy * cs + by in
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
