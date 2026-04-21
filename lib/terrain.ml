(* this needs to be properly implemented based off of fill_chunk *)
let height_at _x _z = 0

(* basic chunk generation. todo: make this more complicated, using perlin
   noise *)
let fill_chunk ~cx:_ ~cy ~cz:_ =
  let cs = Config.chunk_size in
  let blocks = Array.make (cs * cs * cs) Block.Air in
  (* generate a slope of grass blocks *)
  if cy = 0 then
    for bx = 0 to cs - 1 do
      for bz = 0 to cs - 1 do
        blocks.(Chunk.index bx (bz / 4) bz) <- Block.Grass
      done
    done;
  blocks
