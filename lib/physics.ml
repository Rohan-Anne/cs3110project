open Math3d

type aabb = {
  min : vec3;
  max : vec3;
}

let eye_above_feet = Config.player_height -. 0.2

let at_position pos =
  let hw = Config.player_width /. 2.0 in
  {
    min = vec3 (pos.x -. hw) (pos.y -. eye_above_feet) (pos.z -. hw);
    max = vec3 (pos.x +. hw) (pos.y +. 0.2) (pos.z +. hw);
  }

let ifloor f = int_of_float (floor f)

(* resolve movement along X: returns the largest dx with the same sign that
   doesn't cause the box to overlap a solid block *)
let resolve_x world box dx =
  if dx = 0.0 then 0.0
  else
    let result = ref dx in
    let y0 = ifloor box.min.y and y1 = ifloor (box.max.y -. 1e-6) in
    let z0 = ifloor box.min.z and z1 = ifloor (box.max.z -. 1e-6) in
    if dx > 0.0 then begin
      let x0 = ifloor box.max.x and x1 = ifloor (box.max.x +. dx) in
      for bx = x0 to x1 do
        let corr = float_of_int bx -. box.max.x in
        if corr >= 0.0 then
          for by = y0 to y1 do
            for bz = z0 to z1 do
              if World.get_block world bx by bz <> Block.Air then
                result := Float.min !result corr
            done
          done
      done
    end else begin
      let x0 = ifloor (box.min.x +. dx) and x1 = ifloor box.min.x in
      for bx = x0 to x1 do
        let corr = float_of_int (bx + 1) -. box.min.x in
        if corr <= 0.0 then
          for by = y0 to y1 do
            for bz = z0 to z1 do
              if World.get_block world bx by bz <> Block.Air then
                result := Float.max !result corr
            done
          done
      done
    end;
    !result

let resolve_y world box dy =
  if dy = 0.0 then 0.0
  else
    let result = ref dy in
    let x0 = ifloor box.min.x and x1 = ifloor (box.max.x -. 1e-6) in
    let z0 = ifloor box.min.z and z1 = ifloor (box.max.z -. 1e-6) in
    if dy > 0.0 then begin
      let y0 = ifloor box.max.y and y1 = ifloor (box.max.y +. dy) in
      for by = y0 to y1 do
        let corr = float_of_int by -. box.max.y in
        if corr >= 0.0 then
          for bx = x0 to x1 do
            for bz = z0 to z1 do
              if World.get_block world bx by bz <> Block.Air then
                result := Float.min !result corr
            done
          done
      done
    end else begin
      let y0 = ifloor (box.min.y +. dy) and y1 = ifloor box.min.y in
      for by = y0 to y1 do
        let corr = float_of_int (by + 1) -. box.min.y in
        for bx = x0 to x1 do
          for bz = z0 to z1 do
            if World.get_block world bx by bz <> Block.Air then
              result := Float.max !result corr
          done
        done
      done
    end;
    !result

let resolve_z world box dz =
  if dz = 0.0 then 0.0
  else
    let result = ref dz in
    let x0 = ifloor box.min.x and x1 = ifloor (box.max.x -. 1e-6) in
    let y0 = ifloor box.min.y and y1 = ifloor (box.max.y -. 1e-6) in
    if dz > 0.0 then begin
      let z0 = ifloor box.max.z and z1 = ifloor (box.max.z +. dz) in
      for bz = z0 to z1 do
        let corr = float_of_int bz -. box.max.z in
        if corr >= 0.0 then
          for bx = x0 to x1 do
            for by = y0 to y1 do
              if World.get_block world bx by bz <> Block.Air then
                result := Float.min !result corr
            done
          done
      done
    end else begin
      let z0 = ifloor (box.min.z +. dz) and z1 = ifloor box.min.z in
      for bz = z0 to z1 do
        let corr = float_of_int (bz + 1) -. box.min.z in
        if corr <= 0.0 then
          for bx = x0 to x1 do
            for by = y0 to y1 do
              if World.get_block world bx by bz <> Block.Air then
                result := Float.max !result corr
            done
          done
      done
    end;
    !result

let move world box delta =
  let dx = resolve_x world box delta.x in
  let box1 =
    { min = { box.min with x = box.min.x +. dx };
      max = { box.max with x = box.max.x +. dx } }
  in
  let dy = resolve_y world box1 delta.y in
  let box2 =
    { min = { box1.min with y = box1.min.y +. dy };
      max = { box1.max with y = box1.max.y +. dy } }
  in
  let dz = resolve_z world box2 delta.z in
  vec3 dx dy dz
