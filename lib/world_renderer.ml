(** renders the voxel world: chunk meshes with frustum culling *)

let vertex_source = [%blob "shaders/vertex.vert"]
let fragment_source = [%blob "shaders/fragment.frag"]

type t = { shader : Shader.t }

let create () = { shader = Shader.create ~vertex_source ~fragment_source }

let draw t ~chunk_bufs ~mvp =
  Shader.use t.shader;
  Shader.set_uniform_mat4 t.shader "mvp" mvp;
  let frustum = Frustum.of_mvp mvp in
  let cs_f = Float.of_int Config.chunk_size in
  Hashtbl.iter
    (fun (cx, cy, cz) buf ->
      let bmin =
        Math3d.vec3
          (Float.of_int cx *. cs_f)
          (Float.of_int cy *. cs_f)
          (Float.of_int cz *. cs_f)
      in
      let bmax =
        Math3d.vec3 (bmin.x +. cs_f) (bmin.y +. cs_f) (bmin.z +. cs_f)
      in
      if Frustum.intersects_aabb frustum ~min:bmin ~max:bmax then
        Buffer.draw buf)
    chunk_bufs

let destroy t = Shader.destroy t.shader
