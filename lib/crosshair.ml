open Tgl3

(** GPU state for the 2D crosshair overlay *)
type t = {
  vao : int;
  vbo : int;
  shader : Shader.t;
}

let vertex_source = [%blob "shaders/crosshair.vert"]
let fragment_source = [%blob "shaders/crosshair.frag"]

let create () =
  (* half-length and half-thickness in screen-height NDC units *)
  let sz = 0.025 and th = 0.002 in
  let verts =
    [|
      (* horizontal bar *)
      -.sz;
      -.th;
      sz;
      -.th;
      sz;
      th;
      -.sz;
      -.th;
      sz;
      th;
      -.sz;
      th;
      (* vertical bar *)
      -.th;
      -.sz;
      th;
      -.sz;
      th;
      sz;
      -.th;
      -.sz;
      th;
      sz;
      -.th;
      sz;
    |]
  in
  let vao = Gl_utils.get_int (Gl.gen_vertex_arrays 1) in
  Gl.bind_vertex_array vao;
  let vbo = Gl_utils.get_int (Gl.gen_buffers 1) in
  Gl.bind_buffer Gl.array_buffer vbo;
  let data = Gl_utils.float32_array verts in
  Gl.buffer_data Gl.array_buffer
    (Gl.bigarray_byte_size data)
    (Some data) Gl.static_draw;
  Gl.enable_vertex_attrib_array 0;
  Gl.vertex_attrib_pointer 0 2 Gl.float false 0 (`Offset 0);
  Gl.bind_vertex_array 0;
  Gl.bind_buffer Gl.array_buffer 0;
  let shader = Shader.create ~vertex_source ~fragment_source in
  { vao; vbo; shader }

let draw t aspect =
  Gl.disable Gl.depth_test;
  Shader.use t.shader;
  Shader.set_uniform_float t.shader "aspect" aspect;
  Gl.bind_vertex_array t.vao;
  Gl.draw_arrays Gl.triangles 0 12;
  Gl.bind_vertex_array 0;
  Gl.enable Gl.depth_test

let destroy t =
  Gl_utils.with_int (Gl.delete_vertex_arrays 1) t.vao;
  Gl_utils.with_int (Gl.delete_buffers 1) t.vbo;
  Shader.destroy t.shader
