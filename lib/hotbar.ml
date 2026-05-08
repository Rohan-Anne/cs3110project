open Tgl3

(** GPU state for the 2D hotbar overlay *)
type t = {
  hb_vao : int;
  hb_vbo : int;
  hb_shader : Shader.t;
}

let vertex_source = [%blob "shaders/hotbar.vert"]
let fragment_source = [%blob "shaders/hotbar.frag"]

let block_display_color = function
  | Block.Stone -> (0.5, 0.5, 0.5)
  | Block.Dirt -> (0.55, 0.35, 0.15)
  | Block.Grass -> (0.3, 0.7, 0.2)
  | Block.Air -> (0.0, 0.0, 0.0)

let hotbar_quad_append buf x0 y0 x1 y1 r g b =
  Array.append buf
    [| x0; y0; r; g; b; x1; y0; r; g; b; x1; y1; r; g; b;
       x0; y0; r; g; b; x1; y1; r; g; b; x0; y1; r; g; b |]
    [@@ocamlformat "disable"]

let hotbar_verts held =
  let slot_s = 0.08 and gap = 0.01 and outline_pad = 0.007 in
  let y_bot = -0.92 and y_top = -0.84 in
  let slots = [| Block.Stone; Block.Dirt; Block.Grass |] in
  let n = Array.length slots in
  let total_x = (Float.of_int n *. slot_s) +. (Float.of_int (n - 1) *. gap) in
  let start_x = -.total_x /. 2.0 in
  let buf = ref [||] in
  Array.iteri
    (fun i block ->
      let x0 = start_x +. (Float.of_int i *. (slot_s +. gap)) in
      let x1 = x0 +. slot_s in
      if block = held then
        buf :=
          hotbar_quad_append !buf (x0 -. outline_pad) (y_bot -. outline_pad)
            (x1 +. outline_pad) (y_top +. outline_pad) 1.0 1.0 0.9;
      let r, g, b = block_display_color block in
      buf := hotbar_quad_append !buf x0 y_bot x1 y_top r g b)
    slots;
  !buf

let create () =
  let vao = Gl_utils.get_int (Gl.gen_vertex_arrays 1) in
  Gl.bind_vertex_array vao;
  let vbo = Gl_utils.get_int (Gl.gen_buffers 1) in
  Gl.bind_buffer Gl.array_buffer vbo;
  let stride = 5 * 4 in
  Gl.enable_vertex_attrib_array 0;
  Gl.vertex_attrib_pointer 0 2 Gl.float false stride (`Offset 0);
  Gl.enable_vertex_attrib_array 1;
  Gl.vertex_attrib_pointer 1 3 Gl.float false stride (`Offset (2 * 4));
  Gl.bind_vertex_array 0;
  Gl.bind_buffer Gl.array_buffer 0;
  let hb_shader = Shader.create ~vertex_source ~fragment_source in
  { hb_vao = vao; hb_vbo = vbo; hb_shader }

let draw t ~held ~aspect =
  let verts = hotbar_verts held in
  let n_verts = Array.length verts / 5 in
  Gl.disable Gl.depth_test;
  Gl.bind_vertex_array t.hb_vao;
  Gl.bind_buffer Gl.array_buffer t.hb_vbo;
  let data = Gl_utils.float32_array verts in
  Gl.buffer_data Gl.array_buffer
    (Gl.bigarray_byte_size data)
    (Some data) Gl.dynamic_draw;
  Shader.use t.hb_shader;
  Shader.set_uniform_float t.hb_shader "aspect" aspect;
  Gl.draw_arrays Gl.triangles 0 n_verts;
  Gl.bind_vertex_array 0;
  Gl.bind_buffer Gl.array_buffer 0;
  Gl.enable Gl.depth_test

let destroy t =
  Gl_utils.with_int (Gl.delete_vertex_arrays 1) t.hb_vao;
  Gl_utils.with_int (Gl.delete_buffers 1) t.hb_vbo;
  Shader.destroy t.hb_shader
