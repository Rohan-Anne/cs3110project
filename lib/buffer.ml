open Tgl3

(** integers are IDs for GPU-side objects *)
type t = {
  vao : int; (* VAO for the attribute layout *)
  vertex_buffer : int; (* VBO for xyz position for each vertex *)
  color_buffer : int; (* VBO for rgb color for each vertex *)
  count : int; (* num vertices = 3 * num triangles *)
}

(** tells GL how to feed one VBO into a shader input slot, [location] is the
    layout location, [size] is the number of floats per vertex *)
let bind_attribute ~buffer ~location ~size =
  Gl.bind_buffer Gl.array_buffer buffer;
  Gl.enable_vertex_attrib_array location;
  Gl.vertex_attrib_pointer location size Gl.float false 0 (`Offset 0)

let create ~positions ~colors =
  let vao = Gl_utils.get_int (Gl.gen_vertex_arrays 1) in
  let vertex_data = Gl_utils.float32_array positions in
  let color_data = Gl_utils.float32_array colors in
  (* alloc, bind, and upload position data *)
  let vertex_buffer =
    let id = Gl_utils.get_int (Gl.gen_buffers 1) in
    Gl.bind_buffer Gl.array_buffer id;
    Gl.buffer_data Gl.array_buffer
      (Gl.bigarray_byte_size vertex_data)
      (Some vertex_data) Gl.static_draw;
    id
  in
  (* same for color data *)
  let color_buffer =
    let id = Gl_utils.get_int (Gl.gen_buffers 1) in
    Gl.bind_buffer Gl.array_buffer id;
    Gl.buffer_data Gl.array_buffer
      (Gl.bigarray_byte_size color_data)
      (Some color_data) Gl.static_draw;
    id
  in
  (* bind the vao *)
  Gl.bind_vertex_array vao;
  (* position in first slot *)
  bind_attribute ~buffer:vertex_buffer ~location:0 ~size:3;
  (* color in second slot *)
  bind_attribute ~buffer:color_buffer ~location:1 ~size:3;
  (* unbind when done *)
  Gl.bind_vertex_array 0;
  Gl.bind_buffer Gl.array_buffer 0;
  { vao; vertex_buffer; color_buffer; count = Array.length positions / 3 }

let draw t =
  Gl.bind_vertex_array t.vao;
  Gl.draw_arrays Gl.triangles 0 t.count

let destroy t =
  Gl_utils.with_int (Gl.delete_vertex_arrays 1) t.vao;
  Gl_utils.with_int (Gl.delete_buffers 1) t.vertex_buffer;
  Gl_utils.with_int (Gl.delete_buffers 1) t.color_buffer
