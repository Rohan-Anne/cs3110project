open Tgl3

type t = {
  program : int;  (** the GPU program *)
  uniform_cache : (string, int) Hashtbl.t;
      (** cache of mappings of uniform name to location *)
}

(* error handling util *)
let fail kind log = failwith (Printf.sprintf "%s failed:\n%s" kind log)

(** compiler a shader stage from GLSL source *)
let compile_stage ~shader_type ~label source_code =
  let shader = Gl.create_shader shader_type in
  Gl.shader_source shader source_code;
  Gl.compile_shader shader;
  if Gl_utils.shader_status shader Gl.compile_status <> Gl.true_ then begin
    let log_len = Gl_utils.shader_status shader Gl.info_log_length in
    let log =
      Gl_utils.get_string log_len (Gl.get_shader_info_log shader log_len None)
    in
    Gl.delete_shader shader;
    fail label log
  end;
  shader

let create ~vertex_source ~fragment_source =
  let vertex =
    compile_stage ~shader_type:Gl.vertex_shader ~label:"vertex shader"
      vertex_source
  in
  let fragment =
    compile_stage ~shader_type:Gl.fragment_shader ~label:"fragment shader"
      fragment_source
  in
  let program = Gl.create_program () in
  Gl.attach_shader program vertex;
  Gl.attach_shader program fragment;
  Gl.link_program program;
  (* shader obj are no longer needed after linking to the program *)
  Gl.delete_shader vertex;
  Gl.delete_shader fragment;
  if Gl_utils.program_status program Gl.link_status <> Gl.true_ then begin
    let log_len = Gl_utils.program_status program Gl.info_log_length in
    let log =
      Gl_utils.get_string log_len (Gl.get_program_info_log program log_len None)
    in
    Gl.delete_program program;
    fail "program link" log
  end;
  { program; uniform_cache = Hashtbl.create 16 }

let use t = Gl.use_program t.program

let uniform_location t name =
  match Hashtbl.find_opt t.uniform_cache name with
  | Some loc -> loc
  | None ->
      let loc = Gl.get_uniform_location t.program name in
      Hashtbl.replace t.uniform_cache name loc;
      loc

(* persistent scratch buffer to prevent bigarray alloc *)
let mat4_scratch = Gl_utils.bigarray_create Bigarray.float32 16

let set_uniform_mat4 t name values =
  let loc = uniform_location t name in
  if loc <> -1 then begin
    Array.iteri (Bigarray.Array1.set mat4_scratch) values;
    Gl.uniform_matrix4fv loc 1 false mat4_scratch
  end

let destroy t = Gl.delete_program t.program
