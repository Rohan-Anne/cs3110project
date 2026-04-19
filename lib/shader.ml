open Tgl3

type t = { program : int }

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

(** link a vertex and frag shader into a GPU program *)
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
  { program }

let use t = Gl.use_program t.program
let destroy t = Gl.delete_program t.program
