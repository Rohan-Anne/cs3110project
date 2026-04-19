open Tgl3

(** GL functinos return values by writing to C arrays, so we use Bigarrays to
    alloc memory that is accessible by C *)
let bigarray_create kind len = Bigarray.(Array1.create kind c_layout len)

(* GL output int parameter *)
let get_int setter =
  (* allocate a 1-element array *)
  let values = bigarray_create Bigarray.int32 1 in
  (* send to GL *)
  setter values;
  (* read the value that GL wrote, and return it *)
  Int32.to_int values.{0}

(** GL input int parameter *)
let with_int setter value =
  (* write a value to a 1-element array *)
  let values = bigarray_create Bigarray.int32 1 in
  values.{0} <- Int32.of_int value;
  (* pass it to GL *)
  setter values

(** GL output string parameter *)
let get_string len fill =
  let bytes = bigarray_create Bigarray.char len in
  fill bytes;
  Gl.string_of_bigarray bytes

(** GL requires float data as a contiguous C float array, not an OCaml array, so
    we convert *)
let float32_array values =
  let data = bigarray_create Bigarray.float32 (Array.length values) in
  Array.iteri (Bigarray.Array1.set data) values;
  data

let shader_status shader param = get_int (Gl.get_shaderiv shader param)
let program_status program param = get_int (Gl.get_programiv program param)
