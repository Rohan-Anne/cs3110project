(** various GL util functions to help with sending/receiving data from GL *)

(** allocate a C-layout bigarray. used to pass data to and from GL *)
val bigarray_create :
  ('a, 'b) Bigarray.kind -> int -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t

(** [get_int setter] gets the integer returned from calling the GL function
    [setter]. GL returns integers by writing to a C array pointer, which is
    tedious, so this wrapper helps *)
val get_int :
  ((int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t -> unit) ->
  int

(** [with_int setter value] writes [value] to a tmp array and passes it to the
    GL function [setter] *)
val with_int :
  ((int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t -> unit) ->
  int ->
  unit

(** [get_string len fill] allocates a char bigarray of [len], calls [fill] to
    let GL write into it, and returns it as a string *)
val get_string :
  int ->
  ((char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  unit) ->
  string

(** convert an OCaml float array to a C float32 bigarray for upload to GL *)
val float32_array :
  float array ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

(** query an int status parameter from a shader object *)
val shader_status : int -> int -> int

(** query an int status parameter from a program object *)
val program_status : int -> int -> int

(** [check_nonzero label n] raises if [n] is 0, indicating a GL object
    allocation failure *)
val check_nonzero : string -> int -> unit

(** [gl_check label] calls [glGetError] and raises if the result is not
    [no_error] *)
val gl_check : string -> unit
