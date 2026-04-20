type mat4 = float array

type vec3 = {
  x : float;
  y : float;
  z : float;
}

let pi = 4.0 *. atan 1.0
let vec3 x y z = { x; y; z }
let add a b = vec3 (a.x +. b.x) (a.y +. b.y) (a.z +. b.z)
let sub a b = vec3 (a.x -. b.x) (a.y -. b.y) (a.z -. b.z)
let scale v s = vec3 (v.x *. s) (v.y *. s) (v.z *. s)
let length v = sqrt ((v.x *. v.x) +. (v.y *. v.y) +. (v.z *. v.z))

let normalize v =
  let len = length v in
  if len = 0.0 then v else scale v (1.0 /. len)

let identity () =
  [|
    1.0;
    0.0;
    0.0;
    0.0;
    0.0;
    1.0;
    0.0;
    0.0;
    0.0;
    0.0;
    1.0;
    0.0;
    0.0;
    0.0;
    0.0;
    1.0;
  |]

(* column-major multiply: a and b are both column-major mat4 *)
let multiply a b =
  let out = Array.make 16 0.0 in
  for col = 0 to 3 do
    for row = 0 to 3 do
      let idx = (col * 4) + row in
      out.(idx) <-
        (a.((0 * 4) + row) *. b.((col * 4) + 0))
        +. (a.((1 * 4) + row) *. b.((col * 4) + 1))
        +. (a.((2 * 4) + row) *. b.((col * 4) + 2))
        +. (a.((3 * 4) + row) *. b.((col * 4) + 3))
    done
  done;
  out

let translation ~x ~y ~z =
  [| 1.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; x; y; z; 1.0 |]

let rotation_x angle =
  let c = cos angle in
  let s = sin angle in
  [| 1.0; 0.0; 0.0; 0.0; 0.0; c; s; 0.0; 0.0; -.s; c; 0.0; 0.0; 0.0; 0.0; 1.0 |]

let rotation_y angle =
  let c = cos angle in
  let s = sin angle in
  [| c; 0.0; -.s; 0.0; 0.0; 1.0; 0.0; 0.0; s; 0.0; c; 0.0; 0.0; 0.0; 0.0; 1.0 |]

(** standard OpenGL perspective matrix, fov_y in radians *)
let perspective ~fov_y_radians ~aspect ~near ~far =
  let f = 1.0 /. tan (fov_y_radians /. 2.0) in
  let nf = 1.0 /. (near -. far) in
  [|
    f /. aspect;
    0.0;
    0.0;
    0.0;
    0.0;
    f;
    0.0;
    0.0;
    0.0;
    0.0;
    (far +. near) *. nf;
    -1.0;
    0.0;
    0.0;
    2.0 *. far *. near *. nf;
    0.0;
  |]

(** build a view matrix from position + yaw/pitch angles (radians) *)
let view_from_camera ~position ~yaw ~pitch =
  let t = translation ~x:(-.position.x) ~y:(-.position.y) ~z:(-.position.z) in
  multiply (rotation_x (-.pitch)) (multiply (rotation_y (-.yaw)) t)
