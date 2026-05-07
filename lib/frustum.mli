(** view-frustum represented as 6 inward-facing planes *)
type t

(** [of_mvp m] extracts the 6 frustum planes (left, right, bottom, top, near,
    far) from the combined model-view-projection matrix [m]. Each plane is
    oriented so that points inside the frustum satisfy
    [a*x + b*y + c*z + d >= 0], and is normalised so that [(a, b, c)] is a unit
    vector. *)
val of_mvp : Math3d.mat4 -> t

(** [intersects_aabb t ~min ~max] returns [true] if the axis-aligned bounding
    box spanning from [min] to [max] (with [min.x <= max.x], etc.) could overlap
    the frustum. The test is conservative: it never falsely excludes a box that
    touches the frustum, but may return [true] for boxes that lie entirely
    outside the frustum near a corner. *)
val intersects_aabb : t -> min:Math3d.vec3 -> max:Math3d.vec3 -> bool
