(** block kinds. [Air] is transparent and unrendered. we can add traits to each
    block like texture id, hardness, and other properties *)
type t =
  | Air
  | Stone
  | Dirt
  | Grass
