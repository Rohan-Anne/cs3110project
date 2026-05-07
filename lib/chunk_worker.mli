(** Off-thread chunk generator. Owns one worker [Domain] that pulls
    [(cx, cy, cz)] requests from a queue, runs [Terrain.fill_chunk], and
    pushes the resulting block array back to the main thread.

    GL calls are NOT made by the worker; the main thread is responsible for
    turning each result into a [Chunk.t], inserting it into the world, and
    uploading the mesh. *)

type t

(** [create ()] spawns the worker domain and returns a handle to it. *)
val create : unit -> t

(** [request t (cx, cy, cz)] enqueues a generation request. If a request
    for the same coordinate is already pending or in-flight (i.e. a result
    has not yet been consumed via [poll]/[poll_blocking]), this call is a
    no-op. *)
val request : t -> int * int * int -> unit

(** [pending t coord] is [true] if there is an outstanding request for
    [coord] (queued, in-flight, or waiting in the result queue). *)
val pending : t -> int * int * int -> bool

(** [poll t] returns one finished result if any are ready, or [None]. The
    returned coordinate is removed from the pending set. *)
val poll : t -> ((int * int * int) * Block.t array) option

(** [poll_blocking t] is like [poll] but blocks until a result is available
    or the worker has been destroyed. Returns [None] only after [destroy]. *)
val poll_blocking : t -> ((int * int * int) * Block.t array) option

(** [destroy t] signals the worker to stop, drops any unconsumed requests,
    and joins the domain. Idempotent. *)
val destroy : t -> unit
