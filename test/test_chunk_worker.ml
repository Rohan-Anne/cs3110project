(** Tests for the {!Chunk_worker} module.

    {1 Module Overview}
    [Chunk_worker] runs a single OCaml [Domain] that pulls [(cx, cy, cz)]
    requests off a queue, runs [Terrain.fill_chunk] on them, and pushes the
    resulting [Block.t array] back to the main thread. The main thread consumes
    results via [poll] (non-blocking) or [poll_blocking].

    {1 Invariants under test}

    {2 Lifecycle}
    - [create] returns a usable handle without raising.
    - [destroy] joins the worker without hanging, even if no work was ever
      requested.

    {2 [request] → [poll_blocking]}
    - For any [(cx, cy, cz)], [request t (cx, cy, cz)] eventually causes
      [poll_blocking t] to return [Some ((cx, cy, cz), blocks)] where [blocks]
      equals [Terrain.fill_chunk ~cx ~cy ~cz].
    - When several distinct requests are made, results for all of them
      eventually come back.

    {2 Deduplication}
    - Repeated [request] calls for the same coordinate while it is pending do
      not produce duplicate results. After consuming the single result via
      [poll], a fresh [request] for the same coordinate works again.

    {2 [pending]}
    - After [request], [pending] returns [true] for that coordinate until a
      matching [poll] consumes the result.
    - [pending] returns [false] for coordinates that were never requested. *)

open OUnit2

(* ------------------------------------------------------------------ *)
(*  Helpers                                                             *)
(* ------------------------------------------------------------------ *)

(** printer for [(int * int * int)] coords. *)
let pp_xyz (x, y, z) = Printf.sprintf "(%d, %d, %d)" x y z

(** printer for a list of int triples. *)
let pp_xyz_list lst = "[" ^ String.concat "; " (List.map pp_xyz lst) ^ "]"

(** printer for results poll'd off the worker queue. *)
let pp_poll_result = function
  | None -> "None"
  | Some ((cx, cy, cz), _) -> Printf.sprintf "Some ((%d, %d, %d), _)" cx cy cz

let with_worker f =
  let w = Chunk_worker.create () in
  let finally () = Chunk_worker.destroy w in
  try
    let r = f w in
    finally ();
    r
  with e ->
    finally ();
    raise e

let array_equal eq a b =
  Array.length a = Array.length b
  &&
  let n = Array.length a in
  let i = ref 0 and ok = ref true in
  while !ok && !i < n do
    if not (eq a.(!i) b.(!i)) then ok := false;
    incr i
  done;
  !ok

(* ------------------------------------------------------------------ *)
(*  Lifecycle                                                           *)
(* ------------------------------------------------------------------ *)

let test_create_destroy_no_work _ =
  let w = Chunk_worker.create () in
  Chunk_worker.destroy w
(* the test is that we got here without hanging *)

let test_destroy_idle_after_drain _ =
  with_worker (fun w ->
      Chunk_worker.request w (0, 0, 0);
      let _ = Chunk_worker.poll_blocking w in
      ())

(* ------------------------------------------------------------------ *)
(*  Single request                                                      *)
(* ------------------------------------------------------------------ *)

let test_single_request_returns_correct_blocks _ =
  with_worker (fun w ->
      let coord = (0, 0, 0) in
      Chunk_worker.request w coord;
      match Chunk_worker.poll_blocking w with
      | None -> assert_failure "poll_blocking returned None unexpectedly"
      | Some (got_coord, got_blocks) ->
          assert_equal ~printer:pp_xyz ~msg:"coord matches" coord got_coord;
          let cx, cy, cz = coord in
          let want = Terrain.fill_chunk ~cx ~cy ~cz in
          assert_bool "blocks equal Terrain.fill_chunk"
            (array_equal ( = ) want got_blocks))

let test_negative_coord _ =
  with_worker (fun w ->
      let coord = (-3, -1, 4) in
      Chunk_worker.request w coord;
      match Chunk_worker.poll_blocking w with
      | None -> assert_failure "no result"
      | Some (got_coord, got_blocks) ->
          assert_equal ~printer:pp_xyz coord got_coord;
          let cx, cy, cz = coord in
          assert_bool "blocks correct for negative coord"
            (array_equal ( = ) (Terrain.fill_chunk ~cx ~cy ~cz) got_blocks))

(* ------------------------------------------------------------------ *)
(*  Multiple distinct requests                                          *)
(* ------------------------------------------------------------------ *)

let test_multiple_requests_all_complete _ =
  with_worker (fun w ->
      let coords = [ (0, 0, 0); (1, 0, 0); (0, 0, 1); (-1, 0, 0) ] in
      List.iter (Chunk_worker.request w) coords;
      let got = ref [] in
      List.iter
        (fun _ ->
          match Chunk_worker.poll_blocking w with
          | None -> assert_failure "premature None"
          | Some (c, _) -> got := c :: !got)
        coords;
      let sorted_got = List.sort compare !got in
      let sorted_expected = List.sort compare coords in
      assert_equal ~printer:pp_xyz_list ~msg:"all requested coords returned"
        sorted_expected sorted_got)

(* ------------------------------------------------------------------ *)
(*  Deduplication                                                       *)
(* ------------------------------------------------------------------ *)

(** request(A) three times, then request(B). Worker is FIFO, so we should see A
    then B, and crucially only ONE A even though we requested it three times.
    After draining B, no further results should be pending. *)
let test_request_dedup _ =
  with_worker (fun w ->
      Chunk_worker.request w (0, 0, 0);
      Chunk_worker.request w (0, 0, 0);
      Chunk_worker.request w (0, 0, 0);
      Chunk_worker.request w (1, 0, 0);
      let r1 = Chunk_worker.poll_blocking w in
      let r2 = Chunk_worker.poll_blocking w in
      let coord_of = function
        | Some ((c, _) : (int * int * int) * _) -> c
        | None -> assert_failure "unexpected None"
      in
      let c1 = coord_of r1 and c2 = coord_of r2 in
      let got = List.sort compare [ c1; c2 ] in
      assert_equal ~printer:pp_xyz_list ~msg:"exactly the two unique coords"
        [ (0, 0, 0); (1, 0, 0) ]
        got;
      (* both unique requests processed; no third result should exist *)
      assert_equal ~printer:pp_poll_result ~msg:"queue empty after dedup" None
        (Chunk_worker.poll w))

(** After consuming a result, the same coordinate can be requested again and
    will produce a fresh result (pending_set has been cleared). *)
let test_request_after_consume _ =
  with_worker (fun w ->
      Chunk_worker.request w (5, 0, 5);
      let _ = Chunk_worker.poll_blocking w in
      Chunk_worker.request w (5, 0, 5);
      match Chunk_worker.poll_blocking w with
      | None -> assert_failure "second request yielded no result"
      | Some (c, _) -> assert_equal ~printer:pp_xyz (5, 0, 5) c)

(* ------------------------------------------------------------------ *)
(*  pending predicate                                                   *)
(* ------------------------------------------------------------------ *)

let test_pending_false_when_never_requested _ =
  with_worker (fun w ->
      assert_bool "pending is false for fresh worker"
        (not (Chunk_worker.pending w (7, 7, 7))))

let test_pending_clears_after_poll _ =
  with_worker (fun w ->
      Chunk_worker.request w (2, 0, 0);
      let _ = Chunk_worker.poll_blocking w in
      assert_bool "pending is false after poll consumes the result"
        (not (Chunk_worker.pending w (2, 0, 0))))

let test_poll_empty_returns_none _ =
  with_worker (fun w ->
      assert_equal ~printer:pp_poll_result ~msg:"poll on empty queue is None"
        None (Chunk_worker.poll w))

let tests =
  "Chunk_worker"
  >::: [
         "create_destroy_no_work" >:: test_create_destroy_no_work;
         "destroy_idle_after_drain" >:: test_destroy_idle_after_drain;
         "single_request_correct_blocks"
         >:: test_single_request_returns_correct_blocks;
         "negative_coord" >:: test_negative_coord;
         "multiple_requests_all_complete"
         >:: test_multiple_requests_all_complete;
         "request_dedup" >:: test_request_dedup;
         "request_after_consume" >:: test_request_after_consume;
         "pending_false_when_never_requested"
         >:: test_pending_false_when_never_requested;
         "pending_clears_after_poll" >:: test_pending_clears_after_poll;
         "poll_empty_returns_none" >:: test_poll_empty_returns_none;
       ]

let _ = run_test_tt_main tests
