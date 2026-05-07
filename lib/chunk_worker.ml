type result = (int * int * int) * Block.t array

type t = {
  mutex : Mutex.t;
  request_cv : Condition.t;
  result_cv : Condition.t;
  requests : (int * int * int) Queue.t;
  results : result Queue.t;
  pending_set : (int * int * int, unit) Hashtbl.t;
  stop : bool Atomic.t;
  domain : unit Domain.t;
}

(* AF: A [t] value represents a producer/consumer pipeline backed by a single
   worker [Domain]. The main thread enqueues coordinates into [requests] and
   reads finished [(coord, blocks)] pairs out of [results]. The worker pops from
   [requests], runs [Terrain.fill_chunk], and pushes into [results].
   [pending_set] tracks coordinates that are queued, being generated, or waiting
   to be consumed. RI: All accesses to [requests], [results], and [pending_set]
   are made while [mutex] is held. [Atomic.get stop] is [true] iff [destroy] has
   been called. *)

let create () =
  let mutex = Mutex.create () in
  let request_cv = Condition.create () in
  let result_cv = Condition.create () in
  let requests : (int * int * int) Queue.t = Queue.create () in
  let results : result Queue.t = Queue.create () in
  let pending_set : (int * int * int, unit) Hashtbl.t = Hashtbl.create 64 in
  let stop = Atomic.make false in
  let domain =
    Domain.spawn (fun () ->
        let continue = ref true in
        while !continue do
          Mutex.lock mutex;
          while (not (Atomic.get stop)) && Queue.is_empty requests do
            Condition.wait request_cv mutex
          done;
          if Atomic.get stop then begin
            Mutex.unlock mutex;
            continue := false
          end
          else begin
            let req = Queue.pop requests in
            Mutex.unlock mutex;
            let cx, cy, cz = req in
            let blocks = Terrain.fill_chunk ~cx ~cy ~cz in
            Mutex.lock mutex;
            Queue.push (req, blocks) results;
            Condition.broadcast result_cv;
            Mutex.unlock mutex
          end
        done)
  in
  { mutex; request_cv; result_cv; requests; results; pending_set; stop; domain }

let request t coord =
  Mutex.lock t.mutex;
  if not (Hashtbl.mem t.pending_set coord) then begin
    Hashtbl.add t.pending_set coord ();
    Queue.push coord t.requests;
    Condition.signal t.request_cv
  end;
  Mutex.unlock t.mutex

let pending t coord =
  Mutex.lock t.mutex;
  let r = Hashtbl.mem t.pending_set coord in
  Mutex.unlock t.mutex;
  r

let poll t =
  Mutex.lock t.mutex;
  let r =
    match Queue.take_opt t.results with
    | Some ((coord, _) as res) ->
        Hashtbl.remove t.pending_set coord;
        Some res
    | None -> None
  in
  Mutex.unlock t.mutex;
  r

let poll_blocking t =
  Mutex.lock t.mutex;
  while Queue.is_empty t.results && not (Atomic.get t.stop) do
    Condition.wait t.result_cv t.mutex
  done;
  let r =
    if Atomic.get t.stop && Queue.is_empty t.results then None
    else begin
      let ((coord, _) as res) = Queue.pop t.results in
      Hashtbl.remove t.pending_set coord;
      Some res
    end
  in
  Mutex.unlock t.mutex;
  r

let destroy t =
  Mutex.lock t.mutex;
  Atomic.set t.stop true;
  Condition.broadcast t.request_cv;
  Condition.broadcast t.result_cv;
  Mutex.unlock t.mutex;
  Domain.join t.domain
