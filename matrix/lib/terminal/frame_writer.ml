open Unix

(* Low-level I/O Helper *)

let rec wait_writable fd =
  match Unix.select [] [ fd ] [] (-1.) with
  | _ -> ()
  | exception Unix_error (EINTR, _, _) -> wait_writable fd

let rec write_all fd bytes off len =
  if len <= 0 then ()
  else
    try
      let written = Unix.write fd bytes off len in
      write_all fd bytes (off + written) (len - written)
    with
    | Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _) ->
        wait_writable fd;
        write_all fd bytes off len
    | Unix_error (err, fn, arg) ->
        Printf.eprintf "[Terminal] Frame_writer I/O error: %s\n%!"
          (Unix.error_message err);
        raise (Unix_error (err, fn, arg))

(* Direct mode: single buffer, synchronous writes *)
type direct_state = {
  fd : Unix.file_descr;
  buffer : bytes;
}

(* Threaded mode: double buffering with async writes *)
type threaded_state = {
  fd : Unix.file_descr;
  mutex : Mutex.t;
  cond : Condition.t;
  (* The buffer the app renders into *)
  mutable render_buffer : bytes;
  (* The buffer the worker thread writes from *)
  mutable write_buffer : bytes;
  mutable write_len : int;
  mutable busy : bool;
  mutable closing : bool;
  mutable thread : Thread.t option;
  mutable error : Unix.error option;
}

let worker_loop state =
  let rec loop () =
    Mutex.lock state.mutex;

    (* Wait for work *)
    while (not state.busy) && not state.closing do
      Condition.wait state.cond state.mutex
    done;

    let should_exit = state.closing && not state.busy in
    if should_exit then Mutex.unlock state.mutex
    else
      (* Grab local refs to release lock during I/O *)
      let buf = state.write_buffer in
      let len = state.write_len in

      Mutex.unlock state.mutex;

      (* Blocking Write - capture errors *)
      let write_error =
        try
          write_all state.fd buf 0 len;
          None
        with Unix_error (err, _, _) -> Some err
      in

      (* Re-acquire lock to update status *)
      Mutex.lock state.mutex;
      state.busy <- false;
      (* Store first error encountered *)
      if Option.is_none state.error then state.error <- write_error;

      (* Wake up Main Thread if it was blocked on backpressure *)
      Condition.broadcast state.cond;
      let should_exit_after_flush = state.closing in
      Mutex.unlock state.mutex;

      if should_exit_after_flush then () else loop ()
  in
  loop ()

(* Main API *)

type t = Direct of direct_state | Threaded of threaded_state

let create ~fd ~size ~use_thread =
  if not use_thread then
    Direct { fd; buffer = Bytes.create size }
  else
    let mutex = Mutex.create () in
    let cond = Condition.create () in
    let render_buffer = Bytes.create size in
    let write_buffer = Bytes.create size in

    let state =
      {
        fd;
        mutex;
        cond;
        render_buffer;
        write_buffer;
        write_len = 0;
        busy = false;
        closing = false;
        thread = None;
        error = None;
      }
    in

    let t = Thread.create worker_loop state in
    state.thread <- Some t;
    Threaded state

let render_buffer = function
  | Direct state -> state.buffer
  | Threaded state -> state.render_buffer

let drain = function
  | Direct _ -> ()
  | Threaded state ->
      Mutex.lock state.mutex;
      (try
         while state.busy do
           Condition.wait state.cond state.mutex
         done
       with _ -> ());
      Mutex.unlock state.mutex

let present t len =
  match t with
  | Direct state ->
      if len > 0 then write_all state.fd state.buffer 0 len
  | Threaded state ->
      if len > 0 then (
        Mutex.lock state.mutex;

        (* BACKPRESSURE: Wait if worker is still writing previous frame *)
        while state.busy do
          Condition.wait state.cond state.mutex
        done;

        if state.closing then Mutex.unlock state.mutex
        else (
          (* SWAP BUFFERS *)
          let old_render = state.render_buffer in
          let old_write = state.write_buffer in

          (* Handle resizing if write buffer is too small *)
          let new_write =
            if Bytes.length old_render > Bytes.length old_write then
              Bytes.create (Bytes.length old_render)
            else old_write
          in

          state.write_buffer <- old_render;
          state.render_buffer <- new_write;
          state.write_len <- len;
          state.busy <- true;

          Condition.signal state.cond;
          Mutex.unlock state.mutex))

(* Submit a string for writing. This drains any pending writes first to ensure
   proper ordering, then writes the string synchronously. Use this for control
   sequences that must be serialized with frame data. *)
let submit_string t s =
  if String.length s = 0 then ()
  else (
    drain t;
    match t with
    | Direct state ->
        write_all state.fd (Bytes.unsafe_of_string s) 0 (String.length s)
    | Threaded state ->
        write_all state.fd (Bytes.unsafe_of_string s) 0 (String.length s))

let close t =
  match t with
  | Direct _ -> ()
  | Threaded state ->
      Mutex.lock state.mutex;
      let already_closing = state.closing in
      if not already_closing then (
        state.closing <- true;
        Condition.broadcast state.cond);
      Mutex.unlock state.mutex;
      (* Wait for worker thread to finish. In normal operation, the worker exits
         promptly after seeing closing=true. If it's stuck in a blocking write,
         this may block until the write completes or errors. *)
      if not already_closing then Option.iter Thread.join state.thread
