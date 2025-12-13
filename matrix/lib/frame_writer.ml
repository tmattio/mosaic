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
        Printf.eprintf "[Matrix] Frame_writer I/O error: %s\n%!"
          (Unix.error_message err);
        raise (Unix_error (err, fn, arg))

(* Threaded Logic *)

type threaded_state = {
  fd : Unix.file_descr;
  mutex : Mutex.t;
  cond : Condition.t;
  (* The buffer currently owned by the worker thread *)
  mutable write_buffer : bytes;
  mutable write_len : int;
  (* Status flags *)
  mutable busy : bool; (* True if worker has data to write *)
  mutable closing : bool;
  mutable thread : Thread.t option;
  (* Error tracking: stores the first error encountered by worker thread *)
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

type t = Direct of Unix.file_descr | Threaded of threaded_state

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

let last_error = function
  | Direct _ -> None
  | Threaded state ->
      Mutex.lock state.mutex;
      let err = state.error in
      Mutex.unlock state.mutex;
      err

let clear_error = function
  | Direct _ -> ()
  | Threaded state ->
      Mutex.lock state.mutex;
      state.error <- None;
      Mutex.unlock state.mutex

let create ~fd ~size ~use_thread =
  if not use_thread then Direct fd
  else
    let mutex = Mutex.create () in
    let cond = Condition.create () in
    let write_buffer = Bytes.create size in

    let state =
      {
        fd;
        mutex;
        cond;
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

let submit t app_buffer len =
  match t with
  | Direct fd ->
      if len > 0 then write_all fd app_buffer 0 len;
      app_buffer
  | Threaded state ->
      if len <= 0 then app_buffer
      else (
        Mutex.lock state.mutex;

        (* BACKPRESSURE: Wait if worker is still writing previous frame *)
        while state.busy do
          Condition.wait state.cond state.mutex
        done;

        if state.closing then (
          Mutex.unlock state.mutex;
          app_buffer)
        else
          (* SWAP BUFFERS *)
          let spare_buffer = state.write_buffer in

          (* Handle resizing if necessary *)
          let spare_buffer =
            if Bytes.length spare_buffer < len then Bytes.create len
            else spare_buffer
          in

          state.write_buffer <- app_buffer;
          state.write_len <- len;
          state.busy <- true;

          Condition.signal state.cond;
          Mutex.unlock state.mutex;

          (* Return the spare buffer to the app *)
          spare_buffer)

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
