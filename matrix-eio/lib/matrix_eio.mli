(** Eio backend for {!Matrix}.

    [Matrix_eio] gives {!Matrix.run} a backend whose waits yield to other Eio
    fibers: while the loop waits for input, a wake, a signal or a deadline, the
    domain runs the program's other fibers.

    {1:quick_start Quick start}

    {[
    Eio_main.run @@ fun env ->
    let backend =
      Matrix_eio.backend
        ~clock:(Eio.Stdenv.mono_clock env)
        ~stdin:env#stdin ~stdout:env#stdout ()
    in
    Matrix.run ~backend @@ fun app ->
    let rec loop () =
      match Matrix.next app with
      | Stopped | Event (Key { key = Escape; _ }) -> ()
      | Frame ->
          Matrix.draw app (fun grid -> (* draw into grid *) ignore grid);
          loop ()
      | _ -> loop ()
    in
    loop ()
    ]} *)

val backend :
  clock:_ Eio.Time.Mono.t ->
  stdin:_ Eio_unix.source ->
  stdout:_ Eio_unix.sink ->
  unit ->
  Matrix.Backend.t
(** [backend ~clock ~stdin ~stdout ()] is {!Matrix.Backend.unix} over the
    descriptors of [stdin] and [stdout], reading and writing through Eio, with a
    [wait] that suspends only the fiber that runs the loop.

    The wait is woken as {!Matrix.Backend.unix}'s is: by the input, by
    {!Matrix.Backend.wake_fd}, which {!Matrix.post} and the runtime's signal
    handlers write to, and by [clock] at the deadline. Nothing polls: an idle
    loop costs no wakeups. The clock is the runtime's monotonic one; [clock]
    only sleeps until a deadline on it.

    [stdin] and [stdout] must stay open while the run that uses the backend
    lasts. *)
