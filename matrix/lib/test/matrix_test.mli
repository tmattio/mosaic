(** Headless terminals for testing Matrix applications.

    A [Matrix_test] terminal is a pipe and a file: the run reads its input from
    the pipe, which the test writes ({!feed}), and writes its output to the
    file, which the test reads ({!written}). The run is an ordinary
    {!Matrix.run} on those descriptors ({!val-input}, {!val-output}). The test
    drives it: it gives each step a time with {!Matrix.val-step} and does its
    scripting whenever the run has nothing to do, so the run never waits.

    The test's {e clock} gives the steps their times ({!now}). It starts at the
    run's time when the test first reads it, which must be inside the run and
    before the first step, and moves only when the test moves it ({!advance}). A
    step is a function of the run's state, its input and its time, so a run
    driven by this clock goes the same way however fast the host is. Frames are
    at least [1 / fps] apart on it too: a frame asked for in the same instant as
    the last waits until the test moves the clock to its deadline
    ({!Matrix.deadline}), as {!settle} does.

    The output is not a terminal, so the run sends no query and has no window.
    The terminal reports its size in band, as mode 2048 does: once when it is
    made, which the run takes as it begins, and on each {!resize}. Its
    capabilities are fixed by {!config}: a plain modern terminal (24-bit colour,
    Unicode width tables, bracketed paste; no Kitty extensions), so rendering
    never depends on the environment. Frames are observed by reading the
    presented cell grid ({!screen}).

    A terminal holds its three descriptors from {!create} to {!close}.

    This test types a key and reads the frame it brings:
    {[
    let t = Matrix_test.create ~width:20 ~height:1 () in
    Fun.protect ~finally:(fun () -> Matrix_test.close t) @@ fun () ->
    Matrix.run ~input:(Matrix_test.input t) ~output:(Matrix_test.output t)
      ~config:(Matrix_test.config t)
    @@ fun app ->
    let keys = ref 0 in
    let rec until_idle () =
      match Matrix.step app ~now:(Matrix_test.now t) with
      | Some Frame ->
          Matrix.draw app (fun grid ->
              Matrix.Grid.draw_text grid ~x:0 ~y:0
                ~text:(Printf.sprintf "%d keys" !keys));
          until_idle ()
      | Some (Event (Key _)) ->
          incr keys;
          Matrix.request_redraw app;
          until_idle ()
      | Some _ -> until_idle ()
      | None -> (
          match Matrix.deadline app with
          | Some deadline ->
              Matrix_test.advance t (deadline - Matrix_test.now t);
              until_idle ()
          | None -> ())
    in
    until_idle ();
    Matrix_test.feed t "a";
    until_idle ();
    assert (String.trim (Matrix_test.screen t) = "1 keys")
    ]} *)

type t
(** The type for headless terminals. *)

val create :
  ?mode:Matrix.mode -> ?fps:float -> width:int -> height:int -> unit -> t
(** [create ~width ~height ()] is a terminal of [width] columns and [height]
    rows, with its size already reported on its input.
    - [mode] is the presentation mode of {!config}. Defaults to [`Alt].
    - [fps] is the frame bound of {!config}. Defaults to [60.].

    Raises [Unix.Unix_error] if the pipe or the file cannot be made. *)

val input : t -> Unix.file_descr
(** [input t] is the terminal's input, for {!Matrix.run}'s [input]. *)

val output : t -> Unix.file_descr
(** [output t] is the terminal's output, for {!Matrix.run}'s [output]. *)

val config : t -> Matrix.config
(** [config t] is the configuration a run over [t] uses: its mode and frame
    bound, and the terminal's fixed capabilities. *)

val close : t -> unit
(** [close t] ends the input, as {!stop} does, and closes the terminal's
    descriptors. A run must not use them after. *)

(** {1:time The clock} *)

val now : t -> int
(** [now t] is the time of the test's clock, on the clock of the run that holds
    [t]: the time to give {!Matrix.val-step}. The first call takes the run's
    time ({!Matrix.now}) as the clock's start.

    Raises [Invalid_argument] if it is the first call and no run holds [t]. *)

val elapsed : t -> int
(** [elapsed t] is how far the test has moved its clock, in nanoseconds: [0]
    until the first {!advance}. *)

val advance : t -> int -> unit
(** [advance t ns] moves the clock [ns] nanoseconds forward. The clock never
    moves otherwise.

    Raises [Invalid_argument] if [ns] is negative. *)

val frame_owed : t -> bool
(** [frame_owed t] is [true] iff the run over [t] has a frame to draw that waits
    for its interval to pass: a driver moves the clock to the run's deadline to
    have it drawn. [false] when no run holds [t]. *)

val settle : t -> until:int option -> bool
(** [settle t ~until] moves the clock to [until] if the run owes a frame that
    waits for it, and says whether it did. A driver whose steps must each see
    the presented frame calls it first whenever the run has nothing to do, with
    the run's deadline, and takes its step only when it returns [false]. *)

(** {1:input Input} *)

val feed : t -> string -> unit
(** [feed t bytes] writes [bytes] to the terminal's input, decoded by the run's
    decoder when it next reads. Complete escape sequences decode then; ambiguous
    prefixes (a lone [ESC], an Alt chord) wait for the terminal's silence, a
    deadline the run gives and that passes once the test moves the clock beyond
    it. It blocks while the pipe is full, so a test feeds at most the pipe's
    capacity (64 KiB on Linux) between steps.

    Raises [Invalid_argument] if the input has ended ({!stop}). *)

val feed_event : t -> Matrix.event -> unit
(** [feed_event t event] queues an already-decoded input event for the run that
    holds [t], bypassing the decoder, ahead of the bytes not yet read.

    Raises [Invalid_argument] if no run ever held [t]. *)

val resize : t -> width:int -> height:int -> unit
(** [resize t ~width ~height] reports the new size on the input, as a terminal
    in mode 2048 does. The run applies it when it next draws. *)

val stop : t -> unit
(** [stop t] ends the input, as a terminal that closes it: the run decodes what
    was fed, and {!Matrix.val-step} then returns {!Matrix.Stopped}. *)

(** {1:output Output} *)

val grid : t -> Matrix.Grid.t
(** [grid t] is the presented frame ({!Matrix.current_grid}) of the run that
    holds [t], or of the last run that held it once that run ended. Do not
    mutate.

    Raises [Invalid_argument] if no run ever held it. *)

val size : t -> int * int
(** [size t] is the usable size of that run ({!Matrix.size}): the live region in
    [`Primary] mode.

    Raises [Invalid_argument] if no run ever held it. *)

val screen : t -> string
(** [screen t] is the presented frame as plain text ({!Matrix.Grid.to_text} of
    {!grid}): styling discarded, rows right-stripped. *)

val screen_ansi : t -> string
(** [screen_ansi t] is the presented frame with ANSI styling
    ({!Matrix.Grid.to_ansi}). *)

val written : t -> string
(** [written t] is everything the run wrote so far: the raw ANSI stream.
    Replaying it through a VTE reconstructs {!screen}. *)
