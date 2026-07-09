(** Headless deterministic backend for testing Matrix applications.

    [Matrix_test] builds a {!Matrix.app} wired to an in-memory backend: input
    comes from a queue the test feeds, output is captured, the terminal size is
    programmatic, and time is a virtual value that only moves when the test
    moves it. Frames are observed by reading the cell grid ({!screen}), not by
    parsing the output stream.

    The backend is scheduler-agnostic. {!create}'s [on_idle] callback is invoked
    whenever the event loop runs out of queued input and would otherwise block;
    a test driver does its scripting or fiber coordination there — feed input,
    move the clock, snapshot, or {!stop}. If [on_idle] returns without acting,
    the loop calls it again.

    The terminal models a plain modern terminal with fixed capabilities (24-bit
    colour, Unicode width tables, bracketed paste; no Kitty extensions), so
    rendering never depends on the environment.

    For full control over the backend callbacks, use {!Matrix.attach} directly;
    this module packages the deterministic wiring — virtual clock, zero resize
    debounce, pinned capabilities — that headless tests need. *)

type t
(** The type for headless test backends. *)

val create :
  ?mode:Matrix.mode ->
  ?target_fps:float ->
  ?exit_on_ctrl_c:bool ->
  ?now:float ->
  ?on_wake:(unit -> unit) ->
  on_idle:(t -> timeout:float option -> unit) ->
  width:int ->
  height:int ->
  unit ->
  t
(** [create ~on_idle ~width ~height ()] is a headless backend of the given size.

    - [mode] is the presentation mode. Defaults to [`Alt].
    - [target_fps] is the render cadence for live frames (default [60.]). Frame
      deadlines are computed from virtual time, so live rendering only
      progresses when the test moves the clock.
    - [exit_on_ctrl_c] closes the backend on Ctrl+C when [true]. Defaults to
      [false] so tests observe the application's own quit behaviour.
    - [now] is the initial virtual time in seconds (default [0.]).
    - [on_wake] runs when the runtime signals the loop from outside its thread
      of control (e.g. an asynchronous command completed and requested a
      redraw). A concurrent driver unblocks its [on_idle] wait here; a
      synchronous script can omit it.
    - [on_idle t ~timeout] runs when the loop has no queued input and no due
      frame. [timeout] is the loop's requested wait: [None] when only new input
      can change the screen, [Some s] when a virtual deadline — a timer wakeup,
      resize debounce, or a parser flush — is [s] seconds away. A dirty frame
      gated only on the render cadence never reaches [on_idle]: the backend
      advances virtual time to the pacing deadline itself, so [on_idle] always
      observes a fully presented frame. *)

val app : t -> Matrix.app
(** [app t] is the backend to hand to the runtime, e.g.
    [Mosaic.run ~matrix:(Matrix_test.app t)] or {!Matrix.run}. *)

val feed : t -> string -> unit
(** [feed t bytes] queues raw input bytes, decoded by the real input parser on
    the next loop iteration. Complete escape sequences decode immediately;
    ambiguous prefixes (a lone [ESC], an Alt chord) sit on the parser's flush
    deadline and resolve once {!set_now} moves virtual time past it — the
    deadline is surfaced through [on_idle]'s [timeout]. *)

val feed_event : t -> Matrix.Input.t -> unit
(** [feed_event t event] queues an already-decoded input event, bypassing the
    parser. *)

val resize : t -> width:int -> height:int -> unit
(** [resize t ~width ~height] resizes the terminal and queues the resize event.
    The new geometry applies on the next loop iteration. *)

val now : t -> float
(** [now t] is the current virtual time in seconds. *)

val set_now : t -> float -> unit
(** [set_now t time] moves the virtual clock to [time]. Due frame deadlines and
    timers fire on the following loop iterations. The clock never moves
    otherwise. *)

val screen : t -> string
(** [screen t] is the current frame as plain text ({!Matrix.Grid.to_text} of the
    backend grid): styling discarded, rows right-stripped. *)

val screen_ansi : t -> string
(** [screen_ansi t] is the current frame with ANSI styling
    ({!Matrix.Grid.to_ansi}). *)

val output : t -> string
(** [output t] is everything the renderer wrote so far — the raw ANSI delta
    stream. Replaying it through a VTE reconstructs {!screen}. *)

val stop : t -> unit
(** [stop t] stops the event loop; the runtime's run function returns. *)
