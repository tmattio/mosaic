(** Headless deterministic terminal for testing Matrix applications.

    [Matrix_test] is a {!Matrix.Backend.t} over memory: input comes from a queue
    the test feeds, output is captured, the terminal size is set by the test,
    and time is a virtual clock that moves only when the test moves it. Frames
    are observed by reading the presented cell grid ({!screen}), not by parsing
    the output stream.

    The run's loop calls [on_idle] whenever it would block: when it has no input
    to read and its next deadline (a timer, a frame, a terminal that went silent
    mid-sequence) is still ahead of the virtual clock, or it has none. A test
    driver does its scripting there: feed input, move the clock, snapshot, or
    end the input. If [on_idle] returns without acting, the loop calls it again.

    Frames are at least [1 / fps] apart on the virtual clock too: a frame asked
    for in the same instant as the last waits until the test moves the clock to
    its deadline. A driver that wants to observe every frame presented moves the
    clock to [until] while {!frame_owed}, as {!settle} does.

    The terminal models a plain modern terminal with fixed capabilities (24-bit
    colour, Unicode width tables, bracketed paste; no Kitty extensions), set by
    {!config}, so rendering never depends on the environment.

    {[
    let t = Matrix_test.create ~on_idle ~width:20 ~height:2 () in
    Matrix.run ~backend:(Matrix_test.backend t) ~config:(Matrix_test.config t)
      (fun app -> ...)
    ]} *)

type t
(** The type for headless terminals. *)

val create :
  ?mode:Matrix.mode ->
  ?fps:float ->
  ?now:int ->
  ?on_wake:(unit -> unit) ->
  on_idle:(t -> until:int option -> unit) ->
  width:int ->
  height:int ->
  unit ->
  t
(** [create ~on_idle ~width ~height ()] is a headless terminal of the given
    size.

    - [mode] is the presentation mode of {!config}. Defaults to [`Alt].
    - [fps] is the frame bound of {!config}. Defaults to [60.].
    - [now] is the initial virtual time in nanoseconds (default [0]).
    - [on_wake] runs when the loop is woken from outside its thread of control
      (another thread posted). A concurrent driver unblocks its [on_idle] wait
      here; a synchronous script can omit it.
    - [on_idle t ~until] runs when the loop would block. [until] is the time the
      loop waits until, [None] when only input or a post can change anything. *)

val backend : t -> Matrix.Backend.t
(** [backend t] is the backend to give to {!Matrix.run}. *)

val config : t -> Matrix.config
(** [config t] is the configuration a run over [t] uses: its mode and frame
    bound, and the terminal's fixed capabilities. *)

val feed : t -> string -> unit
(** [feed t bytes] queues raw input bytes, decoded by the real input parser when
    the loop next reads. Complete escape sequences decode immediately; ambiguous
    prefixes (a lone [ESC], an Alt chord) wait for the terminal's silence, a
    deadline that [on_idle]'s [until] shows and that passes once the test moves
    the clock beyond it. *)

val feed_event : t -> Matrix.event -> unit
(** [feed_event t event] queues an already-decoded input event, bypassing the
    parser, in order with the bytes {!feed} queued. *)

val resize : t -> width:int -> height:int -> unit
(** [resize t ~width ~height] resizes the terminal and signals it, as a window
    resize does. The run applies the size when it next draws. *)

val now : t -> int
(** [now t] is the current virtual time in nanoseconds. *)

val advance : t -> int -> unit
(** [advance t ns] moves the virtual clock [ns] nanoseconds forward. The clock
    never moves otherwise.

    Raises [Invalid_argument] if [ns] is negative. *)

val frame_owed : t -> bool
(** [frame_owed t] is [true] iff the run over [t] has a frame to draw that waits
    for its interval to pass: a driver moves the clock to [until] to have it
    drawn. [false] when no run holds [t]. *)

val settle : t -> until:int option -> bool
(** [settle t ~until] moves the clock to [until] if the run owes a frame that
    waits for it, and says whether it did. A driver whose steps must each see
    the presented frame calls it first in [on_idle], with the [until] it was
    given, and takes its step only when it returns [false]. *)

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

val output : t -> string
(** [output t] is everything the runtime wrote so far: the raw ANSI delta
    stream. Replaying it through a VTE reconstructs {!screen}. *)

val stop : t -> unit
(** [stop t] ends the input, as a terminal that closes it: the run decodes what
    was fed, and {!Matrix.next} then returns {!Matrix.Stopped}. *)
