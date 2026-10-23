(** Terminal application runtime.

    [Matrix] owns the terminal while an application runs: it negotiates input
    protocols, decodes input, keeps time, builds frames against a
    double-buffered {!Grid.t}, and diffs the grid to emit minimal ANSI output.

    An application runs in a scope, {!run}, which takes the terminal and gives
    it back on every way out. Inside, the application asks for the next thing to
    do, {!next}, and does it: an input event, a timer that came due, a message
    another thread posted, or a frame to draw. Matrix decides the order and the
    timing; the application decides what each step means. {!next} waits for the
    terminal itself; a program that waits with an event loop of its own takes
    the steps with {!val-step} and waits for what {!fds} and {!deadline} say
    ({{!loop}The loop}).

    {[
    let () =
      Matrix.run @@ fun app ->
      let rec loop count =
        match Matrix.next app with
        | Stopped | Event (Key { key = Escape; _ }) -> ()
        | Event (Key _) ->
            Matrix.request_redraw app;
            loop (count + 1)
        | Frame ->
            Matrix.draw app (fun grid ->
                Grid.draw_text grid ~x:0 ~y:0
                  ~text:(Printf.sprintf "%d keys" count));
            loop count
        | Event _ | Timer _ | Posted _ -> loop count
      in
      loop 0
    ]}

    {1:sub_libraries Sub-libraries}

    {!modules:Ansi Grid Input Screen Terminal Terminfo Text Tty} *)

(** {1:sub Sub-libraries} *)

module Ansi = Ansi
(** ANSI escape sequence generation. *)

module Grid = Matrix_grid
(** Cell-based drawing grid. *)

module Input = Matrix_input
(** Terminal input decoding. *)

module Screen = Matrix_screen
(** Double-buffered frame rendering. *)

module Terminal = Matrix_terminal
(** Terminal protocol state machine. *)

module Terminfo = Matrix_terminfo
(** Terminal descriptions from the terminfo database. *)

module Text = Matrix_text
(** Terminal text measurement and segmentation. *)

module Tty = Tty
(** The terminal device and the signals it sends. *)

(** {1:types Types} *)

type kitty_keyboard = [ `Auto | `Off ]
(** The type for kitty keyboard protocol configurations.
    - [`Auto] pushes the protocol's flags once the terminal answers its query,
      and asks for the flags again so that {!capabilities} says which hold.
      Without an answer the runtime sets xterm's modifyOtherKeys instead, so
      that chords such as Ctrl+Alt with a letter stay distinct.
    - [`Off] never uses the protocol. *)

type mode = [ `Alt | `Primary ]
(** The type for presentation modes.
    - [`Alt] uses the alternate screen buffer. The application fully owns the
      screen; content is restored on exit.
    - [`Primary] renders an inline live viewport on the primary screen. Rows
      above the viewport are static transcript rows and enter native scrollback.
      When the input is a terminal, the viewport starts at the cursor, and after
      a resize or a {!release} the runtime finds it again from a cursor position
      report: terminals move their rows as they resize. *)

(** {1:events Events} *)

(** The type for what the terminal says to the application. The input is decoded
    by {!Input.Decoder}; replies to the runtime's queries never reach the
    application, but those that are application facts do, as {!Color_scheme}. A
    batch of queries is owed until the terminal answers it, however late, or, if
    it never does, for 5 s or four round trips of the startup probe, whichever
    is longer; a reply after that decodes as input. *)
type event =
  | Key of Input.Keystroke.t  (** A key pressed or auto-repeated. *)
  | Key_release of Input.Keystroke.release
      (** A key released, reported only when {!config}'s [key_releases] asks. *)
  | Text of string
      (** Text the terminal reports without a key, such as text an input method
          composed. *)
  | Mouse of Input.Mouse_report.t
      (** A mouse report. In [`Primary] mode its row counts from the top of the
          live region. *)
  | Paste_start  (** A bracketed paste begins. *)
  | Paste_data of string
      (** Content of the paste, exactly as sent, in pieces as it arrives. The
          runtime holds none of it and sets it no limit: the application keeps
          what it wants. *)
  | Paste_end of [ `Complete | `Cut ]
      (** The paste ends at its end marker ([`Complete]), or is cut by the end
          of the input or {!abort_paste}. *)
  | Focus_in  (** The terminal window gained focus. *)
  | Focus_out  (** The terminal window lost focus. *)
  | Resize of int * int
      (** The usable size changed to [(cols, rows)], {!size}: the live region in
          [`Primary] mode, the whole terminal in [`Alt] mode. The terminal
          reports its size when it changes, by the window-size signal of the
          process's controlling terminal or its own report (mode 2048); the
          runtime applies the latest report when it next draws, so a storm of
          reports is one [Resize], returned by {!next} just before the {!Frame}
          drawn at that size. *)
  | Color_scheme of [ `Dark | `Light ]
      (** The terminal's color scheme, in reply to a query or on a change. *)
  | Invalid of string
      (** Bytes that are not UTF-8, one ill-formed subsequence. *)
  | Reply of Input.Reply.t
      (** A reply to the application's own queries, sent with {!query}. *)

val pp_event : Format.formatter -> event -> unit
(** [pp_event] formats an event for debugging. *)

(** {1:stopping Stop requests} *)

(** Ending a run from another thread, domain or fiber.

    A {e stop request} is made before the run it stops and given to {!run}, so
    that a stop requested before the run has begun, when nothing could {!post}
    to it yet, is not lost. Once the stop is requested, the run's every step is
    {!Stopped}: the request wakes a run that waits for its descriptors, and a
    run given a request already made stops at its first step. The application
    ends the run by returning, as at the end of its input. *)
module Stop : sig
  type t
  (** The type for stop requests. *)

  val create : unit -> t
  (** [create ()] is a stop request not yet requested. *)

  val request : t -> unit
  (** [request s] requests the stop of every run [s] is given to: each returns
      {!Stopped} from its next step on. A run still taking its terminal or
      giving it back sees the request once that is done: on a terminal that does
      not answer, up to max(500 ms, four round trips) after the run's queries
      were sent ({!run}). Any thread, domain or fiber may call it. Requesting it
      again does nothing. *)
end

(** {1:running Running} *)

type config
(** The type for what a run sets up. *)

val config :
  ?mode:mode ->
  ?raw_mode:bool ->
  ?mouse:Terminal.mouse_mode ->
  ?bracketed_paste:bool ->
  ?kitty_keyboard:kitty_keyboard ->
  ?key_releases:bool ->
  ?fps:float ->
  ?explicit_width:bool ->
  ?min_tui_height:int ->
  ?caps:Terminal.capabilities ->
  unit ->
  config
(** [config ()] is the configuration of a run.

    {b Display:}
    - [mode] presentation mode. Defaults to [`Alt].
    - [raw_mode] whether to switch the input into raw mode. Defaults to [true].
      In raw mode the terminal sends its interrupt and suspend characters as
      keys: Ctrl+C is a {!Key}, whose meaning is the application's.
    - [explicit_width] whether to use built-in wcwidth instead of querying the
      terminal. Defaults to [false].
    - [min_tui_height] the fewest rows the live region keeps in [`Primary] mode
      as static rows are written and the terminal shrinks. Defaults to [1]. The
      run keeps the floor between frames: {!size}, {!effective_size} and each
      {!Resize} count it. A frame that requires its rows with {!draw}'s
      [primary_required_rows] leaves the same screen, but only from that frame
      on.

    {b Frames:}
    - [fps] the most frames per second. Defaults to [60.]. A terminal sends no
      signal when it has shown a frame, so nothing gives the rate: 60 is the
      refresh rate of most displays, which no emulator draws faster than.

    {b Input:}
    - [mouse] mouse tracking. Defaults to [`Sgr_any]; [`Off] leaves the mouse to
      the terminal.
    - [bracketed_paste] whether to enable bracketed paste. Defaults to [true].
    - [kitty_keyboard] kitty keyboard protocol configuration. Defaults to
      [`Auto].
    - [key_releases] whether to ask the kitty keyboard protocol for key releases
      ({!Key_release}), which also has every key reported as a sequence.
      Defaults to [false].
    - [caps] the capabilities to assume where the terminal does not answer, in
      place of what the environment says ({!Terminal.Caps.assume}). By default
      {!run}'s [env] is read when the run begins.

    Focus reports (mode 1004), color scheme reports (2031) and resize reports
    (2048) are always set, without asking whether the terminal has them: no
    reference says what a terminal does with a mode it does not know, but xterm,
    kitty, foot and tmux all ignore one (xterm's [dpmodes] in charproc.c,
    kitty's [set_mode_from_const] in screen.c, foot's [decset_decrst] in csi.c
    and tmux's [input_csi_dispatch_sm_private] in input.c only log it), and the
    runtime resets them when it gives the terminal back.

    What a run's frames show is the application's to draw, its diagnostics
    included ({{!diagnostics}Diagnostics}), and so is the cursor's visibility
    ({!set_cursor}).

    Raises [Invalid_argument] if [fps] is not positive and finite. *)

type 'msg app
(** The type for running applications whose other threads send them messages of
    type ['msg] ({!post}). An [app] exists only inside {!run}, and belongs to
    the thread that called {!run}: every function of this module that takes one
    raises [Invalid_argument] on another thread, except {!post}. *)

val run :
  ?input:Unix.file_descr ->
  ?output:Unix.file_descr ->
  ?env:Terminal.Caps.env ->
  ?stop:Stop.t ->
  ?config:config ->
  ('msg app -> 'a) ->
  'a
(** [run f] takes the terminal whose input is [input] and whose output is
    [output], calls [f app], and gives the terminal back when [f] returns or
    raises, or when the input ends and [f] returns. [input] defaults to
    [Unix.stdin] and [output] to [Unix.stdout]. [env] is the environment of the
    session the terminal runs, from which the runtime guesses what the terminal
    can do until it answers. It defaults to the process's, with the description
    the terminfo database has for its [TERM] ({!Tty.terminfo}). [stop] ends the
    run from elsewhere ({!Stop}).

    Taking the terminal is a sequence of steps, each of which pushes its undo:
    the signals ({{!signals}Signals}), raw mode, the capability probe (whose
    answers are waited for at most max(500 ms, four round trips); input that
    comes meanwhile is kept and returned first by {!next}), the screen (the
    alternate screen, or in [`Primary] mode the live region found from a cursor
    report), the measure of text sizing, and the modes. A terminal that answered
    the probe measures text sizing on cells the run owns, the alternate screen's
    second row or the live region's first, which is then erased. In [`Primary]
    mode the live region takes at least two rows for it, since a space of scale
    2 is two rows high. If a step raises, the undos made so far run in reverse
    and the exception propagates with its backtrace. Giving the terminal back
    runs every undo, each even if another raises: when a reply is owed it first
    waits at most max(500 ms, four round trips) for the terminal to answer, so
    that no reply reaches the shell as typing; then the modes are reset, the
    cursor shown, the screen left (the live region erased below the transcript
    in [`Primary] mode), cooked mode restored, and input not read discarded. The
    process's exit path gives it back too
    ({{!lifecycle}Giving the terminal away}).

    A {e served session} is a terminal the process runs for a client elsewhere,
    such as a remote shell's or a web page's. It runs on the slave side of a
    pseudo-terminal the program opened, with the client's environment as [env],
    while the program copies bytes between the master side and the client. The
    runtime queries only a terminal, so a socket would leave the runtime
    guessing. A terminal that is not the process's controlling one sends the
    process no window signal: the program writes the client's size to the master
    in band, [CSI 48 ; rows ; cols ; height ; width t] as mode 2048 reports it,
    before the run and on every change. An output that is not a terminal has no
    window either, and keeps the size its terminal last reported in band, 80 by
    24 before any.

    Raises [Invalid_argument] if the terminal is the process's controlling
    terminal ({!Tty.is_controlling}) and another run holds it, unless that run
    gave it away for {!release}. Runs on other terminals, such as served
    sessions, each in its domain, run side by side. *)

(** {1:loop The loop}

    The application takes the run's work one {e step} at a time. {!val-step}
    gives the next step at the time the caller gives it, and never waits for
    one. When the run has nothing to do it returns [None], and the caller waits
    until one of the descriptors {!fds} is readable or the time {!deadline}
    comes, then asks again. {!next} is that loop, waiting with [select(2)]. A
    program that waits with an event loop of its own, such as Eio's, calls
    {!val-step} and waits with that loop instead. With nothing to wait for,
    {!deadline} is [None], so an idle application costs no wakeups.

    Steps come from {e turns}. A turn begins with the first step after [None],
    and when the work of the turn before is done. A turn:

    + takes the time of the step that began it as its own ({!now});
    + acts on the signals received;
    + reads the input once, without waiting, and decodes it, unless an event
      decoded earlier is still waiting, which leaves the rest in the operating
      system's buffer;
    + takes the messages posted since the last turn;
    + returns work, one step at a time: first one step of each kind that has
      some (an input {!Event}, a {!Timer} that came due, a {!Posted} message),
      then the events, then the timers, then the posted messages, until none is
      left or one frame interval has passed between the turn's time and the
      latest step's. So a flood of one kind delays the others by at most a
      frame, and within a kind everything comes in order;
    + returns a {!Frame} if one was asked for ({!request_redraw}), one frame
      interval has passed since the last by the latest step's time (work that
      took the budget may have taken the interval), and the runtime waits for no
      answer from the terminal (the place of the live region after a resize or a
      release). Before it, if the terminal's size changed, a {!Resize}.

    Each step is done before the next is asked for, so the application handles
    one event at a time, and an event sees what the previous one did. Steps are
    ordered by kind, not by time: a key read in a turn comes before a timer that
    came due in the same turn, even if the timer's deadline was earlier. The
    input is never held for an answer from the terminal; only frames are. *)

type timer = private int
(** The type for timers, set with {!at}. Two timers set in one run differ. *)

(** The type for the steps of a run. *)
type 'msg step =
  | Event of event  (** An input event, in the order the terminal sent them. *)
  | Timer of timer  (** A timer whose deadline the turn's time reached. *)
  | Posted of 'msg  (** A message another thread sent with {!post}. *)
  | Frame
      (** Draw a frame, with {!draw}. A frame not drawn by the next call of
          {!next} is dropped. *)
  | Stopped
      (** The input ended, and every event decoded before its end has been
          returned; or the run's stop was requested ({!Stop}). Every later call
          returns [Stopped]. *)

val step : 'msg app -> now:int -> 'msg step option
(** [step app ~now] is the next step of the run at the time [now], or [None]
    when the run has nothing to do until one of {!fds} is readable or the time
    {!deadline} comes. It never waits for input, a post or a time: it looks at
    the input without waiting, and reads and decodes what is there. It blocks
    while the terminal takes no output, and while the process is stopped by
    SIGTSTP ({{!signals}Signals}). [now] must be on the clock {!deadline}
    answers in, and never earlier than the time of the step before
    ({{!time}Time}).

    Raises [Invalid_argument] if [now] is earlier than the time of the step
    before. *)

val fds : 'msg app -> Unix.file_descr list
(** [fds app] is the descriptors the run waits for: once one of them is
    readable, {!val-step} has something to do. They are the run's choice, and
    may differ from one wait to the next: the input while it has not ended, and
    the run's wake, which {!post} and the signal handlers write. They never
    include a Windows console's handle, which waiting with [Unix.select] would
    empty of its records that are not keys. A caller must wait for them to be
    readable, and must never read them: what it reads, the run never sees. *)

val deadline : 'msg app -> int option
(** [deadline app] is the time by which {!val-step} must be called again after
    it returned [None], if no descriptor of {!fds} has become readable first. It
    is [None] when only those descriptors can give the run something to do.
    Right after [step app ~now] returned [None], it is [None] or later than
    [now]: a run that has nothing to do at [now] has nothing due at [now]. A
    message posted since ({!post}) makes it [now]. *)

val clock : unit -> int
(** [clock ()] is the run's own clock, {!Tty.now}, in nanoseconds. *)

val next : 'msg app -> 'msg step
(** [next app] is the next step of the run, waiting for one: {!val-step} at
    {!clock}'s time, waiting with [select(2)] whenever it returns [None]. It
    behaves as this loop does, but allocates nothing while the run is idle:
    {[
    let rec next app =
      match Matrix.step app ~now:(Matrix.clock ()) with
      | Some step -> step
      | None ->
          let timeout =
            match Matrix.deadline app with
            | None -> -1.
            | Some d -> Float.max 0. (Float.of_int (d - Matrix.clock ()) *. 1e-9)
          in
          (try ignore (Unix.select (Matrix.fds app) [] [] timeout)
           with Unix.Unix_error (EINTR, _, _) -> ());
          next app
    ]} *)

(** {1:time Time}

    Times are integer nanoseconds from an arbitrary origin, on a clock that
    never goes back, so a deadline computed as [origin + n * interval] is exact
    and two computations of it are equal. A run's time is the time of its steps:
    everything in a turn compares with the time of the step that began it,
    {!now}. A step is a function of the run's state, its input and that time
    alone, so a run whose steps are given the times of a recording or a
    simulation goes as it would have gone at those times.

    The times given to {!val-step} must never decrease, and must be on the clock
    {!deadline} answers in, for the run compares them with deadlines computed
    from earlier steps. That clock is {!clock}, which {!next} reads, which the
    run reads itself as it takes the terminal and gives it back, and which
    {!now} holds before the first step. A test may instead give the steps a
    clock that starts at {!now} before the first step and moves only when the
    test moves it. A time from another clock, such as [Unix.gettimeofday], makes
    the run's deadlines meaningless. *)

val now : 'msg app -> int
(** [now app] is the time of the current turn, in nanoseconds. *)

val at : 'msg app -> int -> timer
(** [at app time] is a timer that {!next} returns as {!Timer} in the first turn
    whose time is at least [time]. A deadline is a lower bound: the loop may be
    late, never early. Timers due in one turn come in deadline order, then in
    the order they were set. *)

val cancel : 'msg app -> timer -> unit
(** [cancel app timer] keeps [timer] from being returned. Cancelling a timer
    that was returned or cancelled does nothing. *)

(** {1:frames Frames} *)

val request_redraw : 'msg app -> unit
(** [request_redraw app] asks for a frame. Requests until that frame is drawn
    are one request. Frames are at least [1 / fps] apart ({!config}): the first
    after a pause is returned in the turn that asked, later ones once their
    interval has passed. A request made while a frame is drawn, from {!Frame} to
    the end of {!draw}, asks for the next frame: an animation asks again from
    each frame, and stops by not asking. A run begins with a frame requested. *)

val frame_dt : 'msg app -> float
(** [frame_dt app] is the time in seconds from the frame before the latest
    {!Frame} to that frame, measured between the times of the steps that
    returned them. It is [0.] until the run's second frame. An animation moves
    by it in each frame. One that stopped asking for frames sees the whole pause
    in the first frame it asks for again. *)

val draw : 'msg app -> ?primary_required_rows:int -> (Grid.t -> unit) -> unit
(** [draw app f] draws the frame {!next} returned: it lays the frame out at
    {!size}, calls [f] with its grid, cleared, and presents it: the frame is
    diffed against the one presented before and the difference written in one
    write, after the terminal operations of the layout; nothing when nothing
    changed.

    In [`Primary] mode the queued static writes go to the transcript first, and
    the live region grows to [primary_required_rows] rows when it has fewer, at
    most the terminal's height, by scrolling the rows above it into the
    terminal's scrollback. The frame is as tall as the live region, so it shows
    the rows it asked for. [primary_required_rows] is ignored in [`Alt] mode.

    A frame drawn across a {!release} is not presented: the next one is drawn
    whole.

    Raises [Invalid_argument] if no {!Frame} is being drawn. *)

val current_grid : 'msg app -> Grid.t
(** [current_grid app] is the presented grid: the committed terminal state as of
    the most recent frame. Blank before the first frame. Do not mutate. *)

val hits : 'msg app -> Screen.Hit_grid.t
(** [hits app] is the hit grid of the frame being drawn. *)

val screen : 'msg app -> Screen.t
(** [screen app] is the runtime's screen. The runtime owns its sizing and
    presentation: {!draw} clears and resizes it before calling its function, and
    diffs and swaps it after. Embedding renderers build frames into it (via
    {!Screen.build}) from the function given to {!draw}, so the runtime presents
    their output with a single diff. Do not call {!Screen.render} on it
    yourself. *)

(** {1:threads Threads} *)

val post : 'msg app -> 'msg -> unit
(** [post app msg] has {!next} return [Posted msg] in a later turn. It is the
    one function of this module that any thread, domain or fiber may call; it
    never blocks and never fails. Posts come in the order they were made: two
    made by one thread in program order. The queue has no bound: a producer that
    posts faster than the loop handles what it posts grows memory, and
    collecting that memory slows the loop. After the run ends, [post] does
    nothing. *)

(** {1:lifecycle Giving the terminal away}

    A run gives its terminal back on every way out: when the function given to
    {!run} returns or raises, for a program run in its place ({!release}), on a
    signal that stops or ends the process ({{!signals}Signals}), and on the
    process's exit path. If the process exits while a run holds its terminal, as
    when that function calls [exit], or when the program's main domain returns
    while a run on another domain is still taking its terminal or giving it
    back, the exit path restores the terminal as an abort does: the bytes that
    reset the modes the run set are written and the terminal's saved settings
    put back. A terminal given back already is left alone. *)

val release : 'msg app -> (unit -> 'a) -> 'a
(** [release app f] gives the terminal back for [f ()], as when the run ends,
    and takes it again when [f] returns or raises, as when the run began: [f]
    can run another program in the terminal, as POSIX's [system()] does for its
    caller. The loop does not run during [f]: nothing reads the terminal, and
    timers and posts wait for [f] to return.

    Before [f]: queued transcript rows are written; the modes are reset; if a
    reply is owed, the runtime waits at most max(500 ms, four round trips) for
    the terminal to answer, so that no reply reaches [f]; the primary screen is
    restored with the cursor shown (in [`Primary] mode the live region is erased
    and the cursor waits at its start, below the transcript; in [`Alt] mode the
    alternate screen is left); cooked mode is set once the output has been sent,
    so input typed for [f] reaches it. Every event decoded and not yet returned
    by {!next}, and every event decoded while waiting for the answer, is
    dropped, but for {!Reply}, and a paste in progress is cut: [f] cannot have
    them. A turn decodes what it reads before returning any of it, so this
    includes keys read in the same turn as the step that called [release],
    whatever that step was: an event, a timer or a post. Keys read in one turn
    cannot be ordered against a timer or a post of that turn, and those that
    follow an event were typed after it, for [f].

    During [f], SIGTSTP, SIGTERM and SIGHUP have their default actions, so the
    terminal's suspend character stops [f]'s program and the runtime with it;
    SIGINT and SIGQUIT, which the terminal sends to [f]'s program as well, are
    caught and discarded when [f] returns.

    After [f]: input left unread is discarded, the terminal's settings are kept
    as the ones to restore, the size is read, the screen and modes are set
    again, and the next frame is drawn whole, in [`Primary] mode below what [f]
    wrote, once a cursor report has said where that ends. A frame being drawn
    when [release] is called is not presented. *)

val stop_job : 'msg app -> unit
(** [stop_job app] stops the job as the terminal's suspend character does in
    cooked mode: it {!release}s the terminal around sending SIGTSTP to the
    process's group, and returns once the job is continued. Raw mode turns the
    suspend character into a key: an application that wants Ctrl+Z to suspend
    calls [stop_job] on it. Nothing happens if SIGTSTP was ignored when the run
    began, as nothing could continue the job, or if the run's terminal is not
    the process's controlling terminal ({!Tty.is_controlling}): the job is not
    that terminal's, and stopping it would stop every other session. *)

(** {1:signals Signals}

    A run holds its terminal's signals as {!Tty.Signal} says every holder of a
    terminal in the process does: a run on a terminal, whichever, takes the
    signals that end the process, SIGINT, SIGTERM, SIGHUP, SIGQUIT and SIGABRT,
    and a run on the process's controlling terminal ({!Tty.is_controlling}) also
    SIGTSTP, SIGWINCH and SIGCONT. All but SIGWINCH and SIGCONT are taken only
    if their action is the default when the run begins, as X/Open Curses allows:
    an application that installed its own action keeps it. The handlers record
    the signal for each run it concerns and wake it (its wake is among {!fds}),
    and the loop acts on it in its next turn. Runs side by side take a signal
    together, and the action it had comes back when the last of them ends.

    - SIGINT, SIGTERM, SIGHUP, SIGQUIT end the process, and every run with it.
      The run whose loop acts on the signal first gives its terminal back, as
      when it ends; every other run's terminal is restored as an abort restores
      it (below), its reset bytes written and its saved settings put back; then
      the signal is raised again with its default action, so the process ends
      with that signal's status and {!run} does not return. An application that
      wants {!run} to return installs its own handler first. A write the
      terminal is not taking is interrupted by them, not resumed.
    - SIGTSTP releases the terminal and stops the process, as {!stop_job} does
      for the whole job; on continuing, the terminal is taken again.
    - SIGCONT the run did not cause, as when the shell continued the job after a
      stop from elsewhere, sets the terminal again: the shell has put back its
      own settings. The size is read and the next frame drawn whole.
    - SIGWINCH has the size read when the next frame is drawn.
    - SIGABRT, from [abort()], returns to no point where OCaml can run, so its
      handler restores every run's terminal itself: it writes the bytes that
      reset the modes the run set to the run's terminal and puts back its saved
      settings, and the process aborts. *)

(** {1:queries Terminal queries} *)

val abort_paste : 'msg app -> unit
(** [abort_paste app] ends the bracketed paste in progress, if any, with
    [Paste_end `Cut], for an application that will wait no longer for its end
    marker: a terminal that stalls mid-paste stops the data, and the paste goes
    on until its end marker or the end of the input. The bytes after it are
    decoded as input. *)

val query : 'msg app -> owed:Input.Decoder.owed -> string -> unit
(** [query app ~owed q] sends the queries [q] to the terminal, followed by a
    primary device attributes query (DA1), and delivers their replies as
    {!Reply} events, ending with the DA1 reply. [owed] says which of the replies
    [q] asks for are cursor position reports or control strings, as
    {!Input.Decoder.expect} counts them: the decoder must know that they are
    coming, or a string reply such as XTVERSION's [DCS > | text ST] decodes as
    Alt+P and typed text, and a cursor report [CSI 1 ; m R] as F3. A terminal
    answers in order and answers DA1, so its DA1 reply says that [q] has been
    answered, or never will be, as for the runtime's own queries. Every DA1
    reply is alike, so one batch that ends with DA1 is in flight at a time: [q]
    is sent once the terminal has answered the batches sent before it, or they
    have been given up.

    A query sent any other way, such as with {!Terminal.send}, is not owed: its
    reply is never a {!Reply}, and one that is a control string, or a cursor
    report from the first row, reaches the application as input. The replies
    delivered are those the terminal sends after it has answered the queries
    sent before [q], so they may include the answer to a query the runtime
    itself sent just before; a report the terminal sends on its own that is an
    application fact is its event ({!Color_scheme}). Nothing is sent when no
    reply could come: when the input is not a terminal in raw mode, or it has
    ended. *)

val mode : 'msg app -> mode
(** [mode app] is the presentation mode of the run. *)

val size : 'msg app -> int * int
(** [size app] is the current dynamic-region dimensions as [(cols, rows)]. In
    [`Primary] mode this is the live viewport, not the full terminal. *)

val full_size : 'msg app -> int * int
(** [full_size app] is the full terminal dimensions [(cols, rows)], ignoring the
    primary-mode render offset. In [`Alt] mode this is identical to {!size}. *)

val effective_size : 'msg app -> int * int
(** [effective_size app] is the dynamic-region dimensions [(cols, rows)] that
    will be in effect once {!draw} flushes the pending static writes, before it
    grows the region to the rows a frame requires. In [`Alt] mode, when the
    static queue is empty, and so in the function given to {!draw}, this equals
    {!size}. *)

val pixel_resolution : 'msg app -> (int * int) option
(** [pixel_resolution app] is the last known pixel resolution as
    [(width, height)], or [None] if the terminal has not reported one. *)

val terminal : 'msg app -> Terminal.t
(** [terminal app] is the underlying terminal handle. *)

val capabilities : 'msg app -> Terminal.capabilities
(** [capabilities app] is the current terminal capabilities. *)

(** {1:static Static output}

    Static output is primary-screen transcript output placed before the live
    viewport. Insertion may move the live viewport downward until it reaches its
    minimum height. Once pinned, inserted rows push the rows above the viewport
    into the terminal's scrollback and leave the viewport its height. Transcript
    rows are never overwritten: they leave the screen only into the scrollback.
    These functions are ignored in [`Alt] mode. *)

val static_write :
  ?preserve_live_region:bool -> ?rows:int -> 'msg app -> Grid.t -> unit
(** [static_write ?preserve_live_region ?rows app grid] queues a snapshot of the
    first [rows] grid rows before the live viewport. [rows] defaults to the full
    grid height, including blank or background-only rows. The snapshot is
    serialized immediately; subsequent grid mutations do not change it. Each row
    replaces the complete terminal row and ends with CRLF. Rows never wrap if
    the terminal becomes narrower before the queued output is presented; excess
    cells overwrite the rightmost column, following terminal margin behavior.
    The rows go out with the next frame {!draw} lays out, which this asks for:
    one written while a frame is drawn goes out with the frame after it.

    Raises [Invalid_argument] if the grid is wider than the current terminal, or
    [rows] is outside the grid height. [rows = 0] queues nothing.

    When [preserve_live_region] is [true], the live region keeps its current
    height: the transcript, these rows included, scrolls up into terminal
    history to make room. Otherwise the rows take theirs from the live region,
    down to [min_tui_height]. *)

val static_replace :
  ?preserve_live_region:bool -> ?rows:int -> 'msg app -> Grid.t -> unit
(** [static_replace ?preserve_live_region ?rows app grid] queues an atomic
    replacement of all primary static content with a snapshot of [grid]. Row
    selection, snapshot ownership, and validation match {!static_write}.
    [rows = 0] clears the transcript without inserting rows. The replacement and
    next live frame are emitted in one transaction.

    {b Warning.} Replacement clears the terminal's entire scrollback, including
    shell output from before the application. Support for [CSI 3 J] varies
    across terminals. This function is ignored in [`Alt] mode. *)

val static_clear : 'msg app -> unit
(** [static_clear app] clears the primary screen immediately, discards pending
    static output, and resets the live viewport to the full terminal height. It
    is an immediate terminal reset rather than a queued frame operation. *)

(** {1:scroll Scroll optimisation} *)

val set_scroll_hint : 'msg app -> Screen.scroll_hint -> unit
(** [set_scroll_hint app hint] sets a scroll hint for the current frame.
    Consumed by the frame being drawn, or the next. The runtime currently
    applies hints only in [`Alt] mode. In [`Primary] mode hints are discarded
    because primary static output already owns the frame's temporary
    scroll-region protocol.

    Use this when a scrollable container's viewport shifts: the renderer applies
    DECSTBM hardware scroll so only the newly-revealed edge rows need cell-level
    diffing. Without the hint, scrolling rewrites every row in the viewport. *)

(** {1:cursor Cursor control} *)

val set_cursor : ?visible:bool -> ?style:Ansi.cursor_style -> 'msg app -> unit
(** [set_cursor ?visible ?style app] updates cursor visibility and/or style,
    shown from the next frame on. A run begins with the cursor hidden; an
    application that wants it shown from the first frame sets it before:
    {[
    Matrix.run @@ fun app ->
    Matrix.set_cursor app ~visible:true;
    loop app
    ]} *)

val set_cursor_style :
  'msg app -> style:Ansi.cursor_style -> blinking:bool -> unit
(** [set_cursor_style app ~style ~blinking] sets cursor shape and blink. *)

val set_cursor_position : 'msg app -> row:int -> col:int -> unit
(** [set_cursor_position app ~row ~col] moves the cursor to 1-based coordinates
    within the dynamic render region. *)

val set_cursor_color : 'msg app -> r:float -> g:float -> b:float -> unit
(** [set_cursor_color app ~r ~g ~b] sets the cursor color. Components are in
    \[[0.];[1.]\]. *)

(** {1:diagnostics Diagnostics}

    What a frame shows about the run is drawn by the application, which owns the
    frame. {!Debug_overlay} draws the frames' timing and the collector's
    statistics in a corner of a frame, and {!dump_frame} writes a frame to a
    file. An application that dumps every thirtieth frame counts them:
    {[
    let frames = ref 0 in
    let on_frame app view =
      incr frames;
      Matrix.draw app (fun grid ->
          view grid;
          if !frames mod 30 = 0 then Matrix.dump_frame ~dir:"dumps" app)
    in
    on_frame
    ]} *)

module Debug_overlay = Debug_overlay

val dump_frame :
  ?hits:bool -> ?dir:string -> ?pattern:string -> 'msg app -> unit
(** [dump_frame app] writes the frame being drawn, as the function given to
    {!draw} has drawn it so far, to a new file of [dir] (default ["frames"],
    made if missing), named [pattern] (default ["frame_"]) followed by a number
    that grows with each dump of the process and [.ansi]. The file holds the
    frame's cells as ANSI text and, if [hits] (default [false]), its hit grid.

    Raises [Invalid_argument] if no {!Frame} is being drawn, [Failure] if [dir]
    cannot be made, and [Sys_error] if the file cannot be written. *)

(**/**)

(* The device a run drives, for the runtime's own tests, which script its
   clock, its input and what the terminal answers. [run] is [run_on] the
   device of its descriptors. A device's [wait] returns once the input is
   readable (when [reading]), once [wake] was called since the last wait, or
   once [now ()] reaches [until], and says whether the input is readable; a
   wait until a time already past only looks. [read_input] returns [0] at the
   end of the input and [-1] when nothing is there. [terminal_size] is [None]
   for an output with no window. [fds] are the descriptors whose readability
   ends a wait. *)

module Backend : sig
  type signal = Winch | Cont | Tstp | Terminate of int

  type t = {
    now : unit -> int;
    wait : until:int option -> reading:bool -> bool;
    wake : unit -> unit;
    fds : reading:bool -> Unix.file_descr list;
    take_signals : unit -> signal list;
    read_input : bytes -> int -> int -> int;
    write_output : bytes -> int -> int -> unit;
    terminal_size : unit -> (int * int) option;
    set_raw_mode : bool -> unit;
    flush_input : unit -> unit;
    input_is_tty : bool;
    output_is_tty : bool;
    controlling : bool;
    on_fatal : string -> unit;
    cleanup : unit -> unit;
    env : Terminal.Caps.env;
  }

  val unix :
    ?input:Unix.file_descr ->
    ?output:Unix.file_descr ->
    ?env:Terminal.Caps.env ->
    unit ->
    t
end

val run_on :
  Backend.t -> ?stop:Stop.t -> ?config:config -> ('msg app -> 'a) -> 'a

(* A terminal emulated in the program's own process, for [Matrix_test], which
   a run takes through its configuration ([on_device]), and then from [run]
   with no descriptor and no environment of its own. Its writes are applied
   before [write] returns and its input is queued whole, so a run on it
   differs from one on a terminal in three ways:
   - a frame asked for outside a frame (by input, a timer, a post or the
     runtime itself, as after a resize) is drawn in the turn that asked, and
     one asked for from a Frame step until the next step, an animation's,
     waits its interval, whose grid a frame drawn early does not move;
   - input is decoded whole once nothing follows it, as the terminal has
     finished writing;
   - the run takes no signal of the process: [resized] is its window signal.
   [wait ~until] says whether input is queued. With none queued and [until]
   later than [read_clock ()], the terminal is silent until then, and the device
   moves its clock there, as a terminal that never answers leaves the run's
   wait for an answer. [read] returns [-1] when nothing is queued, [window]
   [None] for an output with no window, and [frame] is called as each frame
   begins. [next] does not wait on a device: when the run has nothing to do
   it calls [idle ~until ~stepped], [until] the run's deadline and [stepped]
   whether [next] returned a step since its last call, which returns once
   input is queued or the clock has moved, and says whether input is
   queued. *)

type device = {
  read_clock : unit -> int;
  wait : until:int option -> bool;
  read : bytes -> int -> int -> int;
  write : bytes -> int -> int -> unit;
  window : unit -> (int * int) option;
  resized : unit -> bool;
  set_raw : bool -> unit;
  flush : unit -> unit;
  tty : bool;
  env : Terminal.Caps.env;
  frame : unit -> unit;
  idle : until:int option -> stepped:bool -> bool;
}

val on_device : device -> config -> config

(**/**)
