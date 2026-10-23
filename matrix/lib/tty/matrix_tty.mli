(** The terminal device.

    What the operating system knows of a terminal and does to it: whether a
    descriptor is one, the size of its window, the settings of its line
    discipline, and a clock to time it by. The libraries above decide what to
    write to a terminal; this one only asks the system and sets what the system
    keeps.

    {1:device The device} *)

val is_tty : Unix.file_descr -> bool
(** [is_tty fd] is [true] iff [fd] is a terminal (isatty(3)). *)

val is_controlling : Unix.file_descr -> bool
(** [is_controlling fd] is [true] iff [fd] is the process's controlling
    terminal: the terminal whose job the process runs in, which alone has a
    foreground process group the process can read (tcgetpgrp(3)). A
    pseudo-terminal the process opened, or a terminal it serves, is not. *)

val window : Unix.file_descr -> (int * int) option
(** [window fd] is the size of [fd]'s window as [(cols, rows)], as the system
    reports it ([TIOCGWINSZ]; the console screen buffer's window on Windows), or
    [None] when [fd] is not a terminal or reports no size, as a pseudo-terminal
    whose size was never set reports zero. It never guesses: a caller that needs
    a size where there is none chooses its own. *)

val now : unit -> int
(** [now ()] is monotonic time in nanoseconds, from an arbitrary origin: it
    never goes back, and setting the system's time does not move it
    ([CLOCK_MONOTONIC]). On Linux it stops while the system sleeps, so a
    deadline is that much later on the wall clock; on macOS it does not stop. *)

(** {1:settings Settings}

    A terminal's line discipline edits what is typed before a program reads it:
    it echoes, collects a line until Enter, and turns the interrupt, quit and
    suspend characters into signals. A program that reads every key as it is
    typed turns all of that off, and puts it back as it found it. *)

type settings
(** The type for a terminal's settings: what [tcgetattr(3)] gives, the [IEXTEN]
    local flag included, which [Unix.terminal_io] cannot express. *)

val raw : Unix.file_descr -> settings
(** [raw fd] puts [fd] in raw mode and returns its settings before: no echo, no
    line editing, no signal characters (Ctrl+C is a byte, not SIGINT), no
    implementation-defined input specials ([IEXTEN] off: a raw terminal that
    left it on would never see ^O and ^V, which the kernel takes), no flow
    control, CR not turned into LF, and a read returns once one byte is there.
    The change takes effect at once. File status flags such as [O_NONBLOCK] are
    the caller's and stay as they are.

    Raises [Unix.Unix_error] if [fd] is not a terminal. *)

val restore : Unix.file_descr -> settings -> unit
(** [restore fd s] sets [fd]'s settings to [s] once the output written to [fd]
    has been sent, keeping the input not yet read for whoever reads next
    ([TCSADRAIN]).

    Raises [Unix.Unix_error] if [fd] is not a terminal. *)

val flush_input : Unix.file_descr -> unit
(** [flush_input fd] discards the input [fd] received and nobody read
    ([tcflush], [TCIFLUSH]). Nothing happens if [fd] is not a terminal. *)

val enable_vt : Unix.file_descr -> unit
(** [enable_vt fd] has a Windows console interpret the control functions written
    to it as a VT100 does, and take its output as UTF-8. Nothing happens on
    other systems, or if [fd] is not a console. *)

(** {1:signals Signals} *)

(** The signals a terminal sends the process, shared by everything in it that
    holds a terminal.

    A program that changes a terminal's settings or modes must put them back
    when a signal ends or stops the process, or the shell gets a terminal that
    echoes nothing, reports the mouse or shows no cursor. A process has one
    action per signal, which the program, the libraries it links and every
    holder of a terminal in it share: so the actions are taken here, once, by
    one policy, and every holder of a terminal is told of the signals that
    concern it.

    {2:policy The policy}

    - SIGINT, SIGTERM, SIGHUP and SIGQUIT end the process. They concern every
      holder, on any terminal: ending the process is the process's affair. A
      system call they interrupt is not resumed, so a write the terminal is not
      taking returns and the holder can give its terminal back.
    - SIGABRT, from [abort()], returns to no point where OCaml can run: its
      handler restores every holder's terminal itself, as {!restore_all} does,
      and the process aborts.
    - SIGTSTP, SIGWINCH and SIGCONT concern the holders of the controlling
      terminal ({!is_controlling}) alone: its job is the one the shell stops and
      continues, and its window the one that resizes it. A terminal the process
      serves, such as a pseudo-terminal it opened, reports its size itself.

    SIGINT, SIGTERM, SIGHUP, SIGQUIT, SIGABRT and SIGTSTP are taken only when
    their action is the default, as X/Open Curses allows: a program that
    installed its own action keeps it, and its handler is its own to write.
    SIGWINCH and SIGCONT are taken whatever their action, and a handler
    installed before is still called. The system calls that SIGTSTP, SIGWINCH,
    SIGCONT and SIGABRT interrupt are resumed ([SA_RESTART]): a resize never
    cuts a write. A write that a signal cuts all the same, after some of its
    bytes went, goes on from where it stopped ({!write}).

    Holders side by side take a signal together: the first installs the handler,
    and the action it replaced comes back when the last gives it back
    ({!give_back}). The handlers are C: they record the signal for each holder
    it concerns and write one byte to its wake pipe ({!wake_fd}), which wakes a
    holder waiting in [select(2)] whenever the signal lands. *)
module Signal : sig
  (** {1:actions Actions} *)

  val signals : controlling:bool -> int list
  (** [signals ~controlling] is the signals a holder of a terminal takes, by
      their OCaml numbers: SIGABRT, SIGINT, SIGTERM, SIGHUP and SIGQUIT, and,
      when its terminal is the controlling one, SIGWINCH, SIGCONT and SIGTSTP
      first. *)

  val take : int -> bool
  (** [take s] takes [s] as the {{!policy}policy} says and is [true] iff it did:
      [false] when the program has an action of its own for [s] and the policy
      leaves it to it.

      Raises [Invalid_argument] if [s] is none of {!signals}'s. *)

  val give_back : int -> unit
  (** [give_back s] gives back a [take s] that returned [true]: the action [s]
      had before comes back once every holder that took it gave it back. *)

  val ignored : int -> bool
  (** [ignored s] is [true] iff the action of [s] is to ignore it. Nothing could
      continue a job whose SIGTSTP is ignored, if it stopped. *)

  val raise_default : int -> unit
  (** [raise_default s] raises [s] with its default action, whoever took it: the
      process ends, or stops, as it would have with no handler. Once a stopped
      process is continued it returns, and [s]'s action is the default until it
      is taken again. *)

  val stop_group : unit -> unit
  (** [stop_group ()] sends SIGTSTP to the process's group, as the terminal's
      suspend character does: where its action is the default, the job stops,
      the process with it, and [stop_group] returns once it is continued. *)

  (** {1:holders Holders} *)

  type holder
  (** The type for a holder of a terminal: what the handlers know of it. *)

  val hold : controlling:bool -> holder
  (** [hold ~controlling] registers a holder of a terminal, the controlling one
      if [controlling], with a wake pipe of its own. It takes no action: see
      {!take}.

      Raises [Unix.Unix_error] if the pipe cannot be made, and [Failure] on
      Windows, which has no such pipe. *)

  val release : holder -> unit
  (** [release h] ends [h]: the handlers forget it, and its terminal is no
      longer restored by an abort. *)

  val wake_fd : holder -> Unix.file_descr
  (** [wake_fd h] is the read end of [h]'s wake pipe, readable once a signal
      that concerns [h] was received, or {!wake} was called, since it was last
      read empty. It is non-blocking. *)

  val wake : holder -> unit
  (** [wake h] makes {!wake_fd} readable, from any thread or domain. *)

  val received : holder -> int -> bool
  (** [received h s] is [true] iff [s] was received for [h] since the last
      [received h s]. *)

  val terminating : holder -> bool
  (** [terminating h] is [true] iff a signal that ends the process was received
      for [h] and not yet taken with {!received}. *)

  val keep_settings :
    holder -> input:Unix.file_descr -> output:Unix.file_descr -> unit
  (** [keep_settings h ~input ~output] saves the settings [input] has now,
      restored if the process ends where no OCaml code runs, with [h]'s reset
      bytes ({!set_reset}) written to [output]. *)

  val forget_settings : holder -> unit
  (** [forget_settings h] drops the settings {!keep_settings} saved: they were
      put back. *)

  val set_reset : holder -> string -> unit
  (** [set_reset h bytes] has [bytes] written to [h]'s output if the process
      ends where no OCaml code runs: the bytes that reset the modes [h] set, of
      any length. [""] once [h] gave the terminal back. The bytes replace the
      previous ones whole: a signal reads one or the other, never a mix.

      Bytes that describe what a write leaves on the terminal are set once that
      write has returned, not before: a signal in between would reset for a
      screen the terminal does not show. *)

  val restore_all : unit -> unit
  (** [restore_all ()] restores every holder's terminal as an abort does: it
      writes each holder's reset bytes to its output and puts back the settings
      it kept. A holder that ends the process on a termination signal gives its
      own terminal back, then calls it for the others, which no OCaml code will
      run for. *)

  val write : holder -> Unix.file_descr -> bytes -> int -> int -> unit
  (** [write h fd buf off len] writes the [len] bytes of [buf] at [off] to [fd]
      and returns once they are written, going on from where a write stopped
      when a signal cut it, and waiting for [fd] to take more when it is
      non-blocking and full. A signal that ends the process cuts it for good:
      once {!terminating}, it returns with the rest unwritten.

      Raises [Unix.Unix_error] on another error. *)
end
