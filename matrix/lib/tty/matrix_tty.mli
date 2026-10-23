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
