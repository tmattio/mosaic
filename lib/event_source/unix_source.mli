(** Unix-specific event source implementation.

    This module provides event reading for Unix-like systems using ANSI escape
    sequence parsing. Handles standard input through terminal file descriptors
    with proper timeout support.

    Uses Input.parser for incremental ANSI sequence parsing. Handles partial
    reads and buffering. EOF detection works correctly on Unix. Timeout accuracy
    depends on system timer resolution. *)

type t
(** [t] represents a Unix event source.

    Encapsulates terminal handle and parser state. Manages input buffering for
    efficiency. *)

val create : Terminal.t -> t
(** [create terminal] creates a Unix event source from a terminal.

    Initializes ANSI parser for the terminal's input. Terminal should be in raw
    mode for proper event parsing. Mouse events require terminal mouse mode
    enabled. *)

val read :
  t ->
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  timeout:float option ->
  [ `Event of Input.event | `Timeout | `Eof ]
(** [read t ~sw ~clock ~timeout] reads the next event from Unix terminal.

    Uses Eio for async I/O with timeout support. Parses ANSI escape sequences
    into events. Handles partial sequences across reads. Returns `Eof on input
    stream closure.

    @param sw Eio switch for resource management
    @param clock Eio clock for timeout handling
    @param timeout Maximum seconds to wait, None for blocking

    Implementation details:
    - Reads up to 1024 bytes per call for efficiency
    - Buffers incomplete escape sequences between reads
    - Properly detects EOF condition on Unix systems
    - Bracketed paste parsed from ANSI sequences *)
