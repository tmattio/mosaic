(** Incremental input parser that produces user-facing events and capability
    reports.

    Consumes bytes via the tokenizer and emits ({!Event.t}, {!Event.Caps.event})
    pairs. Maintains shared state for both streams. Not thread-safe. *)

type t
(** Opaque parser state.

    A single parser maintains the shared tokenizer/decoder used by both the
    user-facing stream ([Event.t]) and capability responses ([Event.Caps.event])
    so callers do not need to run two parsers over the same bytes. Not
    thread-safe. *)

val create : unit -> t
(** [create ()] returns a fresh parser with empty state. *)

val feed : t -> bytes -> int -> int -> Event.t list * Event.Caps.event list
(** [feed parser buf off len] consumes [len] bytes from [buf] starting at [off]
    and returns the parsed events.

    The first list contains user-visible {!Event.t} values (keys, mouse, paste,
    etc.). The second list contains capability responses (device attributes,
    mode reports, cursor position, vendor OSC, etc.). Incomplete escape
    sequences are buffered internally and combined with future calls. *)

val flush : ?now:float -> t -> Event.t list * Event.Caps.event list
(** [flush ?now parser] emits any pending escape sequence that timed out.

    Lone [Escape] keys only appear after this flush when the terminal split a
    modifier sequence across reads. [now] defaults to [Unix.gettimeofday ()] for
    deterministic testing. *)

val next_flush_deadline : t -> float option
(** [next_flush_deadline parser] returns the absolute timestamp when the next
    flush will fire, or [None] when no flush is scheduled. Useful for driving
    timers in the runtime. *)

val pending : t -> bytes
(** [pending parser] returns buffered bytes for incomplete sequences. Helpful
    for debugging stuck input or tests. *)

val reset : t -> unit
(** [reset parser] clears all parser state, dropping any buffered partial
    sequences and capability tracking. *)

val parse_single : string -> Event.t list * Event.Caps.event list
(** [parse_single s] parses a complete string using a temporary parser. Intended
    for tests and fixtures; does not handle split sequences. *)
