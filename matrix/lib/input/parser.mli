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

val feed :
  t ->
  bytes ->
  int ->
  int ->
  now:float ->
  on_event:(Event.t -> unit) ->
  on_caps:(Event.Caps.event -> unit) ->
  unit
(** [feed parser buf off len ~now ~on_event ~on_caps] consumes [len] bytes from
    [buf] starting at [off] and invokes callbacks for parsed events.

    Each parsed user event is passed to [on_event]; each capability response is
    passed to [on_caps]. Incomplete escape sequences are buffered internally and
    combined with future calls. [now] is the current timestamp in seconds since
    the epoch, used for scheduling flush deadlines. *)

val drain :
  t ->
  now:float ->
  on_event:(Event.t -> unit) ->
  on_caps:(Event.Caps.event -> unit) ->
  unit
(** [drain parser ~now ~on_event ~on_caps] emits any pending escape sequences
    that have timed out.

    Lone [Escape] keys only appear after this drain when the terminal split a
    modifier sequence across reads. Call this after {!deadline} has passed. *)

val deadline : t -> float option
(** [deadline parser] returns the absolute timestamp when the next drain will
    fire, or [None] when no drain is scheduled. Useful for driving timers in the
    runtime. *)

val pending : t -> bytes
(** [pending parser] returns buffered bytes for incomplete sequences. Helpful
    for debugging stuck input or tests. *)

val reset : t -> unit
(** [reset parser] clears all parser state, dropping any buffered partial
    sequences and capability tracking. *)
