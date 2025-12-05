(** Incremental tokenizer for terminal input.

    The tokenizer matches {!Input}'s expectations by splitting the raw byte
    stream into either complete escape/control sequences or contiguous runs of
    non-escape bytes. Bracketed paste markers are detected eagerly so payload
    bytes are buffered until the closing marker.

    Tokens always preserve input order. [Sequence] tokens begin with ESC and
    contain exactly one complete OSC/CSI/DCS/SS3/etc. sequence. [Text] tokens
    never contain ESC and may hold arbitrary UTF-8 (validation is deferred to
    higher layers). The parser is incremental and not thread-safe; use one
    instance per input source. *)

type token =
  | Sequence of string
      (** [Sequence seq] is a complete escape/control sequence. Invariants:
          [seq.[0] = '\x1b'], start/end markers for bracketed paste are emitted
          verbatim, and the sequence never contains trailing bytes from the next
          token. *)
  | Text of string
      (** [Text run] is a maximal run of bytes that contains no ESC. The run may
          contain newline or multi-byte UTF-8 characters. *)
  | Paste of string
      (** [Paste payload] is the complete bracketed paste payload (markers
          stripped). It is emitted after the start marker and before the closing
          marker. *)

type parser
(** Incremental tokenizer state. A parser accumulates partial sequences between
    {!feed} calls and tracks whether a bracketed paste is currently open. *)

val create : unit -> parser
(** [create ()] returns a tokenizer with empty buffers and paste tracking
    disabled. *)

val feed : parser -> bytes -> int -> int -> token list
(** [feed parser bytes off len] ingests [len] bytes starting at [off] and
    returns all tokens that can be emitted with the available data.

    Token order matches the input order. Escape sequences are only emitted when
    the full sequence (including OSC/DCS terminators) has been received. Text
    tokens never cross escape boundaries. When a bracketed paste start marker is
    seen, subsequent bytes are buffered until the matching end marker, at which
    point the payload is returned as a single [Paste] token surrounded by
    [Sequence "\x1b\[200~"] and [Sequence "\x1b\[201~"].

    May return an empty list if the chunk ends in the middle of a sequence or
    inside a paste payload. The function updates internal buffers but never
    mutates [bytes]; the caller may reuse it after the call.

    @raise Invalid_argument if [off] and [len] describe a slice outside [bytes]
*)

val pending : parser -> bytes
(** [pending parser] returns a copy of the incomplete data buffered so far.

    The pending bytes include only ordinary escape/text state and exclude any
    bytes read inside an open bracketed paste (those stay hidden until the end
    marker). Use for diagnostics; the returned buffer should be treated as
    immutable. *)

val reset : parser -> unit
(** [reset parser] drops all buffered data, exits paste mode if necessary, and
    returns the tokenizer to its initial state. *)

val deadline : parser -> float option
(** [deadline parser] returns the absolute time (in seconds since the epoch) at
    which the tokenizer will flush its pending escape sequence, if any. [None]
    means no flush is scheduled. *)

val flush_expired : parser -> float -> token list
(** [flush_expired parser now] emits any pending partial sequence if its flush
    deadline is at or before [now]. Returns an empty list if nothing was
    flushed. *)
