(** Decoding what a terminal sends.

    {1:model What the input is}

    One byte stream carries what the user does at the keyboard and the mouse,
    the reports the program enabled (focus changes, pasted text, resizes,
    color-scheme changes), and the terminal's replies to its queries. Control
    sequences and control strings have the syntax of ECMA-48, which
    {!Ansi.Parser} reads; this module owns only what output never has: an ESC
    before a key for Alt, the X10 mouse report's raw bytes, rxvt's [CSI n $],
    the Linux console's [CSI \[ A], SS3 with parameters, bytes that are not
    UTF-8, and the bracketed paste's content, which has no syntax at all.

    {1:esc ESC, and what silence decides}

    ESC begins every sequence and string, and is also the Escape key; ESC before
    a key is Alt with that key. The bytes decide whenever they can:
    - ESC before a character, a control or DEL is Alt and that key, at once.
    - [ESC \[] before a byte 0x20..0x7E begins a control sequence. [ESC O]
      begins an SS3 sequence before a digit, [;], or a byte an SS3 key ends with
      (SP, [A]..[F], [H], [I], [M], [P]..[S], [X], [a]..[e], [j]..[y]).
    - [ESC P] before a byte 0x20..0x3F, [ESC \]] before a digit, [L] or [l], and
      [ESC _] before [G] begin a control string, but only while a string reply
      is owed ({!section-owed}). Otherwise they are Alt keys at once.
    - Any other byte after an introducer makes Alt and the introducer, then that
      byte. After ESC ESC it makes Alt+Escape and the introducer, as ESC ESC
      before any byte that begins nothing does, whatever is owed: what is owed
      decides only whether a string can begin.

    The bytes cannot decide whether ESC, ESC ESC, or ESC before an introducer
    was a whole keystroke when the terminal stops writing: it may not have
    finished. {!pending} says what is held; the caller, who knows the time,
    calls {!idle} once the terminal has been silent long enough, and the held
    bytes become keys: Escape, Alt+Escape, Alt and the introducer, or Alt+Escape
    and the introducer. With no {!idle} in between, the events do not depend on
    how the bytes were split across calls to {!feed}, once adjacent
    {!Paste_data} are joined.

    {1:replies Replies are never keys}

    Once a control sequence has begun, its bytes never become keys. A sequence
    is a key, a report or a reply by its marker, intermediates, final byte and
    parameters ({!Keystroke}, {!Mouse_report}, {!Reply}); one whose values fall
    outside the form its reference fixes is a {!Reply.Csi}, never a key.
    Silence, the end of the input, or a byte that cannot continue a sequence (a
    C0 control, DEL, ESC or a non-ASCII byte) cuts it: it is dropped as a
    {!Reply.Cut}, and the byte that cut it is decoded after it. ECMA-48 would
    perform a C0 control inside a sequence and ignore DEL; input does not,
    because a terminal writes a sequence whole, so a byte inside it that cannot
    belong to it is the user's next key.

    A control string begins only while a string reply is owed: every string a
    terminal sends answers a query, so an unsolicited [ESC \]] is Alt+\], and
    what the user types after it stays typing. Once begun, a string is never cut
    by silence. It ends at its terminator (ST, or BEL for an OSC), at CAN or
    SUB, at an ESC that does not begin ST, or at the end of the input. Its
    content is then a reply or a {!Reply.Cut}, never keys, however long the
    terminal stalls.

    xterm sends a modified F3 as [CSI 1 ; m R], which is also a cursor position
    report: "it is possible for this sequence to be sent by a function key"
    (xterm's Control Sequences, "Device Status Report"). While a cursor report
    is owed it is a {!Reply.Cursor}, otherwise F3.

    {1:owed What is owed}

    The caller sends the queries, so it knows which replies may come; each query
    says what it is owed. {!expect} adds to the counts, and {!settle} takes back
    what a batch of queries was owed once it is answered or will not be: when
    the DA1 that ends the batch is answered, or when the caller gives up on an
    answer. The decoder only reads the counts.

    The counts decide how a reply decodes, so a reply that arrives after its
    batch was settled is input: its [ESC P], [ESC \]] or [ESC _] is an Alt key
    and the rest is typed, and its [CSI 1 ; m R] is F3. Replies are never keys
    while they are owed; a caller keeps a batch owed until its answer, and gives
    up on one only after longer than a terminal takes to answer.

    {1:paste Paste}

    With bracketed paste on (mode 2004), pasted text arrives between
    [ESC \[ 200 ~] and [ESC \[ 201 ~], "so that the program can differentiate
    pasted text from typed-in text" (xterm's Control Sequences, "Bracketed Paste
    Mode"). The content is passed on exactly, in pieces as it arrives, with
    nothing decoded: an ESC, a second start marker, or a lookalike such as
    [ESC \[ 201 x] is content. The paste ends at the first end marker, at the
    end of the input, or when the caller ends it with {!cut_paste}. Silence
    never ends it, so a stalled paste is never typed. No paste is held: the
    decoder keeps at most the 5 bytes of an end marker not yet complete.

    {1:invalid Bytes that are not UTF-8}

    Text is UTF-8. A byte that cannot begin or continue a character, or a
    character that silence or the end of the input cuts, is {!Invalid}, one
    ill-formed subsequence at a time (Unicode §3.9, maximal subparts): a program
    "shall not interpret such sequences as characters" (Unicode C10). A terminal
    in an 8-bit locale sends Alt as a byte with its high bit set; such a byte is
    {!Invalid} here, not Alt. *)

(** {1:events Events} *)

(** The type for what the input says. *)
type event =
  | Key of Keystroke.t  (** A key pressed, or auto-repeated. *)
  | Key_release of Keystroke.release  (** A key released. *)
  | Text of string
      (** Text the terminal reports without a key, such as text an input method
          composed (the kitty keyboard protocol's key code 0). *)
  | Mouse of Mouse_report.t
  | Focus_in  (** The terminal gained focus ([CSI I], mode 1004). *)
  | Focus_out  (** The terminal lost focus ([CSI O], mode 1004). *)
  | Paste_start  (** A bracketed paste begins. *)
  | Paste_data of string
      (** Content of the paste, exactly as sent, in pieces. *)
  | Paste_end of [ `Complete | `Cut ]
      (** The paste ends at its end marker ([`Complete]), or at the end of the
          input or {!cut_paste} ([`Cut]). *)
  | Invalid of string
      (** One ill-formed subsequence of UTF-8, its bytes as received. *)
  | Reply of Reply.t  (** A reply to a query, or a sequence that is no key. *)

val pp_event : Format.formatter -> event -> unit
(** [pp_event] formats an event for debugging. *)

(** {1:decoders Decoders} *)

type t
(** The type for decoders. Mutable and not thread-safe. *)

val create : unit -> t
(** [create ()] is a decoder at the start of a stream, owed nothing. *)

val feed : t -> bytes -> off:int -> len:int -> (event -> unit) -> unit
(** [feed d b ~off ~len f] decodes the [len] bytes of [b] from [off] and calls
    [f] on each event, in order. What the bytes leave unfinished continues with
    the next call.

    Raises [Invalid_argument] if [off] and [len] do not designate a valid range
    of [b]. *)

(** The type for what a decoder holds that silence would decide. *)
type pending =
  | Nothing
      (** Nothing: silence changes nothing, inside a string or a paste too. *)
  | Ambiguous
      (** ESC, ESC ESC, or ESC before an introducer: keys if the terminal has
          finished writing, the start of a sequence if it has not. *)
  | Sequence
      (** A control sequence, an X10 report or a UTF-8 character in progress: a
          terminal writes these whole, so silence means they were cut. *)

val pending : t -> pending
(** [pending d] is what [d] holds. *)

val idle : t -> (event -> unit) -> unit
(** [idle d f] tells [d] that the terminal has written nothing for as long as
    the caller waits for what {!pending} reports, and calls [f] on what that
    decides: held ESCs become keys, a sequence or character in progress is
    {!Reply.Cut} or {!Invalid}. Strings and pastes go on. Call it only after a
    wait in which nothing arrived: bytes already read are never silence. *)

val finish : t -> (event -> unit) -> unit
(** [finish d f] ends the input: {!idle}, then an open string is {!Reply.Cut}
    and an open paste ends with [Paste_end `Cut]. [d] is then at the start of a
    stream, as after {!reset}. *)

val cut_paste : t -> (event -> unit) -> unit
(** [cut_paste d f] ends the paste in progress, if any, with [Paste_end `Cut],
    for a program that will wait no longer for its end marker. The bytes that
    follow decode as input. *)

val reset : t -> unit
(** [reset d] drops what [d] holds of the stream, with no event, for a caller
    that stops reading and will start again, as around a suspend. The owed
    counts stay: they count queries already sent. *)

(** {1:counts Owed replies} *)

type owed = {
  cursor_reports : int;
      (** Cursor position reports owed: while any is, [CSI 1 ; m R] is a
          {!Reply.Cursor}, not F3. *)
  string_replies : int;
      (** Replies that are control strings owed (OSC, DCS or APC): while any is,
          [ESC \]], [ESC P] and [ESC _] may begin one. *)
}
(** The type for what queries are owed. *)

val expect : t -> owed -> unit
(** [expect d o] adds [o] to what [d] is owed, for queries just sent.

    Raises [Invalid_argument] if a count of [o] is negative. *)

val settle : t -> owed -> unit
(** [settle d o] takes [o] back from what [d] is owed, for queries that have
    been answered or never will be.

    Raises [Invalid_argument] if a count of [o] is negative or more than [d] is
    owed. *)
