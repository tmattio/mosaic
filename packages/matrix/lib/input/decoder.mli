(** Decoders, which read the bytes a terminal sends into events.

    One byte stream carries what the user does at the keyboard and the mouse,
    the reports the program enabled (focus changes, pasted text, resizes,
    color-scheme changes), and the terminal's replies to the program's queries.
    A decoder reads it into {e events} ({!type-event}), in order. Control
    sequences and control strings have the syntax of ECMA-48, which
    {!Ansi.Parser} reads. This module reads the forms that only input has, which
    are an ESC before a key for Alt, the X10 mouse report's raw bytes, rxvt's
    [CSI n $], the Linux console's [CSI \[ A], SS3 with parameters, bytes that
    are not UTF-8, and the content of a bracketed paste, which has no syntax.

    A decoder has no clock. Some bytes form a whole keystroke or the start of a
    sequence depending on whether more follow, so the decoder {e holds} them
    ({!val-pending}), and the caller, who knows the time, says when the terminal
    has been {e silent} long enough ({!idle}). The caller also sends the
    queries, so it tells the decoder which replies it is {e owed}
    ({!section-owed}), and what is owed decides how some bytes decode. A
    sequence or string that does not finish is {e cut}, and its bytes are
    dropped as one {!Cut} event. With no {!idle} in between, the events do not
    depend on how the bytes were split across calls to {!feed}, once adjacent
    {!Paste_data} are joined.

    The life cycle of a decoder goes like this:
    + {!create} a decoder for one input source.
    + Before sending queries, {!expect} the replies they are owed.
    + {!feed} the bytes of each read as they arrive.
    + After a wait in which nothing arrived while {!val-pending} is other than
      [Nothing], call {!idle}.
    + {!settle} a batch of queries once it is answered or the caller gives up on
      it.
    + {!reset} around a suspend, and {!finish} at the end of the input.

    Unless a function's doc says otherwise, a function that takes a callback [f]
    calls it on each event in order before it returns, and no function blocks or
    does I/O. [f] may call {!expect}, {!settle} and {!val-pending} on the
    decoder that calls it, and the bytes after the event then decode with the
    new counts, as when the reply that ends a batch of queries settles it. A
    call from [f] to {!feed}, {!idle}, {!finish}, {!cut_paste} or {!reset} on
    that decoder raises [Invalid_argument], since [f] runs in the middle of
    decoding. If [f] raises, the exception passes through the function that
    called [f], which first drops what the decoder holds of the stream, as
    {!reset} does. What it held from earlier calls is dropped with the bytes of
    that call it had not yet decoded. The decoder is then usable again, at the
    start of a stream and owed what it was owed.

    This loop reads the terminal's input [fd] and passes each event to [handle].
    It waits with a timeout only while the decoder holds bytes, so {!idle} runs
    only after a wait in which nothing arrived.
    {[
    let rec loop d fd buf handle =
      let timeout = if Decoder.pending d = Decoder.Nothing then -1. else 0.05 in
      match Unix.select [ fd ] [] [] timeout with
      | [], _, _ ->
          Decoder.idle d handle;
          loop d fd buf handle
      | _ ->
          let n = Unix.read fd buf 0 (Bytes.length buf) in
          if n = 0 then Decoder.finish d handle
          else (
            Decoder.feed d buf ~off:0 ~len:n handle;
            loop d fd buf handle)
    ]} *)

(** {1:events Events} *)

(** The type for what the input says. *)
type event =
  | Key of Keystroke.t  (** A key pressed, or auto-repeated. *)
  | Key_release of Keystroke.release  (** A key released. *)
  | Text of string
      (** Text the terminal reports without a key, such as text an input method
          composed (the kitty keyboard protocol's key code 0). *)
  | Mouse of Mouse_report.t  (** A mouse report. *)
  | Focus_in  (** The terminal gained focus ([CSI I], mode 1004). *)
  | Focus_out  (** The terminal lost focus ([CSI O], mode 1004). *)
  | Paste_start  (** A bracketed paste begins ({!section-paste}). *)
  | Paste_data of string
      (** Content of the paste, exactly as sent, in pieces. *)
  | Paste_end of [ `Complete | `Cut ]
      (** The paste ends at its end marker ([`Complete]), or at the end of the
          input or {!cut_paste} ([`Cut]). *)
  | Invalid of string
      (** One ill-formed subsequence of UTF-8, its bytes as received
          ({!section-text}). *)
  | Reply of Reply.t
      (** A reply to a query, or a sequence kept as it was sent ({!Reply.Csi},
          {!Reply.Ss3}). *)
  | Cut of [ `Sequence | `String | `Mouse ]
      (** A control sequence, a control string or an X10 mouse report that did
          not finish. Silence, the end of the input, or a byte that cannot
          continue it cuts it ({!section-sequences}, {!section-strings}). Its
          content is in error (ECMA-48 §8.3.6, CAN: "this data shall be
          ignored"), and the event carries none of it. *)
  | Too_long of [ `Dcs | `Osc | `Apc ]
      (** A control string over the memory budget ({!section-strings}), read to
          its end and dropped whole. *)

val pp_event : Format.formatter -> event -> unit
(** [pp_event ppf e] formats [e] for debugging. *)

val encode_focus : bool -> string
(** [encode_focus focused] is the report a terminal with focus reporting on
    (mode 1004) sends when it gains focus ([true], [CSI I]) or loses it
    ([false], [CSI O]). *)

(** {1:decoders Decoders} *)

type t
(** The type for decoders. A decoder is mutable and must be used by one thread
    at a time, so each input source has its own. *)

val create : unit -> t
(** [create ()] is a decoder at the start of a stream, owed nothing. *)

val feed : t -> bytes -> off:int -> len:int -> (event -> unit) -> unit
(** [feed d b ~off ~len f] decodes the [len] bytes of [b] from [off] and calls
    [f] on each event, in order. What the bytes leave unfinished continues with
    the next call.

    Raises [Invalid_argument] if [off] and [len] do not designate a valid range
    of [b]. *)

val finish : t -> (event -> unit) -> unit
(** [finish d f] ends the input. It does what {!idle} does, then cuts an open
    string ({!Cut}) and ends an open paste as {!cut_paste} does. [d] is then at
    the start of a stream, as after {!reset}, and owed what it was owed. *)

val reset : t -> unit
(** [reset d] drops what [d] holds of the stream, with no event, for a caller
    that stops reading and will start again, as around a suspend. What [d] is
    owed stays, since it counts queries already sent. *)

(** {1:esc ESC and silence}

    ESC begins every sequence and string, and is also the Escape key. ESC before
    a key is Alt with that key. An {e introducer} is a byte after ESC that may
    begin a sequence or a string, one of [\[], [O], [P], [\]] and [_]. The bytes
    decide whenever they can:
    - ESC before a character, a control other than ESC, or DEL is Alt and that
      key, immediately.
    - [ESC \[] before a byte 0x20..0x7E begins a control sequence. [ESC O]
      begins an SS3 sequence before a digit, [;], or a byte an SS3 key ends with
      (SP, [A]..[F], [H], [I], [M], [P]..[S], [X], [a]..[e], [j]..[y]).
    - [ESC P] before a byte 0x20..0x3F, [ESC \]] before a digit, [L] or [l], and
      [ESC _] before [G] begin a control string while a string reply is owed
      ({!section-owed}). While none is owed, ESC with [P], [\]] or [_] is an Alt
      key immediately.
    - Any other byte after ESC and an introducer makes Alt with the introducer,
      then is decoded itself. After ESC ESC and an introducer it makes
      Alt+Escape and the introducer, then is decoded itself, as ESC ESC before
      any other byte makes Alt+Escape and then that byte. What is owed decides
      only whether a string can begin.

    A sequence after ESC ESC is Alt with the sequence's key. When the sequence
    gives any other event, it is the Escape key and then that event.

    The bytes cannot decide whether ESC, ESC ESC, or ESC before an introducer
    was a whole keystroke when the terminal stops writing, since it may not have
    finished. The decoder holds them, and {!idle} turns them into Escape,
    Alt+Escape, Alt with the introducer, or Alt+Escape and the introducer. *)

(** The type for what a decoder holds that silence would decide. *)
type pending =
  | Nothing
      (** Silence would change nothing, also inside a string or a paste. *)
  | Ambiguous
      (** ESC, ESC ESC, or ESC before an introducer, which are keys if the
          terminal has finished writing and the start of a sequence if it has
          not. *)
  | Sequence
      (** A control sequence, an X10 report or a UTF-8 character in progress. A
          terminal writes these whole, so silence means they were cut. *)

val pending : t -> pending
(** [pending d] is what [d] holds. *)

val idle : t -> (event -> unit) -> unit
(** [idle d f] tells [d] that the terminal has written nothing for as long as
    the caller chose to wait, and calls [f] on the events that decides. Held
    ESCs become keys, and a sequence or character in progress becomes a {!Cut}
    or an {!Invalid}. Strings and pastes go on. [idle] must be called only after
    a wait in which nothing arrived, since bytes already read are never silence.
*)

(** {1:sequences Control sequences}

    Once a control sequence has begun, its bytes never become keys. A sequence
    is a key, a report or a reply by its marker, intermediates, final byte and
    parameters ({!Keystroke}, {!Mouse_report}, {!module-Reply}). One whose
    values fall outside the form its reference fixes is a {!Reply.Csi}, and one
    that {!Ansi.Parser} finds malformed has no effect.

    Silence, the end of the input, or a byte that cannot continue a sequence (a
    C0 control, DEL, ESC or a non-ASCII byte) cuts it. It is dropped as a
    {!Cut}, and the byte that cut it is decoded after it. ECMA-48 performs a C0
    control inside a sequence and ignores DEL, as {!Ansi.Parser} does for
    output. Input cuts the sequence instead, because a terminal writes a
    sequence whole, so a byte inside it that cannot belong to it is the user's
    next key.

    xterm sends a modified F3 as [CSI 1 ; m R], which is also a cursor position
    report ("it is possible for this sequence to be sent by a function key",
    xterm's Control Sequences, "Device Status Report"). While a cursor report is
    owed it is a {!Reply.Cursor}, and otherwise F3. *)

(** {1:strings Control strings}

    A control string begins only while a string reply is owed. Every string a
    terminal sends answers a query, so an unsolicited [ESC \]] is Alt+\], and
    what the user types after it stays typing. Once begun, a string is never cut
    by silence. It ends at its terminator (ST, or BEL for an OSC), at CAN or
    SUB, at an ESC that does not begin ST, or at the end of the input. Its
    content then becomes a reply, a {!Cut} or a {!Too_long}, however long the
    terminal stalls. A DCS whose header {!Ansi.Parser} finds malformed gives no
    event when it ends at ST.

    The content of a control string is kept until the string ends, so that it
    can be decoded. Its size is bounded by 16 MiB, a {e memory budget}, since no
    reference bounds a string and a clipboard read can be megabytes. A string
    over the budget is read to its end and reported as {!Too_long}, its content
    dropped. Control sequences hold no content and need no budget. *)

(** {1:owed Owed replies}

    The caller sends the queries, so it knows which replies may come, and each
    query says what it is owed. {!expect} adds to the counts, and {!settle}
    takes back what a batch of queries was owed once the batch is answered or
    never will be. A terminal answers its queries in order and answers DA1, so a
    batch of queries ends with DA1 ({!Reply.Primary_attributes}), whose reply
    says the queries before it have been answered or never will be ("If an
    answer for the device attributes is received without getting back an answer
    for the progressive enhancement the terminal does not support this
    protocol", kitty keyboard protocol, "Detection of support"). The caller
    settles a batch when the DA1 that ends it is answered, or when it gives up
    on an answer. The decoder only reads the counts.

    The counts decide how a reply decodes, so a reply that arrives after its
    batch was settled is input. Its [ESC P], [ESC \]] or [ESC _] is an Alt key
    and the rest is typed, and its [CSI 1 ; m R] is F3. While a reply is owed it
    never becomes keys. A caller must keep a batch owed until its answer
    arrives, and give up on one only after longer than a terminal takes to
    answer. *)

type owed = {
  cursor_reports : int;
      (** Cursor position reports owed. While any is owed, [CSI 1 ; m R] is a
          {!Reply.Cursor}, and otherwise F3. *)
  string_replies : int;
      (** Control-string replies owed (OSC, DCS or APC). While any is owed,
          [ESC \]], [ESC P] and [ESC _] may begin one. *)
}
(** The type for the replies queries are owed. *)

val expect : t -> owed -> unit
(** [expect d o] adds [o] to what [d] is owed, for queries just sent.

    Raises [Invalid_argument] if a count of [o] is negative. *)

val settle : t -> owed -> unit
(** [settle d o] takes [o] back from what [d] is owed, for queries that have
    been answered or never will be.

    Raises [Invalid_argument] if a count of [o] is negative or more than [d] is
    owed. *)

(** {1:paste Paste}

    With bracketed paste on (mode 2004), pasted text arrives between
    [ESC \[ 200 ~] and [ESC \[ 201 ~], "so that the program can differentiate
    pasted text from typed-in text" (xterm's Control Sequences, "Bracketed Paste
    Mode"). The content is passed on exactly, in pieces as it arrives, with
    nothing decoded, so an ESC, a second start marker, or a lookalike such as
    [ESC \[ 201 x] is content. The paste ends at the first end marker, at the
    end of the input, or when the caller ends it with {!cut_paste}. Silence
    leaves it open, so a stalled paste is never typed. The decoder passes
    content on as it arrives and holds at most the 5 bytes of an end marker not
    yet complete. *)

val cut_paste : t -> (event -> unit) -> unit
(** [cut_paste d f] ends the paste in progress, if any, for a program that will
    wait no longer for its end marker. It calls [f] on the bytes of an end
    marker it held, as {!Paste_data}, then on [Paste_end `Cut]. The bytes that
    follow decode as input. *)

val encode_paste : bracketed:bool -> string -> string
(** [encode_paste ~bracketed s] is the bytes a terminal sends when its user
    pastes [s]: [s] between the start and end markers when [bracketed] (mode
    2004 set), and [s] as typed otherwise. The terminal sends [s] as it is, so
    an end marker inside [s] ends the paste there. *)

(** {1:text Text}

    Text is UTF-8. A byte that cannot begin or continue a character, or a
    character that silence or the end of the input cuts, is {!Invalid}, one
    ill-formed subsequence at a time (Unicode §3.9, maximal subparts). A program
    "shall not interpret such sequences as characters" (Unicode C10). A terminal
    in an 8-bit locale sends Alt as a byte with its high bit set, and here such
    a byte is {!Invalid}. *)
