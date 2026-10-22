(** Control functions in a UTF-8 stream (ECMA-48, ECMA-35).

    {1:model What a stream is made of}

    A terminal receives characters: graphic characters, which are shown, and the
    coded forms of control functions, which are performed. ECMA-48 §5 codes a
    control function in five ways:
    - a C0 control: one character U+0000..U+001F, such as LF;
    - a C1 control written ESC Fe: ESC, then a byte 0x40..0x5F. ESC D is IND;
    - an escape sequence ESC I.. F: intermediate bytes 0x20..0x2F, then a final
      byte 0x30..0x7E, as in ESC ( 0. The intermediates and the final byte name
      the function (ECMA-35 §13.1);
    - a control sequence CSI P.. I.. F: CSI is [ESC \[], then parameter bytes
      0x30..0x3F, intermediates and a final byte 0x40..0x7E, as in
      [CSI 1 ; 31 m];
    - a control string: an opening (DCS, OSC, APC, PM or SOS, written [ESC P],
      [ESC \]], [ESC _], [ESC ^] and [ESC X]), its content, then ST, written
      [ESC \\] (ECMA-48 §5.6). xterm also ends an OSC with BEL.

    The bytes are UTF-8, decoded first. Each ill-formed subsequence, such as a
    lone byte 0x80..0xFF, is one U+FFFD (Unicode §3.9, maximal subparts). The
    code points U+0080..U+009F are never controls: a UTF-8 stream is not the
    8-bit code in which C1 controls are single bytes, and xterm, whose protocol
    programs target, states that "it is not possible to use a C1 control
    obtained from decoding the UTF-8 text". They have no glyph either, so they
    are dropped wherever they occur. Performing them would let mis-encoded text
    swallow the output that follows it: Windows-1252 "Ÿ" read as Latin-1 is
    U+009F, APC.

    {1:recovery What every character does in every state}

    + ESC, CAN and SUB are never part of anything. Each ends the sequence or
      string in progress (VT100 User Guide ch. 3, VT510 §4.3.5), so one ESC
      always brings the parser back. CAN and SUB are then reported as
      {!Control}; ESC starts a new escape sequence.
    + A C0 control inside an escape or control sequence is performed where it
      occurs, and the sequence continues: CSI 2 LF C is LF, then CSI 2 C (VT100
      User Guide ch. 3: "Embedded control characters are executed as soon as
      they are encountered"). Inside a DCS header or a string a C0 control is
      never performed: BS..CR are string content (ECMA-48 §5.6), BEL ends an
      OSC, and the others are dropped.
    + DEL is dropped inside sequences and strings. Elsewhere it is reported,
      since only the device knows whether a 96-character set makes it a graphic
      character.
    + A byte that may appear in a sequence but not where it occurs (a private
      marker after a parameter, a parameter after an intermediate), a fifth
      intermediate, or a 257th parameter field makes the sequence malformed. It
      is consumed to its final byte, to the end of the string for a DCS, and has
      no effect, never a partial one.
    + A non-ASCII character belongs to no sequence. It ends an escape or control
      sequence, which has no effect, and is dropped, as xterm does. In a DCS
      header it makes the DCS malformed, because the content of a string never
      becomes text.

    {1:meaning Syntax, not meaning}

    Tokens report how a function was written, not what it does. Its meaning and
    the defaults of its parameters are the device's: ECMA-48 §6 makes HPR the
    same as CUF only in a device "without a data component", and xterm reads CSI
    s as SCOSC or DECSLRM depending on a mode. The device matches
    [(marker, intermediates, final)].

    {1:chunks Streaming}

    Bytes may be fed in chunks split anywhere. Whatever the split, the tokens
    are the same once adjacent {!Text} tokens, and adjacent {!Data} tokens, are
    joined; neither is ever cut inside a character. The parser holds no text and
    no string content, only the sequence in progress. *)

(** {1:params Parameters} *)

module Params : sig
  type t
  (** The type for the parameters of a control sequence or DCS header (ECMA-48
      §5.4.2). Parameters are separated by [;]. Each has one or more fields
      separated by [:], the sub-parameters SGR 38, 48 and 58 use. A field is
      absent, as both are in [CSI ; H], or a non-negative integer. An integer
      too large for [int] saturates at [max_int]: clamp a value to the distance
      it can act on before adding it to anything.

      ECMA-48 §5.4.3 sets no maximum count. A parser that holds bounded memory
      must still bound the fields it keeps, because a sequence's meaning depends
      on its final byte, which comes last. The bound is 256 fields, above the
      number of distinct values any variadic function assigns; a sequence with
      more has no effect. *)

  val count : t -> int
  (** [count p] is the number of parameters: 0 for [CSI m], 2 for [CSI ; m]. *)

  val pn : t -> int -> default:int -> int
  (** [pn p i ~default] reads parameter [i] as a count or a position (ECMA-48
      §8.1, Pn). Absent, 0 or [i >= count p] means [default]: DEC terminals give
      0 the default's meaning for these parameters (VT510 §4.3.3: "A 0 value or
      omitted parameter indicates a default value"), as does ECMA-48's ZERO
      DEFAULT MODE for the functions it lists (F.4.2). Reads the first field. *)

  val ps : t -> int -> default:int -> int
  (** [ps p i ~default] reads parameter [i] as a selector (ECMA-48 §8.1, Ps).
      Absent or [i >= count p] means [default]; 0 is the value 0, as in ED 0 or
      SGR 0. Reads the first field. *)

  val value : t -> int -> int option
  (** [value p i] is the first field of parameter [i], [None] if it is absent or
      [i >= count p]. For functions that skip absent parameters, such as SM and
      RM. *)

  val fields : t -> int -> int
  (** [fields p i] is the number of fields of parameter [i]: 1 for [5], 3 for
      [5:1:2], 0 if [i >= count p]. *)

  val field : t -> int -> int -> int option
  (** [field p i j] is field [j] of parameter [i], [None] if it is absent or out
      of range. Field 0 is the one {!value} reads. *)
end

(** {1:tokens Tokens} *)

type sequence = {
  marker : char option;
      (** The private marker [<], [=], [>] or [?], written as the first
          parameter byte (ECMA-48 §5.4.1 b): [Some '?'] in [CSI ? 25 h]. *)
  params : Params.t;
  intermediates : string;  (** Bytes 0x20..0x2F, at most 4. *)
  final : char;
      (** A byte 0x40..0x7E. With [marker] and [intermediates] it names the
          function. *)
}
(** The type for the header of a control sequence or of a DCS. *)

(** The type for the kinds of control strings. *)
type opening =
  | Dcs of sequence  (** DEVICE CONTROL STRING, with its header. *)
  | Osc  (** OPERATING SYSTEM COMMAND. *)
  | Apc  (** APPLICATION PROGRAM COMMAND. *)
  | Pm  (** PRIVACY MESSAGE. *)
  | Sos  (** START OF STRING. *)

(** The type for tokens. *)
type token =
  | Text of string
      (** Graphic characters: valid UTF-8 without C0, DEL or U+0080..U+009F. *)
  | Control of char
      (** A C0 control other than ESC, DEL outside sequences and strings, or a
          C1 control 0x80..0x9F written ESC Fe: ESC D is [Control '\x84'], IND.
          CSI, DCS, OSC, APC, PM, SOS and ST are never reported this way; a lone
          ST is dropped. *)
  | Esc of { intermediates : string; final : char }
      (** An escape sequence that is not ESC Fe: [ESC 7], [ESC ( B],
          [ESC ( % 5], [ESC # 8], [ESC SP F]. ECMA-35 allows any number of
          intermediates (§13.2.2); up to 4 are kept, and no device acts on more.
          A DRCS designation, [ESC $ ( SP F], has 4 (§14.4.2). *)
  | Csi of sequence  (** A control sequence. *)
  | Open of opening
      (** The start of a control string. Its content follows as {!Data}, then
          exactly one {!Close}, with no other token in between. *)
  | Data of string
      (** Content of the open string: valid UTF-8, made of ECMA-48 §5.6's
          command-string characters (BS..CR and 0x20..0x7E) and non-ASCII
          characters other than U+0080..U+009F. *)
  | Close of [ `St | `Bel | `Aborted ]
      (** The end of the open string. [`St] ends any string, [`Bel] only an OSC;
          xterm replies to an OSC with the terminator of its query. [`Aborted]
          is a string ended by CAN, SUB, an ESC that does not begin ST, or the
          end of the input. Its content is in error (ECMA-48 §8.3.6, CAN: "this
          data shall be ignored"), so act on nothing that has not already been
          acted on. *)

val pp_token : Format.formatter -> token -> unit
(** [pp_token] formats a token in ECMA-48's notation: [CSI ?25 h], [CSI 1;5 H],
    [CSI 38:2::1:2:3 m], [CSI 1 SP @], [ESC ( B], [LF], [IND], [DCS $ q], [OSC],
    [ST], [BEL], [Aborted]. Text and data are quoted strings. *)

(** {1:parsers Parsers} *)

type t
(** The type for parsers. Mutable and not thread-safe. *)

val create : unit -> t
(** [create ()] is a parser at the start of a stream. *)

val reset : t -> unit
(** [reset p] returns [p] to the start of a stream, dropping the character,
    sequence or string in progress without reporting it. *)

val feed : t -> bytes -> off:int -> len:int -> (token -> unit) -> unit
(** [feed p b ~off ~len f] reads the [len] bytes of [b] from [off] and calls [f]
    on each token, in order. A character, sequence or string unfinished at the
    end of the bytes continues with the next call.

    Raises [Invalid_argument] if [off] and [len] do not designate a valid range
    of [b]. *)

val finish : t -> (token -> unit) -> unit
(** [finish p f] ends the stream: it calls [f] on what the end of the input
    makes of the unfinished parts, in order. An unfinished character is U+FFFD,
    an open string is closed [`Aborted], and an unfinished sequence has no
    effect. [p] is then at the start of a stream, as after {!reset}. *)

val parse : string -> token list
(** [parse s] are the tokens of the complete stream [s]: those {!feed} and
    {!finish} give. *)
