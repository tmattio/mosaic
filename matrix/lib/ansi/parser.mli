(** Streaming ANSI escape sequence parser.

    Converts byte streams into high-level tokens representing ANSI escape
    sequences, control codes, and plain text. Handles partial input gracefully,
    preserving UTF-8 boundaries and buffering incomplete sequences across
    chunks.

    {1 Parser Overview}

    The parser processes terminal output and extracts semantic tokens:

    - {b SGR (Select Graphic Rendition)}: Style attributes and colors
    - {b CSI (Control Sequence Introducer)}: Cursor movement, screen
      manipulation
    - {b OSC (Operating System Command)}: Terminal title, hyperlinks (OSC 8)
    - {b Plain text}: UTF-8 encoded text between escape sequences

    The parser is stateful and reentrant, allowing incremental processing of
    data arriving in arbitrary-sized chunks.

    {1 Usage}

    Create a parser and feed data incrementally:
    {[
      let parser = Parser.create () in
      let tokens1 = Parser.feed parser bytes1 0 (Bytes.length bytes1) in
      let tokens2 = Parser.feed parser bytes2 0 (Bytes.length bytes2) in
      (* Process tokens... *)
    ]}

    For complete strings:
    {[
      let tokens = Parser.parse "\027[1;31mHello\027[0m World"
      (* Returns: [SGR [`Bold; `Fg Red]; Text "Hello"; SGR [`Reset]; Text "
         World"] *)
    ]}

    {1 UTF-8 Handling}

    The parser preserves UTF-8 integrity by buffering incomplete multi-byte
    sequences at chunk boundaries. Only complete UTF-8 characters are emitted as
    {!Text} tokens. Malformed UTF-8 sequences are replaced with U+FFFD
    (replacement character).

    {1 Hyperlink Support}

    OSC 8 hyperlink sequences are parsed into {!Hyperlink} control tokens:
    - [ESC ] 8 ; ; ST\]: Close hyperlink -> [Hyperlink None]
    - [ESC ] 8 ; params ; url ST\]: Open hyperlink ->
      [Hyperlink (Some (params, url))]

    Parameters are parsed as key=value pairs separated by colons.

    {1 Contracts}

    - The parser enforces conservative maximum lengths ([max_escape_length] for
      CSI and [max_osc_length] for OSC) and emits {!Unknown} tokens if a
      sequence exceeds those bounds, preventing unbounded buffering.
    - It never raises on malformed input. All undecodable sequences become
      {!Unknown} controls. Incomplete UTF-8 sequences at chunk boundaries are
      buffered until they can be completed.

    {1 Allocation Behavior}

    Each {!Text} token allocates a new string via [Buffer.contents]. ASCII text
    is processed with minimal overhead (single-byte copies). For streaming use
    cases, buffers are reused between {!feed} calls.

    {1 Control Character Handling}

    C0 control characters (0x00-0x1F) except ESC are passed through in {!Text}
    tokens. Applications must handle CR, LF, TAB, BEL, BS, etc. as needed. Only
    ESC-initiated sequences are parsed into {!Control} tokens. *)

type control =
  | CUU of int  (** Cursor Up by n lines. *)
  | CUD of int  (** Cursor Down by n lines. *)
  | CUF of int  (** Cursor Forward by n columns. *)
  | CUB of int  (** Cursor Backward by n columns. *)
  | CNL of int  (** Cursor Next Line (down n lines, column 1). *)
  | CPL of int  (** Cursor Previous Line (up n lines, column 1). *)
  | CHA of int  (** Cursor Horizontal Absolute to column n. *)
  | VPA of int  (** Vertical Position Absolute to row n. *)
  | CUP of int * int  (** Cursor Position to (row, col). *)
  | ED of int
      (** Erase in Display (0=cursor to end, 1=start to cursor, 2=all). *)
  | EL of int  (** Erase in Line (0=cursor to end, 1=start to cursor, 2=all). *)
  | IL of int  (** Insert n blank Lines at cursor. *)
  | DL of int  (** Delete n Lines at cursor. *)
  | DCH of int  (** Delete n Characters at cursor. *)
  | ICH of int  (** Insert n blank Characters at cursor. *)
  | OSC of int * string
      (** Generic OSC (Operating System Command).

          Format: [OSC code ; payload ST]. Common codes include 0 (set title), 8
          (hyperlink). Unhandled OSC codes are preserved as generic OSC tokens.
      *)
  | Hyperlink of ((string * string) list * string) option
      (** OSC 8 hyperlink control.

          - [None]: Close current hyperlink ([ESC ] 8 ; ; ST\])
          - [Some (params, url)]: Open hyperlink with optional key=value
            parameters separated by colons (e.g., ["id=123:name=foo"])

          Example: [ESC ] 8 ; id=123 ; https://ocaml.org ST\] parses to
          [Hyperlink (Some ([("id", "123")], "https://ocaml.org"))]. *)
  | Reset  (** RIS (Reset to Initial State) - [ESC c]. *)
  | DECSC  (** Save Cursor Position - [ESC 7]. *)
  | DECRC  (** Restore Cursor Position - [ESC 8]. *)
  | Unknown of string
      (** Unrecognized escape sequence.

          Contains the raw sequence string for logging or debugging. Allows
          forward compatibility with new escape sequences. *)

type sgr_attr =
  [ `Reset  (** Reset all attributes to default. *)
  | `Bold
  | `Dim
  | `Italic
  | `Underline
  | `Double_underline
  | `Blink
  | `Reverse  (** Swap foreground and background. *)
  | `Conceal  (** Hidden text. *)
  | `Strikethrough
  | `Overline
  | `Framed
  | `Encircled
  | `No_bold
  | `No_dim
  | `No_italic
  | `No_underline
  | `No_blink
  | `No_reverse
  | `No_conceal
  | `No_strikethrough
  | `No_overline
  | `No_framed
  | `No_encircled
  | `Fg of Color.t  (** Set foreground color. *)
  | `Bg of Color.t  (** Set background color. *) ]
(** SGR (Select Graphic Rendition) attributes.

    Represents individual style changes from a single SGR sequence. A sequence
    like [ESC \[ 1 ; 31 m] produces [[`Bold; `Fg Red]]. *)

type token =
  | Text of string
      (** Plain UTF-8 text.

          Contains text content between escape sequences. Always contains
          complete UTF-8 characters; incomplete sequences are buffered until
          more data arrives. Malformed UTF-8 is replaced with U+FFFD. *)
  | SGR of sgr_attr list
      (** Select Graphic Rendition command.

          Contains all attributes from a single SGR sequence. Empty list
          represents SGR with no parameters (equivalent to [`Reset]). *)
  | Control of control
      (** Other control sequences.

          Includes cursor movement, screen manipulation, and OSC commands
          excluding SGR. *)

type t
(** Mutable parser state.

    Maintains internal buffers for incomplete escape sequences and UTF-8
    characters. Not thread-safe; use one parser per thread. *)

(** {1 Parser Operations} *)

val create : unit -> t
(** [create ()] creates a fresh parser.

    Starts in the default state with empty buffers. *)

val reset : t -> unit
(** [reset t] clears internal buffers and returns to the default state.

    Discards any buffered partial sequences or UTF-8 bytes. Use when abandoning
    a parse operation or starting fresh with the same parser instance. *)

val has_pending : t -> bool
(** [has_pending t] returns [true] if the parser has buffered data.

    This includes incomplete escape sequences or pending UTF-8 bytes awaiting
    more data. Useful for checking if more input is expected without allocating.
*)

val pending : t -> bytes
(** [pending t] returns a copy of raw input bytes not yet fully processed.

    These are bytes from the input stream that couldn't be consumed because they
    may be part of an incomplete sequence (e.g., a lone ESC at chunk end). Note
    that escape sequence bodies being accumulated (CSI parameters, OSC payloads)
    are stored in separate internal buffers and are not included here.

    Allocates a new bytes value. Use {!has_pending} if you only need to check
    whether more input is expected. *)

val feed : t -> bytes -> int -> int -> (token -> unit) -> unit
(** [feed t buf off len f] processes [len] bytes from [buf] starting at [off],
    calling [f] for each complete token.

    Incomplete escape sequences and UTF-8 multi-byte sequences at chunk
    boundaries are buffered internally. The parser handles arbitrary chunk
    sizes, including single-byte feeds.

    Example:
    {[
      let p = Parser.create () in
      Parser.feed p bytes 0 len (fun tok ->
          match tok with
          | Text s -> print_string s
          | SGR attrs -> apply_styles attrs
          | Control c -> handle_control c)
    ]} *)

val parse : string -> token list
(** [parse s] parses a complete string.

    Convenience function for one-shot parsing. Creates a temporary parser, feeds
    the entire string, and returns all tokens. Not suitable for streaming; use
    {!feed} for incremental parsing. *)
