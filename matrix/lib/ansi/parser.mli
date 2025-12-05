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
      (* Returns: [SGR [`Bold; `Fg Red]; Text "Hello"; SGR [`Reset]; Text " World"] *)
    ]}

    {1 UTF-8 Handling}

    The parser preserves UTF-8 integrity by buffering incomplete multi-byte
    sequences at chunk boundaries. Only complete UTF-8 characters are emitted as
    {!Text} tokens.

    {1 Hyperlink Support}

    OSC 8 hyperlink sequences are parsed into {!Hyperlink} control tokens:
    - [ESC ] 8 ; ; ST\]: Close hyperlink → [Hyperlink None]
    - [ESC ] 8 ; params ; url ST\]: Open hyperlink →
      [Hyperlink (Some (params, url))]

    Parameters are parsed as key=value pairs separated by colons.

    {1 Contracts}

    - The parser enforces conservative maximum lengths ([max_escape_length] for
      CSI/DCS and [max_osc_length] for OSC) and emits {!Unknown} tokens if a
      sequence exceeds those bounds, preventing unbounded buffering.
    - It never raises on malformed input. All undecodable sequences become
      {!Unknown} controls, and truncated UTF-8 fragments are kept in the pending
      buffer until they can be completed.
    - {!feed} does not mutate the supplied bytes; callers may reuse buffers
      across invocations. *)

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
          more data arrives. *)
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

val pending : t -> bytes
(** [pending t] returns currently buffered bytes.

    Contains incomplete escape sequences or UTF-8 characters awaiting more data.
    Useful for diagnostics or determining if the parser expects more input. *)

val feed : t -> bytes -> int -> int -> token list
(** [feed t buf off len] processes [len] bytes from [buf] starting at [off].

    @return
      List of complete tokens recognized. May be empty if input only contains
      partial sequences.

    Partial escape sequences or UTF-8 characters are buffered internally. Call
    with subsequent chunks to complete them. The parser handles arbitrary chunk
    sizes, including single-byte feeds.

    Example:
    {[
      let p = Parser.create () in
      let tokens1 = Parser.feed p (Bytes.of_string "\027[3") 0 3 in
      (* tokens1 = [] - incomplete CSI sequence *)
      let tokens2 = Parser.feed p (Bytes.of_string "1m") 0 2 in
      (* tokens2 = [SGR [`Fg Red]] - completes the sequence *)
    ]} *)

val parse : string -> token list
(** [parse s] parses a complete string.

    Convenience function for one-shot parsing. Creates a temporary parser, feeds
    the entire string, and returns all tokens. Not suitable for streaming; use
    {!feed} for incremental parsing. *)
