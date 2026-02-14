(** ANSI terminal escape sequences, styling, and control.

    Escape sequences are represented as functions ({!type-t}) that write
    directly to a {!writer} buffer. This provides zero-allocation composition:
    combining sequences creates a new function, not a new string.

    For quick one-off output, convert to string with {!to_string}:
    {[
      print_string Ansi.(to_string (cursor_position ~row:10 ~col:5))
    ]}

    For performance-critical render loops, use {!emit} with a pre-allocated
    buffer:
    {[
      let buf = Bytes.create 65536 in
      let w = Ansi.make buf in
      Ansi.emit (Ansi.cursor_position ~row:1 ~col:1) w;
      Ansi.emit (Ansi.literal "Hello") w
    ]}

    {1 Zero-Allocation Primitives}

    For hot render loops where even closure allocation matters, use the
    low-level SGR and hyperlink primitives that write directly to the buffer:

    {[
      (* Zero-allocation SGR: \027[0;38;2;255;0;0;1m *)
      Ansi.sgr_open w;
      Ansi.sgr_code w 0;
      (* reset *)
      Ansi.sgr_sep w;
      Ansi.sgr_code w 38;
      (* fg color *)
      Ansi.sgr_sep w;
      Ansi.sgr_code w 2;
      Ansi.sgr_sep w;
      Ansi.sgr_code w 255;
      (* red *)
      Ansi.sgr_sep w;
      Ansi.sgr_code w 0;
      Ansi.sgr_sep w;
      Ansi.sgr_code w 0;
      Ansi.sgr_sep w;
      Ansi.sgr_code w 1;
      (* bold *)
      Ansi.sgr_close w;

      (* Zero-allocation hyperlinks *)
      Ansi.hyperlink_open w "https://example.com";
      (* ... emit link text ... *)
      Ansi.hyperlink_close w
    ]}

    See also {!Sgr_state} which wraps these primitives with state tracking.

    {1 Contracts and Compatibility}

    - Cursor movement helpers clamp negative distances to zero and emit nothing
      when the movement would be a no-op.
    - Absolute coordinates are 1-based; values ≤ 0 are coerced to 1 to match
      terminal semantics.
    - Screen/line erase modes outside their documented ranges fall back to the
      most commonly supported mode.
    - The only API that raises is {!set_scrolling_region}, which follows DECSTBM
      by rejecting invalid bounds with [Invalid_argument].

    Functions use widely-supported ANSI escape sequences. Modern features such
    as truecolor, hyperlinks (OSC 8), or explicit width (OSC 66) degrade
    gracefully on terminals that ignore or partially implement the extensions.
*)

include module type of struct
  include Escape
end

(** {1 High-level Convenience}

    These functions operate at a higher abstraction level than the escape
    builders above. They take strings as input and return strings directly. *)

val styled :
  ?reset:bool ->
  ?fg:Color.t ->
  ?bg:Color.t ->
  ?bold:bool ->
  ?dim:bool ->
  ?italic:bool ->
  ?underline:bool ->
  ?blink:bool ->
  ?inverse:bool ->
  ?hidden:bool ->
  ?strikethrough:bool ->
  ?overline:bool ->
  ?double_underline:bool ->
  ?framed:bool ->
  ?encircled:bool ->
  ?link:string ->
  string ->
  string
(** [styled ?reset ?fg ?bg ... str] applies styles to [str].

    Builds a {!Style.t} under the hood and emits only the necessary SGR
    transitions. When [link] is provided, wraps the output in OSC 8 hyperlink
    sequences (open before text, close after).

    @param reset
      Controls style persistence after [str]. Defaults to [~reset:false] (style
      persists). Pass [~reset:true] to append reset sequence [\[ESC[0m]].
    @param link
      Optional URL for OSC 8 hyperlink. When provided, emits hyperlink open
      before the styled text and hyperlink close after.

    {[
      print_string (Ansi.styled ~fg:Color.red "Error: ");
      print_endline (Ansi.styled ~reset:true ~fg:Color.white "details")
    ]} *)

val render : ?hyperlinks_enabled:bool -> (Style.t * string) list -> string
(** [render ?hyperlinks_enabled segments] renders styled segments to a string.

    Emits minimal SGR transitions by tracking terminal state; identical
    consecutive styles generate no output. Automatically closes all open
    hyperlinks at the end. Always appends a final reset [\[ESC[0m]].

    @param hyperlinks_enabled
      When [false], suppresses OSC 8 sequences. Defaults to [true].

    {[
      let output =
        Ansi.render
          [
            (Style.make ~fg:Color.yellow (), "Warning:");
            (Style.default, " Low disk space");
          ]
    ]}

    {b Streaming}: For streaming rendering (e.g., in render loops), use
    {!Sgr_state} directly. *)

val parse : string -> Parser.token list
(** [parse s] tokenizes ANSI-encoded string [s].

    Parses escape sequences, control codes, and plain text into high-level
    tokens. Parsing never raises; malformed sequences are emitted as unknown
    controls. *)

val strip : string -> string
(** [strip s] removes all ANSI escape sequences from [s].

    Returns only plain text content. Useful for display width calculation,
    logging to files, or text processing. Time complexity: O(n).

    {[
      Ansi.strip "\x1b[31mRed\x1b[0m" = "Red"
    ]} *)

(** {1 Sub-modules} *)

module Color = Color
(** Color representations and conversions. *)

module Attr = Attr
(** Text attribute flags (bold, italic, underline, etc.). *)

module Style = Style
(** Complete styling with colors, attributes, and hyperlinks. *)

module Parser = Parser
(** Streaming ANSI escape sequence parser. *)

module Sgr_state = Sgr_state
(** Terminal SGR state tracker for delta encoding in render loops. *)
