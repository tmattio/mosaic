(** Terminal replies.

    A terminal writes into its input both what the user does and its answers to
    the program's queries ("in response to commands from the host", VT510
    §4.4.2). A reply is never a keystroke: a program that mistook one for typing
    would act on keys nobody pressed. Every reply is a control sequence or a
    control string, so it is told apart from keys by its form; a sequence or
    string that does not finish is no reply, and is never typed
    ({!Decoder.Cut}).

    {1:typed Typed replies}

    A reply whose form a reference fixes is decoded to its values; one whose
    values fall outside that form is kept as the sequence it is ({!Csi}, {!Dcs},
    {!Osc}). Sizes and positions saturate at [max_int], as every parameter does.

    An OSC reply ends with the terminator of the query it answers: xterm "uses
    the same terminator used in a query" (xterm's Control Sequences, "Operating
    System Commands"), BEL or ST. A program that sent BEL expects BEL back, so
    each OSC reply keeps its terminator. *)

(** The type for the settings a DEC mode report gives (DECRPM, xterm's Control
    Sequences, "Request DEC private mode"). *)
type setting =
  | Not_recognized  (** 0: the terminal does not know the mode. *)
  | Set  (** 1. *)
  | Reset  (** 2. *)
  | Permanently_set  (** 3: set, and it cannot be changed. *)
  | Permanently_reset  (** 4: reset, and it cannot be changed. *)

(** The type for replies. *)
type t =
  | Primary_attributes of int list
      (** DA1, [CSI ? Ps ; ... c]: the terminal's class and features. A terminal
          answers its queries in order and answers DA1, so a batch of queries
          ends with DA1, and its reply says the queries before it have been
          answered or never will be ("If an answer for the device attributes is
          received without getting back an answer for the progressive
          enhancement the terminal does not support this protocol", kitty
          keyboard protocol, "Detection of support"). *)
  | Secondary_attributes of int list
      (** DA2, [CSI > Ps ; ... c]: the terminal's type, version and options. *)
  | Unit_id of string
      (** DA3, [DCS ! | id ST]: the unit's identity, as the hex digits sent. *)
  | Mode of { dec : bool; mode : int; setting : setting }
      (** DECRPM, [CSI ? mode ; setting $ y] for a DEC private mode
          ([dec = true]) or [CSI mode ; setting $ y] for an ANSI mode. *)
  | Cursor of { row : int; col : int; page : int option }
      (** CPR, [CSI row ; col R], with no page, or DECXCPR,
          [CSI ? row ; col ; page R], from row 0 and column 0 and page 1 ("the
          cursor is currently at line Pl, column Pc, on page Pp", VT510,
          DECXCPR). xterm omits the page, "the default page, i.e., "1"" (xterm's
          Control Sequences, "Device Status Report"). xterm sends a modified F3
          as [CSI 1 ; m R], which a CPR can be; see {!Decoder.expect}. *)
  | Operating_status of [ `Ok | `Malfunction ]
      (** DSR, [CSI 0 n] or [CSI 3 n]: the terminal "is in good operating
          condition" or "has a malfunction", the reply to [CSI 5 n] (VT510,
          DSR—Operating Status). *)
  | Text_area_pixels of { height : int; width : int }
      (** XTWINOPS, [CSI 4 ; height ; width t], the reply to [CSI 14 t]. *)
  | Cell_pixels of { height : int; width : int }
      (** XTWINOPS, [CSI 6 ; height ; width t], the reply to [CSI 16 t]. *)
  | Text_area of { rows : int; cols : int }
      (** XTWINOPS, [CSI 8 ; rows ; cols t], the reply to [CSI 18 t]. *)
  | Resize of { rows : int; cols : int; height : int; width : int }
      (** The in-band resize report, [CSI 48 ; rows ; cols ; height ; width t],
          sent on every resize while mode 2048 is set, with the text area in
          cells and in pixels. Sub-parameters are ignored, as its definition
          requires of a client ("If a client does not understand these, it MUST
          ignore subparameters"). *)
  | Keyboard_flags of int
      (** [CSI ? flags u]: the kitty keyboard protocol's enhancement flags. *)
  | Color_scheme of [ `Dark | `Light ]
      (** [CSI ? 997 ; 1 n] (dark) or [CSI ? 997 ; 2 n] (light): the reply to
          [CSI ? 996 n], and the report a terminal sends on a change while mode
          2031 is set (contour, "Dark and Light Mode detection"). *)
  | Version of string  (** XTVERSION, [DCS > | text ST]: name and version. *)
  | Termcap of { valid : bool; entries : (string * string option) list }
      (** XTGETTCAP, [DCS 1 + r name=value ; ... ST] ([valid]) or
          [DCS 0 + r name ST]: each capability's name and value, decoded from
          hex. *)
  | Color of {
      target : [ `Foreground | `Background | `Cursor | `Palette of int ];
      rgb : int * int * int;
      terminator : Ansi.terminator;
    }
      (** [OSC 10], [OSC 11], [OSC 12] or [OSC 4 ; index] with a color in a form
          {!rgb_of_spec} reads, the OSC that would set that color. Other color
          forms are {!Osc}. *)
  | Clipboard of {
      selection : string;
      data : string;
      terminator : Ansi.terminator;
    }
      (** [OSC 52 ; selection ; data]: a clipboard read, its data decoded from
          base64. Data that is not base64 is {!Osc}. *)
  | Graphics of { id : int option; message : string }
      (** [APC G keys ; message ST]: the kitty graphics protocol's reply, with
          the image id of its [i] key and its message, such as ["OK"]. *)
  | Osc of { content : string; terminator : Ansi.terminator }
      (** Any other OSC, its content as sent. *)
  | Dcs of { header : Ansi.Parser.sequence; data : string }
      (** Any other DCS, such as DECRQSS's reply [DCS 1 $ r ... ST]. *)
  | Csi of Ansi.Parser.sequence
      (** Any other control sequence: one no reference defines as input, or a
          key or report whose values are out of the range its reference fixes.
      *)
  | Ss3 of Ansi.Parser.sequence
      (** An SS3 sequence that names no key, written as the control sequence its
          bytes would be after [CSI]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a reply for debugging. *)

(** {1:strings Reading control strings}

    A control string's content is read to the reply its form says, by the rules
    of the constructors above; content of no known form is kept as it is. *)

val of_osc : Ansi.terminator -> string -> t
(** [of_osc e s] is the reply an OSC with content [s], ended by [e], is:
    {!Color} or {!Clipboard} when [s] has their form, {!Osc} otherwise. *)

val of_dcs : Ansi.Parser.sequence -> string -> t
(** [of_dcs h s] is the reply a DCS with header [h] and data [s] is: {!Version},
    {!Unit_id} or {!Termcap} when [h] and [s] have their form, {!Dcs} otherwise.
*)

val graphics : string -> t
(** [graphics s] is the kitty graphics protocol's reply whose keys and message
    are [s], the content of an APC after its [G]: {!Graphics}. *)

val rgb_of_spec : string -> (int * int * int) option
(** [rgb_of_spec s] is the color the X11 color string [s] names, in its numeric
    forms (Xlib, ch. 6, "Color Strings"), each channel from 0 to 255:
    - [rgb:r/g/b], each channel of 1 to 4 hex digits, "scaled" to its range, so
      [rgb:f/8/0] is [(255, 136, 0)];
    - [#rgb], [#rrggbb], [#rrrgggbbb] or [#rrrrggggbbbb], the older form, whose
      digits "represent the most significant bits of the value", so [#f80] is
      [(240, 128, 0)].

    It is [None] for any other form: a color name, which needs X's database, or
    a device-independent form such as [CIEXYZ:]. The OSCs that set a color (4,
    10, 11 and 12) and their replies take these forms. *)

(** {1:encoding Encoding}

    A terminal writes each reply in the form its constructor gives, with 7-bit
    controls: CSI is [ESC \[] and ST is [ESC \\ ]. The decoder reads the bytes
    back: for every reply [r] it can produce, it decodes [encode [r]] to [r]
    while [r] is owed ({!Decoder.expect}), as a string reply and a CPR of row 0,
    whose form a modified F3 shares, need. *)

val encode : t list -> string
(** [encode rs] are the bytes a terminal sends for the replies [rs], in order;
    [""] for none. A color is written [rgb:rrrr/gggg/bbbb], each channel scaled
    to 16 bits as xterm writes it; capability names and values in hex; a
    clipboard's data in base64 with padding (RFC 4648 §4).

    Raises [Invalid_argument] if a reply holds a value its form cannot carry,
    which would read back as another reply: a negative number, a position of
    [max_int], a page below 1, a channel outside 0..255, a palette index or
    graphics id above 999,999,999, an empty capability name, a selection holding
    [;], or string content that is not UTF-8 or holds a control other than BS to
    CR. *)
