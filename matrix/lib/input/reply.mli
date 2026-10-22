(** Terminal replies.

    A terminal writes into its input both what the user does and its answers to
    the program's queries ("in response to commands from the host", VT510
    §4.4.2). A reply is never a keystroke: a program that mistook one for typing
    would act on keys nobody pressed. Every reply is a control sequence or a
    control string, so it is told apart from keys by its form, and a sequence or
    string that does not finish is dropped and reported as {!Cut}, never typed.

    {1:typed Typed replies}

    A reply whose form a reference fixes is decoded to its values; one whose
    values fall outside that form is kept as the sequence it is ({!Csi}, {!Dcs},
    {!Osc}). Sizes and positions saturate at [max_int], as every parameter does.

    {1:budget The memory budget}

    The content of a control string is kept until the string ends, so that it
    can be decoded. Its size is bounded by 16 MiB, a memory budget: no reference
    bounds a string, and a clipboard read can be megabytes. A string over it is
    read to its end and reported as {!Too_long}, its content dropped. Control
    sequences hold no content and need no budget. *)

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
  | Cursor of { row : int; col : int }
      (** CPR, [CSI row ; col R], or DECXCPR, [CSI ? row ; col R], from row 0
          and column 0. xterm sends a modified F3 as [CSI 1 ; m R], which a CPR
          can be; see {!Decoder.expect}. *)
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
    }
      (** [OSC 10], [OSC 11], [OSC 12] or [OSC 4 ; index] with a color written
          [rgb:r/g/b], each channel of 1 to 4 hex digits scaled to 0..255, as
          X11's XParseColor reads them. Other color forms are {!Osc}. *)
  | Clipboard of { selection : string; data : string }
      (** [OSC 52 ; selection ; data]: a clipboard read, its data decoded from
          base64. Data that is not base64 is {!Osc}. *)
  | Graphics of { id : int option; message : string }
      (** [APC G keys ; message ST]: the kitty graphics protocol's reply, with
          the image id of its [i] key and its message, such as ["OK"]. *)
  | Osc of string  (** Any other OSC, its content as sent. *)
  | Dcs of { header : Ansi.Parser.sequence; data : string }
      (** Any other DCS, such as DECRQSS's reply [DCS 1 $ r ... ST]. *)
  | Csi of Ansi.Parser.sequence
      (** Any other control sequence: one no reference defines as input, or a
          key or report whose values are out of the range its reference fixes.
      *)
  | Ss3 of Ansi.Parser.sequence
      (** An SS3 sequence that names no key, written as the control sequence its
          bytes would be after [CSI]. *)
  | Cut of [ `Sequence | `String | `Mouse ]
      (** A control sequence, a control string or an X10 mouse report that did
          not finish: cut by silence, by the end of the input, or by a byte that
          cannot continue it. Its content is in error (ECMA-48 §8.3.6, CAN:
          "this data shall be ignored"), so it is dropped. *)
  | Too_long of [ `Dcs | `Osc | `Apc ]
      (** A control string over the memory budget, read to its end and dropped
          whole. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a reply for debugging. *)
