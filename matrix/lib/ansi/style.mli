(** {1 Core Types} *)

(** [color] represents terminal color values.

    Basic colors map to standard 16-color palette (0-7). Bright variants use
    high-intensity palette (8-15). Default resets to terminal's configured
    color. Index accesses 256-color palette. RGB enables 24-bit true color.

    Compatibility:
    - Basic and bright colors: universal support
    - Default: universal (SGR 39/49)
    - Index: common in modern terminals (xterm-256color)
    - RGB: varies; graceful fallback to nearest 256-color

    Examples:
    {[
      let red_text = Ansi.sgr [ `Fg Red ]
      let bright_bg = Ansi.sgr [ `Bg Bright_yellow ]
      let orange = Ansi.sgr [ `Fg (Index 208) ]
      let custom = Ansi.sgr [ `Fg (RGB (255, 128, 0)) ]
    ]} *)
type color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default
  | Bright_black
  | Bright_red
  | Bright_green
  | Bright_yellow
  | Bright_blue
  | Bright_magenta
  | Bright_cyan
  | Bright_white
  | Index of int  (** 256-color palette index *)
  | RGB of int * int * int  (** 24-bit RGB color *)
  | RGBA of int * int * int * int
      (** 32-bit RGBA color with alpha channel for blending *)

type style =
  [ `Bold  (** Increases font weight (SGR 1) *)
  | `Dim  (** Reduces intensity (SGR 2) *)
  | `Italic  (** Slants text (SGR 3; limited support) *)
  | `Underline  (** Single underline (SGR 4) *)
  | `Double_underline
    (** Double underline (SGR 21; conflicts with bold reset on some terminals)
    *)
  | `Blink  (** Flashing text (SGR 5; often disabled by users) *)
  | `Reverse  (** Swaps foreground/background (SGR 7) *)
  | `Conceal  (** Hides text (SGR 8; limited support) *)
  | `Strikethrough  (** Line through text (SGR 9) *)
  | `Overline  (** Line above text (SGR 53; limited support) *)
  | `Framed  (** Frame border (SGR 51; rare support) *)
  | `Encircled  (** Circle border (SGR 52; rare support) *) ]
(** [style] represents text formatting attributes.

    Styles modify text appearance. Multiple styles can combine. Support varies
    by terminal:
    - Universal: Bold, Underline, Reverse
    - Common: Dim, Strikethrough, Italic
    - Limited: Double_underline, Blink, Conceal
    - Rare: Overline, Framed, Encircled

    Reset functions remove specific styles without affecting others. Use
    {!reset} to clear all attributes.

    Example:
    {[
      let title = Ansi.style [ `Bold; `Underline ] "Chapter 1"
      let secret = Ansi.style [ `Conceal ] "password123"
    ]} *)

type attr =
  [ `Fg of color  (** Sets foreground (text) color *)
  | `Bg of color  (** Sets background color *)
  | `Reset  (** Resets all attributes to defaults (SGR 0) *)
  | `No_bold  (** SGR 22: Neither bold nor dim *)
  | `No_dim  (** SGR 22: Neither bold nor dim *)
  | `No_italic  (** SGR 23 *)
  | `No_underline  (** SGR 24 *)
  | `No_blink  (** SGR 25 *)
  | `No_reverse  (** SGR 27 *)
  | `No_conceal  (** SGR 28 *)
  | `No_strikethrough  (** SGR 29 *)
  | `No_overline  (** SGR 55 *)
  | `No_framed  (** SGR 54 *)
  | `No_encircled  (** SGR 54 *)
  | style ]
(** [attr] combines colors and styles into display attributes.

    Attributes can be combined in {!sgr} or {!style}. Later attributes override
    earlier ones for the same property. [`Reset] clears all attributes; prefer
    targeted resets when preserving some attributes.

    Precedence rules:
    - Multiple [`Fg] values: last one wins
    - Multiple [`Bg] values: last one wins
    - [`Bold] and [`Dim]: both can be active unless [`Reset] intervenes
    - [`Reset]: clears everything, position in list matters

    Example:
    {[
      (* Bold red on blue background *)
      let attrs = Ansi.sgr [ `Bold; `Fg Red; `Bg Blue ]

      (* Override red with green *)
      let changed = Ansi.sgr [ `Fg Red; `Fg Green ] (* Results in green *)
    ]} *)

(** {1 Style Management} *)

type t = private int64
(** Represents the graphical styling of a single terminal cell. Bit-packed for
    performance with full RGB color support. The type is private to ensure
    styles are only created through the proper constructors. *)

val default : t
(** The default style attributes for a new terminal or after a reset. *)

val make :
  ?bold:bool ->
  ?dim:bool ->
  ?italic:bool ->
  ?underline:bool ->
  ?double_underline:bool ->
  ?fg:color ->
  ?bg:color ->
  ?reversed:bool ->
  ?strikethrough:bool ->
  ?overline:bool ->
  ?blink:bool ->
  unit ->
  t
(** Create a style with the given attributes. All attributes default to the
    default style values. *)

val apply_sgr_attr : t -> attr -> t
(** [apply_sgr_attr style attr] applies an ANSI SGR attribute to a style,
    returning the updated style. *)

val equal : t -> t -> bool
(** [equal a b] performs equality check on styles. More efficient than
    polymorphic equality. *)

(** {2 Style composition} *)

val merge : t -> t -> t
(** [merge parent child] merges two styles, with the child style taking
    precedence for any conflicting attributes.

    Merge rules:
    - Foreground color: child overrides parent if not Default
    - Background color: child overrides parent if not Default
    - Style flags: union of both (e.g., parent bold + child italic = both)
    - Link ID: child overrides parent if non-zero

    This is useful for composing styles in declarative UI frameworks where a
    component inherits a base style from its container but can override specific
    attributes. *)

val ( ++ ) : t -> t -> t
(** [parent ++ child] is an infix operator for [merge parent child]. The child
    style takes precedence over the parent style. *)

(** {2 Accessors} *)

val bold : t -> bool
val dim : t -> bool
val italic : t -> bool
val underline : t -> bool
val double_underline : t -> bool
val fg : t -> color
val bg : t -> color
val reversed : t -> bool
val strikethrough : t -> bool
val overline : t -> bool
val blink : t -> bool

(** {2 Style builders} *)

val with_fg : color -> t -> t
val with_bg : color -> t -> t
val with_bold : bool -> t -> t
val with_italic : bool -> t -> t
val with_underline : bool -> t -> t
val with_double_underline : bool -> t -> t
val with_strikethrough : bool -> t -> t
val with_reversed : bool -> t -> t
val with_blink : bool -> t -> t
val with_dim : bool -> t -> t
val with_overline : bool -> t -> t

(* Internal functions exposed for testing *)
val encode_color : color -> int64
val decode_color : int64 -> color

(* Link ID accessors for Grid.Storage *)
val get_link_id : t -> int
val set_link_id : t -> int -> t

val to_sgr : ?prev_style:t option -> t -> string
(** [to_sgr ?prev_style style] generates the ANSI SGR escape sequence string for this style.
      If [prev_style] is provided, it will detect when attributes are turning off and emit
      a reset if needed. Returns "\027[0m" for default style to ensure clean state. *)

val hash : t -> int
(** [hash style] returns a hash value for the style. Suitable for use with
    Hashtbl. *)

val of_int64 : int64 -> t
(** [of_int64 i] converts a raw int64 to a style. This is for internal use only
    when reading styles from storage. The int64 must have been created by a
    valid style constructor. *)

val color_to_codes : bg:bool -> color -> int list
(** [color_to_codes ~bg color] converts a color to a list of SGR codes. If [~bg]
    is true, it generates background color codes; otherwise, it generates
    foreground color codes. *)

val style_to_code : style -> int
(** [style_to_code style] converts a style to an SGR code. This is used for
    rendering styles in ANSI escape sequences. *)

val attr_to_codes : attr -> int list
(** [attr_to_codes attr] converts an attr to a list of SGR codes. This is used
    for rendering attributes in ANSI escape sequences. *)

(** {2 Pretty-printing} *)

val pp : Format.formatter -> t -> unit
(** [pp fmt style] pretty-prints a style for debugging. *)

val pp_color : Format.formatter -> color -> unit
(** [pp_color fmt color] pretty-prints a color value. *)

(** {2 Equality} *)

val equal_color : color -> color -> bool
(** [equal_color c1 c2] returns true if the two colors are equal. *)

(** {2 Alpha Blending} *)

val color_to_rgba : color -> int * int * int * int
(** [color_to_rgba color] converts any color to RGBA tuple (r, g, b, a) where
    each component is 0-255. Non-RGBA colors are treated as fully opaque
    (alpha=255). *)

val blend_colors : src:color -> dst:color -> color
(** [blend_colors ~src ~dst] performs alpha blending of src over dst using the
    standard Porter-Duff "over" operator. The result is always an opaque RGB
    color. *)
