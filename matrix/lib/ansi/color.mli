(** Color representations for ANSI terminal output.

    Provides a unified type for terminal colors supporting 16 basic ANSI colors,
    256-color palette, and 24-bit truecolor with optional alpha channels.
    Includes conversions between color spaces, color blending, terminal
    capability detection, and compact binary encoding.

    {1 Color Representations}

    Colors are represented using multiple schemes:

    - {b 16 Basic Colors}: Standard ANSI colors (black, red, etc.) and their
      bright variants
    - {b 256-Color Palette}: Extended palette with 216 colors (6x6x6 cube) plus
      24 grayscale levels
    - {b Truecolor RGB/RGBA}: 24-bit color with optional alpha for transparency

    All color values are clamped to valid ranges automatically. RGB/RGBA
    components use integer values 0-255. The 256-color extended palette uses
    indices 0-255, where indices 232-255 represent grayscale levels.

    {1 Usage Basics}

    Create colors using constructor functions:
    {[
      let red = Color.red in
      let custom = Color.of_rgb 100 150 200 in
      let gray = Color.grayscale ~level:12 in
      let from_hex = Color.of_hex_exn "#FF5733"
    ]}

    Convert between color spaces:
    {[
      let r, g, b = Color.to_rgb custom in
      let h, s, l, a = Color.to_hsl red in
      let downgraded = Color.downgrade ~level:`Ansi256 custom
    ]}

    {1 Color Blending}

    Blend colors using linear or perceptual blending:
    {[
      let blended = Color.blend ~mode:`Perceptual ~src:fg ~dst:bg () in
      (* Perceptual mode adjusts alpha to appear more natural to human vision *)
    ]}

    {1 Terminal Compatibility}

    The library automatically detects terminal capabilities from environment
    variables and can downgrade colors to supported levels. Use {!downgrade} to
    explicitly convert colors to a target color depth, or rely on automatic
    detection.

    {1 Allocation Behavior}

    Zero-allocation functions:
    - Basic color constructors ([of_rgb], [of_rgba], [of_palette_index],
      [grayscale])
    - Comparison functions ({!equal}, {!hash}, {!alpha})
    - SGR emission with {!emit_sgr_codes}
    - Binary encoding with {!pack}/{!unpack}

    Functions that allocate:
    - {!to_rgb}, {!to_rgba}, {!to_rgba_f} - allocate tuples
    - {!to_sgr_codes} - allocates a list
    - {!to_hsl} - allocates a tuple
    - {!of_hex}, {!of_hex_exn} - allocate strings for normalization
    - {!to_hex} - allocates a string
    - {!blend} - allocates float tuples internally

    {1 Contracts}

    - Constructors clamp components to their valid ranges instead of raising.
    - [{!Default}] represents "use terminal default" and is encoded as RGBA
      (0,0,0,0); any function returning RGBA values documents how to interpret
      that sentinel.
    - Palette indices are coerced into their supported range \[0,255\] to match
      terminal behaviour.
    - All conversion helpers preserve semantic equality: converting to packed
      form and back through {!pack}/{!unpack} round-trips exactly, including
      alpha and [{!Default}]'s sentinel value. *)

type t =
  | Default  (** Terminal default color (foreground or background). *)
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Bright_black  (** Often rendered as gray. *)
  | Bright_red
  | Bright_green
  | Bright_yellow
  | Bright_blue
  | Bright_magenta
  | Bright_cyan
  | Bright_white
  | Extended of int
      (** Extended palette color with index 0-255.

          Indices map directly to the 256-color palette:
          - 0-15: Basic 16 colors (same as {!Black} through {!Bright_white})
          - 16-231: 6x6x6 RGB cube (r*36 + g*6 + b + 16 where r,g,b ∈ \[0,5\])
          - 232-255: 24-level grayscale ramp

          Values outside \[0, 255\] are clamped automatically. *)
  | Rgb of { r : int; g : int; b : int }
      (** Truecolor RGB.

          Components are 8-bit values in \[0, 255\], clamped automatically.
          Rendered using ANSI truecolor escape sequences (38;2;r;g;b for
          foreground). *)
  | Rgba of { r : int; g : int; b : int; a : int }
      (** Truecolor RGBA with alpha channel.

          Alpha channel (a) represents opacity in \[0, 255\] where 0 is fully
          transparent and 255 is fully opaque. Alpha is used for blending
          operations but is not directly supported by terminal escape sequences.
      *)

(** {1 Basic Color Constructors} *)

val default : t
(** Terminal's default color (depends on user's terminal configuration). *)

val black : t
val red : t
val green : t
val yellow : t
val blue : t
val magenta : t
val cyan : t
val white : t

val bright_black : t
(** Often displayed as gray. *)

val bright_red : t
val bright_green : t
val bright_yellow : t
val bright_blue : t
val bright_magenta : t
val bright_cyan : t
val bright_white : t

(** {1 Color Construction} *)

val grayscale : level:int -> t
(** [grayscale ~level] creates a grayscale color from the 24-level ramp.

    @param level
      Grayscale intensity in \[0, 23\] where 0 is darkest and 23 is lightest.
      Values outside this range are clamped automatically.

    Maps to Extended palette indices 232-255. *)

val of_rgb : int -> int -> int -> t
(** [of_rgb r g b] creates a truecolor RGB value.

    All components are clamped to \[0, 255\]. Produces an opaque color (alpha =
    255). *)

val of_rgba : int -> int -> int -> int -> t
(** [of_rgba r g b a] creates a truecolor RGBA value with transparency.

    All components are clamped to \[0, 255\]. *)

val of_rgba_f : float -> float -> float -> float -> t
(** [of_rgba_f r g b a] creates a color from normalized RGBA floats in
    \[0.0, 1.0\].

    Components are clamped to \[0.0, 1.0\] and converted to 8-bit integers.
    Inverse of {!to_rgba_f}: [of_rgba_f] and {!to_rgba_f} round-trip within
    rounding tolerance. *)

val of_palette_index : int -> t
(** [of_palette_index idx] creates a color from 256-color palette index.

    @param idx Palette index in \[0, 255\], clamped automatically.

    Indices 0-15 map to basic ANSI colors, 16-231 to the 6x6x6 RGB cube, and
    232-255 to the 24-level grayscale ramp. Returns the most specific color
    variant for the index. *)

val of_hsl : h:float -> s:float -> l:float -> ?a:float -> unit -> t
(** [of_hsl ~h ~s ~l ?a ()] creates a color from HSL values.

    @param h
      Hue in degrees \[0.0, 360.0\), wrapped automatically. Negative values are
      normalized (e.g., -10 becomes 350).
    @param s Saturation in \[0.0, 1.0\], clamped automatically.
    @param l Lightness in \[0.0, 1.0\], clamped automatically.
    @param a
      Alpha in \[0.0, 1.0\], defaults to 1.0 (opaque), clamped automatically.

    Returns {!Rgb} if alpha is 1.0, otherwise {!Rgba}. *)

(** {1 Color Comparisons} *)

val equal : t -> t -> bool
(** [equal a b] tests equality based on RGBA components.

    {b Zero-allocation}: uses packed integer comparison internally.

    Colors with different representations but identical RGBA values are equal.
    For example, [Extended 0] equals [Black] since both map to the same RGB. *)

val compare : t -> t -> int
(** [compare a b] returns a total ordering over colors based on RGBA components.

    {b Zero-allocation}: uses packed integer comparison internally.

    Consistent with {!equal}. Suitable for use as keys in [Map] or [Set]. *)

val hash : t -> int
(** [hash color] computes a hash from RGBA components.

    {b Zero-allocation}: uses packed integer representation.

    Suitable for hash tables. Consistent with {!equal}. *)

(** {1 Color Properties} *)

val alpha : t -> float
(** [alpha color] returns the alpha channel as a float in \[0.0, 1.0\].

    {b Zero-allocation}: uses direct pattern matching.

    Non-RGBA colors (basic, extended and RGB variants) return 1.0 (fully
    opaque). {!Default} returns 0.0 to preserve the "use terminal default"
    sentinel; check [alpha c = 0.0] before interpreting the corresponding RGB
    components. *)

val to_rgba_f : t -> float * float * float * float
(** [to_rgba_f color] converts to normalized RGBA floats in \[0.0, 1.0\].

    {b Allocates} a 4-tuple. For {!Default}, returns [(0.0, 0.0, 0.0, 0.0)]; the
    zero alpha plays the same sentinel role as in {!to_rgba}. *)

val with_rgba_f : t -> (float -> float -> float -> float -> 'a) -> 'a
(** [with_rgba_f color f] calls [f r g b a] with normalized RGBA floats in
    \[0.0, 1.0\].

    {b Zero-allocation}: avoids the 4-tuple allocated by {!to_rgba_f}.
    Equivalent to [let r, g, b, a = to_rgba_f color in f r g b a] but without
    the intermediate tuple. *)

val to_rgba : t -> int * int * int * int
(** [to_rgba color] converts to RGBA integers in \[0, 255\].

    {b Allocates} a 4-tuple. Non-RGBA colors return alpha = 255. {!Default}
    returns [(0, 0, 0, 0)] with alpha = 0 as a sentinel to indicate terminal
    default. When alpha = 0, renderers must detect and substitute configured
    defaults; compare alpha before using RGB components. *)

val to_rgb : t -> int * int * int
(** [to_rgb color] converts to RGB integers in \[0, 255\], discarding alpha.

    {b Allocates} a 3-tuple. [{!Default}] is mapped to [(0, 0, 0)] here; use
    {!alpha} or {!to_rgba} if you need to distinguish it from an explicit black.
*)

val to_hsl : t -> float * float * float * float
(** [to_hsl color] converts to HSL color space.

    {b Allocates} a 4-tuple.

    @return
      [(h, s, l, a)] where h is hue in \[0.0, 360.0\), s and l are in \[0.0,
      1.0\], and a is alpha in \[0.0, 1.0\]. Achromatic colors (grays) have hue
      = 0. *)

(** {1 Color Operations} *)

val blend : ?mode:[ `Linear | `Perceptual ] -> src:t -> dst:t -> unit -> t
(** [blend ~mode ~src ~dst ()] alpha-blends [src] over [dst].

    @param mode
      Blending algorithm:
      - [`Linear]: Standard alpha compositing
      - [`Perceptual]: Adjusts alpha using perceptual curves for more natural
        appearance, especially for semi-transparent overlays

    If [src] has alpha ≥ 0.999, returns [src] RGB (fully opaque). If alpha ≤ ε,
    returns [dst] unchanged. The resulting alpha is
    [src_a + dst_a * (1 - src_a)].

    @return
      A {!Rgb} or {!Rgba} color depending on the blended alpha channel. The
      destination is left untouched so it can be reused. *)

val downgrade : ?level:[ `Ansi16 | `Ansi256 | `Truecolor ] -> t -> t
(** [downgrade ~level color] converts to the specified color depth.

    @param level
      Target color level. If omitted, detects from environment variables
      (COLORTERM, TERM).

    Uses nearest-neighbor matching in RGB space (squared Euclidean distance).
    [`Truecolor] returns the color unchanged. Color quantization may be visually
    inaccurate for gradients; dithering not supported. *)

val invert : t -> t
(** [invert color] inverts RGB components.

    Maps each component [c] to [255 - c]. The result is always an opaque RGB
    color; any existing alpha channel is discarded. Use {!blend} if you need to
    keep transparency while inverting colors. *)

(** {1 ANSI Escape Sequence Generation} *)

val to_sgr_codes : bg:bool -> t -> int list
(** [to_sgr_codes ~bg color] converts to SGR parameter codes.

    {b Allocates} a list. Use {!emit_sgr_codes} for a zero-allocation
    alternative.

    @param bg
      If [true], generates background codes; if [false], foreground codes.

    Emits the most efficient SGR sequence for each color type:
    - Basic colors: single code (30-37 / 40-47 for normal, 90-97 / 100-107 for
      bright)
    - Extended palette: 38;5;idx / 48;5;idx
    - RGB/RGBA: truecolor 38;2;r;g;b / 48;2;r;g;b *)

val emit_sgr_codes : bg:bool -> (int -> unit) -> t -> unit
(** [emit_sgr_codes ~bg push color] emits SGR codes via a push callback.

    {b Zero-allocation}: pattern matching is inlined to avoid constructing
    intermediate tuples or lists. Calls the [push] function for each SGR
    parameter code.

    @param bg If [true], emits background codes; if [false], foreground codes.
    @param push Callback function called for each SGR code *)

(** {1 Binary Encoding} *)

val pack : t -> int64
(** [pack color] encodes to a 64-bit integer for compact storage.

    Encoding uses tagged representation:
    - Default: 0
    - Basic colors (Black-Bright_white): tag 1 + index
    - Extended: tag 2 + clamped index
    - RGB: tag 3 + 24-bit RGB value
    - RGBA: tag 4 + 32-bit RGBA value

    Invariant: [equal color (unpack (pack color))]. *)

val unpack : int64 -> t
(** [unpack bits] decodes a packed color.

    Invalid tags decode to {!Default}. *)

(** {1 String Conversions} *)

val pp : Format.formatter -> t -> unit
(** [pp fmt color] prints a human-readable representation.

    Examples: ["Red"], ["Rgb(100,150,200)"], ["Extended(42)"]. *)

val to_hex : t -> string
(** [to_hex color] converts to a hex color string.

    @return ["#RRGGBB"] format (always 6 hex digits). Alpha channel is ignored.
*)

val of_hex : string -> t option
(** [of_hex hex] parses a hex color string.

    Accepted formats (with or without '#' prefix):
    - 3 digits: ["RGB"] expands to ["RRGGBB"]
    - 4 digits: ["RGBA"] expands to ["RRGGBBAA"]
    - 6 digits: ["RRGGBB"] (opaque)
    - 8 digits: ["RRGGBBAA"] (with alpha)

    @return [None] if parsing fails due to invalid format or non-hex characters.
*)

val of_hex_exn : string -> t
(** [of_hex_exn hex] parses a hex color string.

    @raise Invalid_argument if parsing fails. *)
