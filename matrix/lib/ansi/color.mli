(** Terminal colors.

    Colors preserve their terminal emission intent: literal RGB, indexed
    palette, or terminal default. The RGB channels are also stored as a visual
    snapshot so callers can still blend, compare, and inspect colors without
    losing whether the renderer should emit [38;2], [38;5], or [39]/[49].

    Constructors clamp components to their valid ranges instead of raising. *)

(** {1:colors Colors} *)

type t
(** The type for terminal colors.

    Invariant: values built by this module contain clamped 8-bit RGBA channels
    and one of the intents in {!type-intent}. *)

type intent =
  | Default
  | Indexed of int
  | Rgb
      (** The terminal emission intent of a color. [Indexed i] has [i] in
          \[0,255\]. [Rgb] denotes literal truecolor RGBA. [Default] denotes
          terminal default foreground/background. *)

(** {1:constructors Constructors} *)

val default_fg : t
(** [default_fg] is the opaque terminal-default foreground, with a white RGB
    snapshot for compositing. Emits SGR [39] in the foreground. *)

val default_bg : t
(** [default_bg] is the opaque terminal-default background, with a black RGB
    snapshot for compositing. Emits SGR [49] in the background. *)

val transparent : t
(** [transparent] is transparent black. Compositing it leaves the destination
    unchanged. *)

val black : t
val red : t
val green : t
val yellow : t
val blue : t
val magenta : t
val cyan : t
val white : t
val bright_black : t
val bright_red : t
val bright_green : t
val bright_yellow : t
val bright_blue : t
val bright_magenta : t
val bright_cyan : t
val bright_white : t

val of_rgb : int -> int -> int -> t
(** [of_rgb r g b] is a literal truecolor RGB value. Components are clamped to
    \[0,255\]. The result is opaque. *)

val of_rgba : int -> int -> int -> int -> t
(** [of_rgba r g b a] is a literal truecolor RGBA value. Components are clamped
    to \[0,255\]. Alpha is used for blending and transparent SGR defaults. *)

val of_rgba_f : float -> float -> float -> float -> t
(** [of_rgba_f r g b a] is a color from normalized RGBA floats in \[0.0,1.0\].
    Components are clamped and converted to 8-bit integers. Inverse of
    {!to_rgba_f} within rounding tolerance. *)

val indexed : int -> t
(** [indexed i] is a color from the 256-color palette at [i], clamped to
    \[0,255\]. The color keeps the indexed intent and an RGB fallback snapshot.
*)

val of_palette_index : int -> t
(** [of_palette_index] is {!indexed}. *)

val grayscale : level:int -> t
(** [grayscale ~level] is a grayscale palette color. [level] is in \[0,23\]
    where [0] is darkest and [23] lightest; out-of-range values are clamped. *)

val of_hsl : h:float -> s:float -> l:float -> ?a:float -> unit -> t
(** [of_hsl ~h ~s ~l ?a ()] is a literal RGB/RGBA color from HSL values with:
    - [h] is hue in degrees \[0.0,360.0\), wrapped.
    - [s] is saturation in \[0.0,1.0\], clamped.
    - [l] is lightness in \[0.0,1.0\], clamped.
    - [a] is alpha in \[0.0,1.0\], defaults to [1.0], clamped. *)

val of_hex : string -> t option
(** [of_hex s] parses a literal RGB/RGBA hex color string. Accepted formats with
    or without ['#'] prefix are ["RGB"], ["RGBA"], ["RRGGBB"], and ["RRGGBBAA"].
    Returns [None] on invalid input. *)

val of_hex_exn : string -> t
(** [of_hex_exn s] is like {!of_hex} but raises [Invalid_argument] on invalid
    input. *)

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have identical RGBA channels and
    identical emission intent. The order is compatible with {!compare}. *)

val equal_rgba : t -> t -> bool
(** [equal_rgba a b] is [true] iff [a] and [b] have identical RGBA channels,
    ignoring emission intent. *)

val compare : t -> t -> int
(** [compare a b] orders [a] and [b] by channels and emission intent. The order
    is compatible with {!equal}. *)

val hash : t -> int
(** [hash c] hashes [c]'s channels and emission intent. Compatible with
    {!equal}. *)

(** {1:properties Properties} *)

val intent : t -> intent
(** [intent c] is [c]'s terminal emission intent. *)

val alpha : t -> float
(** [alpha c] is the alpha channel of [c] as a float in \[0.0,1.0\]. *)

val to_rgb : t -> int * int * int
(** [to_rgb c] is the RGB snapshot of [c] in \[0,255\], discarding alpha. *)

val to_rgba : t -> int * int * int * int
(** [to_rgba c] is the RGBA snapshot of [c] in \[0,255\]. *)

val to_rgba_f : t -> float * float * float * float
(** [to_rgba_f c] is the RGBA snapshot of [c] as normalized floats in
    \[0.0,1.0\]. *)

val with_rgba_f : t -> (float -> float -> float -> float -> 'a) -> 'a
(** [with_rgba_f c f] calls [f r g b a] with normalized RGBA floats. Avoids the
    tuple allocation of {!to_rgba_f}. *)

val to_hsl : t -> float * float * float * float
(** [to_hsl c] is [(h, s, l, a)] where [h] is hue in \[0.0,360.0\), [s] and [l]
    are in \[0.0,1.0\], and [a] is alpha in \[0.0,1.0\]. *)

(** {1:operations Operations} *)

val blend : src:t -> dst:t -> t
(** [blend ~src ~dst] composites [src] over [dst] using source-over alpha
    blending on the stored RGB snapshots. Transparent [src] returns [dst];
    opaque [src] returns [src], preserving terminal intent. Other results are
    literal RGB colors with accumulated alpha. *)

val invert : t -> t
(** [invert c] maps each RGB component [v] to [255 - v]. The result is an opaque
    literal RGB color. *)

(** {1:depth Color depth}

    A terminal shows colors at one of a few depths. With direct color it shows
    any RGB value ([38;2]). With 256 colors it shows the entries of xterm's
    palette ([38;5]): 0 to 15 are the 16 colors below, 16 to 231 a 6×6×6 cube
    whose levels in each channel are 0, 95, 135, 175, 215 and 255, and 232 to
    255 a ramp of grays from 8 to 238 by steps of 10, "intentionally leaving out
    black and white" (xterm, [256colres.pl]). With 16 colors it shows entries 0
    to 15 ([30]..[37], [90]..[97]), with 8 colors 0 to 7, and a monochrome
    terminal shows only its default colors.

    Entries 16 to 255 are the same in every terminal that has them. Entries 0 to
    15 are not: xterm takes them from resources, "usually more-or-less a
    reproduction of the standard ANSI colors, but possibly more pleasing shades"
    (256colres.pl), and users and themes set them. A program cannot know them;
    {!indexed} gives them the 16 colors of HTML 4.01 (maroon [#800000], green
    [#008000], ..., silver [#c0c0c0], gray [#808080], red [#ff0000], ..., white
    [#ffffff]), which this module uses wherever it needs their RGB. *)

type depth = [ `Truecolor | `Ansi256 | `Ansi16 | `Ansi8 | `Mono ]
(** The type for color depths: direct color, 256, 16 and 8 colors, and none. *)

val to_depth : depth -> t -> t
(** [to_depth d c] is [c] as a terminal of depth [d] can show it:
    - A {!Default} color is unchanged at every depth, and so is every color at
      [`Truecolor].
    - At [`Mono] every color becomes the terminal default, keeping [c]'s RGBA
      snapshot.
    - At [`Ansi256], [`Ansi16] and [`Ansi8], a color already {!Indexed} within
      the depth, or with alpha [0], is unchanged. Any other color becomes the
      palette entry nearest to its RGB: one of 16 to 255 at [`Ansi256], one of 0
      to 15 at [`Ansi16], one of 0 to 7 at [`Ansi8]. It keeps [c]'s alpha.

    Nearest is by the squared Euclidean distance between RGB values, a tie going
    to the higher entry. This is how xterm itself shows a direct color when it
    is built without direct-color support: "it uses the closest match in its
    palette for the given RGB" (ctlseqs, SGR 38), by that distance
    (xtermClosestColor in xterm's misc.c). A perceptual distance, such as
    CIELAB's, would match some colors differently; this one gives what xterm
    shows. At [`Ansi256] the search leaves out entries 0 to 15, whose colors are
    the user's, so the result shows the color it was chosen for. At [`Ansi16]
    and [`Ansi8] there is nothing else, and the result is nearest by the colors
    {!indexed} assumes.

    [to_depth] reads no environment: the depth is the caller's, from what the
    terminal reports. *)

(** {1:sgr ANSI SGR codes} *)

val to_sgr_codes : bg:bool -> t -> int list
(** [to_sgr_codes ~bg c] is the SGR parameter codes for [c]. If [bg] is [true],
    generates background codes; otherwise foreground codes. *)

val emit_sgr_codes : bg:bool -> (int -> unit) -> t -> unit
(** [emit_sgr_codes ~bg push c] emits SGR codes for [c] via [push].
    Zero-allocation alternative to {!to_sgr_codes}. *)

(** {1:packed Packed representation} *)

module Packed : sig
  (** Low-level packed color operations.

      Packed colors are unboxed integers for render and grid hot paths. The bit
      layout is private to this module; callers may store and compare packed
      values, but must use these operations to inspect or emit them.

      Requires a 64-bit platform ([Sys.int_size >= 62]). *)

  type color = t
  (** The unpacked color type. *)

  val encode : color -> int
  (** [encode c] is [c] encoded as an unboxed integer. *)

  val decode : int -> color
  (** [decode bits] decodes [bits]. Invalid intent tags decode to {!default_bg}.
  *)

  val red : int -> int
  val green : int -> int
  val blue : int -> int

  val alpha : int -> int
  (** Color channels in \[0,255\]. *)

  val red_f : int -> float
  val green_f : int -> float
  val blue_f : int -> float

  val alpha_f : int -> float
  (** Color channels in \[0.0,1.0\]. *)

  val intent : int -> intent
  (** [intent bits] is the terminal emission intent encoded in [bits]. *)

  val indexed_slot : int -> int
  (** [indexed_slot bits] is the indexed color slot encoded in [bits]. The value
      is meaningful only when [intent bits] is [Indexed _]. *)

  val with_alpha : int -> int -> int
  (** [with_alpha color a] replaces the alpha channel, clamped to \[0,255\],
      preserving RGB channels and terminal intent. *)

  val blend : src:int -> dst:int -> int
  (** [blend ~src ~dst] composites packed [src] over [dst] with source-over
      alpha blending, without allocation. *)

  val of_rgba_f : float -> float -> float -> float -> int
  (** [of_rgba_f r g b a] is a packed literal RGBA color from normalized
      channels. *)
end

(** {1:converting Converting} *)

val to_hex : t -> string
(** [to_hex c] is the ["#RRGGBB"] hex string for [c]. Alpha and intent are
    ignored. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats [c] for inspection. *)
