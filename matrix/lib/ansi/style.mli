(** Text styling composition and management.

    This module aggregates foreground color, background color, text attributes,
    and hyperlinks into immutable [Style.t] objects.

    {1 Overview}

    A style consists of four components:

    - {b Colors}: Optional foreground and background. [None] inherits the
      terminal's current color.
    - {b Attributes}: A bitmask of flags (e.g., bold, italic).
    - {b Hyperlink}: An optional OSC 8 hyperlink URL.

    Styles are composed using overlay semantics: colors and hyperlinks from the
    overlay replace those in the base, while attributes are unioned.

    {1 Usage Basics}

    Create and combine styles:
    {[
      let error = Style.make ~fg:Color.red ~bold:true ()
      let base = Style.make ~bg:Color.white ()
      let combined = base ++ error
    ]} *)

type t = private {
  fg : Color.t option;
  bg : Color.t option;
  attrs : Attr.t;
  link : string option;
}
(** The read-only style definition.

    Fields are exposed for pattern matching but cannot be modified directly. Use
    {!make} or the modifier functions to create new instances. *)

(** {1 Predefined Styles} *)

val default : t
(** [default] is the empty style.

    It contains no colors, no attributes, and no hyperlink. Emitting this style
    performs a reset if the previous style was different. *)

val error : t
(** [error] is a standard style for errors.

    Defined as bright red foreground. *)

val success : t
(** [success] is a standard style for success messages.

    Defined as bright green foreground. *)

val warning : t
(** [warning] is a standard style for warnings.

    Defined as bright yellow foreground. *)

val info : t
(** [info] is a standard style for informational messages.

    Defined as bright blue foreground. *)

(** {1 Construction} *)

val make :
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
  unit ->
  t
(** [make ?fg ?bg ... ()] creates a new style with the specified properties.

    All parameters are optional:
    - [fg] and [bg] default to [None] (inherit terminal default).
    - All boolean attributes (e.g., [bold], [italic]) default to [false].
    - [link] defaults to [None] (no hyperlink). *)

(** {1 Modifiers} *)

(** {2 Colors} *)

val fg : Color.t -> t -> t
(** [fg color t] sets the foreground color of [t] to [color]. *)

val bg : Color.t -> t -> t
(** [bg color t] sets the background color of [t] to [color]. *)

val with_no_fg : t -> t
(** [with_no_fg t] removes the foreground color from [t].

    The resulting style will inherit the terminal's default foreground. *)

val with_no_bg : t -> t
(** [with_no_bg t] removes the background color from [t].

    The resulting style will inherit the terminal's default background. *)

(** {2 Attributes} *)

val with_attrs : Attr.t -> t -> t
(** [with_attrs attrs t] replaces the attribute set of [t] with [attrs]. *)

val overlay_attrs : t -> Attr.t -> t
(** [overlay_attrs t attrs] adds [attrs] to the existing attributes of [t].

    Equivalent to taking the union of [t.attrs] and [attrs]. *)

val add_attr : Attr.flag -> t -> t
(** [add_attr flag t] enables the specific attribute [flag] in [t]. *)

val remove_attr : Attr.flag -> t -> t
(** [remove_attr flag t] disables the specific attribute [flag] in [t]. *)

val with_bold : bool -> t -> t
(** [with_bold enabled t] sets or clears the bold attribute. *)

val with_dim : bool -> t -> t
(** [with_dim enabled t] sets or clears the dim attribute. *)

val with_italic : bool -> t -> t
(** [with_italic enabled t] sets or clears the italic attribute. *)

val with_underline : bool -> t -> t
(** [with_underline enabled t] sets or clears the underline attribute. *)

val with_double_underline : bool -> t -> t
(** [with_double_underline enabled t] sets or clears the double-underline
    attribute. *)

val with_blink : bool -> t -> t
(** [with_blink enabled t] sets or clears the blink attribute. *)

val with_inverse : bool -> t -> t
(** [with_inverse enabled t] sets or clears the inverse attribute (swap
    foreground and background). *)

val with_hidden : bool -> t -> t
(** [with_hidden enabled t] sets or clears the hidden attribute. *)

val with_strikethrough : bool -> t -> t
(** [with_strikethrough enabled t] sets or clears the strikethrough attribute.
*)

val with_overline : bool -> t -> t
(** [with_overline enabled t] sets or clears the overline attribute. *)

val with_framed : bool -> t -> t
(** [with_framed enabled t] sets or clears the framed attribute. *)

val with_encircled : bool -> t -> t
(** [with_encircled enabled t] sets or clears the encircled attribute. *)

(** {2 Hyperlinks} *)

val hyperlink : string -> t -> t
(** [hyperlink url t] sets the OSC 8 hyperlink URL of [t].

    If [url] is empty, behavior depends on the terminal, but typically no link
    is created. *)

val link : t -> string option
(** [link t] returns the current hyperlink URL of [t], if any. *)

val unlink : t -> t
(** [unlink t] removes the hyperlink from [t]. *)

(** {1 Composition} *)

val merge : base:t -> overlay:t -> t
(** [merge ~base ~overlay] combines two styles.

    The composition rules are:
    - {b Colors}: [overlay] takes precedence. If [overlay.fg] is [None],
      [base.fg] is kept.
    - {b Link}: [overlay] takes precedence.
    - {b Attributes}: The union of [base.attrs] and [overlay.attrs] is used. *)

val ( ++ ) : t -> t -> t
(** [base ++ overlay] is an infix alias for {!merge}.

    Example:
    {[
      let s = Style.default ++ Style.make ~bold:true () ++ Style.fg Color.red
    ]} *)

val resolve : t list -> t
(** [resolve styles] merges a list of styles from left to right.

    Starts with {!default} as the accumulator. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** [equal a b] tests structural equality.

    Returns [true] if all colors, attributes, and links are identical. *)

val compare : t -> t -> int
(** [compare a b] returns a total ordering for styles.

    {b Performance Note}: Comparison is optimized. It uses integer operations
    for attributes and packed 64-bit integer comparisons for colors, avoiding
    heavy structural recursion or float allocation.

    This function is suitable for using styles as keys in [Map] or [Set]. *)

val hash : t -> int
(** [hash t] computes a hash value for the style.

    Compatible with {!equal}. *)

(** {1 Emission} *)

val to_sgr_codes : ?prev:t -> t -> int list
(** [to_sgr_codes ?prev t] calculates the minimal list of SGR integer codes
    required to transition from [prev] to [t].

    {ul
     {- If [prev] is omitted, it defaults to {!default}. }
     {- Returns an empty list if [prev] and [t] are equal. }
     {- If [t] is {!default}, returns [[0]] (reset). }
     {- Otherwise, returns only the codes needed to:
        - Disable attributes present in [prev] but not in [t]
        - Change foreground color (or [[39]] to reset to default)
        - Change background color (or [[49]] to reset to default)
        - Enable attributes present in [t] but not in [prev]
     }
    }

    {b Note}: Allocates a list. Use {!emit} for allocation-sensitive code. *)

val sgr_sequence : ?prev:t -> t -> string
(** [sgr_sequence ?prev t] returns the ANSI escape sequence string for [t].

    Equivalent to converting the result of {!to_sgr_codes} to a string. Returns
    an empty string if no transition is required. *)

val emit : ?prev:t -> t -> Writer.t -> unit
(** [emit ?prev t writer] writes the minimal SGR codes to [writer].

    Computes the minimal state difference between [prev] (defaulting to
    {!default}) and [t], emitting only the codes necessary to transition:

    - Attribute disable codes for attributes being removed
    - Color codes only if colors changed (or [[39]]/[[49]] to reset to default)
    - Attribute enable codes for attributes being added

    Handles shared disable codes correctly: Bold/Dim share code 22,
    Underline/Double_underline share 24, Framed/Encircled share 54.

    {b Note}: This function emits SGR sequences only (colors/attributes). The
    [link] field is not emitted here. For hyperlink support, use {!Ansi.render}
    or {!Ansi.emit} which handle OSC 8 sequences.

    {b Example}: Transitioning from [bold + red] to [italic + red] emits only
    [[22;3]] (disable bold, enable italic), not [[0;3;38;2;255;0;0]]. *)

val styled : ?reset:bool -> t -> string -> string
(** [styled ?reset t s] returns the string [s] wrapped in the escape codes for
    [t].

    If [reset] is [true] (default: [false]), the function appends a reset
    sequence ([ESC\[0m]) to the end of the string. *)

(** {1 Debugging} *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] prints a human-readable representation of the style.

    Example output: [Style{fg=#FF0000, attrs=[Bold, Underline]}] *)
