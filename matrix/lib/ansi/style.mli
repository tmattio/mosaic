(** Styles, the colors, attributes and hyperlink of shown text.

    A {e style} gives text an optional foreground and background color
    ({!Color}), a set of attributes ({!Attr}), and an optional hyperlink, a URL
    that OSC 8 attaches to the text. A style is immutable.

    Styles compose by {e merging} an {e overlay} over a {e base} ({!merge}), so
    that a style drawn over a theme refines it. The merge has the overlay's
    colors and link where the overlay has them and the base's otherwise, and the
    attributes of both. An absent color or link therefore inherits, and a merge
    never turns an attribute off. A program that needs one off, or no link,
    applies a modifier, such as {!with_bold}[ false] or {!unlink}, to the merged
    style.

    Written to a terminal, an absent color is the terminal's default color.
    {!emit} writes the SGR codes that turn one style into another, and writes no
    hyperlink, which {!Ansi.render} and {!Sgr_state} handle.

    This makes a title bold over a theme and prints it. The merge keeps the
    theme's colors, since the overlay has none:
    {[
    let theme = Ansi.Style.make ~fg:Ansi.Color.white ~bg:Ansi.Color.blue () in
    let title = Ansi.Style.(theme ++ make ~bold:true ()) in
    print_string (Ansi.Style.styled ~reset:true title "Report")
    ]} *)

(** {1:styles Styles} *)

type t = private {
  fg : Color.t option;
  bg : Color.t option;
  attrs : Attr.t;
  link : string option;
}
(** The type for styles. A program reads the fields by pattern matching and
    builds a style with this module's functions. *)

val default : t
(** [default] is the style without colors, attributes or hyperlink. *)

(** {1:constructors Constructors} *)

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
(** [make ?fg ?bg ... ?link ()] is the style with the colors [fg] and [bg], the
    attributes whose arguments are [true], and the hyperlink [link]. An omitted
    color or link is absent, and an omitted attribute is off. *)

(** {1:modifiers Modifiers}

    A modifier is the style it is given with one property changed. *)

(** {2:mod_colors Colors} *)

val fg : Color.t -> t -> t
(** [fg c s] is [s] with foreground [c]. *)

val bg : Color.t -> t -> t
(** [bg c s] is [s] with background [c]. *)

val with_no_fg : t -> t
(** [with_no_fg s] is [s] without a foreground. *)

val with_no_bg : t -> t
(** [with_no_bg s] is [s] without a background. *)

(** {2:mod_attrs Attributes}

    [with_bold b s] to [with_encircled b s] are [s] with one attribute on if [b]
    is [true] and off otherwise. *)

val with_attrs : Attr.t -> t -> t
(** [with_attrs a s] is [s] with the attributes [a] in place of its own. *)

val overlay_attrs : t -> Attr.t -> t
(** [overlay_attrs s a] is [s] with the attributes [a] added to its own. *)

val add_attr : Attr.flag -> t -> t
(** [add_attr f s] is [s] with [f] on. *)

val remove_attr : Attr.flag -> t -> t
(** [remove_attr f s] is [s] with [f] off. *)

val with_bold : bool -> t -> t
(** [with_bold b s] is [s] with bold [b]. *)

val with_dim : bool -> t -> t
(** [with_dim b s] is [s] with dim [b]. *)

val with_italic : bool -> t -> t
(** [with_italic b s] is [s] with italic [b]. *)

val with_underline : bool -> t -> t
(** [with_underline b s] is [s] with underline [b]. *)

val with_double_underline : bool -> t -> t
(** [with_double_underline b s] is [s] with double underline [b]. *)

val with_blink : bool -> t -> t
(** [with_blink b s] is [s] with blink [b]. *)

val with_inverse : bool -> t -> t
(** [with_inverse b s] is [s] with inverse [b]. *)

val with_hidden : bool -> t -> t
(** [with_hidden b s] is [s] with hidden [b]. *)

val with_strikethrough : bool -> t -> t
(** [with_strikethrough b s] is [s] with strikethrough [b]. *)

val with_overline : bool -> t -> t
(** [with_overline b s] is [s] with overline [b]. *)

val with_framed : bool -> t -> t
(** [with_framed b s] is [s] with framed [b]. *)

val with_encircled : bool -> t -> t
(** [with_encircled b s] is [s] with encircled [b]. *)

(** {2:mod_links Hyperlinks} *)

val hyperlink : string -> t -> t
(** [hyperlink url s] is [s] linked to [url]. *)

val link : t -> string option
(** [link s] is the URL [s] links to, if any. *)

val unlink : t -> t
(** [unlink s] is [s] without a hyperlink. *)

(** {1:composition Merging} *)

val merge : base:t -> overlay:t -> t
(** [merge ~base ~overlay] is the merge of [overlay] over [base]. *)

val ( ++ ) : t -> t -> t
(** [base ++ overlay] is {!merge}[ ~base ~overlay]. *)

val resolve : t list -> t
(** [resolve ss] is {!default} with each style of [ss] merged over it in turn,
    from first to last. *)

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have equal colors ({!Color.equal}),
    attributes and links. *)

val compare : t -> t -> int
(** [compare a b] is a total order on styles, compatible with {!equal}. *)

val hash : t -> int
(** [hash s] is a hash of [s], compatible with {!equal}. *)

(** {1:emission Writing}

    The {e transition} from a style [prev] to a style [s] is the SGR codes that
    make a terminal showing [prev] show [s]. Its codes are those that clear
    attributes, then those that set the foreground and the background where they
    change, then those that set attributes. An attribute of [s] that shares its
    clearing code with a cleared one is set again. The transition ignores links.

    A transition is written for a terminal of a color {e depth}
    ({!Color.type-depth}), [`Truecolor] unless [depth] says otherwise. Its
    colors are reduced by {!Color.to_depth} and written by
    {!Color.emit_sgr_codes} at that depth, as {!Sgr_state} writes them. *)

val to_sgr_codes : ?depth:Color.depth -> ?prev:t -> t -> int list
(** [to_sgr_codes ~depth ~prev s] is the codes of the transition from [prev] to
    [s] at [depth]. It is [[]] if [prev] and [s] have the same colors and
    attributes, and [[0]] if [s] has those of {!default} and [prev] does not.
    [prev] defaults to {!default}. *)

val sgr_sequence : ?depth:Color.depth -> ?prev:t -> t -> string
(** [sgr_sequence ~depth ~prev s] is the SGR sequence of
    {!to_sgr_codes}[ ~depth ~prev s], and [""] if that is [[]]. *)

val emit : ?depth:Color.depth -> ?prev:t -> t -> Writer.t -> unit
(** [emit ~depth ~prev s w] writes to [w] the SGR sequence of the transition
    from [prev] to [s] at [depth]. It writes nothing if the two have the same
    colors and attributes, and [CSI 0 m] if [s] has the colors and attributes of
    {!default}. [prev] defaults to {!default}. *)

val styled : ?depth:Color.depth -> ?reset:bool -> t -> string -> string
(** [styled ~depth ~reset s str] is [str] after the transition from {!default}
    to [s] at [depth], followed by [CSI 0 m] if [reset] is [true]. It is [str]
    itself if [s] is {!default}. It writes no hyperlink. [reset] defaults to
    [false]. *)

(** {1:reading Reading SGR} *)

val apply_sgr : Parser.Params.t -> t -> t
(** [apply_sgr p s] is [s] after SELECT GRAPHIC RENDITION with the parameters
    [p] (ECMA-48 §8.3.117). The parameters apply from left to right, so [7;0]
    leaves inverse off. An absent parameter is 0, and an empty list is a single
    0.

    - 0 resets the rendition to no colors and no attributes. The hyperlink
      stays, since OSC 8 sets it and SGR does not.
    - An attribute code means what {!emit} writes for it, so 1 is bold, 22
      neither bold nor dim, 24 neither underline, and so on. 6, rapid blink, is
      blink. [4:0] is 24, [4:2] is 21 (double underline), and the other
      underline styles, such as [4:3] (curly), are 4.
    - 30..37, 90..97, 40..47 and 100..107 select a palette color, and 39 and 49
      select {!Color.default_fg} and {!Color.default_bg}. 38 and 48 select a
      color written as T.416 sub-parameters ([38:5:n], [38:2:r:g:b] or
      [38:2:cs:r:g:b]) or as xterm's separate parameters ([38;5;n] or
      [38;2;r;g;b]). An incomplete color in the second form ends the list. 58,
      the underline color, is read the same way and changes nothing.
    - A parameter with sub-parameters changes nothing unless its code is 4, 38,
      48 or 58, the codes whose sub-parameters SGR defines (ECMA-48 §5.4.2). So
      [1:2] leaves bold as it was, and [:] changes nothing.
    - Other codes change nothing. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf s] formats [s] for inspection, such as
    ["Style\{fg=#ff0000, attrs=[Bold]\}"]. *)
