(** Container renderable with borders, backgrounds, and titles.

    Box provides a rectangular container that can display borders, background
    colors, and optional titles. It supports customizable border styles,
    selective border sides, and focus-aware border coloring. The box serves as
    the foundation for many higher-level UI components.

    {1 Border Behavior}

    Borders consume layout space. When enabled, each border side adds 1 cell to
    the box's dimensions (border width is always 1 cell regardless of
    [border_style]). The [border_sides] parameter controls which sides are
    drawn. If [border] is [false], no borders are rendered regardless of
    [border_sides].

    Border characters adapt based on [border_style]. Use [custom_border_chars]
    to override default character sets for specific visual effects.

    {1 Fill and Clipping}

    The [should_fill] parameter controls background rendering. When [true], the
    box fills its entire area with [background] color. Child elements are
    clipped to the content area inside borders via scissor rectangles during
    rendering.

    Children are positioned relative to the box's content area, which excludes
    border space. *)

module Props : sig
  type t

  (** Default box properties. *)

  val make :
    ?background:Ansi.Color.t ->
    ?border:bool ->
    ?border_sides:Grid.Border.side list ->
    ?border_style:Grid.Border.t ->
    ?border_color:Ansi.Color.t ->
    ?focused_border_color:Ansi.Color.t ->
    ?should_fill:bool ->
    ?custom_border_chars:Grid.Border.t ->
    ?title:string ->
    ?title_alignment:[ `Left | `Center | `Right ] ->
    ?gap:Toffee.Style.Length_percentage.t ->
    ?row_gap:Toffee.Style.Length_percentage.t ->
    ?column_gap:Toffee.Style.Length_percentage.t ->
    unit ->
    t
  (** [make ... ()] constructs box properties *)

  val default : t
  (** [default] is the default box properties. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if box properties [a] and [b] are equal. *)
end

type t

val mount : ?props:Props.t -> Renderable.t -> t
(** [mount ?props node] configures [node] to render a box with borders,
    backgrounds, and titles. Applies render callback, child scissor clipping,
    and border-aware layout style adjustment. Border-related props (e.g.,
    [border_style], [border_color]) auto-enable borders when present. *)

val node : t -> Renderable.t
(** [node t] returns the underlying renderable node. *)

val set_background : t -> Ansi.Color.t option -> unit
(** [set_background t color] updates the background color. [None] makes the
    background transparent. Triggers re-render if changed. *)

val set_border : t -> bool -> unit
(** [set_border t flag] enables or disables border rendering. Adjusts layout to
    account for border space. *)

val set_border_sides : t -> Grid.Border.side list -> unit
(** [set_border_sides t sides] specifies which border sides to render. Empty
    list renders no borders. If [border] is [false], no borders render
    regardless of this value. *)

val set_border_style : t -> Grid.Border.t -> unit
(** [set_border_style t style] changes the border character set. *)

val set_border_color : t -> Ansi.Color.t -> unit
(** [set_border_color t color] updates the unfocused border color. *)

val set_focused_border_color : t -> Ansi.Color.t option -> unit
(** [set_focused_border_color t color] updates the focused border color. [None]
    uses the unfocused color. *)

val set_should_fill : t -> bool -> unit
(** [set_should_fill t flag] controls background fill rendering. *)

val set_custom_border_chars : t -> Grid.Border.t option -> unit
(** [set_custom_border_chars t chars] overrides border characters. [None] uses
    default for the current [border_style]. *)

val set_title : t -> string option -> unit
(** [set_title t text] updates the title text. [None] removes the title. *)

val set_title_alignment : t -> [ `Left | `Center | `Right ] -> unit
(** [set_title_alignment t alignment] repositions the title on the top border.
*)

val set_style : t -> Toffee.Style.t -> (unit, Renderable.error) result
(** [set_style t style] updates the box's layout style. The style is
    automatically adjusted to account for border space. Returns [Error] if
    layout style application fails. *)

val set_gap : t -> Toffee.Style.Length_percentage.t -> unit
(** [set_gap t v] sets both row and column gaps on the underlying layout style
    to [v] and triggers a reflow and re-render. *)

val set_row_gap : t -> Toffee.Style.Length_percentage.t -> unit
(** [set_row_gap t v] sets the row gap (vertical spacing between rows) on the
    underlying layout style to [v] and triggers a reflow and re-render. *)

val set_column_gap : t -> Toffee.Style.Length_percentage.t -> unit
(** [set_column_gap t v] sets the column gap (horizontal spacing between
    columns) on the underlying layout style to [v] and triggers a reflow and
    re-render. *)

val apply_props : t -> Props.t -> unit
(** [apply_props box props] applies [props] to a mounted box using its setters.
*)
