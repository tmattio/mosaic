(** Shared text rendering surface for text-based widgets.

    Text_surface provides a reusable renderable that manages a text buffer,
    handles wrapping, and renders styled text with automatic layout measurement.
    It serves as the foundation for higher-level text widgets like {!Text} and
    {!Text_input}.

    {1 Overview}

    Text_surface combines a {!Text_buffer.t} with a renderable node, providing
    automatic size measurement based on buffer content and configured wrapping
    behavior. Wrap width is determined by Toffee layout computation during the
    measure phase, considering known dimensions and available space. Grapheme
    pooling shares a single {!Glyph.pool} across renderables for memory
    efficiency.

    {1 Wrapping Behavior}

    When [wrap_mode] is not [`None], the surface measures available width from
    the layout system and wraps text according to [wrap_mode]:
    - [`Word]: Wraps at word boundaries (default)
    - [`Char]: Wraps at character boundaries

    When [wrap_mode] is [`None], text renders on a single line with horizontal
    overflow. *)

module Props : sig
  type wrap_mode = [ `None | `Char | `Word ]
  type t

  val make :
    ?wrap_mode:wrap_mode ->
    ?tab_indicator:int ->
    ?tab_indicator_color:Ansi.Color.t ->
    ?selection_bg:Ansi.Color.t ->
    ?selection_fg:Ansi.Color.t ->
    ?selectable:bool ->
    ?default_style:Ansi.Style.t ->
    unit ->
    t

  val default : t
  val equal : t -> t -> bool
end

type t

val apply_props : t -> Props.t -> unit
(** [apply_props surface props] applies [props] to a mounted text surface using
    its setters for wrap mode, tab indicators, selection colors, selectability,
    and default style. *)

val mount : ?props:Props.t -> Renderable.t -> t
(** [mount ?props node] configures [node] to render a text surface. *)

val node : t -> Renderable.t
(** [node t] returns the underlying renderable node. *)

val buffer : t -> Text_buffer.t
(** [buffer t] returns the underlying text buffer. Direct manipulation should be
    avoided; use {!replace_content} or {!set_plain_text} instead. *)

val view : t -> Text_buffer_view.t
(** [view t] returns the view handle tracking wrapping, viewport, and selection
    state for [t]. *)

val default_style : t -> Ansi.Style.t
(** [default_style t] returns the current default text style. *)

val set_default_style : t -> Ansi.Style.t -> unit
(** [set_default_style t style] updates the default style used when composing
    effective styles. Applies immediately to the underlying buffer, restyling
    existing content. *)

val wrap_mode : t -> Props.wrap_mode
(** [wrap_mode t] returns the current wrapping strategy. [`None] disables
    wrapping. *)

val set_wrap_mode : t -> Props.wrap_mode -> unit
(** [set_wrap_mode t mode] changes the wrapping strategy. [`None] disables
    wrapping. Marks layout dirty to trigger recalculation. *)

val selectable : t -> bool
(** [selectable t] returns whether [t] exposes a selection capability to the
    renderer. When [false], mouse-driven text selection is disabled for this
    surface. *)

val set_selectable : t -> bool -> unit
(** [set_selectable t flag] enables or disables renderer-managed selection for
    [t]. When enabled, the underlying renderable registers a selection callback;
    when disabled, sets selection to [None] and clears local highlights. *)

val set_selection_bg : t -> Ansi.Color.t option -> unit
val set_selection_fg : t -> Ansi.Color.t option -> unit

val set_selection : t -> Text_buffer.Selection.t -> unit
(** [set_selection t sel] applies a programmatic selection to the underlying
    buffer. This is independent from the renderer-managed global selection. *)

val clear_selection : t -> unit
(** [clear_selection t] clears any programmatic selection on [t]. *)

val set_tab_width : t -> int -> unit
(** [set_tab_width t w] sets the tab stop width in cells (min 1). *)

val set_tab_indicator : t -> int option -> unit
(** [set_tab_indicator t code] configures a codepoint to draw on the first cell
    of a tab expansion. [None] disables the indicator. Applied at render time,
    so updates take effect immediately without rewriting the buffer. *)

val set_tab_indicator_color : t -> Ansi.Color.t option -> unit
(** [set_tab_indicator_color t color] sets the color for the tab indicator
    glyph. When [None], the current text color is used. Applied at render time,
    so updates take effect immediately without rewriting the buffer. *)

val replace_content : t -> (Text_buffer.t -> unit) -> unit
(** [replace_content t writer] resets the buffer, executes [writer], finalizes
    the buffer, and requests a render. The [writer] function receives the
    cleared buffer and can populate it with styled text chunks. *)

val set_plain_text : t -> ?style:Ansi.Style.t -> string -> unit
(** [set_plain_text t ?style text] replaces buffer content with [text].
    Convenience for [replace_content] that writes a single styled chunk using
    [style] or [default_style t] if not provided. *)

val request_render : t -> unit
(** [request_render t] schedules a redraw without modifying content. *)
