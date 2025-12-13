(** Markdown renderable component.

    Renders CommonMark markdown content as a composable UI element. Supports
    headings, paragraphs, lists, code blocks with syntax highlighting,
    blockquotes, tables, and inline formatting. *)

module Style = Markdown_style
(** Styling configuration for markdown elements. *)

(** {1 Props} *)

module Props : sig
  type t

  val make :
    ?style:Style.t ->
    ?width:int ->
    ?strict:bool ->
    ?code_grammar_resolvers:(string -> Mosaic_ui.Code.grammar option) list ->
    ?content:string ->
    unit ->
    t
  (** [make ?style ?width ?strict ?code_grammar_resolvers ?content ()] creates
      markdown props.

      @param style Theme configuration (defaults to {!Style.default})
      @param width Target width constraint for layout (defaults to 80)
      @param strict Parse strictly according to CommonMark (default [false])
      @param code_grammar_resolvers
        Grammar resolvers for fenced code blocks syntax highlighting
      @param content Markdown text to render *)

  val default : t
  val equal : t -> t -> bool
end

(** {1 Component} *)

type t

val mount : ?props:Props.t -> Mosaic_ui.Renderable.t -> t
(** [mount ?props node] mounts a markdown component onto [node].

    The component parses and renders the markdown content as child elements. Use
    setters to update content or configuration after mounting. *)

val node : t -> Mosaic_ui.Renderable.t
(** [node t] returns the underlying renderable node. *)

(** {1 Setters} *)

val set_content : t -> string -> unit
(** [set_content t markdown] updates the markdown content.

    Clears existing children and re-renders with the new content. *)

val set_style : t -> Style.t -> unit
(** [set_style t style] updates the styling configuration. *)

val set_width : t -> int -> unit
(** [set_width t width] updates the target width constraint. *)

val set_strict : t -> bool -> unit
(** [set_strict t strict] updates the CommonMark strict parsing mode. *)

val set_code_grammar_resolvers :
  t -> (string -> Mosaic_ui.Code.grammar option) list option -> unit
(** [set_code_grammar_resolvers t resolvers] updates the grammar resolvers for
    code block syntax highlighting. *)

val apply_props : t -> Props.t -> unit
(** [apply_props t props] applies [props] to a mounted markdown using its
    setters. Used by the reconciler to update mounted components. *)

(** {1 Element API} *)

val markdown :
  ?id:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?live:bool ->
  ?display:Toffee.Style.display ->
  ?box_sizing:Toffee.Style.box_sizing ->
  ?position:Toffee.Style.position ->
  ?overflow:Toffee.Style.overflow Toffee.Geometry.point ->
  ?scrollbar_width:float ->
  ?inset:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?min_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?max_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?aspect_ratio:float ->
  ?margin:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?padding:Toffee.Style.length_percentage Toffee.Geometry.rect ->
  ?gap:Toffee.Style.length_percentage Toffee.Geometry.size ->
  ?align_items:Toffee.Style.align_items ->
  ?align_self:Toffee.Style.align_self ->
  ?align_content:Toffee.Style.align_content ->
  ?justify_items:Toffee.Style.justify_items ->
  ?justify_self:Toffee.Style.justify_self ->
  ?justify_content:Toffee.Style.justify_content ->
  ?flex_direction:Toffee.Style.flex_direction ->
  ?flex_wrap:Toffee.Style.flex_wrap ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:Toffee.Style.dimension ->
  ?grid_template_rows:Toffee.Style.grid_template_component list ->
  ?grid_template_columns:Toffee.Style.grid_template_component list ->
  ?grid_auto_rows:Toffee.Style.track_sizing_function list ->
  ?grid_auto_columns:Toffee.Style.track_sizing_function list ->
  ?grid_auto_flow:Toffee.Style.grid_auto_flow ->
  ?grid_template_areas:Toffee.Style.grid_template_area list ->
  ?grid_row:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?grid_column:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?style:Style.t ->
  ?width:int ->
  ?strict:bool ->
  ?code_grammar_resolvers:(string -> Mosaic_ui.Code.grammar option) list ->
  ?content:string ->
  ?on_mount:(t -> unit) ->
  unit ->
  Mosaic_ui.element
(** [markdown ...] creates a markdown element.

    Renders CommonMark markdown content as a composable UI element. Supports
    headings, paragraphs, lists, code blocks with syntax highlighting,
    blockquotes, tables, and inline formatting.

    {2 Identity and Host Props}
    - [id]: Node identifier (auto-generated if not provided)
    - [visible]: Whether the node is visible (default true)
    - [z_index]: Z-order for overlapping nodes (default 0)
    - [buffer]: Buffer mode for rendering (default [`None])
    - [live]: Whether node updates trigger re-renders (default false)

    {2 Layout Style}
    Standard layout properties matching other UI elements.

    {2 Markdown Props}
    - [style]: Theme configuration (defaults to {!Style.default})
    - [width]: Target width constraint for layout (defaults to 80)
    - [strict]: Parse strictly according to CommonMark (default false)
    - [code_grammar_resolvers]: Grammar resolvers for code block highlighting
    - [content]: Markdown text to render *)
