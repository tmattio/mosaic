(** Rich text rendering with styled fragments.

    Text provides a high-level renderable for displaying styled text with
    support for nested style hierarchies via fragments. It wraps {!Text_surface}
    and manages fragment-to-buffer conversion, enabling declarative rich text
    composition.

    {1 Overview}

    Text supports two representations:
    - Content: Plain string content set via [content]
    - Fragments/Spans: Structured/styled content set at runtime

    Fragments support nested style hierarchies; spans are flat styled text
    chunks. The text renderable handles style merging and buffer population
    automatically.

    {1 Style Merging}

    Styles merge hierarchically: parent styles provide the base and child
    overrides apply on top, preserving unspecified parent attributes. The
    [text_style] parameter provides the base style for all fragments. *)

type t
type wrap_mode = [ `None | `Char | `Word ]

type fragment =
  | Text of { text : string; style : Ansi.Style.t option }
  | Span of { style : Ansi.Style.t option; children : fragment list }
      (** Structured text fragments for rich text composition.

          - [Text]: Leaf node containing text and optional style override
          - [Span]: Container with optional style applied to all children *)

type span = { text : string; style : Ansi.Style.t option }
(** Flat span representation for styled content. *)

module Fragment : sig
  type t = fragment

  val text : ?style:Ansi.Style.t -> string -> t
  (** [text text] creates a text fragment with optional style. *)

  val span : ?style:Ansi.Style.t -> t list -> t
  (** [span children] creates a container fragment grouping [children] with
      optional shared style. *)

  val bold : t list -> t
  (** Convenience builders for common text styles. *)

  val italic : t list -> t
  val underline : t list -> t
  val dim : t list -> t
  val blink : t list -> t
  val inverse : t list -> t
  val hidden : t list -> t
  val strikethrough : t list -> t
  val bold_italic : t list -> t
  val bold_underline : t list -> t
  val italic_underline : t list -> t
  val bold_italic_underline : t list -> t
  val fg : Ansi.Color.t -> t list -> t
  val bg : Ansi.Color.t -> t list -> t
  val color : Ansi.Color.t -> t list -> t
  val bg_color : Ansi.Color.t -> t list -> t
  val styled : Ansi.Style.t -> t list -> t
end

module Props : sig
  type t

  val make :
    ?text_style:Ansi.Style.t ->
    ?content:string ->
    ?wrap_mode:wrap_mode ->
    ?tab_indicator:int ->
    ?tab_indicator_color:Ansi.Color.t ->
    ?selection_bg:Ansi.Color.t ->
    ?selection_fg:Ansi.Color.t ->
    ?selectable:bool ->
    unit ->
    t

  val default : t
  val equal : t -> t -> bool
end

val mount : ?props:Props.t -> Renderable.t -> t
(** [mount ?props node] configures [node] to render rich text. *)

val node : t -> Renderable.t
(** [node t] returns the underlying renderable node. *)

val fragments : t -> fragment list
(** [fragments t] returns the current fragment list. *)

val set_fragments : t -> fragment list -> unit
(** [set_fragments t fragments] replaces content with [fragments]. Normalization
    merges adjacent text with identical styles, removes empty fragments, and
    flattens empty spans. *)

val fragments_equal : fragment list -> fragment list -> bool
(** [fragments_equal a b] checks structural equality of fragment lists. *)

val plain_text : t -> string
(** [plain_text t] extracts text content without styling. *)

val spans : t -> span list
(** [spans t] converts fragments to a flat span list. Caches the result; cache
    invalidates on fragment changes. *)

val set_content : t -> string -> unit
(** [set_content t text] replaces content with plain [text]. *)

val set_spans : t -> span list -> unit
(** [set_spans t spans] replaces content with flat [spans]. *)

val append_span : t -> span -> unit
(** [append_span t span] adds [span] to the end of the text. *)

val clear_spans : t -> unit
(** [clear_spans t] removes all content. *)

val set_text_style : t -> Ansi.Style.t -> unit
(** [set_text_style t style] updates the default style used for fragments
    without explicit style overrides. Triggers buffer rebuild. *)

val wrap_mode : t -> wrap_mode
(** [wrap_mode t] returns current wrap mode. *)

val set_wrap_mode : t -> wrap_mode -> unit
(** [set_wrap_mode t mode] changes wrapping strategy. *)

val set_tab_width : t -> int -> unit
val set_tab_indicator : t -> int option -> unit
val set_tab_indicator_char : t -> string option -> unit
val set_tab_indicator_color : t -> Ansi.Color.t option -> unit
val set_selection_bg : t -> Ansi.Color.t option -> unit
val set_selection_fg : t -> Ansi.Color.t option -> unit

val selectable : t -> bool
(** Whether the text is selectable. Default is [true]. *)

val set_selectable : t -> bool -> unit
(** Enable/disable selection for this text renderable. *)

val should_start_selection : t -> x:int -> y:int -> bool
(** Hit-test to determine if a selection should start at global coordinates
    [(x, y)]. Returns [false] when [selectable] is [false] or the point is
    outside the renderable bounds. *)

(* Viewport sizing/culling is handled internally by Text_surface;
   Text does not expose separate viewport controls. *)

val set_selection : t -> Text_buffer.Selection.t -> unit
(** [set_selection t selection] highlights a text range. *)

val clear_selection : t -> unit
(** [clear_selection t] removes text highlighting. *)

val set_style : t -> Toffee.Style.t -> (unit, Renderable.error) result
(** [set_style t style] updates the layout style. *)

val apply_props : t -> Props.t -> unit
(** [apply_props text props] applies [props] to a mounted text renderable using
    its setters. *)
