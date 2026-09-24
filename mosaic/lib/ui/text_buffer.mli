(** Styled text storage with per-character styles and highlight overlays.

    A text buffer stores content as a sequence of styled {!type-span}s. It
    tracks logical line boundaries and per-line display widths. Highlights (see
    {!Highlight}) provide priority-based style overlays for selection and syntax
    highlighting.

    Consumers typically wrap a buffer in a {!Text_surface.t} for rendering. *)

type t
(** A mutable styled text buffer. *)

(** {1:spans Spans} *)

type span = { text : string; style : Ansi.Style.t }
(** A contiguous run of text with a single visual style. *)

(** {1:highlights Highlights} *)

(** Character-range style overlays.

    A highlight applies a style overlay to a range of grapheme offsets in the
    buffer. Highlights are layered by [priority] (higher wins) and grouped by
    [ref_id] for batch removal via {!remove_highlights_by_ref}. *)
module Highlight : sig
  type t
  (** A highlight overlay. *)

  val make :
    start_offset:int ->
    end_offset:int ->
    style:Ansi.Style.t ->
    ?priority:int ->
    ref_id:int ->
    unit ->
    t
  (** [make ~start_offset ~end_offset ~style ~ref_id ()] is a highlight covering
      grapheme offsets from [start_offset] (inclusive) to [end_offset]
      (exclusive).

      [priority] defaults to [0]. Higher values render on top. [ref_id] groups
      related highlights for batch removal via {!remove_highlights_by_ref}. *)

  val start_offset : t -> int
  (** [start_offset h] is the inclusive start grapheme offset. *)

  val end_offset : t -> int
  (** [end_offset h] is the exclusive end grapheme offset. *)

  val style : t -> Ansi.Style.t
  (** [style h] is the style overlay. *)

  val priority : t -> int
  (** [priority h] is the rendering priority. *)

  val ref_id : t -> int
  (** [ref_id h] is the batch-removal reference. *)
end

(** {1:construction Construction} *)

val create :
  ?default_style:Ansi.Style.t ->
  ?width_method:Matrix.Text.width_method ->
  ?tab_width:int ->
  unit ->
  t
(** [create ()] is an empty buffer with:
    - [default_style]: style applied to plain text set via {!set_text} and
      {!append}. Defaults to {!Ansi.Style.default}.
    - [width_method]: grapheme width computation method. Defaults to [`Unicode].
    - [tab_width]: constant tab width, clamped to [>= 1]. Defaults to [2]. *)

(** {1:content Content} *)

val set_text : t -> string -> unit
(** [set_text t s] replaces content with plain text [s] styled with
    {!default_style}. Retains the source index and cached metrics when [s]
    equals the installed text. *)

val styled_text : t -> span list
(** [styled_text t] is the installed base spans in source order, preserving
    their exact text, including line breaks. Highlight overlays are separate.
    The returned immutable list remains valid after buffer changes. *)

val set_styled_text : t -> span list -> unit
(** [set_styled_text t spans] replaces content with pre-styled [spans]. Retains
    the source index and cached metrics when the concatenated text is unchanged,
    including when span boundaries differ. *)

val append : t -> string -> unit
(** [append t s] appends plain text [s] styled with {!default_style}.
    Invalidates cached metrics. *)

val append_styled : t -> span list -> unit
(** [append_styled t spans] appends pre-styled [spans] to existing content.
    Invalidates cached metrics. *)

val clear : t -> unit
(** [clear t] removes all content and highlights. *)

val grapheme_count : t -> int
(** [grapheme_count t] is the total number of grapheme clusters. *)

val plain_text : t -> string
(** [plain_text t] is the concatenation of all span texts. Cached; recomputed on
    content change. *)

(** {1:default_style Default style} *)

val default_style : t -> Ansi.Style.t
(** [default_style t] is the style applied to plain text via {!set_text} and
    {!append}. *)

val set_default_style : t -> Ansi.Style.t -> unit
(** [set_default_style t s] changes the default style. Does not re-style
    existing content. *)

(** {1:lines Line information} *)

val line_count : t -> int
(** [line_count t] is the number of logical lines (delimited by line breaks).
    Cached; recomputed on content change. *)

val line_width : t -> int -> int
(** [line_width t n] is the display width of logical line [n]. Returns [0] if
    [n] is out of range. Derived from the shared source index. *)

val max_line_width : t -> int
(** [max_line_width t] is the display width of the widest logical line. Cached;
    recomputed on content change. *)

val text_in_range : t -> start:int -> len:int -> string
(** [text_in_range t ~start ~len] is the plain text for grapheme offsets from
    [start] (inclusive) to [start + len] (exclusive). If [start + len] exceeds
    {!grapheme_count}, the result extends to the end of the buffer. Returns [""]
    if [start] is out of bounds or [len <= 0]. *)

(** {1:highlights_ops Highlight operations} *)

val add_highlight : t -> Highlight.t -> unit
(** [add_highlight t h] adds a highlight overlay. *)

val remove_highlights_by_ref : t -> int -> unit
(** [remove_highlights_by_ref t ref_id] removes all highlights whose
    {!Highlight.ref_id} equals [ref_id]. *)

val clear_highlights : t -> unit
(** [clear_highlights t] removes all highlights. *)

val highlights_in_range : t -> start:int -> len:int -> Highlight.t list
(** [highlights_in_range t ~start ~len] is the highlights intersecting grapheme
    offsets from [start] (inclusive) to [start + len] (exclusive), sorted by
    ascending {!Highlight.priority}. *)

(** {1:tab_width Tab width} *)

val tab_width : t -> int
(** [tab_width t] is the current constant tab width. Defaults to [2]. *)

val set_tab_width : t -> int -> unit
(** [set_tab_width t w] changes the constant tab width. [w] is clamped to
    [>= 1]. Invalidates cached line info if the value changes. *)

(** {1:width_method Width method} *)

val width_method : t -> Matrix.Text.width_method
(** [width_method t] is the grapheme width computation method. *)

val set_width_method : t -> Matrix.Text.width_method -> unit
(** [set_width_method t m] changes the width method. Invalidates cached line
    info if [m] differs from the current value. *)

(** {1:versioning Versioning} *)

val version : t -> int
(** [version t] is a monotonically increasing geometry revision. Text, source,
    tab width, or width method changes increment it; style changes do not. Used
    for cache invalidation by consumers such as {!Text_surface}. *)

val source : t -> Matrix.Text.Layout.Source.t
(** [source t] is the indexed installed text, shared across width layouts. *)

val set_source : t -> Matrix.Text.Layout.Source.t -> unit
(** [set_source t source] installs [source]. Existing styles are retained when
    its bytes equal the current text; otherwise the default style is used. *)

val style_at : t -> int -> Ansi.Style.t * int
(** [style_at t g] is the resolved style at source grapheme [g] and the next
    style boundary. Overlays follow existing priority order. Preparation is
    cached until content, base style, or highlights change. *)
