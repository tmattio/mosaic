(** {1 Mosaic Markdown}

    A Markdown renderer for Mosaic UI.

    This library parses CommonMark (via [Cmarkit]) and renders it into a tree of
    Mosaic UI nodes (Text / Box / Code / Table). The public API is organized as
    follows:

    - {!Style}: purely visual theming (colors, margins/padding)
    - {!Props}: component configuration including the document to render,
      rendering policy (wrapping, link/image behavior, fences, etc.), and syntax
      highlighting client

    Use {!parse} to convert a markdown string into a [Cmarkit.Doc.t], then pass
    the doc to the renderer via {!Props}. This separation allows caching parsed
    documents and manipulating the AST before rendering.

    Layout of the host node is controlled entirely by normal Toffee styles (see
    the [layout] argument on {!markdown}). The markdown renderer never
    overwrites the host node layout style. *)

module Style : sig
  (** Purely visual theme configuration.

      All spacing values are expressed in terminal cells. *)

  type insets = { top : int; right : int; bottom : int; left : int }
  (** Insets in terminal cells. Used for margin/padding. *)

  val insets :
    ?top:int -> ?right:int -> ?bottom:int -> ?left:int -> unit -> insets
  (** [insets ?top ?right ?bottom ?left ()] creates insets with the specified
      spacing in terminal cells. All parameters default to 0. *)

  val zero_insets : insets
  (** Zero insets with all sides set to 0. Equivalent to [insets ()]. *)

  type block = { text : Ansi.Style.t; margin : insets; padding : insets }
  (** Block-level theme: a text style plus spacing around/inside the block. *)

  val block :
    ?text:Ansi.Style.t -> ?margin:insets -> ?padding:insets -> unit -> block
  (** [block ?text ?margin ?padding ()] creates a block style.

      Defaults: [text] is {!Ansi.Style.default}, [margin] and [padding] are
      {!zero_insets}. *)

  type headings = {
    base : block;
        (** Base heading block style applied to all levels in addition to
            [levels]. Provides default spacing and text styling. *)
    levels : block list;
        (** Per-level overrides for heading blocks. Intended length is 6 for
            levels 1..6; shorter lists are allowed. Missing levels fall back to
            the last provided entry, or to [base] if the list is empty. Level 1
            uses index 0, level 2 uses index 1, etc. *)
    prefix : Ansi.Style.t;
        (** Style for the rendered heading prefix (e.g. "#" for level 1, "##"
            for level 2). Only applied when {!Props.headings.show_prefix} is
            true. *)
  }
  (** Heading theme configuration.

      Heading styles combine [base] with per-level overrides from [levels]. Use
      {!heading} to resolve the final style for a given level. *)

  type list_style = {
    block : block;
        (** Block style for list containers. Applied to the entire list. *)
    unordered_marker : string;
        (** Unordered list marker text, e.g. "•", "-", "*". *)
    unordered_marker_style : Ansi.Style.t;
        (** Style for unordered list markers. *)
    ordered_marker_style : Ansi.Style.t;
        (** Style for ordered list markers. Markers are rendered as
            "N\{ordered_suffix\}", where N is the item number. *)
    ordered_suffix : string;
        (** Suffix appended to ordered list numbers, e.g. "." or ")". *)
    item_gap : int;
        (** Horizontal gap in terminal cells between marker and item content. *)
    indent_per_level : int;
        (** Indentation in terminal cells applied per nesting level. For nested
            lists, depth 0 has no indent, depth 1 has [indent_per_level] indent,
            depth 2 has [2 * indent_per_level], etc. *)
    task_unchecked : string * Ansi.Style.t;
        (** Marker and style for unchecked GFM task list items, e.g. ["[ ]"]. *)
    task_checked : string * Ansi.Style.t;
        (** Marker and style for checked GFM task list items, e.g. ["[x]"]. *)
    task_cancelled : string * Ansi.Style.t;
        (** Marker and style for cancelled GFM task list items, e.g. ["[-]"]. *)
    task_other : string * Ansi.Style.t;
        (** Marker and style for other GFM task list items with unrecognized
            status. *)
  }
  (** List theme configuration.

      Supports both ordered and unordered lists, nested lists, and GFM task
      lists with custom markers and styles. *)

  type code_block = {
    block : block;  (** Block style for code block containers. *)
    fence : Ansi.Style.t;
        (** Style for fence markers (the backticks: ```). Only rendered when
            {!Props.code_blocks.show_fences} is true. *)
    language : Ansi.Style.t;
        (** Style for the language identifier appearing after the opening fence.
        *)
  }
  (** Code block theme configuration.

      Controls styling for fenced code blocks, including the fence markers and
      language identifier. Code content uses syntax highlighting when a syntax
      client is configured. *)

  type thematic_break = { style : Ansi.Style.t; glyph : string }
  (** Thematic break (horizontal rule) theme configuration.

      The [glyph] string is repeated to fill the available wrap width. Defaults
      to "─" in the default themes. Empty glyphs are replaced with "-". *)

  type table_style = {
    block : block;  (** Block style for table containers. *)
    header : Ansi.Style.t;  (** Text style for table header cells. *)
    cell : Ansi.Style.t;  (** Text style for table body cells. *)
    border : Ansi.Style.t;  (** Style for table borders and dividers. *)
    box_style : Mosaic_ui.Table.box_style;
        (** Box drawing style for table borders. See
            {!Mosaic_ui.Table.box_style} for available styles. *)
  }
  (** Table theme configuration for GFM tables. *)

  type inline = {
    emph : Ansi.Style.t;
        (** Style for emphasized text (typically rendered as italic). *)
    strong : Ansi.Style.t;
        (** Style for strong emphasis (typically rendered as bold). *)
    code : Ansi.Style.t;  (** Style for inline code spans. *)
    link : Ansi.Style.t;
        (** Style for links. Hyperlink rendering behavior is controlled by
            {!Props.links}. *)
    image : Ansi.Style.t;
        (** Style for image placeholders. Image rendering behavior is controlled
            by {!Props.images}. *)
    raw_html : Ansi.Style.t;
        (** Style for raw HTML when rendered as text. Only used when
            [Props.raw_html] is [`Show_as_text]. *)
    strike : Ansi.Style.t;  (** Style for strikethrough text (GFM extension). *)
  }
  (** Inline element theme configuration.

      Controls text styling for inline markdown elements. These styles are
      applied in addition to any block-level text styles. *)

  type t = {
    document : block;
        (** Root document container style. Applied to the outermost markdown
            container. *)
    paragraph : block;  (** Style for paragraph blocks. *)
    headings : headings;  (** Heading configuration for all levels (1-6). *)
    block_quote : block;
        (** Style for block quotes. Rendered with a left border using the
            foreground color from [block_quote.text]. *)
    list : list_style;
        (** List configuration for ordered, unordered, and task lists. *)
    code_block : code_block;  (** Code block configuration. *)
    thematic_break : thematic_break;
        (** Thematic break (horizontal rule) configuration. *)
    table : table_style;  (** Table configuration for GFM tables. *)
    inline : inline;  (** Inline element styles. *)
  }
  (** Complete theme configuration for markdown rendering.

      Provides comprehensive styling for all markdown elements. Use
      {!default_dark} or {!default_light} as a starting point and customize with
      the [map_*] helpers. *)

  val default_dark : t
  (** Default dark theme tuned for terminal legibility. *)

  val default_light : t
  (** Default light theme tuned for terminal legibility. *)

  val heading : t -> level:int -> block
  (** [heading t ~level] returns the resolved heading style for the given level.

      Combines [t.headings.base] with the per-level override from
      [t.headings.levels]. Levels are clamped to the range 1..6. If [levels] is
      empty, returns [base]. If [level] exceeds the length of [levels], uses the
      last entry in [levels]. *)

  val with_heading_palette : Ansi.Color.t list -> t -> t
  (** [with_heading_palette colors t] replaces heading foreground colors with
      the given palette.

      The list is interpreted as colors for levels 1..6 (index 0 for level 1,
      index 1 for level 2, etc.). Shorter lists repeat the last color for
      remaining levels. Empty lists default to a neutral color. Returns a new
      theme with updated [headings.levels]. *)

  (** {2 Convenience updaters}

      These helpers make it easy to tweak nested theme values without verbose
      record updates. *)

  val map_document : (block -> block) -> t -> t
  (** [map_document f t] updates the document block style by applying [f]. *)

  val map_paragraph : (block -> block) -> t -> t
  (** [map_paragraph f t] updates the paragraph block style by applying [f]. *)

  val map_block_quote : (block -> block) -> t -> t
  (** [map_block_quote f t] updates the block quote style by applying [f]. *)

  val map_list : (list_style -> list_style) -> t -> t
  (** [map_list f t] updates the list style by applying [f]. *)

  val map_code_block : (code_block -> code_block) -> t -> t
  (** [map_code_block f t] updates the code block style by applying [f]. *)

  val map_table : (table_style -> table_style) -> t -> t
  (** [map_table f t] updates the table style by applying [f]. *)

  val map_inline : (inline -> inline) -> t -> t
  (** [map_inline f t] updates the inline styles by applying [f]. *)

  val map_headings : (headings -> headings) -> t -> t
  (** [map_headings f t] updates the headings configuration by applying [f]. *)
end

module Props : sig
  (** Component props.

      All fields are applied together as a single update to avoid redundant
      rebuilds. *)

  (** {2 Rendering policy types} *)

  type wrap_width = [ `Auto | `Columns of int ]
  (** [wrap_width] controls the renderer's logical wrapping width.

      - [`Auto] means "use the host node's measured layout width when
        available"; if unavailable, the renderer falls back to no explicit
        constraint.
      - [`Columns n] means "wrap as if the available width is [n] columns". *)

  type wrap_mode = [ `None | `Word | `Char ]
  (** Text wrapping strategy for text-like nodes.

      - [`None]: No wrapping. Text overflows container width.
      - [`Word]: Wrap at word boundaries.
      - [`Char]: Wrap at character boundaries. Falls back to [`Word] in some
        contexts as character-level wrapping is not universally supported. *)

  type raw_html = [ `Show_as_text | `Drop ]
  (** Raw HTML handling strategy.

      - [`Show_as_text]: Render HTML as plain text with {!Style.inline.raw_html}
        styling.
      - [`Drop]: Omit HTML from output entirely. *)

  type unknown = [ `Drop | `Plain_text | `Debug ]
  (** Handling strategy for unsupported or unknown markdown nodes.

      - [`Drop]: Omit unknown nodes from output.
      - [`Plain_text]: Render as plain text, extracting text content where
        possible.
      - [`Debug]: Render as plain text with a debug marker prefix. Useful for
        identifying unsupported content. *)

  (** Link rendering strategy.

      Controls how markdown links are rendered. The link destination is resolved
      from inline link definitions or reference-style link definitions in the
      document. *)
  type link =
    | Hyperlink
        (** Render as terminal hyperlink using ANSI escape codes. Link text is
            styled with {!Style.inline.link} and made clickable. *)
    | Plain
        (** Render link text with {!Style.inline.link} styling but no hyperlink
            escape codes or URL display. *)
    | Inline_url of {
        left : string;
        right : string;
        style : Ansi.Style.t option;
      }
        (** Render link text followed by the URL in brackets. The URL is
            displayed as " [left]<url>[right]". If [left] or [right] are empty,
            defaults to "(" and ")". [style] controls URL styling; defaults to
            {!Style.inline.link} if [None]. *)
    | Custom of
        (text:Mosaic_ui.Text.Fragment.t list ->
        dest:string option ->
        Mosaic_ui.Text.Fragment.t list)
        (** Custom link renderer. [text] contains the link text fragments,
            [dest] is the link destination if available. Returns the final
            fragments to render. *)

  (** Image rendering strategy.

      Terminal applications cannot display images directly, so this controls how
      image references are represented in text. *)
  type image =
    | Alt_only
        (** Render alt text as "[Image: <alt>]" with {!Style.inline.image}
            styling. *)
    | Alt_and_url
        (** Render as "[Image: <alt> <<uri>>]" with {!Style.inline.image}
            styling. If no URI is provided, renders like [Alt_only]. *)
    | Hidden  (** Omit images from output entirely. *)
    | Custom of
        (alt:string -> uri:string option -> Mosaic_ui.Text.Fragment.t list)
        (** Custom image renderer. [alt] is the image alt text, [uri] is the
            image source if available. Returns fragments to render. *)

  type headings = { show_prefix : bool; wrap : wrap_mode }
  (** Heading rendering configuration.

      - [show_prefix]: If true, render heading level prefix (e.g. "#", "##")
        before heading text. Defaults to true.
      - [wrap]: Text wrapping mode for heading content. Defaults to [`Word]. *)

  type code_blocks = {
    show_fences : bool;
        (** If true, render fence markers (```) above and below code blocks.
            Defaults to true. *)
    wrap : wrap_mode;
        (** Text wrapping mode for code content. Defaults to [`None]. *)
    syntax : [ `Auto | `Theme of Mosaic_ui.Code.Theme.t ];
        (** Syntax highlighting configuration. [`Auto] builds a default theme
            from [Style.code_block.block.text]. Custom themes can be provided
            via [`Theme]. *)
  }
  (** Code block rendering configuration. *)

  val headings : ?show_prefix:bool -> ?wrap:wrap_mode -> unit -> headings
  (** [headings ?show_prefix ?wrap ()] creates a headings configuration.

      Defaults: [show_prefix] is true, [wrap] is [`Word]. *)

  val code_blocks :
    ?show_fences:bool ->
    ?wrap:wrap_mode ->
    ?syntax:[ `Auto | `Theme of Mosaic_ui.Code.Theme.t ] ->
    unit ->
    code_blocks
  (** [code_blocks ?show_fences ?wrap ?syntax ()] creates a code block
      configuration.

      Defaults: [show_fences] is true, [wrap] is [`None], [syntax] is [`Auto].
  *)

  (** {2 Props record} *)

  type t = {
    content : string;
        (** The markdown content to render. Parsed internally when rendering. *)
    style : Style.t;
        (** Visual theme configuration for all markdown elements. *)
    wrap_width : wrap_width;
        (** Logical wrapping width for the renderer. [`Auto] uses the host
            node's measured width; [`Columns n] uses a fixed width. *)
    paragraph_wrap : wrap_mode;  (** Text wrapping mode for paragraph blocks. *)
    block_quote_wrap : wrap_mode;
        (** Text wrapping mode for block quote content. *)
    headings : headings;  (** Heading rendering configuration. *)
    code_blocks : code_blocks;
        (** Code block rendering and syntax highlighting configuration. *)
    raw_html : raw_html;  (** Raw HTML handling strategy. *)
    links : link;  (** Link rendering strategy. *)
    images : image;  (** Image rendering strategy. *)
    unknown_inline : unknown;
        (** Handling strategy for unsupported inline elements. *)
    unknown_block : unknown;
        (** Handling strategy for unsupported block elements. *)
    languages : Mosaic_syntax.Set.t;
        (** Language set for syntax highlighting in code blocks. *)
  }
  (** Complete props configuration for the markdown renderer.

      All fields are applied together as a single update to avoid redundant
      rebuilds. Use {!make} to create with defaults, then override specific
      fields. *)

  val make :
    ?content:string ->
    ?style:Style.t ->
    ?wrap_width:wrap_width ->
    ?paragraph_wrap:wrap_mode ->
    ?block_quote_wrap:wrap_mode ->
    ?headings:headings ->
    ?code_blocks:code_blocks ->
    ?raw_html:raw_html ->
    ?links:link ->
    ?images:image ->
    ?unknown_inline:unknown ->
    ?unknown_block:unknown ->
    ?languages:Mosaic_syntax.Set.t ->
    unit ->
    t
  (** [make ?content ?style ?wrap_width ...  ()] creates props with the given
      configuration.

      All parameters are optional and default to sensible values from
      {!default}. Defaults include:
      - [content]: Empty string
      - [style]: {!Style.default_dark}
      - [wrap_width]: [`Auto]
      - [paragraph_wrap]: [`Word]
      - [block_quote_wrap]: [`Word]
      - [headings]: {!headings}[ ()]
      - [code_blocks]: {!code_blocks}[ ()]
      - [raw_html]: [`Show_as_text]
      - [links]: [Hyperlink]
      - [images]: [Alt_and_url]
      - [unknown_inline]: [`Plain_text]
      - [unknown_block]: [`Plain_text]
      - [languages]: [Mosaic_syntax.builtins ()] *)

  val default : t
  (** Default props configuration. Equivalent to [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] compares two props for equality.

      Uses structural equality for [content] and physical equality for [style]
      and [languages]; structural equality for other fields. This enables
      efficient change detection to avoid redundant re-renders. *)
end

type t
(** Mounted markdown component instance.

    Created via {!mount} or the {!markdown} element constructor. Provides
    imperative access to update props and retrieve the underlying renderable
    node. *)

val mount : ?props:Props.t -> Mosaic_ui.Renderable.t -> t
(** [mount ?props node] mounts a markdown component onto an existing host node.

    The renderer creates internal child nodes for markdown content but never
    overwrites the host node's Toffee layout style. Pass [props] to configure
    the initial document and rendering options. Defaults to {!Props.default} if
    omitted.

    Returns a component instance for imperative updates via {!apply_props},
    {!update}, or the convenience setters. *)

val node : t -> Mosaic_ui.Renderable.t
(** [node t] returns the host node for the markdown component. *)

val props : t -> Props.t
(** [props t] returns the current props configuration. *)

val apply_props : t -> Props.t -> unit
(** [apply_props t props] updates the component with new props.

    Compares [props] with the current props using {!Props.equal}. If different,
    clears and re-renders the markdown content in a single batched update. No-op
    if props are equal. *)

val update : t -> (Props.t -> Props.t) -> unit
(** [update t f] updates props by applying [f] to the current props.

    Equivalent to [apply_props t (f (props t))]. *)

val set_content : t -> string -> unit
(** [set_content t content] updates the markdown content to render.

    Equivalent to [update t (fun p -> { p with content })]. *)

val set_style : t -> Style.t -> unit
(** [set_style t style] updates the visual theme.

    Equivalent to [update t (fun p -> { p with style })]. *)

val set_languages : t -> Mosaic_syntax.Set.t -> unit
(** [set_languages t languages] updates the language set for syntax
    highlighting.

    Equivalent to [update t (fun p -> { p with languages })]. *)

val parse : ?strict:bool -> string -> Cmarkit.Doc.t
(** [parse ?strict markdown] parses a markdown string into a document.

    Returns a [Cmarkit.Doc.t] suitable for rendering via [Props.doc]. Parsing is
    separate from rendering to enable caching parsed documents and manipulating
    the AST before display.

    The [strict] parameter controls parsing strictness. Defaults to false,
    allowing lenient CommonMark parsing with GFM extensions. Set to true for
    strict CommonMark compliance. *)

val markdown :
  ?id:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?live:bool ->
  ?layout:Toffee.Style.t ->
  ?props:Props.t ->
  ?on_mount:(t -> unit) ->
  unit ->
  Mosaic_ui.element
(** [markdown ?id ?visible ?z_index ?buffer ?live ?layout ?props ?on_mount ()]
    creates a markdown element.

    The element can be composed into a Mosaic UI tree and will render the
    markdown document specified in [props]. The host node's layout is controlled
    by [layout]; the renderer never overwrites it.

    Parameters:
    - [id]: Optional element identifier. Auto-generated if omitted.
    - [visible]: Visibility flag. Defaults to true.
    - [z_index]: Rendering layer. Defaults to 0.
    - [buffer]: Buffer mode for rendering. See
      {!Mosaic_ui.Renderable.Props.buffer_mode}.
    - [live]: Live update mode flag.
    - [layout]: Toffee layout style for the host node. Defaults to a vertical
      flex container with 100% width.
    - [props]: Markdown configuration. Defaults to {!Props.default}.
    - [on_mount]: Callback invoked with the component instance after mounting.
      Useful for obtaining a handle for imperative updates.

    {4 Examples}

    Render a markdown string:
    {[
      markdown ~props:(Props.make ~content:"# Hello\n\nWorld!" ()) ()
    ]}

    Render with custom theme and on_mount callback:
    {[
      let md_ref = ref None in
      markdown
        ~props:(Props.make ~content ~style:Style.default_light ())
        ~on_mount:(fun m -> md_ref := Some m)
        ()
    ]} *)
