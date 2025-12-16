(** Code renderable with Tree-sitter syntax highlighting.

    Code provides a specialized text renderable for displaying source code with
    syntax highlighting powered by Tree-sitter parsers via {!Mosaic_syntax}. It
    performs incremental parsing, supports multiple languages, and renders
    highlighted code with optional text selection and wrapping.

    {1 Overview}

    Code manages a text buffer, performs syntax highlighting using
    {!Mosaic_syntax.Session} for incremental parsing, and renders styled output
    directly to the grid. The widget supports wrapping modes, tab rendering with
    indicators, and text selection.

    {1 Usage Basics}

    To render highlighted code: 1. Create a language set with
    {!Mosaic_syntax.builtins} or build a custom set 2. Specify a [filetype] to
    select the appropriate parser 3. Provide a [Theme.t] mapping capture group
    names to styles 4. Set [content] with the source code to display

    {1 Syntax Highlighting}

    Highlighting uses Tree-sitter parsers registered in the [languages] set.
    When [filetype] is set, the corresponding parser analyzes the code and
    generates syntax tokens. The [theme] maps capture group names (e.g.,
    "keyword", "string", "comment") to {!Ansi.Style.t} values.

    When [filetype] is [None], the code renders as plain text using the base
    style from the theme.

    {1 Themes}

    A {!Theme.t} consists of a base style and overlay rules. The theme resolver
    supports hierarchical fallback: "keyword.control.flow" falls back to
    "keyword.control", then "keyword", then the base style. This enables concise
    theme definitions with broad defaults and specific overrides. *)

module Theme : sig
  type t
  (** Syntax highlighting theme mapping capture groups to styles. *)

  val create : base:Ansi.Style.t -> (string * Ansi.Style.t) list -> t
  (** [create ~base rules] constructs a theme with [base] style and overlay
      [rules].

      Each rule is [(scope, style)] where [scope] is a capture group name like
      "keyword" or "comment.line". The resolver performs hierarchical fallback
      when resolving scopes. *)

  val base : t -> Ansi.Style.t
  (** [base t] returns the base style used when no overlay matches. *)

  val resolve : t -> string -> Ansi.Style.t
  (** [resolve t scope] resolves a capture group [scope] to a style.

      Resolution attempts progressively shorter prefixes: "keyword.control.flow"
      falls back to "keyword.control", then "keyword". If no match is found,
      returns the base style merged with any partial overlay. *)

  val default : ?base:Ansi.Style.t -> unit -> t
  (** [default ?base ()] creates a default theme with common syntax groups.

      Includes styles for keywords, strings, comments, functions, types, and
      punctuation using palette colors. Uses [base] if provided, otherwise
      {!Ansi.Style.default}. *)
end

module Props : sig
  type wrap_mode = [ `None | `Char | `Word ]
  (** Text wrapping mode: no wrap, character-level wrap, or word-level wrap. *)

  type t = {
    content : string;
    filetype : Mosaic_syntax.filetype option;
    languages : Mosaic_syntax.Set.t;
    theme : Theme.t;
    conceal : bool;
    draw_unstyled_text : bool;
    wrap_mode : wrap_mode;
    tab_width : int;
    tab_indicator : int option;
    tab_indicator_color : Ansi.Color.t option;
    selection_bg : Ansi.Color.t option;
    selection_fg : Ansi.Color.t option;
    selectable : bool;
  }
  (** Code renderable properties.

      - [content]: Source code text to display
      - [filetype]: Optional language identifier for syntax highlighting
      - [languages]: Language parser set (typically {!Mosaic_syntax.builtins})
      - [theme]: Style mapping for syntax tokens
      - [conceal]: Enable concealment of syntax elements (default [true])
      - [draw_unstyled_text]: Render text without highlighting when parsing
        fails (default [true])
      - [wrap_mode]: Text wrapping behavior (default [`Word])
      - [tab_width]: Tab stop width in cells (default 4)
      - [tab_indicator]: Optional Unicode codepoint for first tab cell
      - [tab_indicator_color]: Optional color for tab indicator
      - [selection_bg]: Selection background color
      - [selection_fg]: Selection foreground color
      - [selectable]: Enable text selection (default [true]) *)

  val make :
    ?content:string ->
    ?filetype:Mosaic_syntax.filetype ->
    ?languages:Mosaic_syntax.Set.t ->
    ?theme:Theme.t ->
    ?conceal:bool ->
    ?draw_unstyled_text:bool ->
    ?wrap_mode:wrap_mode ->
    ?tab_width:int ->
    ?tab_indicator:int ->
    ?tab_indicator_color:Ansi.Color.t ->
    ?selection_bg:Ansi.Color.t ->
    ?selection_fg:Ansi.Color.t ->
    ?selectable:bool ->
    unit ->
    t
  (** [make ()] constructs code properties with defaults.

      @param content Source code text. Default is [""].
      @param filetype Language for highlighting. [None] renders plain text.
      @param languages Parser set. Default is {!Mosaic_syntax.builtins}.
      @param theme Style theme. Default is {!Theme.default}.
      @param conceal Enable syntax concealment. Default is [true].
      @param draw_unstyled_text
        Render without highlighting on parse errors. Default is [true].
      @param wrap_mode Wrapping behavior. Default is [`Word].
      @param tab_width Tab stop width in cells. Default is 4.
      @param tab_indicator Unicode codepoint for tab indicator.
      @param tab_indicator_color Color for tab indicator.
      @param selection_bg Selection background color.
      @param selection_fg Selection foreground color.
      @param selectable Enable text selection. Default is [true]. *)

  val default : t
  (** [default] is the default code properties with empty content. *)

  val equal : t -> t -> bool
  (** [equal a b] checks structural equality of code properties. Language set
      and theme are compared by physical equality. *)
end

type t
(** Code renderable state. *)

val mount : ?props:Props.t -> Renderable.t -> t
(** [mount ?props node] configures [node] to render syntax-highlighted code.
    Initializes a text buffer, creates a Tree-sitter parsing session when
    [filetype] is set, and wires up measurement and rendering callbacks. *)

val node : t -> Renderable.t
(** [node t] returns the underlying renderable node. *)

val apply_props : t -> Props.t -> unit
(** [apply_props code props] applies [props] to a mounted code renderable using
    its setters for all mutable properties. *)

val set_content : t -> string -> unit
(** [set_content t text] replaces the source code content. Triggers incremental
    parsing if a syntax session is active, and re-renders the highlighted text.
*)

val set_filetype : t -> Mosaic_syntax.filetype option -> unit
(** [set_filetype t filetype] sets the language for syntax highlighting.

    When [Some filetype], creates or updates the Tree-sitter parsing session
    with the corresponding language from the [languages] set. When [None],
    destroys the parsing session and renders plain text. Triggers re-parse and
    re-render. *)

val set_languages : t -> Mosaic_syntax.Set.t -> unit
(** [set_languages t languages] updates the language parser set. If a [filetype]
    is set, re-initializes the parsing session with the new language set.
    Triggers re-parse and re-render. *)

val set_theme : t -> Theme.t -> unit
(** [set_theme t theme] updates the syntax highlighting theme. Re-renders code
    with new styles applied to existing syntax tokens. *)

val set_wrap_mode : t -> Props.wrap_mode -> unit
(** [set_wrap_mode t mode] changes text wrapping behavior.

    - [`None]: No wrapping, text extends horizontally
    - [`Char]: Wrap at character boundaries
    - [`Word]: Wrap at word boundaries

    Triggers layout recalculation and re-render. *)

val set_conceal : t -> bool -> unit
(** [set_conceal t flag] enables or disables syntax concealment. Triggers
    re-render. *)

val set_draw_unstyled_text : t -> bool -> unit
(** [set_draw_unstyled_text t flag] controls fallback rendering when parsing
    fails. When [true], renders text without syntax highlighting. When [false],
    may render nothing on parse errors. Triggers re-render. *)

val set_selection_bg : t -> Ansi.Color.t option -> unit
(** [set_selection_bg t color] updates selection background color. *)

val set_selection_fg : t -> Ansi.Color.t option -> unit
(** [set_selection_fg t color] updates selection foreground color. *)

val set_tab_width : t -> int -> unit
(** [set_tab_width t width] sets tab stop width in cells. Triggers re-render. *)

val set_tab_indicator : t -> int option -> unit
(** [set_tab_indicator t codepoint] sets Unicode codepoint for rendering the
    first cell of tab expansions. [None] disables the indicator. Triggers
    re-render. *)

val set_tab_indicator_color : t -> Ansi.Color.t option -> unit
(** [set_tab_indicator_color t color] sets color for tab indicator. [None] uses
    current text color. Triggers re-render. *)

val plain_text : t -> string
(** [plain_text t] returns the raw source code content without styling. *)

val has_selection : t -> bool
(** [has_selection t] returns [true] if a text selection is active. *)

val get_selected_text : t -> string
(** [get_selected_text t] returns the currently selected text, or empty string
    if no selection is active. *)
