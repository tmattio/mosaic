open Mosaic

(** Markdown rendering for terminal user interfaces.

    This module converts markdown text to Mosaic UI elements with full styling
    support. Handles common markdown constructs including headings, lists, code
    blocks, and inline formatting. Integrates seamlessly with Mosaic's rendering
    system.

    Rendering preserves markdown structure in terminal constraints. Word
    wrapping respects width limits. Nested lists maintain proper indentation.
    Code blocks preserve formatting and whitespace.

    {1 Features}

    - Block elements: paragraphs, headings (h1-h6), lists, quotes, code blocks
    - Inline elements: emphasis, strong, code, links
    - Automatic word wrapping and text reflow
    - Configurable styling through theme system
    - Terminal-optimized output *)

(** {1 Styling} *)

module Style : module type of Markdown_style
(** @inline *)

(** {1 Rendering} *)

val render : ?style:Style.t -> ?width:int -> string -> Ui.element
(** [render ?style ?width markdown_text] converts markdown to a UI element tree.

    Parses CommonMark-compliant markdown and applies styling. Block elements
    become vertical boxes with appropriate spacing. Inline elements receive text
    styling. Word wrapping applied based on width constraint.

    @param style Theme configuration for all elements (default: Style.default)
    @param width Maximum line width in character cells (default: 80)
    @param markdown_text Input markdown string to render

    @return UI element tree ready for display

    Example: Renders markdown documentation.
    {[
      let doc = "# Title\n\nSome **bold** text with `code`." in
      let element = Mosaic_markdown.render ~width:60 doc in
      Ui.view element
    ]} *)
