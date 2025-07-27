(** Style configuration for markdown rendering.

    This module defines styling for markdown elements inspired by Glamour.
    Provides comprehensive theming for all markdown constructs with spacing and
    color control.

    All spacing values are in terminal character cells. Margins collapse between
    adjacent blocks. Padding adds to block width. Styles cascade from general to
    specific (document -> block -> inline). *)

type block = {
  style : Ui.Style.t;  (** Base text style for the block. *)
  margin_top : int;  (** Vertical space above the block. *)
  margin_bottom : int;  (** Vertical space below the block. *)
  padding_left : int;  (** Left padding for the block's content. *)
  padding_right : int;  (** Right padding for the block's content. *)
}
(** [block] defines styling for block-level markdown elements.

    Controls spacing and base text appearance. Margins create space between
    blocks. Padding indents content within the block. All values in character
    cells. *)

type list_block = {
  block : block;
  item_prefix : string;  (** Prefix for unordered list items (e.g., "•"). *)
  item_prefix_style : Ui.Style.t;  (** Style for the item prefix. *)
  item_gap : int;  (** Space between the prefix and the item content. *)
  level_indent : int;  (** Indentation for nested lists. *)
  task_style : Ui.Style.t;  (** Style for unchecked task items "[ ]". *)
  checked_style : Ui.Style.t;  (** Style for checked task items "[x]". *)
}
(** [list_block] extends block styling for list elements.

    Item prefix appears before each list item (bullets for unordered, numbers
    for ordered). Gap separates prefix from content. Level indent multiplied by
    nesting depth for sublists. Task styles apply to GFM task lists. *)

type code_block = {
  block : block;
  lang_style : Ui.Style.t;  (** Style for the language specifier. *)
  fence_style : Ui.Style.t;  (** Style for the code fence ("```"). *)
}
(** [code_block] extends block styling for fenced code blocks.

    Language specifier shown after opening fence. Fence style applies to
    delimiter lines. Block style used for code content. Typically uses
    monospace-like appearance. *)

type table_block = {
  block : block;
  header_style : Ui.Style.t;  (** Style for table header cells. *)
  separator_style : Ui.Style.t * string;
      (* Style and character for table separators. *)
}
(** [table_block] extends block styling for tables.

    Header style applies to first row. Separator used for horizontal lines and
    column dividers. *)

type t = {
  document : block;
  paragraph : block;
  heading : block;
  heading_prefix : Ui.Style.t;
  h1 : block;
  h2 : block;
  h3 : block;
  h4 : block;
  h5 : block;
  h6 : block;
  block_quote : block;
  code_block : code_block;
  horizontal_rule : Ui.Style.t * string;
  list : list_block;
  table : table_block;
  emph : Ui.Style.t;
  strong : Ui.Style.t;
  code : Ui.Style.t;
  link : Ui.Style.t;
  image : Ui.Style.t;
  html : Ui.Style.t;
  strike : Ui.Style.t;
}
(** [t] provides complete markdown styling configuration.

    Document style wraps entire content. Heading styles cascade from general to
    specific levels. Horizontal rule is (style, character) pair repeated across
    width. Inline styles (emph, strong, code, link, image, html) apply within
    text. *)

val default : t
(** [default] provides a dark theme optimized for terminal display.

    Features high contrast colors, clear hierarchy through spacing and styling.
    Inspired by Glamour's default theme. Works well on dark terminal
    backgrounds. *)
