(** Style configuration for Markdown rendering in Mosaic.

    The style record mirrors the structure of Markdown by providing block-level
    spacing, inline text styling, and component-specific options (lists, tables,
    code blocks). All spacing values are expressed in terminal cells. *)

type block = {
  text_style : Ansi.Style.t;
  margin_top : int;
  margin_bottom : int;
  padding_left : int;
  padding_right : int;
}
(** Block-level styling.

    [text_style] applies to all text within the block. Margins add spacing
    outside the block, while paddings indent the block content. *)

type list_block = {
  block : block;
  item_prefix : string;
  item_prefix_style : Ansi.Style.t;
  item_gap : int;
  level_indent : int;
  task_style : Ansi.Style.t;
  checked_style : Ansi.Style.t;
}
(** Styling for lists and task lists. *)

type code_block = {
  block : block;
  lang_style : Ansi.Style.t;
  fence_style : Ansi.Style.t;
  show_fences : bool;
}
(** Styling for fenced code blocks. [show_fences] toggles the ``` fences. *)

type table_block = {
  block : block;
  header_style : Ansi.Style.t;
  cell_style : Ansi.Style.t;
  separator_style : Ansi.Style.t * string;
  box_style : Mosaic_ui.Table.box_style;
}
(** Styling overrides for tables. *)

type t = {
  document : block;
  paragraph : block;
  heading : block;
  heading_prefix : Ansi.Style.t;
  h1 : block;
  h2 : block;
  h3 : block;
  h4 : block;
  h5 : block;
  h6 : block;
  block_quote : block;
  code_block : code_block;
  horizontal_rule : Ansi.Style.t * string;
  list : list_block;
  table : table_block;
  emph : Ansi.Style.t;
  strong : Ansi.Style.t;
  code : Ansi.Style.t;
  link : Ansi.Style.t;
  image : Ansi.Style.t;
  html : Ansi.Style.t;
  strike : Ansi.Style.t;
}
(** Style configuration for the renderer. *)

val default : t
(** Default dark theme tuned for terminal legibility. *)
