open Mosaic

(** Style configuration for markdown rendering, inspired by Glamour. *)

type block = {
  style : Style.t;  (** Base text style for the block. *)
  margin_top : int;  (** Vertical space above the block. *)
  margin_bottom : int;  (** Vertical space below the block. *)
  padding_left : int;  (** Left padding for the block's content. *)
  padding_right : int;  (** Right padding for the block's content. *)
}
(** A style for a block-level element. *)

type list_block = {
  block : block;
  item_prefix : string;  (** Prefix for unordered list items (e.g., "•"). *)
  item_prefix_style : Style.t;  (** Style for the item prefix. *)
  item_gap : int;  (** Space between the prefix and the item content. *)
  level_indent : int;  (** Indentation for nested lists. *)
}
(** A style for a list block. *)

type code_block = {
  block : block;
  lang_style : Style.t;  (** Style for the language specifier. *)
  fence_style : Style.t;  (** Style for the code fence ("```"). *)
}
(** A style for a code block. *)

type t = {
  document : block;
  paragraph : block;
  heading : block;
  h1 : block;
  h2 : block;
  h3 : block;
  h4 : block;
  h5 : block;
  h6 : block;
  block_quote : block;
  code_block : code_block;
  horizontal_rule : Style.t * string;
  list : list_block;
  emph : Style.t;
  strong : Style.t;
  code : Style.t;
  link : Style.t;
  image : Style.t;
}
(** The main style configuration record. *)

val default : t
(** The default "dark" theme style configuration. *)
