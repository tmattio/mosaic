(** Code renderable with Tree-sitter highlighting.

    This renderable owns a text buffer, performs syntax highlighting using
    Tree-sitter, and renders directly into the grid. It is not a composite of
    Text_surface. Wrapping, selection, and measurement are implemented inline.
*)

module Syntax_style : sig
  type t

  val create : default:Ansi.Style.t -> (string * Ansi.Style.t) list -> t
  val default : t -> Ansi.Style.t
  val resolve : t -> string -> Ansi.Style.t

  val of_default_theme : ?base:Ansi.Style.t -> unit -> t
  (** Create a default, palette-based syntax theme. When [base] is provided,
      overlay styles are merged on top of it (useful to align with a text
      style). Defaults to [Ansi.Style.default]. *)
end

type grammar = {
  ts_language : Tree_sitter.Language.t;
  query : Tree_sitter.Query.t;
}

module Props : sig
  type wrap_mode = [ `None | `Char | `Word ]

  type t = {
    content : string;
    filetype : string option;
    grammar : grammar option;
    grammar_resolver : (string -> grammar option) option;
    tree_syntax : Mosaic_syntax.t option;
    syntax_style : Syntax_style.t;
    conceal : bool;
    draw_unstyled_text : bool;
    wrap_mode : wrap_mode;
    tab_indicator : int option;
    tab_indicator_color : Ansi.Color.t option;
    selection_bg : Ansi.Color.t option;
    selection_fg : Ansi.Color.t option;
    default_style : Ansi.Style.t;
    selectable : bool;
  }

  val make :
    ?content:string ->
    ?filetype:string ->
    ?grammar:grammar ->
    ?grammar_resolvers:(string -> grammar option) list ->
    ?conceal:bool ->
    ?draw_unstyled_text:bool ->
    ?wrap_mode:wrap_mode ->
    ?tab_indicator:int ->
    ?tab_indicator_color:Ansi.Color.t ->
    ?selection_bg:Ansi.Color.t ->
    ?selection_fg:Ansi.Color.t ->
    ?default_style:Ansi.Style.t ->
    ?selectable:bool ->
    ?tree_syntax:Mosaic_syntax.t ->
    ?syntax_style:Syntax_style.t ->
    unit ->
    t

  val default : t
  val equal : t -> t -> bool
end

type t

val apply_props : t -> Props.t -> unit
(** [apply_props code props] applies [props] to a mounted code renderable using
    its setters for content, filetype, grammar or syntax client, wrapping,
    conceal, selection colours, and tab indicators. Creation-time defaults such
    as [default_style] and [selectable] remain unchanged. *)

val mount : ?props:Props.t -> Renderable.t -> t
val node : t -> Renderable.t
val set_content : t -> string -> unit
val set_filetype : t -> string option -> unit
val set_grammar : t -> grammar option -> unit
val set_tree_syntax : t -> Mosaic_syntax.t option -> unit
val set_syntax_style : t -> Syntax_style.t -> unit
val set_wrap_mode : t -> Props.wrap_mode -> unit
val set_conceal : t -> bool -> unit
val set_draw_unstyled_text : t -> bool -> unit
val set_selection_bg : t -> Ansi.Color.t option -> unit
val set_selection_fg : t -> Ansi.Color.t option -> unit
val set_tab_width : t -> int -> unit
val set_tab_indicator : t -> int option -> unit
val set_tab_indicator_color : t -> Ansi.Color.t option -> unit
val plain_text : t -> string
val has_selection : t -> bool
val get_selected_text : t -> string
