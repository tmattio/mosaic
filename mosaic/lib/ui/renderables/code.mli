(** Code renderable with Tree-sitter highlighting via [Mosaic_syntax].

    This renderable owns a text buffer, performs syntax highlighting using
    [Mosaic_syntax.Session] (incremental parsing), and renders directly into the
    grid. Wrapping, selection, and measurement are implemented inline.

    Mental model for users:
    - Provide [Props.languages] (an immutable [Mosaic_syntax.Set.t]).
    - Set [Props.filetype] to enable highlighting. [None] renders plain text.
    - Provide a [Theme.t] that maps capture group names to [Ansi.Style.t]. *)

module Theme : sig
  type t

  val create : base:Ansi.Style.t -> (string * Ansi.Style.t) list -> t
  val base : t -> Ansi.Style.t
  val resolve : t -> string -> Ansi.Style.t
  val default : ?base:Ansi.Style.t -> unit -> t
end

module Props : sig
  type wrap_mode = [ `None | `Char | `Word ]

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

  val default : t
  val equal : t -> t -> bool
end

type t

val mount : ?props:Props.t -> Renderable.t -> t
val node : t -> Renderable.t

val apply_props : t -> Props.t -> unit
(** Applies props using setters. *)

val set_content : t -> string -> unit
val set_filetype : t -> Mosaic_syntax.filetype option -> unit
val set_languages : t -> Mosaic_syntax.Set.t -> unit
val set_theme : t -> Theme.t -> unit
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
