(** Vertical list selector with optional descriptions and scroll indicator.

    Select provides a focusable, keyboard-navigable list where a single item is
    selected. It supports optional per-item descriptions, wrapping navigation,
    configurable spacing between items, and an optional scroll indicator.
    Rendering is buffered for performance. *)

type item = { name : string; description : string option }
type t

module Props : sig
  type t

  val make :
    ?options:item list ->
    ?background:Ansi.Color.t ->
    ?text_color:Ansi.Color.t ->
    ?focused_background:Ansi.Color.t ->
    ?focused_text_color:Ansi.Color.t ->
    ?selected_background:Ansi.Color.t ->
    ?selected_text_color:Ansi.Color.t ->
    ?description_color:Ansi.Color.t ->
    ?selected_description_color:Ansi.Color.t ->
    ?show_scroll_indicator:bool ->
    ?wrap_selection:bool ->
    ?show_description:bool ->
    ?item_spacing:int ->
    ?fast_scroll_step:int ->
    ?selected_index:int ->
    ?autofocus:bool ->
    unit ->
    t

  val default : t
  val equal : t -> t -> bool
end

val mount : ?props:Props.t -> Renderable.t -> t
(** [mount ?props node] configures [node] to render a vertical select list. *)

val node : t -> Renderable.t
(** [node t] returns the underlying renderable node. *)

val options : t -> item list
(** [options t] returns current items. *)

val set_options : t -> item list -> unit
(** [set_options t items] replaces item list. Clamps selection into range. *)

val selected_index : t -> int
(** [selected_index t] returns the 0-based selection index. *)

val selected_item : t -> item option
(** [selected_item t] returns the currently selected item, if any. *)

val set_selected_index : t -> int -> unit
(** [set_selected_index t i] selects item [i] and fires change callbacks. *)

val set_wrap_selection : t -> bool -> unit
val set_show_description : t -> bool -> unit
val set_show_scroll_indicator : t -> bool -> unit
val set_item_spacing : t -> int -> unit
val set_fast_scroll_step : t -> int -> unit
val set_background : t -> Ansi.Color.t -> unit
val set_text_color : t -> Ansi.Color.t -> unit
val set_focused_background : t -> Ansi.Color.t -> unit
val set_focused_text_color : t -> Ansi.Color.t -> unit
val set_selected_background : t -> Ansi.Color.t -> unit
val set_selected_text_color : t -> Ansi.Color.t -> unit
val set_description_color : t -> Ansi.Color.t -> unit
val set_selected_description_color : t -> Ansi.Color.t -> unit

val set_on_change : t -> (int -> unit) option -> unit
(** Called when the selected index changes. *)

val set_on_activate : t -> (int -> unit) option -> unit
(** Called when the current item is activated (Enter). *)

val handle_key : t -> Event.key -> bool
(** [handle_key t event] handles Up/Down, j/k, Enter. Returns [true] if used. *)

val handle_mouse : t -> Event.mouse -> unit
(** [handle_mouse t event] handles clicks to select and wheel to navigate. *)

val apply_props : t -> Props.t -> unit
(** [apply_props select props] applies [props] to a mounted select renderable
    using its setters. *)
