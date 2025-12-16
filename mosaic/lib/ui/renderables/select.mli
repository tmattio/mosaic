(** Vertical list selector with optional descriptions and scroll indicator.

    Select provides a focusable, keyboard-navigable list where a single item is
    selected. It supports optional per-item descriptions, wrapping navigation,
    configurable spacing between items, and an optional scroll indicator.
    Rendering is buffered for performance.

    {1 Overview}

    Select displays a vertical list of items with a highlighted selection. Each
    item consists of a name and optional description. The widget automatically
    scrolls to keep the selected item visible within the viewport, centering the
    selection when possible.

    {1 Navigation}

    Keyboard navigation supports:
    - Up/Down arrows or j/k: Move selection
    - Ctrl+Up/Ctrl+Down: Fast scroll by [fast_scroll_step] items
    - Enter: Activate current selection (fires [on_activate])
    - Page Up/Page Down: Scroll by viewport height

    Mouse support includes:
    - Click to select item
    - Scroll wheel to navigate

    {1 Scroll Behavior}

    The viewport displays as many items as fit within the available height. When
    an item is selected, the view automatically scrolls to center it within the
    visible area. The [show_scroll_indicator] option displays a visual indicator
    of the current scroll position. *)

type item = { name : string; description : string option }
(** List item with name and optional description text. *)

type t
(** Select widget state. *)

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
  (** [make ()] constructs select properties.

      @param options List of items to display. Default is [[]].
      @param background Unfocused background color. Default is transparent.
      @param text_color Unfocused text color. Default is white.
      @param focused_background Focused background color. Default is dark gray.
      @param focused_text_color
        Focused text color. Default matches [text_color].
      @param selected_background Selected item background. Default is dark blue.
      @param selected_text_color Selected item text color. Default is yellow.
      @param description_color Description text color. Default is gray.
      @param selected_description_color
        Selected item description color. Default is light gray.
      @param show_scroll_indicator
        Display scroll position indicator. Default is [false].
      @param wrap_selection
        Wrap to opposite end when navigating past boundaries. Default is
        [false].
      @param show_description
        Display description line for items. Default is [true].
      @param item_spacing Vertical spacing between items in cells. Default is 0.
      @param fast_scroll_step
        Number of items to skip with Ctrl+Up/Down. Default is 5.
      @param selected_index Initial selection index. Default is 0.
      @param autofocus Request focus on mount. Default is [false]. *)

  val default : t
  (** [default] is the default select properties with an empty item list. *)

  val equal : t -> t -> bool
  (** [equal a b] checks structural equality of select properties. *)
end

val mount : ?props:Props.t -> Renderable.t -> t
(** [mount ?props node] configures [node] to render a vertical select list.
    Marks the node focusable for keyboard navigation. *)

val node : t -> Renderable.t
(** [node t] returns the underlying renderable node. *)

val options : t -> item list
(** [options t] returns current item list. *)

val set_options : t -> item list -> unit
(** [set_options t items] replaces the item list. Clamps current selection index
    to valid range [0, length - 1]. Triggers re-render. *)

val selected_index : t -> int
(** [selected_index t] returns the 0-based selection index. *)

val selected_item : t -> item option
(** [selected_item t] returns the currently selected item, or [None] if the list
    is empty. *)

val set_selected_index : t -> int -> unit
(** [set_selected_index t i] selects item at index [i]. Index is clamped to
    valid range. Fires [on_change] callback. Triggers re-render and scroll
    adjustment. *)

val set_wrap_selection : t -> bool -> unit
(** [set_wrap_selection t flag] enables or disables wrapping at list boundaries.
*)

val set_show_description : t -> bool -> unit
(** [set_show_description t flag] shows or hides item description lines. *)

val set_show_scroll_indicator : t -> bool -> unit
(** [set_show_scroll_indicator t flag] shows or hides scroll position indicator.
*)

val set_item_spacing : t -> int -> unit
(** [set_item_spacing t spacing] sets vertical spacing between items in cells.
    Clamped to minimum 0. *)

val set_fast_scroll_step : t -> int -> unit
(** [set_fast_scroll_step t step] sets number of items to skip with Ctrl+Up/Down
    navigation. Clamped to minimum 1. *)

val set_background : t -> Ansi.Color.t -> unit
(** [set_background t color] updates unfocused background color. *)

val set_text_color : t -> Ansi.Color.t -> unit
(** [set_text_color t color] updates unfocused text color. *)

val set_focused_background : t -> Ansi.Color.t -> unit
(** [set_focused_background t color] updates focused background color. *)

val set_focused_text_color : t -> Ansi.Color.t -> unit
(** [set_focused_text_color t color] updates focused text color. *)

val set_selected_background : t -> Ansi.Color.t -> unit
(** [set_selected_background t color] updates selected item background color. *)

val set_selected_text_color : t -> Ansi.Color.t -> unit
(** [set_selected_text_color t color] updates selected item text color. *)

val set_description_color : t -> Ansi.Color.t -> unit
(** [set_description_color t color] updates unselected description text color.
*)

val set_selected_description_color : t -> Ansi.Color.t -> unit
(** [set_selected_description_color t color] updates selected item description
    text color. *)

val set_on_change : t -> (int -> unit) option -> unit
(** [set_on_change t callback] registers a callback fired when the selected
    index changes. Receives the new index. *)

val set_on_activate : t -> (int -> unit) option -> unit
(** [set_on_activate t callback] registers a callback fired when the current
    item is activated via Enter key. Receives the activated item index. *)

val handle_key : t -> Event.key -> bool
(** [handle_key t event] processes keyboard input for navigation and activation.
    Supports Up/Down arrows, j/k, Ctrl+Up/Down for fast scroll, Page Up/Down,
    and Enter for activation. Returns [true] when the event is consumed, [false]
    otherwise. *)

val handle_mouse : t -> Event.mouse -> unit
(** [handle_mouse t event] processes mouse input. Left click selects an item at
    the click position. Scroll wheel navigates up or down. *)

val apply_props : t -> Props.t -> unit
(** [apply_props select props] applies [props] to a mounted select using its
    setters for all mutable properties. *)
