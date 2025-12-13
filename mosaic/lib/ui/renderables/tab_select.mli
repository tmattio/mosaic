(** Horizontal tab selection widget with scrolling and descriptions.

    Tab_select provides a keyboard and mouse navigable tab bar with optional
    descriptions, scroll indicators, and customizable appearance. Tabs can wrap
    around when reaching edges and support activation callbacks.

    {1 Overview}

    Tab_select renders tabs horizontally with fixed width cells. When tab count
    exceeds available space, it shows scroll arrows and provides keyboard/mouse
    navigation. Each tab can have a label and optional description line.

    {1 Usage Basics}

    Mount a basic tab selector:
    {[
      let renderer = Renderer.create () in
      let tabs =
        [
          ("Home", Some "Main page");
          ("Settings", Some "Configure app");
          ("Help", None);
        ]
      in
      match Renderer.create_node renderer () with
      | Error err -> failwith (Renderable.error_to_string err)
      | Ok node ->
          let props = Tab_select.Props.make ~options:tabs () in
          let selector = Tab_select.mount ~props node in
          Tab_select.set_on_change selector
            (Some (fun index -> Printf.printf "Selected tab %d\n" index));
          selector
    ]}

    {1 Navigation}

    - Keyboard: Left/Right arrows, [ and ], Home/End, Enter to activate
    - Mouse: Click tabs, scroll wheel to navigate
    - Wrapping: Enable with [wrap_selection] to cycle through edges *)

type t

module Props : sig
  type t

  val make :
    ?options:(string * string option) list ->
    ?wrap_selection:bool ->
    ?show_description:bool ->
    ?show_underline:bool ->
    ?show_scroll_arrows:bool ->
    ?mouse_navigation:bool ->
    ?autofocus:bool ->
    ?tab_width:int ->
    ?background:Ansi.Color.t ->
    ?text_color:Ansi.Color.t ->
    ?focused_background:Ansi.Color.t ->
    ?focused_text:Ansi.Color.t ->
    ?selected_background:Ansi.Color.t ->
    ?selected_text:Ansi.Color.t ->
    ?selected_description:Ansi.Color.t ->
    unit ->
    t

  val default : t
  val equal : t -> t -> bool
end

val mount : ?props:Props.t -> Renderable.t -> t
(** [mount ?props node] configures [node] to render a tab selector. *)

val node : t -> Renderable.t
(** [node t] returns the underlying renderable node. *)

val options : t -> (string * string option) list
(** [options t] returns current tab list. *)

val option_at : t -> int -> (string * string option) option
(** [option_at t i] returns [Some (label, desc)] at index [i] or [None] if out
    of bounds. *)

val set_options : t -> (string * string option) list -> unit
(** [set_options t tabs] replaces tab list. Clamps selected index to valid
    range. *)

val selected_index : t -> int
(** [selected_index t] returns current selection index. *)

val selected_option : t -> (string * string option) option
(** [selected_option t] returns the currently selected tab, if any. *)

val set_selected_index : t -> int -> unit
(** [set_selected_index t index] selects tab at [index]. Fires [on_change]
    callback. *)

val set_tab_width : t -> int -> unit
(** [set_tab_width t width] updates fixed tab width. *)

val set_wrap_selection : t -> bool -> unit
(** [set_wrap_selection t flag] enables or disables edge wrapping. *)

val set_show_description : t -> bool -> unit
(** [set_show_description t flag] shows or hides description line. *)

val set_show_underline : t -> bool -> unit
(** [set_show_underline t flag] shows or hides selection underline. *)

val set_show_scroll_arrows : t -> bool -> unit
(** [set_show_scroll_arrows t flag] shows or hides scroll indicators. *)

val set_extra_navigation : t -> bool -> unit
(** [set_extra_navigation t flag] enables Home/End keys and mouse interactions.
    Disabled by default to keep the minimal set of bindings; enable for
    power-user shortcuts. *)

val set_background : t -> Ansi.Color.t -> unit
(** [set_background t color] updates unfocused background color. *)

val set_text_color : t -> Ansi.Color.t -> unit
(** [set_text_color t color] updates unfocused text color. *)

val set_focused_background : t -> Ansi.Color.t -> unit
(** [set_focused_background t color] updates focused background color. *)

val set_focused_text : t -> Ansi.Color.t -> unit
(** [set_focused_text t color] updates focused text color. *)

val set_selected_background : t -> Ansi.Color.t -> unit
(** [set_selected_background t color] updates selected tab background. *)

val set_selected_text : t -> Ansi.Color.t -> unit
(** [set_selected_text t color] updates selected tab text color. *)

val set_selected_description_color : t -> Ansi.Color.t -> unit
(** [set_selected_description_color t color] updates description text color. *)

val set_on_change : t -> (int -> unit) option -> unit
(** [set_on_change t callback] registers selection change handler. *)

val set_on_activate : t -> (int -> unit) option -> unit
(** [set_on_activate t callback] registers activation (Enter key) handler. *)

val set_on_change_full :
  t -> (int * (string * string option) -> unit) option -> unit
(** [set_on_change_full t callback] registers selection change handler that
    receives both index and tab. The index-only and full handlers both fire when
    set. *)

val set_on_activate_full :
  t -> (int * (string * string option) -> unit) option -> unit
(** [set_on_activate_full t callback] registers activation handler that receives
    both index and tab. The index-only and full handlers both fire when set. *)

val handle_key : t -> Event.key -> bool
(** [handle_key t event] processes keyboard input. Returns [true] if consumed.
*)

val handle_mouse : t -> Event.mouse -> unit
(** [handle_mouse t event] processes mouse input for clicks and scrolling. May
    stop event propagation if consumed. *)

val apply_props : t -> Props.t -> unit
(** [apply_props tabs props] applies [props] to a mounted tab selector using its
    setters and internal recomputation helpers. *)
