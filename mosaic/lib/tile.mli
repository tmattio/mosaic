(** Mosaic.Tile — interactive wrappers and widgets atop Mosaic.Ui *)

(** {1 Re-exports from Ui} *)

type element = Ui.element

module Style = Ui.Style
module Border = Ui.Border
module Theme = Ui.Theme
module Attr = Ui.Attr
module Key = Ui.Key
module Table = Ui.Table
module Spinner = Ui.Spinner
module Progress_bar = Ui.Progress_bar

(** {2 Core UI functions} *)

val text :
  ?style:Style.t ->
  ?align:[ `Left | `Center | `Right ] ->
  ?wrap:[ `Wrap | `Truncate | `Clip ] ->
  ?overflow_x:Ui.overflow ->
  ?overflow_y:Ui.overflow ->
  string ->
  element

val with_key : Attr.key -> element -> element

(** {2 Layout helpers} *)

val cells : int -> [ `Cells of int ]
val pct : float -> [ `Pct of float ]
val xy : int -> int -> [> `Cells of int ] Ui.sides

val sides :
  ?top:int ->
  ?bottom:int ->
  ?left:int ->
  ?right:int ->
  unit ->
  [> `Cells of int ] Ui.sides

(** {1 Keys and identity} *)

type key = Attr.key

val use_key : prefix:string -> key
(** Allocate a stable key once per callsite; survives re-renders. *)

(** {1 Low-level events (composable)} *)

module Events : sig
  type key_event = Input.key_event
  (** Concrete event payloads usable in callbacks *)

  type drag_phase = [ `Start | `Move | `End ]

  type drag = {
    phase : drag_phase;
    x : int;
    y : int;  (** current cursor (0-based) *)
    dx : int;
    dy : int;  (** delta since last event *)
    start_x : int;
    start_y : int;
  }

  val on_click : key -> (unit -> unit) -> unit
  (** Subscribe this keyed element to events. Lifetime is tied to the calling
      component. *)

  val on_hover : key -> (bool -> unit) -> unit
  val on_focus : key -> (bool -> unit) -> unit
  val on_drag : key -> (drag -> unit) -> unit
  val on_key : key -> (key_event -> unit) -> unit

  val focusable : key -> ?tab_index:int -> ?auto_focus:bool -> unit -> unit
  (** Mark element focusable and place it in the tab order. *)

  val use_bounds : key -> Ui.Layout_snapshot.rect option
  (** Geometry observation (reads from runtime snapshot if available). *)
end

(** {1 Convenience: make any element interactive} *)

type hotkey = string
(** Simple chords: "enter", "space", "esc". Extend later if needed. *)

val interact :
  ?key:key ->
  ?tab_index:int ->
  ?auto_focus:bool ->
  ?disabled:bool ->
  ?hotkeys:hotkey list ->
  ?tooltip:string ->
  ?on_click:(unit -> unit) ->
  ?on_hover:(bool -> unit) ->
  ?on_focus:(bool -> unit) ->
  ?on_drag:(Events.drag -> unit) ->
  ?on_key:(Events.key_event -> unit) ->
  element ->
  element
(** Wrap any element to give it identity, focus behavior, hotkeys and handlers.
    Rendering is unchanged; this only wires events and optional tooltip. *)

(** {1 Interactive wrappers for common Ui containers} *)

val box :
  ?display:Ui.display ->
  ?position:Ui.position ->
  ?box_sizing:Ui.box_sizing ->
  ?text_align:Ui.text_align ->
  ?flex_direction:Ui.flex_direction ->
  ?flex_wrap:Ui.flex_wrap ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:Ui.dimension ->
  ?align_items:Ui.align_items ->
  ?align_self:Ui.align_self ->
  ?align_content:Ui.align_content ->
  ?justify_content:Ui.justify_content ->
  ?justify_items:Ui.justify_items ->
  ?justify_self:Ui.justify_self ->
  ?overflow_x:Ui.overflow ->
  ?overflow_y:Ui.overflow ->
  ?aspect_ratio:float ->
  ?scrollbar_width:float ->
  ?inset:Ui.length_percentage_auto Ui.sides ->
  ?width:Ui.dimension ->
  ?height:Ui.dimension ->
  ?min_width:Ui.dimension ->
  ?min_height:Ui.dimension ->
  ?max_width:Ui.dimension ->
  ?max_height:Ui.dimension ->
  ?padding:Ui.length_percentage Ui.sides ->
  ?margin:Ui.length_percentage_auto Ui.sides ->
  ?border_width:Ui.length_percentage Ui.sides ->
  ?gap:Ui.length_percentage ->
  ?row_gap:Ui.length_percentage ->
  ?col_gap:Ui.length_percentage ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  (* interactive additions *)
  ?key:key ->
  ?tab_index:int ->
  ?auto_focus:bool ->
  ?disabled:bool ->
  ?hotkeys:hotkey list ->
  ?tooltip:string ->
  ?on_click:(unit -> unit) ->
  ?on_hover:(bool -> unit) ->
  ?on_focus:(bool -> unit) ->
  ?on_drag:(Events.drag -> unit) ->
  ?on_key:(Events.key_event -> unit) ->
  element list ->
  element

val vbox :
  ?width:Ui.dimension ->
  ?height:Ui.dimension ->
  ?min_width:Ui.dimension ->
  ?min_height:Ui.dimension ->
  ?max_width:Ui.dimension ->
  ?max_height:Ui.dimension ->
  ?padding:Ui.length_percentage Ui.sides ->
  ?margin:Ui.length_percentage_auto Ui.sides ->
  ?border_width:Ui.length_percentage Ui.sides ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:Ui.dimension ->
  ?align_self:Ui.align_self ->
  ?align_items:Ui.align_items ->
  ?justify_content:Ui.justify_content ->
  ?gap:Ui.length_percentage ->
  ?overflow_x:Ui.overflow ->
  ?overflow_y:Ui.overflow ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  (* interactive additions *)
  ?key:key ->
  ?tab_index:int ->
  ?auto_focus:bool ->
  ?disabled:bool ->
  ?hotkeys:hotkey list ->
  ?tooltip:string ->
  ?on_click:(unit -> unit) ->
  ?on_hover:(bool -> unit) ->
  ?on_focus:(bool -> unit) ->
  ?on_drag:(Events.drag -> unit) ->
  ?on_key:(Events.key_event -> unit) ->
  element list ->
  element

val hbox :
  ?width:Ui.dimension ->
  ?height:Ui.dimension ->
  ?min_width:Ui.dimension ->
  ?min_height:Ui.dimension ->
  ?max_width:Ui.dimension ->
  ?max_height:Ui.dimension ->
  ?padding:Ui.length_percentage Ui.sides ->
  ?margin:Ui.length_percentage_auto Ui.sides ->
  ?border_width:Ui.length_percentage Ui.sides ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:Ui.dimension ->
  ?align_self:Ui.align_self ->
  ?align_items:Ui.align_items ->
  ?justify_content:Ui.justify_content ->
  ?gap:Ui.length_percentage ->
  ?overflow_x:Ui.overflow ->
  ?overflow_y:Ui.overflow ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  (* interactive additions *)
  ?key:key ->
  ?tab_index:int ->
  ?auto_focus:bool ->
  ?disabled:bool ->
  ?hotkeys:hotkey list ->
  ?tooltip:string ->
  ?on_click:(unit -> unit) ->
  ?on_hover:(bool -> unit) ->
  ?on_focus:(bool -> unit) ->
  ?on_drag:(Events.drag -> unit) ->
  ?on_key:(Events.key_event -> unit) ->
  element list ->
  element

(** {1 Opinionated controls} *)

type variant =
  [ `Primary | `Secondary | `Success | `Danger | `Warning | `Info | `Ghost ]

type size = [ `Small | `Medium | `Large ]

val button :
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  ?variant:variant ->
  ?size:size ->
  ?disabled:bool ->
  ?focused:bool ->
  ?hotkeys:hotkey list ->
  ?tab_index:int ->
  label:string ->
  on_click:(unit -> unit) ->
  ?icon:string ->
  ?tooltip:string ->
  unit ->
  element

val link :
  ?style:Style.t ->
  ?underline:bool ->
  ?hotkeys:hotkey list ->
  ?tab_index:int ->
  label:string ->
  on_click:(unit -> unit) ->
  unit ->
  element
(** Text-styled clickable element; respects focus and Enter/Space. *)

val checkbox :
  ?style:Style.t ->
  ?tab_index:int ->
  checked:bool ->
  label:string ->
  on_change:(bool -> unit) ->
  unit ->
  element

val radio :
  ?style:Style.t ->
  ?tab_index:int ->
  checked:bool ->
  label:string ->
  on_change:(bool -> unit) ->
  unit ->
  element

(** {1 Utilities} *)

val use_keyboard :
  ?ctrl:bool ->
  ?alt:bool ->
  ?shift:bool ->
  Input.key ->
  unit Engine.Cmd.t ->
  unit
(** Global-ish shortcut helper that dispatches a command when a chord is hit.
    Intended for app-level bindings; for element-local keys use [on_key]. *)
