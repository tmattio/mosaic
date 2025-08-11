(** Mosaic.Tile — interactive wrappers and widgets atop Mosaic.Ui *)

open Ui

type t = Ui.element

(** {1 Re-exports from Ui} *)

include module type of struct
  module Border = Ui.Border

  (* Core UI functions *)

  let text = Ui.text

  (* Core Layout Components *)
  let zbox = Ui.zbox
  let spacer = Ui.spacer
  let divider = Ui.divider
  let scroll_view = Ui.scroll_view
  let center = Ui.center
  let flow = Ui.flow
  let block = Ui.block
  let grid = Ui.grid
  let grid_item = Ui.grid_item

  (* Utility Functions *)
  let styled = Ui.styled
  let empty = Ui.empty
  let cells = Ui.cells
  let pct = Ui.pct
  let auto = Ui.auto
  let calc = Ui.calc
  let all = Ui.all
  let xy = Ui.xy
  let sides = Ui.sides

  (* Grid track sizing functions *)
  let fr = Ui.fr
  let minmax = Ui.minmax
  let fit_content_track = Ui.fit_content_track
  let min_content = Ui.min_content
  let max_content = Ui.max_content
  let track_cells = Ui.track_cells
  let track_pct = Ui.track_pct
  let track_auto = Ui.track_auto
  let repeat = Ui.repeat
  let grid_area = Ui.grid_area

  (* Visual Components *)
  let image = Ui.image
  let list = Ui.list
  let rich_text = Ui.rich_text
  let panel = Ui.panel
  let progress_bar = Ui.progress_bar
  let spinner = Ui.spinner
  let table = Ui.table
  let tree = Ui.tree
  let canvas = Ui.canvas
end

(** {1 Keys and identity} *)

include module type of Tile_key

(** {1 Low-level events (composable)} *)

module Events = Tile_events
(** Event subscriptions module - alias to Tile_events for all event handling
    functionality *)

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

(** {1 Form Components} *)

(* Include form components early so we get the size type *)
include module type of Tile_forms

(** {1 Opinionated controls} *)

type variant =
  [ `Primary | `Secondary | `Success | `Danger | `Warning | `Info | `Ghost ]

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
