open Input
open Ui

type t = Ui.element

(* Re-exports from Ui *)

include struct
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

(* Key and identity *)

include Tile_key

(* Low-level events (composable) *)

module Events = Tile_events

(* Form Components *)

include Tile_forms

(* Common widget styles *)

type variant =
  [ `Primary | `Secondary | `Success | `Danger | `Warning | `Info | `Ghost ]

type hotkey = string

let variant_style = function
  | `Primary -> Style.(bg (RGB (59, 130, 246)) ++ fg White)
  | `Secondary -> Style.(bg (Index 240) ++ fg (Index 250))
  | `Success -> Style.(bg (RGB (34, 197, 94)) ++ fg White)
  | `Danger -> Style.(bg (RGB (239, 68, 68)) ++ fg White)
  | `Warning -> Style.(bg (RGB (245, 158, 11)) ++ fg Black)
  | `Info -> Style.(bg (RGB (14, 165, 233)) ++ fg White)
  | `Ghost -> Style.(fg (Index 250))

let variant_hover_style = function
  | `Primary -> Style.(bg (RGB (37, 99, 235)) ++ fg White ++ bold)
  | `Secondary -> Style.(bg (Index 242) ++ fg White ++ bold)
  | `Success -> Style.(bg (RGB (22, 163, 74)) ++ fg White ++ bold)
  | `Danger -> Style.(bg (RGB (220, 38, 38)) ++ fg White ++ bold)
  | `Warning -> Style.(bg (RGB (217, 119, 6)) ++ fg Black ++ bold)
  | `Info -> Style.(bg (RGB (2, 132, 199)) ++ fg White ++ bold)
  | `Ghost -> Style.(fg White ++ underline)

let disabled_style = Style.(fg (Index 240) ++ bg (Index 236))

let size_padding = function
  | `Small -> xy 2 0
  | `Medium -> xy 3 1
  | `Large -> xy 4 2

let parse_key_chord chord =
  match String.lowercase_ascii chord with
  | "enter" -> Some (fun k -> k.key = Enter)
  | "space" -> Some (fun k -> k.key = Char (Uchar.of_int 0x20))
  | "escape" | "esc" -> Some (fun k -> k.key = Escape)
  | _ -> None

(* Convenience: make any element interactive *)

let interact ?key ?tab_index ?auto_focus ?(disabled = false) ?(hotkeys = [])
    ?tooltip ?on_click ?on_hover ?on_focus ?on_drag ?on_key element =
  let key = match key with Some k -> k | None -> use_key ~prefix:"interact" in

  (* Define noop handlers for unconditional hook calls *)
  let noop_click () = () in
  let noop_hover _entering = () in
  let noop_focus _has_focus = () in
  let noop_drag _evt = () in
  let noop_key _evt = () in

  (* Wire up event handlers unconditionally *)
  Events.on_click key (Option.value ~default:noop_click on_click);
  Events.on_hover key (Option.value ~default:noop_hover on_hover);
  Events.on_focus key (Option.value ~default:noop_focus on_focus);
  Events.on_drag key (Option.value ~default:noop_drag on_drag);

  (* Handle keyboard events including hotkeys *)
  let handle_key =
    match (on_key, hotkeys, on_click) with
    | Some f, [], _ -> f
    | None, [], None -> noop_key
    | _ ->
        (fun event ->
          (* Call custom key handler if provided *)
          Option.iter (fun f -> f event) on_key;

          (* Check hotkeys if we have a click handler *)
          match on_click with
          | Some click_fn when not disabled -> (
              (* Check for Enter/Space defaults *)
              match event.key with
              | Enter -> click_fn ()
              | Char c when Uchar.to_int c = 0x20 -> click_fn ()
              | _ ->
                  (* Check custom hotkeys *)
                  let matches_hotkey =
                    List.exists
                      (fun chord ->
                        match parse_key_chord chord with
                        | Some checker -> checker event
                        | None -> false)
                      hotkeys
                  in
                  if matches_hotkey then click_fn ())
          | _ -> ())
  in
  Events.on_key key handle_key;

  (* Mark as focusable if needed *)
  (if tab_index <> None || auto_focus <> None || on_click <> None then
     let auto_focus = Option.value ~default:false auto_focus in
     Events.focusable key ?tab_index ~auto_focus ());

  (* Add tooltip if hovering *)
  let element_with_tooltip =
    match tooltip with
    | Some tip ->
        let is_hovered = Tile_events.use_hover key in
        if is_hovered then
          Ui.vbox ~gap:(`Cells 0)
            [
              element;
              Ui.box
                ~style:Style.(bg (Index 238) ++ fg (Index 250))
                ~padding:(xy 2 0)
                [ text tip ];
            ]
        else element
    | None -> element
  in

  with_key key element_with_tooltip

(* Interactive wrappers for common Ui containers *)

let box ?display ?position ?box_sizing ?text_align ?flex_direction ?flex_wrap
    ?flex_grow ?flex_shrink ?flex_basis ?align_items ?align_self ?align_content
    ?justify_content ?justify_items ?justify_self ?overflow_x ?overflow_y
    ?aspect_ratio ?scrollbar_width ?inset ?width ?height ?min_width ?min_height
    ?max_width ?max_height ?padding ?margin ?border_width ?gap ?row_gap ?col_gap
    ?style ?border ?border_style ?key ?tab_index ?auto_focus ?disabled ?hotkeys
    ?tooltip ?on_click ?on_hover ?on_focus ?on_drag ?on_key children =
  let base_element =
    Ui.box ?display ?position ?box_sizing ?text_align ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?align_items ?align_self
      ?align_content ?justify_content ?justify_items ?justify_self ?overflow_x
      ?overflow_y ?aspect_ratio ?scrollbar_width ?inset ?width ?height
      ?min_width ?min_height ?max_width ?max_height ?padding ?margin
      ?border_width ?gap ?row_gap ?col_gap ?style ?border ?border_style children
  in
  (* Only wrap with interact if we have interactive properties *)
  match
    ( key,
      tab_index,
      auto_focus,
      hotkeys,
      tooltip,
      on_click,
      on_hover,
      on_focus,
      on_drag,
      on_key )
  with
  | None, None, None, None, None, None, None, None, None, None -> base_element
  | _ ->
      interact ?key ?tab_index ?auto_focus ?disabled ?hotkeys ?tooltip ?on_click
        ?on_hover ?on_focus ?on_drag ?on_key base_element

let vbox ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?border_width ?flex_grow ?flex_shrink ?flex_basis ?align_self
    ?align_items ?justify_content ?gap ?overflow_x ?overflow_y ?style ?border
    ?border_style ?key ?tab_index ?auto_focus ?disabled ?hotkeys ?tooltip
    ?on_click ?on_hover ?on_focus ?on_drag ?on_key children =
  let base_element =
    Ui.vbox ?width ?height ?min_width ?min_height ?max_width ?max_height
      ?padding ?margin ?border_width ?flex_grow ?flex_shrink ?flex_basis
      ?align_self ?align_items ?justify_content ?gap ?overflow_x ?overflow_y
      ?style ?border ?border_style children
  in
  (* Only wrap with interact if we have interactive properties *)
  match
    ( key,
      tab_index,
      auto_focus,
      hotkeys,
      tooltip,
      on_click,
      on_hover,
      on_focus,
      on_drag,
      on_key )
  with
  | None, None, None, None, None, None, None, None, None, None -> base_element
  | _ ->
      interact ?key ?tab_index ?auto_focus ?disabled ?hotkeys ?tooltip ?on_click
        ?on_hover ?on_focus ?on_drag ?on_key base_element

let hbox ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?border_width ?flex_grow ?flex_shrink ?flex_basis ?align_self
    ?align_items ?justify_content ?gap ?overflow_x ?overflow_y ?style ?border
    ?border_style ?key ?tab_index ?auto_focus ?disabled ?hotkeys ?tooltip
    ?on_click ?on_hover ?on_focus ?on_drag ?on_key children =
  let base_element =
    Ui.hbox ?width ?height ?min_width ?min_height ?max_width ?max_height
      ?padding ?margin ?border_width ?flex_grow ?flex_shrink ?flex_basis
      ?align_self ?align_items ?justify_content ?gap ?overflow_x ?overflow_y
      ?style ?border ?border_style children
  in
  (* Only wrap with interact if we have interactive properties *)
  match
    ( key,
      tab_index,
      auto_focus,
      hotkeys,
      tooltip,
      on_click,
      on_hover,
      on_focus,
      on_drag,
      on_key )
  with
  | None, None, None, None, None, None, None, None, None, None -> base_element
  | _ ->
      interact ?key ?tab_index ?auto_focus ?disabled ?hotkeys ?tooltip ?on_click
        ?on_hover ?on_focus ?on_drag ?on_key base_element

(* Opinionated controls *)

let button ?(style = Style.empty) ?(border = Border.rounded)
    ?(border_style = Style.empty) ?(variant = `Secondary) ?(size = `Medium)
    ?(disabled = false) ?(focused = false) ?(hotkeys = []) ?tab_index ~label
    ~on_click ?icon ?tooltip () =
  let key = use_key ~prefix:"btn" in

  let is_hovered = Tile_events.use_hover key in
  let is_focused = Tile_events.use_focus key in
  let is_pressed, set_pressed, _ = Hook.use_state false in

  (* Handle drag for press state *)
  Events.on_drag key (fun evt ->
      match evt.phase with
      | `Start -> set_pressed true
      | `End -> set_pressed false
      | `Move -> ());

  let base_style =
    if disabled then disabled_style
    else if is_pressed then Style.(variant_hover_style variant ++ dim)
    else if is_hovered || is_focused || focused then variant_hover_style variant
    else variant_style variant
  in

  let combined_style = Style.(base_style ++ style) in
  let final_border_style =
    if is_focused || focused then
      Style.(border_style ++ fg (RGB (59, 130, 246)))
    else border_style
  in

  let content =
    match icon with
    | Some icon_str -> Ui.hbox ~gap:(`Cells 1) [ text icon_str; text label ]
    | None -> text label
  in

  let button_element =
    Ui.box ~border ~border_style:final_border_style ~style:combined_style
      ~padding:(size_padding size) [ content ]
  in

  interact ~key ?tab_index
    ?auto_focus:(if focused then Some true else None)
    ~disabled ~hotkeys ?tooltip ~on_click button_element

let link ?(style = Style.empty) ?(underline = true) ?(hotkeys = []) ?tab_index
    ~label ~on_click () =
  let key = use_key ~prefix:"link" in

  let is_hovered = Tile_events.use_hover key in
  let is_focused = Tile_events.use_focus key in

  let link_style =
    let base = if underline then Style.underline else Style.empty in
    if is_hovered || is_focused then
      Style.(base ++ fg (RGB (59, 130, 246)) ++ bold)
    else Style.(base ++ fg (RGB (96, 165, 250)))
  in

  let combined_style = Style.(link_style ++ style) in

  interact ~key ?tab_index ~hotkeys ~on_click (text ~style:combined_style label)

let checkbox ?(style = Style.empty) ?tab_index ~checked ~label ~on_change () =
  let key = use_key ~prefix:"checkbox" in

  let is_focused = Tile_events.use_focus key in

  let checkbox_icon = if checked then "[x]" else "[ ]" in
  let icon_style =
    if is_focused then Style.(style ++ fg (RGB (59, 130, 246)) ++ bold)
    else style
  in

  let element =
    Ui.hbox ~gap:(`Cells 1)
      [ text ~style:icon_style checkbox_icon; text ~style label ]
  in

  interact ~key ?tab_index ~on_click:(fun () -> on_change (not checked)) element

let radio ?(style = Style.empty) ?tab_index ~checked ~label ~on_change () =
  let key = use_key ~prefix:"radio" in

  let is_focused = Tile_events.use_focus key in

  let radio_icon = if checked then "(•)" else "( )" in
  let icon_style =
    if is_focused then Style.(style ++ fg (RGB (59, 130, 246)) ++ bold)
    else style
  in

  let element =
    Ui.hbox ~gap:(`Cells 1)
      [ text ~style:icon_style radio_icon; text ~style label ]
  in

  interact ~key ?tab_index ~on_click:(fun () -> on_change true) element
