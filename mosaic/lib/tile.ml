open Input

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

(** Core UI functions *)
let text = Ui.text

let with_key = Ui.with_key

(** Layout helpers *)
let cells = Ui.cells

let pct = Ui.pct
let xy = Ui.xy
let sides = Ui.sides

(** {1 Keys and identity} *)

type key = Attr.key

(** Key generation implementation *)

(* Global counter for generating unique IDs *)
let next_key_id : int ref = ref 0

(* Generate a unique key with stable identity per hook call site *)
let use_key ~prefix =
  (* Use a ref to store the key, making it stable across re-renders *)
  let key_ref = Hook.use_ref None in
  match !key_ref with
  | Some key -> key
  | None ->
      let id = !next_key_id in
      next_key_id := id + 1;
      let key = Ui.Attr.key (Printf.sprintf "%s_%d" prefix id) in
      key_ref := Some key;
      key

(** {1 Low-level events (composable)} *)

module Events = struct
  type key_event = Input.key_event
  type drag_phase = [ `Start | `Move | `End ]

  type drag = {
    phase : drag_phase;
    x : int;
    y : int;
    dx : int;
    dy : int;
    start_x : int;
    start_y : int;
  }

  (* Convert internal drag event to our public drag type *)
  let convert_drag_event (evt : Engine.Input_router.drag_event) : drag =
    {
      phase = evt.phase;
      x = evt.x;
      y = evt.y;
      dx = evt.dx;
      dy = evt.dy;
      start_x = evt.start_x;
      start_y = evt.start_y;
    }

  let on_click = Tile_events.use_click

  let on_hover key callback =
    Hook.use_effect
      ~deps:(Deps.keys [ Deps.ui_key key ])
      (fun () ->
        match Runtime_context.current () with
        | None -> None
        | Some ctx ->
            let handler = Engine.Input_router.Hover (key, callback) in
            let id = Engine.Input_router.subscribe ctx.input_router handler in
            Some (fun () -> Engine.Input_router.unsubscribe ctx.input_router id))

  let on_focus key callback =
    Hook.use_effect
      ~deps:(Deps.keys [ Deps.ui_key key ])
      (fun () ->
        match Runtime_context.current () with
        | None -> None
        | Some ctx ->
            let handler =
              Engine.Input_router.Focus
                ( key,
                  fun has_focus ->
                    callback has_focus;
                    () )
            in
            let id = Engine.Input_router.subscribe ctx.input_router handler in
            Some (fun () -> Engine.Input_router.unsubscribe ctx.input_router id))

  let on_drag key callback =
    Hook.use_effect
      ~deps:(Deps.keys [ Deps.ui_key key ])
      (fun () ->
        match Runtime_context.current () with
        | None -> None
        | Some ctx ->
            let handler =
              Engine.Input_router.Drag
                ( key,
                  fun evt ->
                    callback (convert_drag_event evt);
                    () )
            in
            let id = Engine.Input_router.subscribe ctx.input_router handler in
            Some (fun () -> Engine.Input_router.unsubscribe ctx.input_router id))

  let on_key = Tile_events.use_key_press
  let focusable = Tile_events.use_focusable
  let use_bounds = Tile_events.use_bounds
end

(** {1 Common widget styles} *)

type variant =
  [ `Primary | `Secondary | `Success | `Danger | `Warning | `Info | `Ghost ]

type size = [ `Small | `Medium | `Large ]
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

(** {1 Convenience: make any element interactive} *)

let interact ?key ?tab_index ?auto_focus ?(disabled = false) ?(hotkeys = [])
    ?tooltip ?on_click ?on_hover ?on_focus ?on_drag ?on_key element =
  let key = match key with Some k -> k | None -> use_key ~prefix:"interact" in

  (* Wire up event handlers *)
  Option.iter (fun f -> Events.on_click key f) on_click;
  Option.iter (fun f -> Events.on_hover key f) on_hover;
  Option.iter (fun f -> Events.on_focus key f) on_focus;
  Option.iter (fun f -> Events.on_drag key f) on_drag;

  (* Handle keyboard events including hotkeys *)
  let handle_key =
    match (on_key, hotkeys, on_click) with
    | Some f, [], _ -> Some f
    | None, [], None -> None
    | _ ->
        Some
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
  Option.iter (fun f -> Events.on_key key f) handle_key;

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

(** {1 Interactive wrappers for common Ui containers} *)

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

(** {1 Opinionated controls} *)

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

(** {1 Utilities} *)

let use_keyboard ?ctrl ?alt ?shift key cmd =
  Hook.use_key ?ctrl ?alt ?shift key cmd
