(** Flexible form components inspired by modern web component libraries *)

open Ui
open Input

(* Helper functions *)
let xy = Ui.xy
let text = Ui.text

(* Re-use key functionality from Tile_key *)
let use_key = Tile_key.use_key
let with_key = Tile_key.with_key

(* Size type *)
type size = [ `Small | `Medium | `Large ]

let size_padding = function
  | `Small -> xy 2 0
  | `Medium -> xy 3 1
  | `Large -> xy 4 2

let disabled_style = Style.(fg (Index 240) ++ bg (Index 236))

type label_position =
  [ `Top (* Label above input *)
  | `Left (* Label to the left of input *)
  | `Right (* Label to the right of input *)
  | `Inline (* Label inline with input, no spacing *)
  | `Hidden ]
(* Label hidden but still accessible *)

(** Render label with optional required indicator *)
let render_label ~label ~label_position ~label_width ~required ~error
    ~is_focused =
  if label = "" || label_position = `Hidden then Ui.empty
  else
    let label_style =
      if error <> None then Style.(fg (RGB (239, 68, 68))) (* Red for error *)
      else if is_focused then Style.(fg (RGB (59, 130, 246)) ++ bold)
      else Style.(fg (Index 250))
    in
    let label_text = if required then label ^ " *" else label in
    match label_width with
    | Some width ->
        (* Fixed width label for alignment *)
        Ui.box ~width:(`Cells width) [ text ~style:label_style label_text ]
    | None -> text ~style:label_style label_text

(** Render helper text or error message *)
let render_helper_or_error ~error ~helper_text =
  match (error, helper_text) with
  | Some err, _ -> text ~style:Style.(fg (RGB (239, 68, 68)) ++ italic) err
  | None, Some help -> text ~style:Style.(fg (Index 240) ++ italic) help
  | None, None -> Ui.empty

(** Enhanced input field with flexible label positioning *)
let input ?(label_position = `Left) ?(label_width = None) ?(required = false)
    ?(helper_text = None) ?(error = None) ?(disabled = false) ?(size = `Small)
    ?(style = Style.empty) ?border ?(border_style = Style.empty)
    ?(password = false) ?tab_index ?placeholder ?value ?on_change ?on_submit
    ~label () =
  let key = use_key ~prefix:"input" in

  (* State management *)
  let internal_value, set_internal_value, _ =
    Hook.use_state (Option.value ~default:"" value)
  in
  let current_value = Option.value ~default:internal_value value in
  let is_focused = Tile_events.use_focus key in

  (* Handle blur for submit *)
  Hook.use_effect
    ~deps:(Deps.keys [ Deps.bool is_focused ])
    (fun () ->
      if (not is_focused) && Option.is_some on_submit then
        Option.iter (fun f -> f current_value) on_submit;
      None);

  (* Helper to check if a character is printable *)
  let is_printable c =
    let ch = Uchar.to_char c in
    ch >= ' ' && ch <= '~'
  in

  (* Handle keyboard events *)
  Tile_events.use_key_press_filter key (fun event ->
      if (not is_focused) || disabled then false
      else
        match event.key with
        | Enter ->
            Option.iter (fun f -> f current_value) on_submit;
            true
        | Char c when is_printable c ->
            let ch = String.make 1 (Uchar.to_char c) in
            let new_value = current_value ^ ch in
            (match value with
            | None -> set_internal_value new_value
            | Some _ -> ());
            Option.iter (fun f -> f new_value) on_change;
            true
        | Backspace when String.length current_value > 0 ->
            let new_value =
              String.sub current_value 0 (pred (String.length current_value))
            in
            (match value with
            | None -> set_internal_value new_value
            | Some _ -> ());
            Option.iter (fun f -> f new_value) on_change;
            true
        | _ -> false);

  (* Mark as focusable *)
  Tile_events.use_focusable key ?tab_index ();

  (* Determine styles *)
  let display_value =
    if password then String.make (String.length current_value) '*'
    else if current_value = "" && placeholder <> None then
      Option.get placeholder
    else current_value
  in

  let value_style =
    if error <> None then Style.(fg (RGB (239, 68, 68))) (* Red for error *)
    else if current_value = "" && placeholder <> None then
      Style.(fg (Index 240))
    else if is_focused then Style.(fg White ++ bold)
    else Style.(fg (Index 250))
  in

  let combined_style = Style.(style ++ value_style) in
  let padding = size_padding size in

  (* Build input field *)
  let input_field =
    match border with
    | Some b ->
        let border_style =
          if error <> None then Style.(border_style ++ fg (RGB (239, 68, 68)))
          else if is_focused then
            Style.(border_style ++ fg (RGB (59, 130, 246)))
          else border_style
        in
        Ui.box ~border:b ~border_style ~padding
          ~style:(if disabled then disabled_style else Style.empty)
          [
            Ui.hbox
              [
                text ~style:combined_style display_value;
                (if is_focused then
                   text ~style:Style.(fg (RGB (59, 130, 246))) " ▌"
                 else Ui.empty);
              ];
          ]
    | None ->
        Ui.hbox
          [
            text "[";
            text ~style:combined_style display_value;
            text "]";
            (if is_focused then text ~style:Style.(fg (RGB (59, 130, 246))) " ◀"
             else text "");
          ]
  in

  (* Compose based on label position *)
  let label_element =
    render_label ~label ~label_position ~label_width ~required ~error
      ~is_focused
  in
  let helper_element = render_helper_or_error ~error ~helper_text in

  let main_content =
    match label_position with
    | `Top -> Ui.vbox ~gap:(`Cells 0) [ label_element; input_field ]
    | `Left -> Ui.hbox ~gap:(`Cells 1) [ label_element; input_field ]
    | `Right -> Ui.hbox ~gap:(`Cells 1) [ input_field; label_element ]
    | `Inline -> Ui.hbox ~gap:(`Cells 0) [ label_element; input_field ]
    | `Hidden -> input_field
  in

  with_key key
    (if helper_element = Ui.empty then main_content
     else Ui.vbox ~gap:(`Cells 0) [ main_content; helper_element ])

(** Select/Dropdown with flexible configuration *)
let select ?(label_position = `Left) ?(label_width = None) ?(required = false)
    ?(helper_text = None) ?(error = None) ?(disabled = false) ?(size = `Small)
    ?(style = Style.empty) ?border ?(border_style = Style.empty) ?tab_index
    ?selected ?on_change ~label ~options () =
  let key = use_key ~prefix:"select" in

  (* State management *)
  let internal_selected, set_internal_selected, _ =
    Hook.use_state (Option.value ~default:0 selected)
  in
  let current_selected = Option.value ~default:internal_selected selected in
  let is_expanded, set_expanded, _ = Hook.use_state false in
  let is_focused = Tile_events.use_focus key in

  (* Handle keyboard events *)
  Tile_events.use_key_press_filter key (fun event ->
      if disabled then false
      else if is_expanded then
        match event.key with
        | Up when current_selected > 0 ->
            let new_selected = current_selected - 1 in
            (match selected with
            | None -> set_internal_selected new_selected
            | Some _ -> ());
            Option.iter
              (fun f -> f new_selected (List.nth options new_selected))
              on_change;
            true
        | Down when current_selected < List.length options - 1 ->
            let new_selected = current_selected + 1 in
            (match selected with
            | None -> set_internal_selected new_selected
            | Some _ -> ());
            Option.iter
              (fun f -> f new_selected (List.nth options new_selected))
              on_change;
            true
        | Enter ->
            (* Select current item and close *)
            Option.iter
              (fun f -> f current_selected (List.nth options current_selected))
              on_change;
            set_expanded false;
            true
        | Escape ->
            set_expanded false;
            true
        | Char c when Uchar.to_int c = 0x20 ->
            (* space - select current and close dropdown *)
            Option.iter
              (fun f -> f current_selected (List.nth options current_selected))
              on_change;
            set_expanded false;
            true
        | _ -> false
      else if is_focused then
        match event.key with
        | Enter ->
            set_expanded true;
            true
        | Char c when Uchar.to_int c = 0x20 ->
            (* space *)
            set_expanded true;
            true
        | Up when current_selected > 0 ->
            let new_selected = current_selected - 1 in
            (match selected with
            | None -> set_internal_selected new_selected
            | Some _ -> ());
            Option.iter
              (fun f -> f new_selected (List.nth options new_selected))
              on_change;
            true
        | Down when current_selected < List.length options - 1 ->
            let new_selected = current_selected + 1 in
            (match selected with
            | None -> set_internal_selected new_selected
            | Some _ -> ());
            Option.iter
              (fun f -> f new_selected (List.nth options new_selected))
              on_change;
            true
        | _ -> false
      else false);

  (* Mark as focusable *)
  Tile_events.use_focusable key ?tab_index ();

  let padding = size_padding size in

  (* Build select field *)
  let select_field =
    match border with
    | Some b ->
        let border_style =
          if error <> None then Style.(border_style ++ fg (RGB (239, 68, 68)))
          else if is_focused then
            Style.(border_style ++ fg (RGB (59, 130, 246)))
          else border_style
        in
        Ui.box ~border:b ~border_style ~padding
          ~style:(if disabled then disabled_style else style)
          [
            Ui.hbox ~justify_content:`Space_between
              [
                text
                  (if List.length options > 0 then
                     List.nth options current_selected
                   else "");
                text
                  ~style:Style.(fg (Index 240))
                  (if is_expanded then "▲" else "▼");
              ];
          ]
    | None ->
        Ui.hbox
          [
            text "[";
            text
              ~style:(if disabled then disabled_style else style)
              (if List.length options > 0 then List.nth options current_selected
               else "");
            text "] ";
            text
              ~style:Style.(fg (Index 240))
              (if is_expanded then "▲" else "▼");
          ]
  in

  let dropdown_menu =
    if is_expanded then
      Ui.vbox ~margin:(xy 2 0)
        ~style:Style.(bg (Index 236))
        (List.mapi
           (fun i option ->
             let is_selected = i = current_selected in
             let option_style =
               if is_selected then Style.(bg (RGB (59, 130, 246)) ++ fg White)
               else Style.(fg (Index 250))
             in
             text ~style:option_style ("  " ^ option))
           options)
    else Ui.empty
  in

  (* Compose based on label position *)
  let label_element =
    render_label ~label ~label_position ~label_width ~required ~error
      ~is_focused
  in
  let helper_element = render_helper_or_error ~error ~helper_text in

  let main_content =
    match label_position with
    | `Top -> Ui.vbox ~gap:(`Cells 0) [ label_element; select_field ]
    | `Left -> Ui.hbox ~gap:(`Cells 1) [ label_element; select_field ]
    | `Right -> Ui.hbox ~gap:(`Cells 1) [ select_field; label_element ]
    | `Inline -> Ui.hbox ~gap:(`Cells 0) [ label_element; select_field ]
    | `Hidden -> select_field
  in

  with_key key
    (Ui.vbox ~gap:(`Cells 0)
       [
         main_content;
         dropdown_menu;
         (if helper_element <> Ui.empty then helper_element else Ui.empty);
       ])

(** Radio group component *)
let radio_group ?(style = Style.empty) ?(disabled = false) ?tab_index ?selected
    ?on_change ~label ~options () =
  let key = use_key ~prefix:"radio_group" in

  (* Use controlled or uncontrolled mode *)
  let internal_selected, set_internal_selected, _ =
    Hook.use_state (Option.value ~default:0 selected)
  in
  let current_selected = Option.value ~default:internal_selected selected in
  let is_focused = Tile_events.use_focus key in

  (* Handle keyboard events *)
  Tile_events.use_key_press_filter key (fun event ->
      if disabled then false
      else
        match event.key with
        | (Up | Left) when current_selected > 0 ->
            let new_selected = current_selected - 1 in
            (match selected with
            | None -> set_internal_selected new_selected
            | Some _ -> ());
            Option.iter
              (fun f -> f new_selected (List.nth options new_selected))
              on_change;
            true
        | (Down | Right) when current_selected < List.length options - 1 ->
            let new_selected = current_selected + 1 in
            (match selected with
            | None -> set_internal_selected new_selected
            | Some _ -> ());
            Option.iter
              (fun f -> f new_selected (List.nth options new_selected))
              on_change;
            true
        | Char c when Uchar.to_int c = 0x20 ->
            (* space - confirm selection *)
            Option.iter
              (fun f -> f current_selected (List.nth options current_selected))
              on_change;
            true
        | _ -> false);

  (* Mark as focusable *)
  Tile_events.use_focusable key ?tab_index ();

  let label_style =
    if is_focused then Style.(fg (RGB (59, 130, 246)) ++ bold)
    else Style.(fg (Index 250))
  in

  let radio_options =
    Ui.vbox ~gap:(`Cells 0)
      (List.mapi
         (fun i option ->
           let is_selected = i = current_selected in
           let icon = if is_selected then "◉" else "○" in
           let icon_style =
             if is_focused && is_selected then
               Style.(fg (RGB (59, 130, 246)) ++ bold)
             else if is_selected then Style.(fg (Index 250))
             else Style.(fg (Index 240))
           in
           let text_style =
             if is_selected then Style.(style ++ fg White ++ bold)
             else Style.(style ++ fg (Index 245))
           in
           Ui.hbox ~gap:(`Cells 1)
             [ text ~style:icon_style icon; text ~style:text_style option ])
         options)
  in

  with_key key
    (if label = "" then radio_options
     else
       Ui.vbox ~gap:(`Cells 0) [ text ~style:label_style label; radio_options ])

(** Textarea component for multi-line input *)
let textarea ?(style = Style.empty) ?border ?(border_style = Style.empty)
    ?(rows = 3) ?(disabled = false) ?tab_index ?placeholder ?value ?on_change
    ?on_submit ~label () =
  let key = use_key ~prefix:"textarea" in

  (* Use controlled or uncontrolled mode *)
  let internal_value, set_internal_value, _ =
    Hook.use_state (Option.value ~default:"" value)
  in
  let current_value = Option.value ~default:internal_value value in
  let is_focused = Tile_events.use_focus key in

  (* Handle blur for submit *)
  Hook.use_effect
    ~deps:(Deps.keys [ Deps.bool is_focused ])
    (fun () ->
      if (not is_focused) && Option.is_some on_submit then
        Option.iter (fun f -> f current_value) on_submit;
      None);

  (* Helper to check if a character is printable *)
  let is_printable c =
    let ch = Uchar.to_char c in
    ch >= ' ' && ch <= '~'
  in

  (* Handle keyboard events *)
  Tile_events.use_key_press_filter key (fun event ->
      if (not is_focused) || disabled then false
      else
        match event.key with
        | Enter when event.modifier.shift ->
            (* Shift+Enter submits *)
            Option.iter (fun f -> f current_value) on_submit;
            true
        | Enter ->
            (* Regular Enter adds newline *)
            let new_value = current_value ^ "\n" in
            (match value with
            | None -> set_internal_value new_value
            | Some _ -> ());
            Option.iter (fun f -> f new_value) on_change;
            true
        | Char c when is_printable c ->
            let ch = String.make 1 (Uchar.to_char c) in
            let new_value = current_value ^ ch in
            (match value with
            | None -> set_internal_value new_value
            | Some _ -> ());
            Option.iter (fun f -> f new_value) on_change;
            true
        | Backspace when String.length current_value > 0 ->
            let new_value =
              String.sub current_value 0 (pred (String.length current_value))
            in
            (match value with
            | None -> set_internal_value new_value
            | Some _ -> ());
            Option.iter (fun f -> f new_value) on_change;
            true
        | _ -> false);

  (* Mark as focusable *)
  Tile_events.use_focusable key ?tab_index ();

  let label_style =
    if is_focused then Style.(fg (RGB (59, 130, 246)) ++ bold)
    else Style.(fg (Index 250))
  in

  let lines = String.split_on_char '\n' current_value in
  let display_lines =
    if current_value = "" && placeholder <> None then [ Option.get placeholder ]
    else lines
  in

  let value_style =
    if current_value = "" && placeholder <> None then Style.(fg (Index 240))
    else if is_focused then Style.(fg White)
    else Style.(fg (Index 250))
  in

  let content_box =
    Ui.vbox
      (List.map (fun line -> text ~style:value_style line) display_lines
      @
      if is_focused then [ text ~style:Style.(fg (RGB (59, 130, 246))) "▌" ]
      else [])
  in

  let input_area =
    match border with
    | Some b ->
        Ui.box ~border:b ~border_style ~padding:(xy 1 0)
          ~min_height:(`Cells rows)
          ~style:(if disabled then disabled_style else style)
          [ content_box ]
    | None ->
        Ui.box ~padding:(xy 1 0) ~min_height:(`Cells rows)
          ~style:
            Style.(bg (Index 236) ++ if disabled then disabled_style else style)
          [ content_box ]
  in

  with_key key
    (if label = "" then input_area
     else Ui.vbox ~gap:(`Cells 0) [ text ~style:label_style label; input_area ])

(** Form layout container for consistent spacing and alignment *)
let form ?(spacing = `Cells 1) children = Ui.vbox ~gap:spacing children

(** Field group for related fields *)
let field_group ?(label = "") ?(direction = `Vertical) ?(spacing = `Cells 1)
    children =
  let content =
    match direction with
    | `Vertical -> Ui.vbox ~gap:spacing children
    | `Horizontal -> Ui.hbox ~gap:spacing children
  in

  if label = "" then content
  else
    Ui.vbox ~gap:(`Cells 0)
      [ text ~style:Style.(fg (Index 250) ++ bold) label; content ]

(** Form section with optional title and border *)
let form_section ?(title = "") ?(border = None) ?(spacing = `Cells 1) children =
  let content = Ui.vbox ~gap:spacing children in

  let section_content =
    if title = "" then content
    else
      Ui.vbox ~gap:(`Cells 1)
        [ text ~style:Style.(fg (RGB (59, 130, 246)) ++ bold) title; content ]
  in

  match border with
  | Some b -> Ui.box ~border:b ~padding:(xy 2 1) [ section_content ]
  | None -> section_content
