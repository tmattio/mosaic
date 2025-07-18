(** Implementation of the single-choice selection component *)

open Mosaic

(* Key binding configuration - just a list of keys to pass through *)
type key_config = Input.key_event list

let default_key_config = []

(* Theme configuration *)
type theme = {
  focused_style : Style.t;
  blurred_style : Style.t;
  selected_style : Style.t;
  highlighted_style : Style.t;
  placeholder_style : Style.t;
  filter_style : Style.t;
}

let default_theme =
  {
    focused_style = Style.(fg (Index 6));
    blurred_style = Style.(fg (Index 8));
    selected_style = Style.(fg Green);
    highlighted_style = Style.(bg (Index 8) ++ fg White);
    placeholder_style = Style.(fg (Index 8));
    filter_style = Style.(fg (Index 6));
  }

(* Model *)
type 'a model = {
  (* Configuration *)
  options : ('a * string) list;
  height : int;
  filterable : bool;
  placeholder : string;
  key_config : key_config;
  (* State *)
  selected_value : 'a option;
  highlighted_index : int;
  is_focused : bool;
  is_open : bool;
  filter_text : string;
  scroll_offset : int;
  (* Theme *)
  theme : theme;
}

(* Messages *)
type msg =
  | Key of Input.key_event
  | Focus
  | Blur
  | Open
  | Close
  | Toggle
  | Select of int
  | Update_filter of string

(* Helper to get filtered options *)
let get_filtered_options model =
  if (not model.filterable) || model.filter_text = "" then model.options
  else
    let lower_filter = String.lowercase_ascii model.filter_text in
    List.filter
      (fun (_, label) ->
        let lower_label = String.lowercase_ascii label in
        let contains s sub =
          let len_s = String.length s in
          let len_sub = String.length sub in
          let rec check i =
            if i + len_sub > len_s then false
            else if String.sub s i len_sub = sub then true
            else check (i + 1)
          in
          check 0
        in
        contains lower_label lower_filter)
      model.options

(* Helper to ensure highlighted index is valid *)
let clamp_highlight model =
  let filtered = get_filtered_options model in
  let max_idx = max 0 (List.length filtered - 1) in
  { model with highlighted_index = max 0 (min max_idx model.highlighted_index) }

(* Helper to ensure scroll shows highlighted item *)
let ensure_highlight_visible model =
  let visible_start = model.scroll_offset in
  let visible_end = model.scroll_offset + model.height - 1 in
  if model.highlighted_index < visible_start then
    { model with scroll_offset = model.highlighted_index }
  else if model.highlighted_index > visible_end then
    { model with scroll_offset = model.highlighted_index - model.height + 1 }
  else model

(* Initialization *)
let init ?(options = []) ?default ?(height = 5) ?(filterable = false)
    ?(placeholder = "Select...") ?(key_config = default_key_config) () =
  let selected_value = default in
  let highlighted_index =
    match default with
    | Some v -> (
        match List.find_index (fun (value, _) -> value = v) options with
        | Some idx -> idx
        | None -> 0)
    | None -> 0
  in
  let model =
    {
      options;
      height;
      filterable;
      placeholder;
      key_config;
      selected_value;
      highlighted_index;
      is_focused = false;
      is_open = false;
      filter_text = "";
      scroll_offset = 0;
      theme = default_theme;
    }
  in
  (model, Cmd.none)

(* Update *)
let update msg model =
  match msg with
  | Key { key; modifier } -> (
      if model.is_open then
        match key with
        | Up ->
            let model =
              { model with highlighted_index = model.highlighted_index - 1 }
            in
            (clamp_highlight model |> ensure_highlight_visible, Cmd.none)
        | Down ->
            let model =
              { model with highlighted_index = model.highlighted_index + 1 }
            in
            (clamp_highlight model |> ensure_highlight_visible, Cmd.none)
        | Page_up ->
            let model =
              {
                model with
                highlighted_index =
                  max 0 (model.highlighted_index - model.height);
                scroll_offset = max 0 (model.scroll_offset - model.height);
              }
            in
            (clamp_highlight model, Cmd.none)
        | Page_down ->
            let filtered = get_filtered_options model in
            let max_idx = List.length filtered - 1 in
            let model =
              {
                model with
                highlighted_index =
                  min max_idx (model.highlighted_index + model.height);
                scroll_offset =
                  min
                    (max 0 (max_idx - model.height + 1))
                    (model.scroll_offset + model.height);
              }
            in
            (clamp_highlight model, Cmd.none)
        | Enter | Tab -> (
            let filtered = get_filtered_options model in
            match List.nth_opt filtered model.highlighted_index with
            | Some (value, _) ->
                ( {
                    model with
                    selected_value = Some value;
                    is_open = false;
                    filter_text = "";
                  },
                  Cmd.none )
            | None -> (model, Cmd.none))
        | Escape -> ({ model with is_open = false; filter_text = "" }, Cmd.none)
        | Char c
          when model.filterable
               && Uchar.to_int c >= 32
               && Uchar.to_int c < 127
               && (not modifier.ctrl) && not modifier.alt ->
            let char = String.make 1 (Uchar.to_char c) in
            let filter_text = model.filter_text ^ char in
            let model =
              {
                model with
                filter_text;
                highlighted_index = 0;
                scroll_offset = 0;
              }
            in
            (clamp_highlight model, Cmd.none)
        | Backspace when model.filterable && String.length model.filter_text > 0
          ->
            let filter_text =
              String.sub model.filter_text 0
                (String.length model.filter_text - 1)
            in
            let model =
              {
                model with
                filter_text;
                highlighted_index = 0;
                scroll_offset = 0;
              }
            in
            (clamp_highlight model, Cmd.none)
        | _ -> (model, Cmd.none)
      else
        (* Closed state *)
        match key with
        | Enter | Down -> ({ model with is_open = true }, Cmd.none)
        | _ -> (model, Cmd.none))
  | Focus -> ({ model with is_focused = true }, Cmd.none)
  | Blur -> ({ model with is_focused = false; is_open = false }, Cmd.none)
  | Open -> ({ model with is_open = true }, Cmd.none)
  | Close -> ({ model with is_open = false; filter_text = "" }, Cmd.none)
  | Toggle -> ({ model with is_open = not model.is_open }, Cmd.none)
  | Select idx -> (
      let filtered = get_filtered_options model in
      match List.nth_opt filtered idx with
      | Some (value, _) ->
          ( {
              model with
              selected_value = Some value;
              is_open = false;
              filter_text = "";
            },
            Cmd.none )
      | None -> (model, Cmd.none))
  | Update_filter text ->
      let model =
        {
          model with
          filter_text = text;
          highlighted_index = 0;
          scroll_offset = 0;
        }
      in
      (clamp_highlight model, Cmd.none)

(* View *)
let view model =
  let open Ui in
  (* Current selection display *)
  let selection_text =
    match model.selected_value with
    | Some value -> (
        match List.find_opt (fun (v, _) -> v = value) model.options with
        | Some (_, label) -> label
        | None -> model.placeholder)
    | None -> model.placeholder
  in

  let selection_style =
    match model.selected_value with
    | Some _ when model.is_focused -> model.theme.focused_style
    | Some _ -> model.theme.blurred_style
    | None -> model.theme.placeholder_style
  in

  let arrow = if model.is_open then "▼" else "▶" in

  let selection_box =
    hbox ~padding:(padding_xy 1 0)
      ~border:
        (border ~style:Solid
           ~color:(if model.is_focused then Style.Index 6 else Style.Index 8)
           ())
      ~justify_content:`Start
      [
        text ~style:selection_style selection_text; spacer ~flex:1 0; text arrow;
      ]
  in

  (* Dropdown list *)
  let dropdown_elem =
    if model.is_open then
      let filtered = get_filtered_options model in

      (* Filter input if filterable *)
      let filter_elem =
        if model.filterable then
          [
            hbox ~padding:(padding_xy 1 0)
              [
                text ~style:model.theme.filter_style "Filter: ";
                text model.filter_text;
                text ~style:Style.reverse " ";
              ];
          ]
        else []
      in

      (* Options list *)
      let visible_options =
        let start = model.scroll_offset in
        let end_ = min (List.length filtered) (start + model.height) in
        let rec take n lst =
          match (n, lst) with
          | 0, _ | _, [] -> []
          | n, h :: t -> h :: take (n - 1) t
        in
        let rec drop n lst =
          match (n, lst) with
          | 0, _ -> lst
          | _, [] -> []
          | n, _ :: t -> drop (n - 1) t
        in
        filtered |> drop start |> take (end_ - start)
      in

      let option_items =
        List.mapi
          (fun i (value, label) ->
            let actual_idx = model.scroll_offset + i in
            let is_selected = Some value = model.selected_value in
            let is_highlighted = actual_idx = model.highlighted_index in

            let prefix =
              if is_highlighted then "▸ "
              else if is_selected then "✓ "
              else "  "
            in

            let style =
              if is_highlighted then model.theme.highlighted_style
              else if is_selected then model.theme.selected_style
              else Style.empty
            in

            text ~style (prefix ^ label))
          visible_options
      in

      [
        vbox ~padding:(padding_all 1)
          ~border:(border ~style:Solid ~color:(Style.Index 8) ())
          (filter_elem @ option_items);
      ]
    else []
  in

  (* Layout *)
  vbox ([ selection_box ] @ dropdown_elem)

(* Helper to check if a key should be passed through *)
let should_pass_through event pass_through_keys =
  let key_event_equal e1 e2 =
    e1.Input.key = e2.Input.key
    && e1.modifier.ctrl = e2.modifier.ctrl
    && e1.modifier.alt = e2.modifier.alt
    && e1.modifier.shift = e2.modifier.shift
  in
  List.exists (key_event_equal event) pass_through_keys

(* Subscriptions *)
let subscriptions model =
  if model.is_focused then
    Sub.keyboard_filter (fun event ->
        if should_pass_through event model.key_config then None
        else Some (Key event))
  else Sub.none

(* Component not exported due to polymorphic type *)

(* Accessors *)
let value model = model.selected_value

let selected_index model =
  match model.selected_value with
  | Some v -> List.find_index (fun (value, _) -> value = v) model.options
  | None -> None

let options model = model.options
let filtered_options model = get_filtered_options model
let is_focused model = model.is_focused
let is_open model = model.is_open
let filter_text model = model.filter_text

(* Actions *)
let focus model = ({ model with is_focused = true }, Cmd.msg Focus)

let blur model =
  ({ model with is_focused = false; is_open = false }, Cmd.msg Blur)

let open_dropdown model = (model, Cmd.msg Open)
let close_dropdown model = (model, Cmd.msg Close)
let toggle_dropdown model = (model, Cmd.msg Toggle)
let select_option idx model = (model, Cmd.msg (Select idx))
let update_filter text model = (model, Cmd.msg (Update_filter text))

let select value model =
  match List.find_opt (fun (v, _) -> v = value) model.options with
  | Some _ ->
      {
        model with
        selected_value = Some value;
        is_open = false;
        filter_text = "";
      }
  | None -> model

let select_index idx model =
  match List.nth_opt model.options idx with
  | Some (value, _) ->
      {
        model with
        selected_value = Some value;
        is_open = false;
        filter_text = "";
      }
  | None -> model

let clear model = { model with selected_value = None }

let set_options options model =
  (* Reset selection if current value is not in new options *)
  let selected_value =
    match model.selected_value with
    | Some v when List.exists (fun (value, _) -> value = v) options -> Some v
    | _ -> None
  in
  {
    model with
    options;
    selected_value;
    highlighted_index = 0;
    scroll_offset = 0;
  }

let set_filter text model =
  if model.filterable then
    { model with filter_text = text; highlighted_index = 0; scroll_offset = 0 }
    |> clamp_highlight
  else model

(* Theming *)
let with_theme theme model = { model with theme }
