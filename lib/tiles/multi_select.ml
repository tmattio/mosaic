(** Implementation of the multiple-choice selection component *)

open Mosaic

(* Key binding configuration - just a list of keys to pass through *)
type key_config = Input.key_event list

let default_key_config = []

(* Theme configuration *)
type theme = {
  focused_style : Style.t;
  blurred_style : Style.t;
  selected_style : Style.t;
  unselected_style : Style.t;
  highlighted_style : Style.t;
  limit_style : Style.t;
  filter_style : Style.t;
}

let default_theme =
  {
    focused_style = Style.(fg (Index 6));
    blurred_style = Style.(fg (Index 8));
    selected_style = Style.(fg Green);
    unselected_style = Style.empty;
    highlighted_style = Style.(bg (Index 8) ++ fg White);
    limit_style = Style.(fg Yellow);
    filter_style = Style.(fg (Index 6));
  }

(* Model *)
type 'a model = {
  (* Configuration *)
  options : ('a * string) list;
  limit : int; (* 0 = unlimited *)
  height : int;
  filterable : bool;
  key_config : key_config;
  (* State *)
  selected_values : 'a list;
  highlighted_index : int;
  is_focused : bool;
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
  | Toggle of int
  | Select_all
  | Clear_all
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

(* Helper to check if value is selected *)
let is_value_selected value selected_values =
  List.exists (fun v -> v = value) selected_values

(* Initialization *)
let init ?(options = []) ?(default = []) ?(limit = 0) ?(height = 5)
    ?(filterable = false) ?(key_config = default_key_config) () =
  let model =
    {
      options;
      limit;
      height;
      filterable;
      key_config;
      selected_values = default;
      highlighted_index = 0;
      is_focused = false;
      filter_text = "";
      scroll_offset = 0;
      theme = default_theme;
    }
  in
  (model, Cmd.none)

(* Update *)
let rec update msg model =
  match msg with
  | Key { key; modifier } -> (
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
              highlighted_index = max 0 (model.highlighted_index - model.height);
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
      | Enter when (not modifier.ctrl) && not modifier.alt ->
          update (Toggle model.highlighted_index) model
      | Char c
        when Uchar.to_char c = ' ' && (not modifier.ctrl) && not modifier.alt ->
          update (Toggle model.highlighted_index) model
      | Char c when Uchar.to_char c = 'a' && modifier.ctrl ->
          update Select_all model
      | Char c when Uchar.to_char c = 'd' && modifier.ctrl ->
          update Clear_all model
      | Char c
        when model.filterable
             && Uchar.to_int c >= 32
             && Uchar.to_int c < 127
             && (not modifier.ctrl) && not modifier.alt ->
          let char = String.make 1 (Uchar.to_char c) in
          let filter_text = model.filter_text ^ char in
          let model =
            { model with filter_text; highlighted_index = 0; scroll_offset = 0 }
          in
          (clamp_highlight model, Cmd.none)
      | Backspace when model.filterable && String.length model.filter_text > 0
        ->
          let filter_text =
            String.sub model.filter_text 0 (String.length model.filter_text - 1)
          in
          let model =
            { model with filter_text; highlighted_index = 0; scroll_offset = 0 }
          in
          (clamp_highlight model, Cmd.none)
      | _ -> (model, Cmd.none))
  | Focus -> ({ model with is_focused = true }, Cmd.none)
  | Blur -> ({ model with is_focused = false }, Cmd.none)
  | Toggle idx -> (
      let filtered = get_filtered_options model in
      match List.nth_opt filtered idx with
      | Some (value, _) ->
          let is_selected = is_value_selected value model.selected_values in

          let selected_values =
            if is_selected then
              (* Deselect *)
              List.filter (fun v -> v <> value) model.selected_values
            else if
              (* Select if not at limit *)
              model.limit = 0 || List.length model.selected_values < model.limit
            then value :: model.selected_values
            else model.selected_values
          in

          ({ model with selected_values }, Cmd.none)
      | None -> (model, Cmd.none))
  | Select_all ->
      let filtered = get_filtered_options model in
      let all_values = List.map fst filtered in
      let selected_values =
        if model.limit = 0 then all_values
        else
          let rec take n lst =
            match (n, lst) with
            | 0, _ | _, [] -> []
            | n, h :: t -> h :: take (n - 1) t
          in
          take model.limit all_values
      in
      ({ model with selected_values }, Cmd.none)
  | Clear_all -> ({ model with selected_values = [] }, Cmd.none)
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
  let filtered = get_filtered_options model in
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
        let is_selected = is_value_selected value model.selected_values in
        let is_highlighted = actual_idx = model.highlighted_index in

        let checkbox = if is_selected then "[✓]" else "[ ]" in

        let style =
          if is_highlighted && model.is_focused then
            model.theme.highlighted_style
          else if is_selected then model.theme.selected_style
          else if model.is_focused then model.theme.focused_style
          else model.theme.blurred_style
        in

        text ~style (checkbox ^ " " ^ label))
      visible_options
  in

  (* Limit info if there's a limit *)
  let limit_info =
    if model.limit > 0 then
      [
        text ~style:model.theme.limit_style
          (Printf.sprintf "Selected: %d/%d"
             (List.length model.selected_values)
             model.limit);
      ]
    else []
  in

  (* Layout *)
  vbox ~padding:(padding_all 1)
    ~border:
      (border ~style:Solid
         ~color:(if model.is_focused then Style.Index 6 else Style.Index 8)
         ())
    (filter_elem @ option_items @ limit_info)

(* Subscriptions *)
(* Helper to check if a key should be passed through *)
let should_pass_through event pass_through_keys =
  let key_event_equal e1 e2 =
    e1.Input.key = e2.Input.key
    && e1.modifier.ctrl = e2.modifier.ctrl
    && e1.modifier.alt = e2.modifier.alt
    && e1.modifier.shift = e2.modifier.shift
  in
  List.exists (key_event_equal event) pass_through_keys

let subscriptions model =
  if model.is_focused then
    Sub.keyboard_filter (fun event ->
        if should_pass_through event model.key_config then None
        else Some (Key event))
  else Sub.none

(* Component not exported due to polymorphic type *)

(* Accessors *)
let values model = model.selected_values

let selected_indices model =
  List.filter_map
    (fun (value, _) -> List.find_index (fun (v, _) -> v = value) model.options)
    (List.filter
       (fun (v, _) -> is_value_selected v model.selected_values)
       model.options)

let options model = model.options
let filtered_options model = get_filtered_options model
let is_selected value model = is_value_selected value model.selected_values
let selection_count model = List.length model.selected_values

let is_at_limit model =
  model.limit > 0 && List.length model.selected_values >= model.limit

let is_focused model = model.is_focused
let filter_text model = model.filter_text

(* Actions *)
let focus model = ({ model with is_focused = true }, Cmd.msg Focus)
let blur model = ({ model with is_focused = false }, Cmd.msg Blur)

let toggle value model =
  let is_selected = is_value_selected value model.selected_values in
  if is_selected then
    {
      model with
      selected_values = List.filter (fun v -> v <> value) model.selected_values;
    }
  else if model.limit = 0 || List.length model.selected_values < model.limit
  then { model with selected_values = value :: model.selected_values }
  else model

let toggle_index idx model =
  match List.nth_opt model.options idx with
  | Some (value, _) -> toggle value model
  | None -> model

let select value model =
  if
    (not (is_value_selected value model.selected_values))
    && (model.limit = 0 || List.length model.selected_values < model.limit)
  then { model with selected_values = value :: model.selected_values }
  else model

let deselect value model =
  {
    model with
    selected_values = List.filter (fun v -> v <> value) model.selected_values;
  }

let select_all model =
  let all_values = List.map fst model.options in
  let selected_values =
    if model.limit = 0 then all_values
    else
      let rec take n lst =
        match (n, lst) with
        | 0, _ | _, [] -> []
        | n, h :: t -> h :: take (n - 1) t
      in
      take model.limit all_values
  in
  { model with selected_values }

let clear model = { model with selected_values = [] }

let set_options options model =
  (* Keep only selected values that are still in options *)
  let selected_values =
    List.filter
      (fun v -> List.exists (fun (value, _) -> value = v) options)
      model.selected_values
  in
  {
    model with
    options;
    selected_values;
    highlighted_index = 0;
    scroll_offset = 0;
  }

let set_filter text model =
  if model.filterable then
    { model with filter_text = text; highlighted_index = 0; scroll_offset = 0 }
    |> clamp_highlight
  else model

let update_filter text model = (model, Cmd.msg (Update_filter text))

(* Theming *)
let with_theme theme model = { model with theme }
