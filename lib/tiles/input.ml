(** Implementation of the input component with advanced features *)

open Mosaic

(* Theme configuration *)
type theme = {
  focused_style : Style.t;
  blurred_style : Style.t;
  error_style : Style.t;
  placeholder_style : Style.t;
  suggestion_style : Style.t;
  selected_suggestion_style : Style.t;
}

let default_theme =
  {
    focused_style = Style.(fg (Index 6));
    blurred_style = Style.(fg (Index 8));
    error_style = Style.(fg Red);
    placeholder_style = Style.(fg (Index 8));
    suggestion_style = Style.(fg (Index 7));
    selected_suggestion_style = Style.(bg (Index 8) ++ fg White);
  }

(* Model *)
type model = {
  (* Content *)
  value : string;
  cursor_pos : int;
  (* Configuration *)
  placeholder : string;
  is_password : bool;
  suggestions : string list;
  validate : (string -> (unit, string) result) option;
  width : int;
  (* State *)
  is_focused : bool;
  error : string option;
  suggestion_index : int option;
  show_suggestions : bool;
  (* Theme *)
  theme : theme;
}

(* Messages *)
type msg =
  | Key of Input.key_event
  | Focus
  | Blur
  | Select_suggestion of int
  | Hide_suggestions

(* Initialization *)
let init ?(placeholder = "") ?(initial_value = "") ?(is_password = false)
    ?(suggestions = []) ?validate ?(width = 40) () =
  let model =
    {
      value = initial_value;
      cursor_pos = String.length initial_value;
      placeholder;
      is_password;
      suggestions;
      validate;
      width;
      is_focused = false;
      error = None;
      suggestion_index = None;
      show_suggestions = false;
      theme = default_theme;
    }
  in
  (* Validate initial value if validator is provided *)
  let model =
    match validate with
    | Some f -> (
        match f initial_value with
        | Ok () -> model
        | Error err -> { model with error = Some err })
    | None -> model
  in
  (model, Cmd.none)

(* Helper to validate current value *)
let validate_value model =
  match model.validate with
  | Some f -> (
      match f model.value with
      | Ok () -> { model with error = None }
      | Error err -> { model with error = Some err })
  | None -> model

(* Helper to filter suggestions based on current value *)
let filter_suggestions model =
  if model.value = "" then []
  else
    let lower_value = String.lowercase_ascii model.value in
    List.filter
      (fun s ->
        String.starts_with ~prefix:lower_value (String.lowercase_ascii s))
      model.suggestions

(* Update *)
let update msg model =
  match msg with
  | Key { key; modifier } -> (
      match key with
      | Char c when modifier.ctrl && Uchar.to_char c = 'u' ->
          (* Clear input on Ctrl+U *)
          let model =
            { model with value = ""; cursor_pos = 0; show_suggestions = false }
          in
          (validate_value model, Cmd.none)
      | Char c
        when Uchar.to_int c >= 32
             && Uchar.to_int c < 127
             && (not modifier.ctrl) && not modifier.alt ->
          let char = String.make 1 (Uchar.to_char c) in
          let before = String.sub model.value 0 model.cursor_pos in
          let after =
            String.sub model.value model.cursor_pos
              (String.length model.value - model.cursor_pos)
          in
          let value = before ^ char ^ after in
          let model =
            {
              model with
              value;
              cursor_pos = model.cursor_pos + 1;
              show_suggestions = true;
              suggestion_index = None;
            }
          in
          (validate_value model, Cmd.none)
      | Backspace when model.cursor_pos > 0 ->
          let before = String.sub model.value 0 (model.cursor_pos - 1) in
          let after =
            String.sub model.value model.cursor_pos
              (String.length model.value - model.cursor_pos)
          in
          let value = before ^ after in
          let model =
            {
              model with
              value;
              cursor_pos = model.cursor_pos - 1;
              show_suggestions = true;
              suggestion_index = None;
            }
          in
          (validate_value model, Cmd.none)
      | Delete when model.cursor_pos < String.length model.value ->
          let before = String.sub model.value 0 model.cursor_pos in
          let after =
            if model.cursor_pos < String.length model.value - 1 then
              String.sub model.value (model.cursor_pos + 1)
                (String.length model.value - model.cursor_pos - 1)
            else ""
          in
          let value = before ^ after in
          let model = { model with value; show_suggestions = true } in
          (validate_value model, Cmd.none)
      | Left when model.cursor_pos > 0 ->
          ({ model with cursor_pos = model.cursor_pos - 1 }, Cmd.none)
      | Right when model.cursor_pos < String.length model.value ->
          ({ model with cursor_pos = model.cursor_pos + 1 }, Cmd.none)
      | Home -> ({ model with cursor_pos = 0 }, Cmd.none)
      | End -> ({ model with cursor_pos = String.length model.value }, Cmd.none)
      | Up when model.show_suggestions ->
          let suggestions = filter_suggestions model in
          let suggestion_index =
            match model.suggestion_index with
            | None -> Some (List.length suggestions - 1)
            | Some idx ->
                if idx > 0 then Some (idx - 1)
                else Some (List.length suggestions - 1)
          in
          ({ model with suggestion_index }, Cmd.none)
      | Down when model.show_suggestions ->
          let suggestions = filter_suggestions model in
          let suggestion_index =
            match model.suggestion_index with
            | None -> Some 0
            | Some idx ->
                if idx < List.length suggestions - 1 then Some (idx + 1)
                else Some 0
          in
          ({ model with suggestion_index }, Cmd.none)
      | Tab when model.show_suggestions && model.suggestion_index <> None -> (
          let suggestions = filter_suggestions model in
          match model.suggestion_index with
          | Some idx -> (
              match List.nth_opt suggestions idx with
              | Some value ->
                  let model =
                    {
                      model with
                      value;
                      cursor_pos = String.length value;
                      show_suggestions = false;
                      suggestion_index = None;
                    }
                  in
                  (validate_value model, Cmd.none)
              | None -> (model, Cmd.none))
          | None -> (model, Cmd.none))
      | Enter -> ({ model with show_suggestions = false }, Cmd.none)
      | Escape when model.show_suggestions ->
          ( { model with show_suggestions = false; suggestion_index = None },
            Cmd.none )
      | _ -> (model, Cmd.none))
  | Focus ->
      ({ model with is_focused = true; show_suggestions = true }, Cmd.none)
  | Blur ->
      ( {
          model with
          is_focused = false;
          show_suggestions = false;
          suggestion_index = None;
        },
        Cmd.none )
  | Select_suggestion idx -> (
      let suggestions = filter_suggestions model in
      match List.nth_opt suggestions idx with
      | Some value ->
          let model =
            {
              model with
              value;
              cursor_pos = String.length value;
              show_suggestions = false;
              suggestion_index = None;
            }
          in
          (validate_value model, Cmd.none)
      | None -> (model, Cmd.none))
  | Hide_suggestions ->
      ( { model with show_suggestions = false; suggestion_index = None },
        Cmd.none )

(* View *)
let view model =
  let open Ui in
  (* Input field *)
  let display_content =
    if model.value = "" && not model.is_focused then
      text ~style:model.theme.placeholder_style model.placeholder
    else
      let visible_value =
        if model.is_password then String.make (String.length model.value) '*'
        else model.value
      in
      let cursor_pos = min model.cursor_pos (String.length visible_value) in
      let before = String.sub visible_value 0 cursor_pos in
      let at_cursor =
        if cursor_pos < String.length visible_value then
          String.sub visible_value cursor_pos 1
        else " "
      in
      let after =
        if cursor_pos < String.length visible_value - 1 then
          String.sub visible_value (cursor_pos + 1)
            (String.length visible_value - cursor_pos - 1)
        else ""
      in
      let cursor_style =
        if model.is_focused then Style.reverse else Style.empty
      in
      let text_style =
        if model.is_focused then model.theme.focused_style
        else model.theme.blurred_style
      in
      hbox
        [
          text ~style:text_style before;
          text ~style:cursor_style at_cursor;
          text ~style:text_style after;
        ]
  in

  let border_color =
    if model.is_focused then Style.Index 6 else Style.Index 8
  in

  let input_field =
    hbox ~padding:(padding_xy 1 0)
      ~border:(border ~style:Solid ~color:border_color ())
      ~width:model.width [ display_content ]
  in

  (* Error message below input *)
  let error_elem =
    match model.error with
    | Some err -> [ text ~style:model.theme.error_style err ]
    | None -> []
  in

  (* Suggestions dropdown *)
  let suggestions_elem =
    if model.show_suggestions && model.is_focused then
      let filtered = filter_suggestions model in
      if filtered = [] then []
      else
        let suggestion_items =
          List.mapi
            (fun idx suggestion ->
              let style =
                match model.suggestion_index with
                | Some i when i = idx -> model.theme.selected_suggestion_style
                | _ -> model.theme.suggestion_style
              in
              text ~style suggestion)
            filtered
        in
        [
          vbox ~padding:(padding_all 1)
            ~border:(border ~style:Solid ~color:(Style.Index 8) ())
            ~width:(model.width - 2) suggestion_items;
        ]
    else []
  in

  (* Layout *)
  vbox ([ input_field ] @ error_elem @ suggestions_elem)

(* Subscriptions *)
let subscriptions model =
  if model.is_focused then Sub.keyboard (fun k -> Key k) else Sub.none

(* Component export *)
let component = Mosaic.app ~init ~update ~view ~subscriptions ()

(* Accessors *)
let value model = model.value

let is_valid model =
  match model.validate with
  | Some f -> ( match f model.value with Ok () -> true | Error _ -> false)
  | None -> true

let error model = model.error
let is_focused model = model.is_focused
let suggestions model = model.suggestions

(* Actions *)
let focus model = ({ model with is_focused = true }, Cmd.msg Focus)
let blur model = ({ model with is_focused = false }, Cmd.msg Blur)

let set_value value model =
  let model = { model with value; cursor_pos = String.length value } in
  validate_value model

let clear model =
  let model = { model with value = ""; cursor_pos = 0 } in
  validate_value model

let set_suggestions suggestions model = { model with suggestions }
let validate model = validate_value model
let select_suggestion idx model = (model, Cmd.msg (Select_suggestion idx))
let hide_suggestions model = (model, Cmd.msg Hide_suggestions)

(* Theming *)
let with_theme theme model = { model with theme }

(* Re-export update, view, subscriptions for component interface *)
let update = update
let view = view
let subscriptions = subscriptions
