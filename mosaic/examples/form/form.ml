open Mosaic

(* Form field types *)
type field_type =
  | Input of { value : string; placeholder : string }
  | Select of { options : string list; selected : int }
  | Radio of { options : string list; selected : int }
  | Checkbox of { label : string; checked : bool }

type field = {
  label : string;
  field_type : field_type;
}

type form_state = {
  fields : field list;
  focused_field : int;
  editing : bool;
  submitted : bool;
}

(* Helper to render a single field *)
let render_field ~is_focused field =
  let open Ui in
  let focus_style = 
    if is_focused then Style.(fg (Index 214) ++ bold)
    else Style.(fg (Index 250))
  in
  
  match field.field_type with
  | Input { value; placeholder } ->
      vbox ~gap:(`Cells 1) [
        text ~style:focus_style field.label;
        hbox [
          text ~style:Style.(fg (Index 245)) "  [";
          text ~style:(if value = "" then Style.(fg (Index 240)) else Style.(fg (Index 255)))
            (if value = "" then placeholder else value);
          text ~style:Style.(fg (Index 245)) "]";
          if is_focused then text ~style:Style.(fg (Index 214)) " ◀" else text "";
        ];
      ]
  
  | Select { options; selected } ->
      vbox ~gap:(`Cells 1) [
        text ~style:focus_style field.label;
        vbox (List.mapi (fun i option ->
          hbox [
            text "  ";
            text ~style:(if i = selected && is_focused then Style.(fg (Index 214) ++ bold) 
                        else if i = selected then Style.(fg (Index 250) ++ bold)
                        else Style.(fg (Index 240)))
              (if i = selected then "▶ " else "  ");
            text ~style:(if i = selected then Style.(fg (Index 255)) else Style.(fg (Index 245))) 
              option;
          ]
        ) options);
      ]
  
  | Radio { options; selected } ->
      vbox ~gap:(`Cells 1) [
        text ~style:focus_style field.label;
        vbox (List.mapi (fun i option ->
          hbox [
            text "  ";
            text ~style:(if is_focused then Style.(fg (Index 214)) else Style.(fg (Index 250)))
              (if i = selected then "◉" else "○");
            text " ";
            text ~style:(if i = selected then Style.(fg (Index 255)) else Style.(fg (Index 245)))
              option;
          ]
        ) options);
      ]
  
  | Checkbox { label; checked } ->
      hbox [
        text ~style:focus_style field.label;
        text " ";
        text ~style:(if is_focused then Style.(fg (Index 214)) else Style.(fg (Index 250)))
          (if checked then "☑" else "☐");
        text " ";
        text ~style:Style.(fg (Index 245)) label;
      ]

(* Form component *)
let form_app () =
  let open Ui in
  
  (* Initialize form state *)
  let state, _set_state, update_state = use_state {
    fields = [
      { label = "Name:"; 
        field_type = Input { value = ""; placeholder = "Enter your name" } };
      { label = "Email:"; 
        field_type = Input { value = ""; placeholder = "user@example.com" } };
      { label = "Country:"; 
        field_type = Select { 
          options = ["United States"; "Canada"; "United Kingdom"; "France"; "Germany"; "Japan"]; 
          selected = 0 
        } };
      { label = "Subscription Plan:"; 
        field_type = Radio { 
          options = ["Free"; "Basic ($9/mo)"; "Pro ($19/mo)"; "Enterprise ($49/mo)"]; 
          selected = 0 
        } };
      { label = ""; 
        field_type = Checkbox { 
          label = "Subscribe to newsletter"; 
          checked = false 
        } };
      { label = ""; 
        field_type = Checkbox { 
          label = "Agree to terms and conditions"; 
          checked = false 
        } };
    ];
    focused_field = 0;
    editing = false;
    submitted = false;
  } in
  
  (* Handle keyboard events *)
  use_subscription (
    Sub.keyboard_filter (fun event ->
      if state.submitted then (
        (* Reset form on any key after submission *)
        update_state (fun s -> {
          s with 
          submitted = false;
          fields = List.map (fun field ->
            match field.field_type with
            | Input _ -> { field with field_type = Input { value = ""; placeholder = 
                (match field.field_type with Input {placeholder; _} -> placeholder | _ -> "") } }
            | Select _ -> { field with field_type = Select { 
                options = (match field.field_type with Select {options; _} -> options | _ -> []); 
                selected = 0 } }
            | Radio _ -> { field with field_type = Radio { 
                options = (match field.field_type with Radio {options; _} -> options | _ -> []); 
                selected = 0 } }
            | Checkbox _ -> { field with field_type = Checkbox { 
                label = (match field.field_type with Checkbox {label; _} -> label | _ -> "");
                checked = false } }
          ) s.fields;
        });
        Some ()
      ) else
        match event.Input.key with
        | Input.Down | Input.Tab ->
            update_state (fun s -> { 
              s with 
              focused_field = min (List.length s.fields - 1) (s.focused_field + 1);
              editing = false;
            });
            Some ()
        | Input.Up ->
            update_state (fun s -> { 
              s with 
              focused_field = max 0 (s.focused_field - 1);
              editing = false;
            });
            Some ()
        | Input.Enter ->
            let field = List.nth state.fields state.focused_field in
            (match field.field_type with
            | Input _ when not state.editing ->
                update_state (fun s -> { s with editing = true });
                Some ()
            | Input _ ->
                update_state (fun s -> { s with editing = false });
                Some ()
            | _ -> None)
        | Input.Char c when Uchar.to_int c = 0x20 -> (* space character *)
            let field = List.nth state.fields state.focused_field in
            (match field.field_type with
            | Checkbox { label; checked } ->
                update_state (fun s -> {
                  s with
                  fields = List.mapi (fun i f ->
                    if i = s.focused_field then
                      { f with field_type = Checkbox { label; checked = not checked } }
                    else f
                  ) s.fields
                });
                Some ()
            | _ -> None)
        | Input.Left ->
            let field = List.nth state.fields state.focused_field in
            (match field.field_type with
            | Select { options; selected } ->
                update_state (fun s -> {
                  s with
                  fields = List.mapi (fun i f ->
                    if i = s.focused_field then
                      { f with field_type = Select { options; selected = max 0 (selected - 1) } }
                    else f
                  ) s.fields
                });
                Some ()
            | Radio { options; selected } ->
                update_state (fun s -> {
                  s with
                  fields = List.mapi (fun i f ->
                    if i = s.focused_field then
                      { f with field_type = Radio { options; selected = max 0 (selected - 1) } }
                    else f
                  ) s.fields
                });
                Some ()
            | _ -> None)
        | Input.Right ->
            let field = List.nth state.fields state.focused_field in
            (match field.field_type with
            | Select { options; selected } ->
                update_state (fun s -> {
                  s with
                  fields = List.mapi (fun i f ->
                    if i = s.focused_field then
                      { f with field_type = Select { options; selected = min (List.length options - 1) (selected + 1) } }
                    else f
                  ) s.fields
                });
                Some ()
            | Radio { options; selected } ->
                update_state (fun s -> {
                  s with
                  fields = List.mapi (fun i f ->
                    if i = s.focused_field then
                      { f with field_type = Radio { options; selected = min (List.length options - 1) (selected + 1) } }
                    else f
                  ) s.fields
                });
                Some ()
            | _ -> None)
        | Input.Char c when state.editing ->
            let ch = Uchar.to_char c in
            if ch >= ' ' && ch <= '~' then (
              let field = List.nth state.fields state.focused_field in
              match field.field_type with
              | Input { value; placeholder } ->
                  update_state (fun s -> {
                    s with
                    fields = List.mapi (fun i f ->
                      if i = s.focused_field then
                        { f with field_type = Input { value = value ^ String.make 1 ch; placeholder } }
                      else f
                    ) s.fields
                  });
                  Some ()
              | _ -> None
            ) else None
        | Input.Backspace when state.editing ->
            let field = List.nth state.fields state.focused_field in
            (match field.field_type with
            | Input { value; placeholder } when String.length value > 0 ->
                update_state (fun s -> {
                  s with
                  fields = List.mapi (fun i f ->
                    if i = s.focused_field then
                      { f with field_type = Input { 
                        value = String.sub value 0 (String.length value - 1); 
                        placeholder 
                      } }
                    else f
                  ) s.fields
                });
                Some ()
            | _ -> None)
        | Input.Char c when Uchar.to_int c = 0x73 -> (* 's' for submit *)
            update_state (fun s -> { s with submitted = true });
            Some ()
        | Input.Char c when Uchar.to_int c = 0x71 -> (* 'q' for quit *)
            exit 0
        | _ -> None
    )
  );
  
  (* Render the form *)
  if state.submitted then
    vbox ~gap:(`Cells 2) [
      text ~style:Style.(fg (Index 40) ++ bold) "✓ Form Submitted Successfully!";
      text ~style:Style.(fg (Index 250)) "Thank you for your submission.";
      text "";
      text ~style:Style.(fg (Index 245)) "Press any key to fill another form...";
    ]
  else
    vbox ~gap:(`Cells 2) [
      (* Header *)
      hbox [
        text ~style:Style.(fg (Index 39) ++ bold) "═══ ";
        text ~style:Style.(fg (Index 214) ++ bold) "User Registration Form";
        text ~style:Style.(fg (Index 39) ++ bold) " ═══";
      ];
      
      (* Form fields *)
      vbox ~gap:(`Cells 2) (
        List.mapi (fun i field ->
          render_field ~is_focused:(i = state.focused_field) field
        ) state.fields
      );
      
      (* Instructions *)
      text "";
      hbox ~gap:(`Cells 2) [
        text ~style:Style.(fg (Index 240)) "[↑↓/Tab]";
        text ~style:Style.(fg (Index 245)) "Navigate";
        text ~style:Style.(fg (Index 240)) "[←→]";
        text ~style:Style.(fg (Index 245)) "Select";
        text ~style:Style.(fg (Index 240)) "[Space]";
        text ~style:Style.(fg (Index 245)) "Check";
        text ~style:Style.(fg (Index 240)) "[Enter]";
        text ~style:Style.(fg (Index 245)) "Edit";
        text ~style:Style.(fg (Index 240)) "[s]";
        text ~style:Style.(fg (Index 245)) "Submit";
        text ~style:Style.(fg (Index 240)) "[q]";
        text ~style:Style.(fg (Index 245)) "Quit";
      ];
      
      (* Show editing indicator *)
      if state.editing then
        text ~style:Style.(fg (Index 226) ++ italic) "📝 Editing mode - Type to enter text, Enter to finish"
      else
        text "";
    ]

(* Main entry point *)
let () =
  (* Use non-alt screen mode to preserve form output in terminal history *)
  Mosaic.run ~alt_screen:false ~mouse:false form_app