open Mosaic

(* Form data type *)
type form_data = {
  name : string;
  email : string;
  country : string;
  plan : string;
  newsletter : bool;
  terms : bool;
}

(* Main form component using Tile form components *)
let form_app () =
  let open Tile in
  (* Form state *)
  let form_data, _, update_form_data =
    use_state
      {
        name = "";
        email = "";
        country = "United States";
        plan = "Free";
        newsletter = false;
        terms = false;
      }
  in

  let submitted, set_submitted, _ = use_state false in

  (* Handle form submission *)
  use_keyboard
    (Input.Char (Uchar.of_int 0x73))
    (* 's' for submit *)
    (Cmd.perform (fun () ->
         set_submitted true;
         None));

  use_keyboard
    (Input.Char (Uchar.of_int 0x71))
    (* 'q' for quit *)
    Cmd.quit;

  (* Reset form after submission *)
  use_subscription
    (Sub.keyboard_filter (fun _ ->
         if submitted then (
           set_submitted false;
           update_form_data (fun _ ->
               {
                 name = "";
                 email = "";
                 country = "United States";
                 plan = "Free";
                 newsletter = false;
                 terms = false;
               });
           Some ())
         else None));

  if submitted then
    vbox ~gap:(`Cells 2)
      [
        text
          ~style:Style.(fg (Index 40) ++ bold)
          "✓ Form Submitted Successfully!";
        text ~style:Style.(fg (Index 250)) "Thank you for your submission.";
        text "";
        text
          ~style:Style.(fg (Index 245))
          "Press any key to fill another form...";
      ]
  else
    vbox ~gap:(`Cells 2)
      [
        (* Header *)
        hbox
          [
            text ~style:Style.(fg (Index 39) ++ bold) "═══ ";
            text ~style:Style.(fg (Index 214) ++ bold) "User Registration Form";
            text ~style:Style.(fg (Index 39) ++ bold) " ═══";
          ];
        (* Form fields using Tile components *)
        vbox ~gap:(`Cells 1)
          [
            (* Name input *)
            input ~label:"Name:" ~placeholder:"Enter your name"
              ~value:form_data.name
              ~on_change:(fun value ->
                update_form_data (fun d -> { d with name = value }))
              ~tab_index:0 ();
            (* Email input - with border for demonstration *)
            input ~label:"Email:" ~placeholder:"user@example.com"
              ~border:Border.rounded ~value:form_data.email
              ~on_change:(fun value ->
                update_form_data (fun d -> { d with email = value }))
              ~tab_index:1 ();
            (* Country select *)
            select ~label:"Country:"
              ~options:
                [
                  "United States";
                  "Canada";
                  "United Kingdom";
                  "France";
                  "Germany";
                  "Japan";
                ]
              ~selected:
                (List.find_index
                   (fun c -> c = form_data.country)
                   [
                     "United States";
                     "Canada";
                     "United Kingdom";
                     "France";
                     "Germany";
                     "Japan";
                   ]
                |> Option.value ~default:0)
              ~on_change:(fun _ value ->
                update_form_data (fun d -> { d with country = value }))
              ~tab_index:2 ();
            (* Subscription plan radio group *)
            radio_group ~label:"Subscription Plan:"
              ~options:
                [
                  "Free"; "Basic ($9/mo)"; "Pro ($19/mo)"; "Enterprise ($49/mo)";
                ]
              ~selected:
                (List.find_index
                   (fun p -> p = form_data.plan)
                   [
                     "Free";
                     "Basic ($9/mo)";
                     "Pro ($19/mo)";
                     "Enterprise ($49/mo)";
                   ]
                |> Option.value ~default:0)
              ~on_change:(fun _ value ->
                update_form_data (fun d -> { d with plan = value }))
              ~tab_index:3 ();
            (* Newsletter checkbox *)
            checkbox ~label:"Subscribe to newsletter"
              ~checked:form_data.newsletter
              ~on_change:(fun value ->
                update_form_data (fun d -> { d with newsletter = value }))
              ~tab_index:4 ();
            (* Terms checkbox *)
            checkbox ~label:"Agree to terms and conditions"
              ~checked:form_data.terms
              ~on_change:(fun value ->
                update_form_data (fun d -> { d with terms = value }))
              ~tab_index:5 ();
          ];
        (* Instructions *)
        text "";
        hbox ~gap:(`Cells 2)
          [
            text ~style:Style.(fg (Index 240)) "[Tab]";
            text ~style:Style.(fg (Index 245)) "Navigate";
            text ~style:Style.(fg (Index 240)) "[Enter]";
            text ~style:Style.(fg (Index 245)) "Select/Edit";
            text ~style:Style.(fg (Index 240)) "[←→]";
            text ~style:Style.(fg (Index 245)) "Change selection";
            text ~style:Style.(fg (Index 240)) "[Space]";
            text ~style:Style.(fg (Index 245)) "Check/Select";
            text ~style:Style.(fg (Index 240)) "[s]";
            text ~style:Style.(fg (Index 245)) "Submit";
            text ~style:Style.(fg (Index 240)) "[q]";
            text ~style:Style.(fg (Index 245)) "Quit";
          ];
      ]

(* Main entry point *)
let () =
  (* Use non-alt screen mode to preserve form output in terminal history *)
  Mosaic.run ~alt_screen:false ~mouse:false form_app
