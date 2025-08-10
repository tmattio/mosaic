open Mosaic

(* Simple Counter Component using React-style hooks *)
let counter label =
  let open Ui in
  (* useState hook for counter value *)
  let count, _, update_count = use_state 0 in

  (* useEffect hook - runs on mount and when count changes *)
  use_effect
    ~deps:Deps.(keys [ int count ])
    (fun () ->
      if count > 0 then Printf.printf "%s: Count is now %d\n%!" label count;
      None (* No cleanup needed *));

  (* Subscribe to keyboard events using functional updates to avoid stale closures *)
  use_subscription
    (Sub.keyboard_filter (fun event ->
         match event.Input.key with
         | Input.Char c when Uchar.to_int c = 0x2B ->
             (* '+' *)
             update_count (( + ) 1);
             Some ()
         | Input.Char c when Uchar.to_int c = 0x2D ->
             (* '-' *)
             update_count (fun x -> x - 1);
             Some ()
         | Input.Char c when Uchar.to_int c = 0x30 ->
             (* '0' *)
             update_count (Fun.const 0);
             Some ()
         | _ -> None));

  (* Render the UI *)
  vbox ~gap:(`Cells 1)
    [
      text ~style:Style.(fg (RGB (100, 100, 200))) label;
      hbox ~gap:(`Cells 2)
        [
          text (Printf.sprintf "Count: %d" count);
          text ~style:Style.(fg (Index 8)) "[+/-/0 keys to change]";
        ];
    ]

(* Theme Context *)
type theme = Light | Dark

let theme_context = create_context ~default:Dark ()

(* Themed Box Component *)
let themed_box children =
  let open Ui in
  let theme = use_context theme_context in
  let style =
    match theme with
    | Light -> Style.(bg (Index 255) ++ fg Black)
    | Dark -> Style.(bg (Index 236) ++ fg White)
  in
  let padding = all 1 in
  box ~padding ~style [ children () ]

(* Main App Component *)
let app () =
  let open Ui in
  (* Theme state *)
  let theme, _, update_theme = use_state Dark in

  (* Subscribe to theme toggle *)
  use_subscription
    (Sub.keyboard_filter (fun event ->
         match event.Input.key with
         | Input.Char c when Uchar.to_int c = 0x74 ->
             (* 't' *)
             update_theme (function Light -> Dark | Dark -> Light);
             Some ()
         | _ -> None));

  (* Subscribe to quit *)
  use_subscription
    (Sub.keyboard_filter (fun event ->
         match event.Input.key with
         | Input.Char c when Uchar.to_int c = 0x71 ->
             (* 'q' *)
             dispatch_cmd Cmd.quit;
             Some ()
         | Input.Char c when Uchar.to_int c = 0x03 && event.Input.modifier.ctrl
           ->
             (* Ctrl+C *)
             dispatch_cmd Cmd.quit;
             Some ()
         | _ -> None));

  (* Provide theme context and render *)
  provide theme_context theme (fun () ->
      vbox ~gap:(`Cells 1)
        [
          text
            ~style:Style.(fg (RGB (200, 100, 0)) ++ bold)
            "🎯 React Counter Example";
          text
            ~style:Style.(fg (Index 8))
            "Demonstrating hooks: useState, useEffect, useContext";
          spacer ();
          (* Multiple counter instances *)
          themed_box (fun () -> counter "Counter A");
          themed_box (fun () -> counter "Counter B");
          spacer ();
          (* Current theme display *)
          text
            (Printf.sprintf "Current theme: %s"
               (match theme with Light -> "Light ☀️" | Dark -> "Dark 🌙"));
          spacer ();
          (* Instructions *)
          text ~style:Style.(fg (Index 8)) "[t] Toggle theme";
          text ~style:Style.(fg (Index 8)) "[q/Ctrl+C] Quit";
        ])

(* Run the app *)
let () =
  Printexc.record_backtrace true;
  run ~alt_screen:true app
