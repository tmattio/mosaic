open Mosaic
open Ui

(* Rectangle type with position, size, and color *)
type rect = {
  id : int;
  x : int;
  y : int;
  width : int;
  height : int;
  color : Style.color;
  label : string;
}

(* Create initial rectangles with alpha transparency *)
let initial_rects =
  [
    {
      id = 1;
      x = 5;
      y = 3;
      width = 20;
      height = 6;
      color = Style.RGBA (255, 100, 100, 180);
      label = "Red Box";
    };
    {
      id = 2;
      x = 15;
      y = 5;
      width = 25;
      height = 8;
      color = Style.RGBA (100, 255, 100, 150);
      label = "Green Box";
    };
    {
      id = 3;
      x = 10;
      y = 8;
      width = 18;
      height = 5;
      color = Style.RGBA (100, 150, 255, 200);
      label = "Blue Box";
    };
    {
      id = 4;
      x = 20;
      y = 10;
      width = 22;
      height = 7;
      color = Style.RGBA (255, 200, 100, 120);
      label = "Orange Box";
    };
  ]

(* Draggable rectangle component using the new API *)
let draggable_rect rect ~handle_drag ~on_bring_to_front ~is_dragging =
  (* Use a stable key based on the rectangle's ID *)
  let key = Ui.Key.of_int rect.id in

  (* Subscribe to drag events - these are hooks so they work in components *)
  use_subscription
    (on_drag key (fun drag_event ->
         match drag_event.phase with
         | `Start -> on_bring_to_front rect.id
         | `Move ->
             (* Calculate new position based on drag delta *)
             let new_x = rect.x + drag_event.dx in
             let new_y = rect.y + drag_event.dy in
             handle_drag rect.id new_x new_y
         | `End -> ()));

  (* Visual feedback when dragging *)
  let style =
    if is_dragging then Style.(bg rect.color ++ fg Black ++ bold ++ dim)
    else Style.(bg rect.color ++ fg Black)
  in

  (* Position the rectangle absolutely and attach the key *)
  box ~position:`Absolute
    ~inset:(sides ~top:rect.y ~left:rect.x ())
    [
      with_key key
        (box ~style ~padding:(all 0) ~min_width:(`Cells rect.width)
           ~max_width:(`Cells rect.width) ~min_height:(`Cells rect.height)
           ~max_height:(`Cells rect.height)
           [
             vbox ~align_items:`Center ~justify_content:`Center
               [ text rect.label ];
           ]);
    ]

(* Main app component *)
let drag_app () =
  (* State *)
  let rects, set_rects, update_rects = use_state initial_rects in
  let dragging_id, set_dragging_id, _ = use_state None in
  let mouse_pos, set_mouse_pos, _ = use_state (0, 0) in

  (* Track mouse position globally *)
  use_subscription (Sub.on_mouse_motion (fun x y -> set_mouse_pos (x, y)));

  (* Keyboard shortcuts *)
  use_subscription
    (Sub.keyboard_filter (fun event ->
         match event.Input.key with
         | Input.Escape ->
             set_dragging_id None;
             Some ()
         | _ -> None));

  use_subscription
    (Sub.keyboard_filter (fun event ->
         match event.Input.key with
         | Input.Char c when Uchar.to_int c = 0x72 ->
             (* 'r' *)
             set_rects initial_rects;
             set_dragging_id None;
             Some ()
         | _ -> None));

  use_subscription
    (Sub.keyboard_filter (fun event ->
         match event.Input.key with
         | Input.Char c when Uchar.to_int c = 0x71 ->
             (* 'q' *)
             dispatch_cmd Cmd.quit;
             Some ()
         | _ -> None));

  (* Callbacks for drag operations *)
  let handle_drag rect_id new_x new_y =
    set_dragging_id (Some rect_id);
    update_rects (fun rs ->
        List.map
          (fun r ->
            if r.id = rect_id then
              (* Clamp to positive coordinates *)
              { r with x = max 0 new_x; y = max 0 new_y }
            else r)
          rs)
  in

  let bring_to_front rect_id =
    set_dragging_id (Some rect_id);
    update_rects (fun rs ->
        match List.partition (fun r -> r.id = rect_id) rs with
        | [ rect ], others -> others @ [ rect ] (* Move to end = on top *)
        | _ -> rs)
  in

  (* Render *)
  vbox
    [
      (* Header *)
      hbox
        ~style:Style.(bg (Index 236) ++ fg (Index 250))
        [
          text " 🎯 Drag & Drop Demo (New API) ";
          spacer ~flex_grow:1. ();
          text
            (Printf.sprintf " Mouse: (%d, %d) " (fst mouse_pos) (snd mouse_pos));
        ];
      (* Canvas with draggable rectangles *)
      (* We need to ensure the canvas fills its space and clears the background *)
      box ~position:`Relative ~flex_grow:1.
        ~min_width:(`Cells 80) (* Ensure it fills the width *)
        ~min_height:(`Cells 20) (* Ensure it has a minimum height *)
        ~style:Style.(bg (Index 234))
        (* First child: a background filler that covers the entire area *)
        (box ~position:`Absolute
           ~inset:(sides ~top:0 ~left:0 ~right:0 ~bottom:0 ())
           ~style:Style.(bg (Index 234))
           []
        :: (* Then the draggable rectangles on top *)
           List.map
             (fun rect ->
               draggable_rect rect ~handle_drag
                 ~on_bring_to_front:bring_to_front
                 ~is_dragging:(Some rect.id = dragging_id))
             rects);
      (* Status bar *)
      hbox
        ~style:Style.(bg (Index 238) ++ fg (Index 250))
        [
          text " [Click & Drag] Move boxes ";
          text " [r] Reset ";
          text " [Esc] Cancel drag ";
          text " [q] Quit ";
          spacer ~flex_grow:1. ();
          text
            (match dragging_id with
            | None -> " Ready "
            | Some id -> (
                match List.find_opt (fun r -> r.id = id) rects with
                | Some r -> Printf.sprintf " Dragging: %s " r.label
                | None -> " Dragging... "));
        ];
    ]

(* Run the app *)
let () =
  Printexc.record_backtrace true;
  run ~alt_screen:true ~mouse:true drag_app
