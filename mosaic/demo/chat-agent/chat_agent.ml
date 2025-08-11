open Mosaic

(* ===== Starfield Module ===== *)
module Starfield = struct
  type star_point = {
    x : int;
    y : int;
    brightness : int;
    twinkle_phase : float;
    speed : float;
  }

  type asteroid = {
    ax : float;
    ay : float;
    active : bool;
    trail_length : int;
    speed_x : float;
    speed_y : float;
    char_type : int; (* Different asteroid types *)
  }

  let random_star width height =
    {
      x = Random.int width;
      y = Random.int height;
      brightness = Random.int 4;
      (* More brightness levels *)
      twinkle_phase = Random.float (2.0 *. Float.pi);
      speed = 0.5 +. Random.float 2.0;
      (* Variable twinkle speeds *)
    }

  let init_stars width height density =
    let num_stars = width * height * density / 100 in
    List.init num_stars (fun _ -> random_star width height)

  let update_star time star =
    let twinkle = sin ((time *. star.speed) +. star.twinkle_phase) in
    let new_brightness =
      if twinkle > 0.5 then 3
      else if twinkle > 0.0 then 2
      else if twinkle > -0.5 then 1
      else 0
    in
    { star with brightness = new_brightness }

  let update_asteroid asteroid frame width height =
    if asteroid.active then
      let new_x = asteroid.ax +. asteroid.speed_x in
      let new_y = asteroid.ay +. asteroid.speed_y in
      if
        new_x > float_of_int width
        || new_y > float_of_int height
        || new_x < -10.0 || new_y < -5.0
      then { asteroid with active = false }
      else { asteroid with ax = new_x; ay = new_y }
    else if frame mod 30 = 0 && Random.int 100 < 15 then
      (* 15% chance every 30 frames *)
      let start_side = Random.int 4 in
      let sx, sy, speed_x, speed_y =
        match start_side with
        | 0 ->
            (* From left *)
            ( 0.0,
              Random.float (float_of_int height),
              1.5 +. Random.float 2.0,
              Random.float 1.0 -. 0.5 )
        | 1 ->
            (* From top *)
            ( Random.float (float_of_int width),
              0.0,
              Random.float 1.0 -. 0.5,
              1.0 +. Random.float 1.5 )
        | 2 ->
            (* From right *)
            ( float_of_int width,
              Random.float (float_of_int height),
              -1.5 -. Random.float 2.0,
              Random.float 1.0 -. 0.5 )
        | _ ->
            (* Diagonal from top-left *)
            ( Random.float 20.0,
              0.0,
              1.0 +. Random.float 1.5,
              0.5 +. Random.float 1.0 )
      in
      {
        ax = sx;
        ay = sy;
        active = true;
        trail_length = 3 + Random.int 4;
        speed_x;
        speed_y;
        char_type = Random.int 3;
      }
    else asteroid

  let star_char brightness =
    match brightness with 0 -> "·" | 1 -> "∙" | 2 -> "✦" | 3 -> "✧" | _ -> "*"

  let asteroid_char char_type trail_pos =
    match char_type with
    | 0 ->
        (* Classic shooting star *)
        if trail_pos = 0 then "◉"
        else if trail_pos = 1 then "○"
        else if trail_pos = 2 then "∘"
        else "·"
    | 1 ->
        (* Comet-like *)
        if trail_pos = 0 then "✦"
        else if trail_pos = 1 then "+"
        else if trail_pos = 2 then "·"
        else "."
    | _ ->
        (* Small meteor *)
        if trail_pos = 0 then "★" else if trail_pos = 1 then "✦" else "·"

  let render width height stars asteroids =
    let grid = Array.make_matrix height width None in

    (* Place stars *)
    List.iter
      (fun star ->
        if star.y >= 0 && star.y < height && star.x >= 0 && star.x < width then
          grid.(star.y).(star.x) <-
            Some (star_char star.brightness, Style.(fg (Index 252))))
      stars;

    (* Place asteroids with trails *)
    List.iter
      (fun asteroid ->
        if asteroid.active then
          let ax = int_of_float asteroid.ax in
          let ay = int_of_float asteroid.ay in
          for i = 0 to asteroid.trail_length - 1 do
            let tx = ax - int_of_float (float_of_int i *. asteroid.speed_x) in
            let ty = ay - int_of_float (float_of_int i *. asteroid.speed_y) in
            if ty >= 0 && ty < height && tx >= 0 && tx < width then
              let char = asteroid_char asteroid.char_type i in
              let color =
                if i = 0 then Style.(fg (Index 231)) (* Bright white *)
                else if i = 1 then Style.(fg (Index 253))
                else if i = 2 then Style.(fg (Index 250))
                else Style.(fg (Index 245))
              in
              grid.(ty).(tx) <- Some (char, color)
          done)
      asteroids;

    grid
end

(* ===== Chat Agent Types and Functions ===== *)
type message = { text : string; is_user : bool; timestamp : string }
type agent_state = Idle | Thinking | Responding
type command = NewSession | Help | Share | Models | Editor | Redo | None

let format_time () =
  let hour = Random.int 12 + 1 in
  let minute = Random.int 60 in
  let period = if Random.bool () then "AM" else "PM" in
  Printf.sprintf "%02d:%02d %s" hour minute period

let mock_ai_response _prompt =
  let responses =
    [
      "I understand your question. Let me help you with that.";
      "That's an interesting point! Here's what I think...";
      "Based on what you're asking, I would suggest...";
      "Let me analyze this for you...";
      "Here's my perspective on this topic...";
      "I can help you with that. Here's what you need to know...";
    ]
  in
  let idx = Random.int (List.length responses) in
  List.nth responses idx

let parse_command input =
  match String.lowercase_ascii input with
  | "/new" -> NewSession
  | "/help" -> Help
  | "/share" -> Share
  | "/models" -> Models
  | "/editor" -> Editor
  | "/redo" -> Redo
  | _ -> None

let message_component msg =
  let open Ui in
  if msg.is_user then
    vbox ~gap:(`Cells 0)
      [
        text ~style:Style.(fg (Index 252)) msg.text;
        text
          ~style:Style.(fg (Index 244))
          (Printf.sprintf "tmattio (%s)" msg.timestamp);
      ]
  else
    vbox ~gap:(`Cells 0)
      [
        text ~style:Style.(fg (Index 252)) msg.text;
        text
          ~style:Style.(fg (Index 45))
          (Printf.sprintf "Assistant (%s)" msg.timestamp);
      ]

let input_area ~current_input ~agent_state max_width =
  let open Ui in
  let prompt_text =
    match agent_state with
    | Idle -> "enter send"
    | Thinking -> "working..   esc interrupt"
    | Responding -> "typing..."
  in
  box ~max_width:(`Cells max_width)
    ~style:Style.(bg (Index 235))
    ~border:Border.rounded
    ~padding:(sides ~left:2 ~right:2 ~top:1 ~bottom:1 ())
    [
      vbox ~gap:(`Cells 1)
        [
          hbox
            [
              text ~style:Style.(fg (Index 252) ++ bold) ">";
              text ~style:Style.(fg (Index 252)) " ";
              text ~style:Style.(fg (Index 252)) current_input;
              spacer ~flex_grow:1. ();
            ];
          hbox [ text ~style:Style.(fg (Index 244)) prompt_text ];
        ];
    ]

let chat_agent_app () =
  let open Ui in
  (* State *)
  let messages, set_messages, update_messages = use_state [] in
  let current_input, set_current_input, update_current_input = use_state "" in
  let agent_state, set_agent_state, _ = use_state Idle in
  let scroll_offset, set_scroll_offset, update_scroll_offset = use_state 0 in
  let window_height, set_window_height, _ = use_state 24 in
  let window_width, set_window_width, _ = use_state 80 in
  let show_welcome, set_show_welcome, _ = use_state true in

  (* Starfield state *)
  let time, _, update_time = use_state 0.0 in
  let frame, _, update_frame = use_state 0 in
  let stars, _, update_stars =
    use_state (Starfield.init_stars window_width window_height 3)
  in
  let asteroids, _, update_asteroids =
    use_state
      (List.init 5 (fun _ ->
           {
             Starfield.ax = 0.0;
             ay = 0.0;
             active = false;
             trail_length = 5;
             speed_x = 0.0;
             speed_y = 0.0;
             char_type = 0;
           }))
  in

  (* Update starfield animation *)
  use_tick (fun delta ->
      update_time (fun t -> t +. delta);
      update_frame (fun f -> f + 1);
      update_stars (fun current_stars ->
          List.map (Starfield.update_star (time +. delta)) current_stars);
      update_asteroids (fun current_asteroids ->
          List.map
            (fun a ->
              Starfield.update_asteroid a (frame + 1) window_width window_height)
            current_asteroids));

  (* Simulate AI response with subscription *)
  use_subscription
    (if agent_state = Thinking then
       Sub.timer ~every:1.5 (fun () ->
           set_agent_state Responding;
           let last_user_msg =
             List.find_opt (fun m -> m.is_user) messages
             |> Option.map (fun m -> m.text)
             |> Option.value ~default:""
           in
           let response = mock_ai_response last_user_msg in
           update_messages (fun msgs ->
               { text = response; is_user = false; timestamp = format_time () }
               :: msgs);
           set_agent_state Idle;
           set_scroll_offset (max 0 (List.length messages - 5));
           ())
     else Sub.none);

  (* Handle keyboard input *)
  use_subscription
    (Sub.keyboard_filter (fun event ->
         match event.Input.key with
         | Input.Char c when agent_state = Idle ->
             let ch = Buffer.create 4 in
             Uutf.Buffer.add_utf_8 ch c;
             update_current_input (fun s -> s ^ Buffer.contents ch);
             Some ()
         | Input.Backspace when agent_state = Idle ->
             update_current_input (fun s ->
                 if String.length s > 0 then String.sub s 0 (String.length s - 1)
                 else s);
             Some ()
         | Input.Enter
           when agent_state = Idle && String.length current_input > 0 -> (
             let cmd = parse_command current_input in
             match cmd with
             | NewSession ->
                 set_messages [];
                 set_current_input "";
                 set_show_welcome true;
                 Some ()
             | Help | Share | Models | Editor | Redo ->
                 let msg =
                   {
                     text =
                       Printf.sprintf "Command %s not implemented yet"
                         current_input;
                     is_user = false;
                     timestamp = format_time ();
                   }
                 in
                 update_messages (fun msgs -> msg :: msgs);
                 set_current_input "";
                 Some ()
             | None ->
                 set_show_welcome false;
                 let new_msg =
                   {
                     text = current_input;
                     is_user = true;
                     timestamp = format_time ();
                   }
                 in
                 update_messages (fun msgs -> new_msg :: msgs);
                 set_current_input "";
                 set_agent_state Thinking;
                 Some ())
         | Input.Up ->
             update_scroll_offset (fun x -> max 0 (x - 1));
             Some ()
         | Input.Down ->
             update_scroll_offset (( + ) 1);
             Some ()
         | Input.Page_up ->
             update_scroll_offset (fun x -> max 0 (x - 5));
             Some ()
         | Input.Page_down ->
             update_scroll_offset (( + ) 5);
             Some ()
         | Input.Escape when agent_state = Thinking ->
             set_agent_state Idle;
             Some ()
         | Input.Escape -> exit 0
         | Input.Char c when Uchar.to_int c = 0x71 ->
             (* 'q' *)
             exit 0
         | _ -> None));

  (* Handle window resize *)
  use_subscription
    (Sub.window (fun size ->
         set_window_height size.height;
         set_window_width size.width;
         (* Regenerate stars for new size *)
         update_stars (fun _ -> Starfield.init_stars size.width size.height 3);
         ()));

  (* Render starfield background *)
  let starfield_grid =
    Starfield.render window_width window_height stars asteroids
  in
  let starfield_background =
    vbox ~gap:(`Cells 0)
      (Array.to_list
         (Array.mapi
            (fun _y row ->
              hbox ~gap:(`Cells 0)
                (Array.to_list
                   (Array.map
                      (fun cell ->
                        match cell with
                        | Some (char, style) -> text ~style char
                        | None -> text " ")
                      row)))
            starfield_grid))
  in

  (* Calculate centered content dimensions *)
  let content_width = min 80 (window_width - 4) in
  let content_height = min 30 (window_height - 4) in

  (* Render messages *)
  let messages_view =
    if List.length messages = 0 then
      vbox ~gap:(`Cells 1)
        ~style:Style.(fg (Index 244))
        [ text "# New session"; text "/share to create a shareable link" ]
    else
      vbox ~gap:(`Cells 1)
        (List.rev messages
        |> List.map (fun msg ->
               box
                 ~padding:(sides ~left:1 ~right:1 ())
                 ~style:Style.(bg (Index 236))
                 ~border:Border.rounded
                 [ message_component msg ]))
  in

  (* Main content *)
  let main_content =
    if show_welcome then
      (* Just the input prompt, centered with fixed width *)
      box ~min_width:(`Cells window_width) ~min_height:(`Cells window_height)
        [ center (input_area ~current_input ~agent_state 60) ]
    else
      box ~min_width:(`Cells window_width) ~min_height:(`Cells window_height)
        [
          center
            (box ~max_width:(`Cells content_width)
               ~max_height:(`Cells content_height)
               ~style:Style.(bg (Index 235))
               ~border:Border.rounded
               ~padding:(sides ~left:2 ~right:2 ~top:1 ~bottom:1 ())
               [
                 vbox ~gap:(`Cells 1)
                   [
                     scroll_view
                       ~min_height:(`Cells (max 10 (content_height - 8)))
                       ~h_offset:0 ~v_offset:scroll_offset messages_view;
                     spacer ~flex_grow:1. ();
                     input_area ~current_input ~agent_state (content_width - 4);
                   ];
               ]);
        ]
  in

  (* Compose final layout with starfield background and centered content *)
  zbox
    [
      (* Background starfield *)
      starfield_background;
      (* Main content *)
      main_content;
    ]

(* Main entry point *)
let () =
  Random.self_init ();
  run ~alt_screen:true ~mouse:true ~fps:30 chat_agent_app
    ~debug:"mosaic-chat.log"
