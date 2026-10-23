(** Countdown timer with start/stop/reset controls. *)

open Mosaic

type timer_state = Idle | Running | Paused

type model = {
  time_remaining : int;
  initial_time : int;
  state : timer_state;
  input_minutes : Editor.t;
  input_seconds : Editor.t;
  elapsed_time : float;
  scroll : Scroll.t;
  focused : [ `Minutes | `Seconds ];
}

type msg =
  | Scrolled of Scroll.t
  | Focus of [ `Minutes | `Seconds ]
  | Start
  | Stop
  | Reset
  | Tick of float
  | Set_minutes of Editor.t
  | Set_seconds of Editor.t
  | Set_time
  | Quit

let format_time seconds =
  let minutes = seconds / 60 in
  let secs = seconds mod 60 in
  Printf.sprintf "%02d:%02d" minutes secs

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let init () =
  ( {
      time_remaining = 0;
      initial_time = 0;
      state = Idle;
      input_minutes = Editor.empty;
      input_seconds = Editor.empty;
      scroll = Scroll.at 0;
      focused = `Minutes;
      elapsed_time = 0.0;
    },
    Cmd.none )

let update msg m =
  match msg with
  | Scrolled scroll -> ({ m with scroll }, Cmd.none)
  | Focus focused -> ({ m with focused }, Cmd.none)
  | Start -> (
      match m.state with
      | Idle | Paused -> ({ m with state = Running }, Cmd.none)
      | Running -> (m, Cmd.none))
  | Stop -> (
      match m.state with
      | Running -> ({ m with state = Paused }, Cmd.none)
      | Idle | Paused -> (m, Cmd.none))
  | Reset ->
      ( {
          m with
          time_remaining = m.initial_time;
          state = Idle;
          elapsed_time = 0.0;
        },
        Cmd.none )
  | Tick dt ->
      if m.state = Running then
        let elapsed = m.elapsed_time +. dt in
        if elapsed >= 1.0 then
          let new_time = max 0 (m.time_remaining - 1) in
          let new_state = if new_time = 0 then Idle else Running in
          ( {
              m with
              time_remaining = new_time;
              state = new_state;
              elapsed_time = 0.0;
            },
            Cmd.none )
        else ({ m with elapsed_time = elapsed }, Cmd.none)
      else (m, Cmd.none)
  | Set_minutes s -> ({ m with input_minutes = s }, Cmd.none)
  | Set_seconds s -> ({ m with input_seconds = s }, Cmd.none)
  | Set_time ->
      let minutes =
        try int_of_string (Editor.text m.input_minutes) with _ -> 0
      in
      let seconds =
        try int_of_string (Editor.text m.input_seconds) with _ -> 0
      in
      let total_seconds = (minutes * 60) + seconds in
      ( {
          m with
          time_remaining = total_seconds;
          initial_time = total_seconds;
          state = Idle;
          elapsed_time = 0.0;
        },
        Cmd.none )
  | Quit -> (m, Cmd.quit)

let view model =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      (* Header *)
      box ~padding:(padding 1) ~background:header_bg
        [
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            ~size:{ width = pct 100; height = auto }
            [
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Timer";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~align_items:Center ~justify_content:Center
        [
          scroll_box ~scroll:model.scroll
            ~on_scroll:(fun scroll -> Scrolled scroll)
            ~size:{ width = pct 100; height = pct 100 }
            ~scroll_y:true ~scroll_x:false
            [
              box ~flex_direction:Column ~align_items:Center ~gap:(gap 2)
                ~border:true ~border_color ~padding:(padding 3)
                [
                  box ~flex_direction:Row ~align_items:Center ~gap:(gap 1)
                    [
                      text
                        ~style:
                          (Ansi.Style.make ~bold:true ~fg:Ansi.Color.white ())
                        (format_time model.time_remaining);
                    ];
                  text
                    ~style:(Ansi.Style.make ~dim:true ())
                    (match model.state with
                    | Idle -> "Status: Idle"
                    | Running -> "Status: Running"
                    | Paused -> "Status: Paused");
                  box ~flex_direction:Row ~align_items:Center ~gap:(gap 1)
                    [
                      text "Minutes:";
                      input
                        ~size:{ width = px 5; height = px 1 }
                        ~state:model.input_minutes
                        ~focused:(model.focused = `Minutes)
                        ~on_focus_request:(Focus `Minutes)
                        ~on_change:(fun v -> Set_minutes v)
                        ();
                      text "Seconds:";
                      input
                        ~size:{ width = px 5; height = px 1 }
                        ~state:model.input_seconds
                        ~focused:(model.focused = `Seconds)
                        ~on_focus_request:(Focus `Seconds)
                        ~on_change:(fun v -> Set_seconds v)
                        ();
                    ];
                  box ~border:true ~padding:(padding 1)
                    ~on_mouse:(fun ev ->
                      match Event.Mouse.kind ev with
                      | Down _ -> Some Set_time
                      | _ -> None)
                    [ text ~style:(Ansi.Style.make ~bold:true ()) "Set Time" ];
                  box ~flex_direction:Row ~gap:(gap 1)
                    [
                      box ~border:true ~padding:(padding 1)
                        ~on_mouse:(fun ev ->
                          match Event.Mouse.kind ev with
                          | Down _ -> Some Start
                          | _ -> None)
                        [
                          text
                            ~style:
                              (Ansi.Style.make ~bold:true ~fg:Ansi.Color.green
                                 ())
                            "Start";
                        ];
                      box ~border:true ~padding:(padding 1)
                        ~on_mouse:(fun ev ->
                          match Event.Mouse.kind ev with
                          | Down _ -> Some Stop
                          | _ -> None)
                        [
                          text
                            ~style:
                              (Ansi.Style.make ~bold:true ~fg:Ansi.Color.red ())
                            "Stop";
                        ];
                      box ~border:true ~padding:(padding 1)
                        ~on_mouse:(fun ev ->
                          match Event.Mouse.kind ev with
                          | Down _ -> Some Reset
                          | _ -> None)
                        [
                          text
                            ~style:
                              (Ansi.Style.make ~bold:true ~fg:Ansi.Color.yellow
                                 ())
                            "Reset";
                        ];
                    ];
                ];
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "Space start/stop  •  r reset  •  q quit" ];
    ]

let subscriptions model =
  Sub.batch
    [
      Sub.on_tick ~id:"animation" (fun ~dt -> Tick dt);
      (* One of the time inputs is always focused, and it would type these
         keys: the application takes them first. *)
      Sub.on_key ~phase:Capture (fun ev ->
          if Shortcut.matches Shortcut.space ev then (
            Event.Key.prevent_default ev;
            Some (match model.state with Running -> Stop | _ -> Start))
          else None);
      Sub.on_keys ~phase:Capture
        [
          (Shortcut.char 'r', Reset);
          (Shortcut.char 'q', Quit);
          (Shortcut.escape, Quit);
        ];
    ]

let () = run { init; update; view; subscriptions }
