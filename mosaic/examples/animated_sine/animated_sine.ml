open Mosaic
open Mosaic_charts

let generate_sine_wave time amplitude frequency phase num_points =
  List.init num_points (fun i ->
      let x = float_of_int i /. float_of_int num_points *. 4.0 *. Float.pi in
      { x; y = amplitude *. sin ((frequency *. x) +. phase +. time) })

let animated_sine () =
  let open Ui in
  let time, _, update_time = use_state 0.0 in
  let amplitude, _, update_amplitude = use_state 1.0 in
  let frequency, _, update_frequency = use_state 1.0 in
  let paused, _, update_paused = use_state false in

  use_tick (fun delta ->
      if not paused then update_time (fun t -> t +. (delta *. 0.002)));

  use_subscription
    (Sub.keyboard_filter (fun event ->
         match event.Input.key with
         | Input.Char c when Uchar.to_int c = 0x20 ->
             update_paused not;
             Some ()
         | Input.Char c when Uchar.to_int c = 0x2B ->
             update_amplitude (( +. ) 0.1);
             Some ()
         | Input.Char c when Uchar.to_int c = 0x2D ->
             update_amplitude (fun a -> max 0.1 (a -. 0.1));
             Some ()
         | Input.Char c when Uchar.to_int c = 0x66 ->
             update_frequency (( +. ) 0.1);
             Some ()
         | Input.Char c when Uchar.to_int c = 0x73 ->
             update_frequency (fun f -> max 0.1 (f -. 0.1));
             Some ()
         | Input.Char c when Uchar.to_int c = 0x72 ->
             update_time (Fun.const 0.0);
             update_amplitude (Fun.const 1.0);
             update_frequency (Fun.const 1.0);
             Some ()
         | Input.Char c when Uchar.to_int c = 0x71 ->
             dispatch_cmd Cmd.quit;
             Some ()
         | Input.Char c when Uchar.to_int c = 0x03 && event.Input.modifier.ctrl
           ->
             dispatch_cmd Cmd.quit;
             Some ()
         | _ -> None));

  let sine_data = generate_sine_wave time amplitude frequency 0.0 100 in
  let cosine_data =
    generate_sine_wave time (amplitude *. 0.7) frequency (Float.pi /. 2.0) 100
  in
  let combined_data =
    generate_sine_wave time (amplitude *. 0.5) (frequency *. 2.0)
      (Float.pi /. 4.0) 100
  in

  vbox ~gap:(`Cells 1)
    [
      text ~style:Style.(fg Cyan ++ bold ++ underline) "📈 Animated Sine Wave";
      text "";
      box
        ~style:Style.(bg (Index 235))
        [
          line ~width:(`Cells 80) ~height:(`Cells 20) ~render_kind:Lines
            ~series_styles:
              [
                Style.(fg (RGB (255, 100, 100)));
                Style.(fg (RGB (100, 100, 255)));
                Style.(fg (RGB (100, 255, 100)));
              ]
            [
              ("sin(x)", sine_data);
              ("cos(x)", cosine_data);
              ("harmonic", combined_data);
            ];
        ];
      text "";
      hbox ~gap:(`Cells 4)
        [
          vbox ~gap:(`Cells 1)
            [
              text ~style:Style.(fg Yellow) "Parameters:";
              text (Printf.sprintf "Amplitude: %.1f" amplitude);
              text (Printf.sprintf "Frequency: %.1f" frequency);
              text (Printf.sprintf "Time: %.3f" time);
            ];
          vbox ~gap:(`Cells 1)
            [
              text ~style:Style.(fg Green) "Status:";
              text
                ~style:(if paused then Style.(fg Red) else Style.(fg Green))
                (if paused then "⏸  PAUSED" else "▶  RUNNING");
            ];
        ];
      text "";
      vbox ~gap:(`Cells 0)
        [
          text ~style:Style.(fg (Index 8)) "Controls:";
          text ~style:Style.(fg (Index 8)) "[SPACE]  Pause/Resume animation";
          text ~style:Style.(fg (Index 8)) "[+/-]    Adjust amplitude";
          text ~style:Style.(fg (Index 8)) "[f/s]    Faster/Slower frequency";
          text ~style:Style.(fg (Index 8)) "[r]      Reset parameters";
          text ~style:Style.(fg (Index 8)) "[q]      Quit";
        ];
    ]

let () =
  Printexc.record_backtrace true;
  run ~alt_screen:true ~fps:30 animated_sine
