open Mosaic

type star_point = { x : int; y : int; brightness : int; twinkle_phase : float }

type shooting_star = {
  sx : float;
  sy : float;
  active : bool;
  trail_length : int;
}

let width = 50
let height = 20
let num_stars = 30

let random_star () =
  {
    x = Random.int width;
    y = Random.int height;
    brightness = Random.int 3;
    twinkle_phase = Random.float (2.0 *. Float.pi);
  }

let init_stars () = List.init num_stars (fun _ -> random_star ())

let update_star time star =
  let twinkle = sin ((time *. 3.0) +. star.twinkle_phase) in
  let new_brightness =
    if twinkle > 0.3 then 2 else if twinkle > -0.3 then 1 else 0
  in
  { star with brightness = new_brightness }

let update_shooting_star shooting_star frame =
  if shooting_star.active then
    let new_x = shooting_star.sx +. 2.0 in
    let new_y = shooting_star.sy +. 1.0 in
    if new_x > float_of_int width || new_y > float_of_int height then
      { shooting_star with active = false }
    else { shooting_star with sx = new_x; sy = new_y }
  else if frame mod 100 = 0 && Random.int 10 = 0 then
    { sx = Random.float 10.0; sy = 0.0; active = true; trail_length = 5 }
  else shooting_star

let star_char brightness =
  match brightness with 0 -> "·" | 1 -> "✦" | 2 -> "✧" | _ -> "*"

let render_line stars shooting_star y =
  let line = Array.make width " " in

  List.iter
    (fun star ->
      if star.y = y && star.x >= 0 && star.x < width then
        line.(star.x) <- star_char star.brightness)
    stars;

  (if shooting_star.active then
     let sx = int_of_float shooting_star.sx in
     let sy = int_of_float shooting_star.sy in
     for i = 0 to shooting_star.trail_length - 1 do
       let tx = sx - (i * 2) in
       let ty = sy - i in
       if ty = y && tx >= 0 && tx < width then
         let char =
           if i = 0 then "◉"
           else if i = 1 then "○"
           else if i = 2 then "∘"
           else "·"
         in
         line.(tx) <- char
     done);

  String.concat "" (Array.to_list line)

let app () =
  let time, _, update_time = use_state 0.0 in
  let frame, _, update_frame = use_state 0 in
  let stars, _, update_stars = use_state (init_stars ()) in
  let shooting_star, _, update_shooting_star_state =
    use_state { sx = 0.0; sy = 0.0; active = false; trail_length = 5 }
  in

  (* Use the new tick hook for smooth animation *)
  use_tick (fun delta ->
      update_time (fun t -> t +. delta);
      update_frame (fun f -> f + 1);
      update_stars (fun current_stars ->
          List.map (update_star (time +. delta)) current_stars);
      update_shooting_star_state (fun ss -> update_shooting_star ss (frame + 1)));

  (* Handle quit *)
  use_subscription
    (Sub.keyboard_filter (fun event ->
         match event.Input.key with
         | Input.Char c when Uchar.to_int c = 0x71 || Uchar.to_int c = 0x51 ->
             (* 'q' or 'Q' *)
             dispatch_cmd Cmd.quit;
             Some ()
         | Input.Char c when Uchar.to_int c = 0x03 && event.Input.modifier.ctrl
           ->
             (* Ctrl+C *)
             dispatch_cmd Cmd.quit;
             Some ()
         | Input.Escape ->
             dispatch_cmd Cmd.quit;
             Some ()
         | _ -> None));

  Ui.(
    vbox ~gap:(`Cells 1)
      [
        text
          ~style:Style.(fg (Index 45))
          "╔══════════════════════════════════════════════════╗";
        text
          ~style:Style.(fg (Index 45))
          "║         ✨ Starfield Animation ✨                ║";
        text
          ~style:Style.(fg (Index 45))
          "╚══════════════════════════════════════════════════╝";
        text "";
        vbox
          (List.init height (fun y -> text (render_line stars shooting_star y)));
        text "";
        hbox ~gap:(`Cells 2)
          [
            text
              ~style:Style.(fg (Index 250))
              "Stars twinkle and shooting stars pass by...";
            text
              ~style:Style.(fg (Index 240))
              (Printf.sprintf "Frame: %d" frame);
          ];
        text ~style:Style.(fg (Index 250)) "Press 'q' or Ctrl+C to exit";
      ])

let () =
  Random.self_init ();
  run ~alt_screen:true ~fps:20 app
