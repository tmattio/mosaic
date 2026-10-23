open Matrix

(* Drawing canvas: mouse-driven paint program with alpha blending *)

type brush = { size : int; color : Ansi.Color.t; alpha : float }
type state = { brush : brush; show_palette : bool; show_help : bool }

let palette_colors =
  [|
    Ansi.Color.red;
    Ansi.Color.bright_red;
    Ansi.Color.yellow;
    Ansi.Color.bright_yellow;
    Ansi.Color.green;
    Ansi.Color.bright_green;
    Ansi.Color.cyan;
    Ansi.Color.bright_cyan;
    Ansi.Color.blue;
    Ansi.Color.bright_blue;
    Ansi.Color.magenta;
    Ansi.Color.bright_magenta;
    Ansi.Color.white;
    Ansi.Color.bright_white;
    Ansi.Color.black;
    Ansi.Color.bright_black;
  |]

let initial_state =
  {
    brush = { size = 1; color = Ansi.Color.white; alpha = 1.0 };
    show_palette = true;
    show_help = true;
  }

let color_with_alpha color alpha =
  let r, g, b, _ = Ansi.Color.to_rgba color in
  Ansi.Color.of_rgba r g b (int_of_float (alpha *. 255.))

let draw_pixel grid ~x ~y ~color ~alpha =
  if alpha >= 0.999 then
    (* Fully opaque - just set the background *)
    let style = Ansi.Style.make ~bg:color () in
    Grid.draw_text ~style grid ~x ~y ~text:" "
  else
    (* Use alpha blending *)
    let bg = color_with_alpha color alpha in
    Grid.set_uchar grid ~x ~y ~uchar:(Uchar.of_char ' ') ~fg:Ansi.Color.white
      ~bg ~attrs:Ansi.Attr.empty ~blend:true ()

let draw_brush grid ~cx ~cy ~brush =
  let half = brush.size / 2 in
  for dy = -half to half do
    for dx = -half to half do
      let x = cx + dx in
      let y = cy + dy in
      (* Simple circle check for larger brushes *)
      if brush.size <= 1 || (dx * dx) + (dy * dy) <= half * half then
        draw_pixel grid ~x ~y ~color:brush.color ~alpha:brush.alpha
    done
  done

let draw_palette grid ~cols ~rows:_ state =
  if not state.show_palette then ()
  else
    let panel_width = 20 in
    let panel_height = Array.length palette_colors + 5 in
    let px = cols - panel_width - 2 in
    let py = 1 in
    (* Draw panel background *)
    let bg_style = Ansi.Style.make ~bg:(Ansi.Color.grayscale ~level:3) () in
    for y = py to py + panel_height - 1 do
      for x = px to px + panel_width - 1 do
        Grid.draw_text ~style:bg_style grid ~x ~y ~text:" "
      done
    done;
    (* Title *)
    let title_style =
      Ansi.Style.make ~fg:Ansi.Color.bright_white ~bold:true ()
    in
    Grid.draw_text ~style:title_style grid ~x:(px + 1) ~y:py ~text:"Palette [P]";
    (* Colors *)
    let label_style = Ansi.Style.make ~fg:Ansi.Color.white () in
    Grid.draw_text ~style:label_style grid ~x:(px + 1) ~y:(py + 1)
      ~text:"Colors (1-9,0):";
    for i = 0 to min 9 (Array.length palette_colors - 1) do
      let color = palette_colors.(i) in
      let style = Ansi.Style.make ~bg:color () in
      let marker =
        if Ansi.Color.equal color state.brush.color then "*" else " "
      in
      Grid.draw_text ~style grid ~x:(px + 1 + (i * 2)) ~y:(py + 2) ~text:marker
    done;
    (* Alpha *)
    Grid.draw_text ~style:label_style grid ~x:(px + 1) ~y:(py + 4)
      ~text:"Alpha (A/Z):";
    let alpha_str = Printf.sprintf "%.0f%%" (state.brush.alpha *. 100.) in
    Grid.draw_text ~style:label_style grid ~x:(px + 13) ~y:(py + 4)
      ~text:alpha_str;
    (* Brush size *)
    Grid.draw_text ~style:label_style grid ~x:(px + 1) ~y:(py + 5)
      ~text:"Size (+/-):";
    let size_str = string_of_int state.brush.size in
    Grid.draw_text ~style:label_style grid ~x:(px + 13) ~y:(py + 5)
      ~text:size_str;
    (* Current brush preview *)
    Grid.draw_text ~style:label_style grid ~x:(px + 1) ~y:(py + 7)
      ~text:"Brush:";
    let preview_style = Ansi.Style.make ~bg:state.brush.color () in
    Grid.draw_text ~style:preview_style grid ~x:(px + 8) ~y:(py + 7) ~text:"   "

let draw_help grid ~rows state =
  if not state.show_help then ()
  else
    let help_lines =
      [
        "Click/drag to draw";
        "1-0: colors";
        "A/Z: alpha +/-";
        "+/-: brush size";
        "C: clear canvas";
        "P: toggle palette";
        "H: toggle help";
        "Esc: quit";
      ]
    in
    let style = Ansi.Style.make ~fg:Ansi.Color.bright_black () in
    List.iteri
      (fun i line ->
        Grid.draw_text ~style grid ~x:2
          ~y:(rows - List.length help_lines + i - 1)
          ~text:line)
      help_lines

let handle_key state (key : Input.Keystroke.key) (mods : Input.Keystroke.mods) =
  match key with
  | Char u when Uchar.is_char u -> (
      match Uchar.to_char u with
      | '1' .. '9' as c ->
          let idx = Char.code c - Char.code '1' in
          if idx < Array.length palette_colors then
            {
              state with
              brush = { state.brush with color = palette_colors.(idx) };
            }
          else state
      | '0' ->
          if 9 < Array.length palette_colors then
            {
              state with
              brush = { state.brush with color = palette_colors.(9) };
            }
          else state
      | 'a' | 'A' ->
          let new_alpha = min 1.0 (state.brush.alpha +. 0.1) in
          { state with brush = { state.brush with alpha = new_alpha } }
      | 'z' | 'Z' ->
          let new_alpha = max 0.1 (state.brush.alpha -. 0.1) in
          { state with brush = { state.brush with alpha = new_alpha } }
      | ('c' | 'C') when not mods.ctrl ->
          state (* Clear is handled separately *)
      | 'p' | 'P' -> { state with show_palette = not state.show_palette }
      | 'h' | 'H' -> { state with show_help = not state.show_help }
      | '+' | '=' ->
          let new_size = min 10 (state.brush.size + 1) in
          { state with brush = { state.brush with size = new_size } }
      | '-' | '_' ->
          let new_size = max 1 (state.brush.size - 1) in
          { state with brush = { state.brush with size = new_size } }
      | _ -> state)
  | _ -> state

(* The canvas keeps what was drawn, at the terminal's size: a resize keeps
   the part of it that still fits. *)
let resize_canvas canvas ~cols ~rows =
  let grid = Grid.create ~width:cols ~height:rows () in
  Grid.clear ~color:Ansi.Color.black grid;
  Option.iter
    (fun old ->
      Grid.blit_region ~src:old ~dst:grid ~src_x:0 ~src_y:0
        ~width:(Grid.width old) ~height:(Grid.height old) ~dst_x:0 ~dst_y:0)
    canvas;
  grid

let () =
  Matrix.run @@ fun app ->
  let cols, rows = Matrix.size app in
  let canvas = ref (resize_canvas None ~cols ~rows) in
  let rec loop state =
    match Matrix.next app with
    | Stopped | Event (Key { key = Escape; _ }) -> ()
    | Event (Key k) when Input.Keystroke.is_interrupt k ->
        (* Raw mode makes Ctrl+C a key: this program quits on it. *)
        ()
    | Event event ->
        Matrix.request_redraw app;
        loop (handle_event state event)
    | Frame ->
        Matrix.draw app (fun grid ->
            let cols, rows = Matrix.size app in
            Grid.blit ~src:!canvas ~dst:grid;
            draw_palette grid ~cols ~rows state;
            draw_help grid ~rows state);
        loop state
    | Timer _ | Posted _ -> loop state
  and handle_event state = function
    | Key { key = Char u; mods; _ }
      when (Uchar.to_int u = Char.code 'c' || Uchar.to_int u = Char.code 'C')
           && not mods.ctrl ->
        Grid.clear ~color:Ansi.Color.black !canvas;
        state
    | Key { key; mods; _ } -> handle_key state key mods
    | Mouse { x; y; kind = Press Left | Motion (Some Left); _ } ->
        draw_brush !canvas ~cx:x ~cy:y ~brush:state.brush;
        state
    | Mouse { x; y; kind = Press Right | Motion (Some Right); _ } ->
        (* Erase with right click *)
        let brush =
          { state.brush with color = Ansi.Color.black; alpha = 1.0 }
        in
        draw_brush !canvas ~cx:x ~cy:y ~brush;
        state
    | Resize (cols, rows) ->
        canvas := resize_canvas (Some !canvas) ~cols ~rows;
        state
    | _ -> state
  in
  loop initial_state
