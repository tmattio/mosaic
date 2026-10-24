(** Snake on a fixed-rate tick, a timer re-armed with [Matrix.at] at every tick;
    a resize restarts the game.

    The arrows steer; p pauses; r restarts after a game over; Esc quits. *)

open Matrix

type direction = Up | Down | Left | Right

type game_state = {
  snake : (int * int) list;
  direction : direction;
  food : int * int;
  score : int;
  game_over : bool;
  paused : bool;
  cols : int;
  rows : int;
}

let opposite_dir d1 d2 =
  match (d1, d2) with
  | Up, Down | Down, Up | Left, Right | Right, Left -> true
  | _ -> false

let random_food ~cols ~rows snake =
  if cols < 3 || rows < 5 then (cols / 2, rows / 2)
  else
    let rec find () =
      let x = 1 + Random.int (cols - 2) in
      let y = 2 + Random.int (rows - 4) in
      if List.mem (x, y) snake then find () else (x, y)
    in
    find ()

let initial_state ~cols ~rows =
  let cx = cols / 2 in
  let cy = rows / 2 in
  let snake = [ (cx, cy); (cx - 1, cy); (cx - 2, cy) ] in
  {
    snake;
    direction = Right;
    food = random_food ~cols ~rows snake;
    score = 0;
    game_over = false;
    paused = false;
    cols;
    rows;
  }

let move_snake state =
  if state.game_over || state.paused then state
  else
    let hx, hy = List.hd state.snake in
    let nx, ny =
      match state.direction with
      | Up -> (hx, hy - 1)
      | Down -> (hx, hy + 1)
      | Left -> (hx - 1, hy)
      | Right -> (hx + 1, hy)
    in
    (* Check wall collision *)
    let hit_wall =
      nx < 1 || nx >= state.cols - 1 || ny < 2 || ny >= state.rows - 2
    in
    (* Check self collision *)
    let hit_self = List.mem (nx, ny) state.snake in
    if hit_wall || hit_self then { state with game_over = true }
    else
      let ate_food = nx = fst state.food && ny = snd state.food in
      let new_snake =
        if ate_food then (nx, ny) :: state.snake
        else
          (* Remove tail *)
          (nx, ny) :: List.rev (List.tl (List.rev state.snake))
      in
      let new_food =
        if ate_food then random_food ~cols:state.cols ~rows:state.rows new_snake
        else state.food
      in
      let new_score = if ate_food then state.score + 10 else state.score in
      { state with snake = new_snake; food = new_food; score = new_score }

let draw_border grid ~cols ~rows =
  let border_style =
    Ansi.Style.make ~fg:Ansi.Color.bright_blue ~bg:Ansi.Color.blue ()
  in
  (* Top and bottom *)
  for x = 0 to cols - 1 do
    Grid.draw_text ~style:border_style grid ~x ~y:1 ~text:" ";
    Grid.draw_text ~style:border_style grid ~x ~y:(rows - 2) ~text:" "
  done;
  (* Left and right *)
  for y = 1 to rows - 2 do
    Grid.draw_text ~style:border_style grid ~x:0 ~y ~text:" ";
    Grid.draw_text ~style:border_style grid ~x:(cols - 1) ~y ~text:" "
  done

let draw_snake grid snake =
  let head_style =
    Ansi.Style.make ~fg:Ansi.Color.bright_green ~bg:Ansi.Color.green ()
  in
  let body_style =
    Ansi.Style.make ~fg:Ansi.Color.green ~bg:Ansi.Color.bright_green ()
  in
  List.iteri
    (fun i (x, y) ->
      let style = if i = 0 then head_style else body_style in
      Grid.draw_text ~style grid ~x ~y ~text:" ")
    snake

let draw_food grid (fx, fy) =
  let food_style =
    Ansi.Style.make ~fg:Ansi.Color.bright_red ~bg:Ansi.Color.red ()
  in
  Grid.draw_text ~style:food_style grid ~x:fx ~y:fy ~text:" "

let draw_ui grid ~cols state =
  let title_style = Ansi.Style.make ~fg:Ansi.Color.bright_cyan ~bold:true () in
  let score_style = Ansi.Style.make ~fg:Ansi.Color.bright_yellow () in
  let help_style = Ansi.Style.make ~fg:Ansi.Color.bright_black () in
  Grid.draw_text ~style:title_style grid ~x:2 ~y:0 ~text:"SNAKE";
  let score_text = Printf.sprintf "Score: %d" state.score in
  Grid.draw_text ~style:score_style grid ~x:10 ~y:0 ~text:score_text;
  let help = "Arrow keys to move | P: pause | R: restart | Esc: quit" in
  let help_x = cols - String.length help - 2 in
  if help_x > 20 then
    Grid.draw_text ~style:help_style grid ~x:help_x ~y:0 ~text:help

let draw_game_over grid ~cols ~rows =
  let msg = "GAME OVER - Press R to restart" in
  let style =
    Ansi.Style.make ~fg:Ansi.Color.bright_white ~bg:Ansi.Color.red ~bold:true ()
  in
  let x = (cols - String.length msg) / 2 in
  let y = rows / 2 in
  Grid.draw_text ~style grid ~x ~y ~text:msg

let draw_paused grid ~cols ~rows =
  let msg = "PAUSED - Press P to continue" in
  let style =
    Ansi.Style.make ~fg:Ansi.Color.black ~bg:Ansi.Color.bright_yellow ~bold:true
      ()
  in
  let x = (cols - String.length msg) / 2 in
  let y = rows / 2 in
  Grid.draw_text ~style grid ~x ~y ~text:msg

(* The snake moves every tenth of a second, on a timer. *)
let tick = 100_000_000

let turn state direction =
  if opposite_dir state.direction direction then state
  else { state with direction }

let () =
  Random.self_init ();
  let config = Matrix.config ~mouse:`Off () in
  Matrix.run ~config @@ fun app ->
  let rec loop state timer =
    match Matrix.next app with
    | Stopped | Event (Key { key = Escape; _ }) -> ()
    | Event (Key k) when Input.Keystroke.is_interrupt k ->
        (* Raw mode makes Ctrl+C a key: this program quits on it. *)
        ()
    | Timer t when t = timer ->
        Matrix.request_redraw app;
        loop (move_snake state) (Matrix.at app (Matrix.now app + tick))
    | Event (Key { key = (Up | Down | Left | Right) as key; _ }) ->
        let direction =
          match key with Up -> Up | Down -> Down | Left -> Left | _ -> Right
        in
        loop (turn state direction) timer
    | Event (Key { key = Char u; _ }) -> (
        Matrix.request_redraw app;
        match Uchar.to_int u with
        | c when c = Char.code 'p' || c = Char.code 'P' ->
            loop { state with paused = not state.paused } timer
        | c when c = Char.code 'r' || c = Char.code 'R' ->
            let cols, rows = Matrix.size app in
            loop (initial_state ~cols ~rows) timer
        | _ -> loop state timer)
    | Event (Resize (cols, rows)) -> loop (initial_state ~cols ~rows) timer
    | Frame ->
        Matrix.draw app (fun grid ->
            let cols, rows = Matrix.size app in
            Grid.clear ~color:Ansi.Color.black grid;
            draw_border grid ~cols ~rows;
            draw_food grid state.food;
            draw_snake grid state.snake;
            draw_ui grid ~cols state;
            if state.game_over then draw_game_over grid ~cols ~rows
            else if state.paused then draw_paused grid ~cols ~rows);
        loop state timer
    | Event _ | Timer _ | Posted _ -> loop state timer
  in
  let cols, rows = Matrix.size app in
  loop (initial_state ~cols ~rows) (Matrix.at app (Matrix.now app + tick))
