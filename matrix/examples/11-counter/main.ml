open Matrix

(* Counter: simple inline TUI demonstrating Primary_inline mode *)

type state = { counter : int; items : string list }

let initial_state = { counter = 0; items = [ "Apple"; "Banana"; "Cherry" ] }

let draw_box grid ~x ~y ~width ~height ~title =
  let border_style = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:12) () in
  let title_style = Ansi.Style.make ~fg:Ansi.Color.bright_cyan ~bold:true () in
  (* Top border *)
  Grid.draw_text ~style:border_style grid ~x ~y ~text:"┌";
  for i = 1 to width - 2 do
    Grid.draw_text ~style:border_style grid ~x:(x + i) ~y ~text:"─"
  done;
  Grid.draw_text ~style:border_style grid ~x:(x + width - 1) ~y ~text:"┐";
  (* Title *)
  if String.length title > 0 then (
    let title_x = x + 2 in
    Grid.draw_text ~style:border_style grid ~x:title_x ~y ~text:" ";
    Grid.draw_text ~style:title_style grid ~x:(title_x + 1) ~y ~text:title;
    Grid.draw_text ~style:border_style grid
      ~x:(title_x + 1 + String.length title)
      ~y ~text:" ");
  (* Sides *)
  for row = 1 to height - 2 do
    Grid.draw_text ~style:border_style grid ~x ~y:(y + row) ~text:"│";
    Grid.draw_text ~style:border_style grid
      ~x:(x + width - 1)
      ~y:(y + row) ~text:"│"
  done;
  (* Bottom border *)
  Grid.draw_text ~style:border_style grid ~x ~y:(y + height - 1) ~text:"└";
  for i = 1 to width - 2 do
    Grid.draw_text ~style:border_style grid ~x:(x + i)
      ~y:(y + height - 1)
      ~text:"─"
  done;
  Grid.draw_text ~style:border_style grid
    ~x:(x + width - 1)
    ~y:(y + height - 1)
    ~text:"┘"

let render grid state =
  let cols = Grid.width grid in
  let label_style = Ansi.Style.make ~fg:Ansi.Color.white () in
  let value_style =
    Ansi.Style.make ~fg:Ansi.Color.bright_yellow ~bold:true ()
  in
  let help_style = Ansi.Style.make ~fg:Ansi.Color.bright_black () in
  let item_style = Ansi.Style.make ~fg:Ansi.Color.bright_green () in
  (* Counter display *)
  Grid.draw_text ~style:label_style grid ~x:2 ~y:0 ~text:"Counter: ";
  Grid.draw_text ~style:value_style grid ~x:11 ~y:0
    ~text:(string_of_int state.counter);
  (* Help text *)
  Grid.draw_text ~style:help_style grid ~x:2 ~y:1
    ~text:"+/- to change | r to reset | a to add item | q to quit";
  (* Items box *)
  let box_width = min 40 (cols - 4) in
  let box_height = List.length state.items + 2 in
  draw_box grid ~x:2 ~y:3 ~width:box_width ~height:box_height ~title:"Items";
  List.iteri
    (fun i item ->
      Grid.draw_text ~style:item_style grid ~x:4 ~y:(4 + i)
        ~text:(Printf.sprintf "• %s" item))
    state.items

let () =
  Random.self_init ();
  let app =
    Matrix.create ~mode:`Primary ~target_fps:(Some 30.) ~mouse_enabled:false
      ~debug_overlay:false ()
  in
  let state = ref initial_state in
  Matrix.run app
    ~on_input:(fun app event ->
      match event with
      | Key { key = Escape; _ } -> Matrix.stop app
      | Key { key = Char u; _ } -> (
          match Uchar.to_int u with
          | c when c = Char.code 'q' || c = Char.code 'Q' -> Matrix.stop app
          | c when c = Char.code '+' || c = Char.code '=' ->
              state := { !state with counter = !state.counter + 1 }
          | c when c = Char.code '-' ->
              state := { !state with counter = !state.counter - 1 }
          | c when c = Char.code 'r' || c = Char.code 'R' ->
              state := initial_state
          | c when c = Char.code 'a' || c = Char.code 'A' ->
              let new_item = Printf.sprintf "Item %d" (Random.int 1000) in
              state := { !state with items = new_item :: !state.items }
          | _ -> ())
      | _ -> ())
    ~primary_required_rows:(fun _ ->
      (* Counter at y=0, help at y=1, and from y=3 a box of items_len + 2
         rows. *)
      Some (5 + List.length !state.items))
    ~on_render:(fun app -> render (Matrix.grid app) !state)
