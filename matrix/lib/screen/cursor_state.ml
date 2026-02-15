type style = [ `Block | `Line | `Underline ]

type t = {
  (* Desired State *)
  mutable row : int;
  mutable col : int;
  mutable has_pos : bool;
  mutable style : style;
  mutable blinking : bool;
  mutable color : (int * int * int) option;
  mutable visible : bool;
  (* Applied State (Snapshot of last emit) None = unknown terminal state, forces
     re-emission *)
  mutable last_visible : bool option;
  mutable last_style : style option;
  mutable last_blink : bool option;
  mutable last_color : (int * int * int) option;
}

type snapshot = {
  row : int;
  col : int;
  has_position : bool;
  style : style;
  blinking : bool;
  color : (int * int * int) option;
  visible : bool;
}

let create () =
  {
    row = 1;
    col = 1;
    has_pos = false;
    style = `Block;
    blinking = true;
    color = None;
    visible = true;
    (* Initialize all applied state as unknown to force emission on first use *)
    last_visible = None;
    last_style = None;
    last_blink = None;
    last_color = None;
  }

let reset (t : t) =
  (* Mark all applied state as unknown to force full re-emission *)
  t.last_visible <- None;
  t.last_style <- None;
  t.last_blink <- None;
  t.last_color <- None

let snapshot (t : t) =
  {
    row = t.row;
    col = t.col;
    has_position = t.has_pos;
    style = t.style;
    blinking = t.blinking;
    color = t.color;
    visible = t.visible;
  }

let set_position (t : t) ~row ~col =
  t.row <- max 1 row;
  t.col <- max 1 col;
  t.has_pos <- true

let clear_position (t : t) = t.has_pos <- false

let set_style (t : t) ~style ~blinking =
  t.style <- style;
  t.blinking <- blinking

let set_color (t : t) c = t.color <- c
let set_visible (t : t) v = t.visible <- v
let is_visible (t : t) = t.visible

let clamp_to_bounds (t : t) ~max_row ~max_col =
  if t.has_pos then (
    t.row <- max 1 (min max_row t.row);
    t.col <- max 1 (min max_col t.col))

let cursor_style_seq style blinking =
  match (style, blinking) with
  | `Block, true -> Ansi.cursor_style ~shape:`Blinking_block
  | `Block, false -> Ansi.cursor_style ~shape:`Block
  | `Line, true -> Ansi.cursor_style ~shape:`Blinking_bar
  | `Line, false -> Ansi.cursor_style ~shape:`Bar
  | `Underline, true -> Ansi.cursor_style ~shape:`Blinking_underline
  | `Underline, false -> Ansi.cursor_style ~shape:`Underline

let hide_temporarily (t : t) w =
  (* Hide cursor if it's visible or in unknown state. When state is unknown, we
     hide to be safe during render. *)
  match t.last_visible with
  | Some false -> () (* Already hidden, nothing to do *)
  | Some true | None ->
      Ansi.emit Ansi.(disable Cursor_visible) w;
      t.last_visible <- Some false

let emit (t : t) ~row_offset w =
  (* 1. Visibility Check *)
  if not t.visible then (
    (* Hide cursor if visible or unknown *)
    match t.last_visible with
    | Some false -> () (* Already hidden *)
    | Some true | None ->
        Ansi.emit Ansi.(disable Cursor_visible) w;
        t.last_visible <- Some false;
        (* Clear attributes so they re-apply if we show cursor again *)
        t.last_style <- None;
        t.last_blink <- None;
        t.last_color <- None)
  else (
    (* 2. Positioning *)
    (if t.has_pos then
       let r = max 1 (t.row + row_offset) in
       let c = max 1 t.col in
       Ansi.cursor_position ~row:r ~col:c w);

    (* 3. Style Changes *)
    let style_changed =
      match (t.last_style, t.last_blink) with
      | Some s, Some b when s = t.style && b = t.blinking -> false
      | _ -> true
    in
    if style_changed then (
      Ansi.emit (cursor_style_seq t.style t.blinking) w;
      t.last_style <- Some t.style;
      t.last_blink <- Some t.blinking);

    (* 4. Color Changes *)
    if t.color <> t.last_color then (
      (match t.color with
      | Some (r, g, b) -> Ansi.emit (Ansi.cursor_color ~r ~g ~b) w
      | None ->
          Ansi.emit Ansi.reset_cursor_color w;
          Ansi.emit Ansi.reset_cursor_color_fallback w);
      t.last_color <- t.color);

    (* 5. Ensure Visible (if it was hidden by hide_temporarily or unknown) *)
    match t.last_visible with
    | Some true -> () (* Already visible *)
    | Some false | None ->
        Ansi.emit Ansi.(enable Cursor_visible) w;
        t.last_visible <- Some true)
