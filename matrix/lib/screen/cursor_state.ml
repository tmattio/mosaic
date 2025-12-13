module Esc = Ansi.Escape

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
  (* Applied State (Snapshot of last emit)
     None = unknown terminal state, forces re-emission *)
  mutable last_visible : bool option;
  mutable last_style : style option;
  mutable last_blink : bool option;
  mutable last_color : (int * int * int) option;
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

let reset t =
  (* Mark all applied state as unknown to force full re-emission *)
  t.last_visible <- None;
  t.last_style <- None;
  t.last_blink <- None;
  t.last_color <- None

let set_position t ~row ~col =
  t.row <- max 1 row;
  t.col <- max 1 col;
  t.has_pos <- true

let clear_position t = t.has_pos <- false

let set_style t ~style ~blinking =
  t.style <- style;
  t.blinking <- blinking

let set_color t c = t.color <- c
let set_visible t v = t.visible <- v
let is_visible t = t.visible

let clamp_to_bounds t ~max_row ~max_col =
  if t.has_pos then (
    t.row <- max 1 (min max_row t.row);
    t.col <- max 1 (min max_col t.col))

let cursor_style_seq style blinking =
  match (style, blinking) with
  | `Block, true -> Esc.cursor_block_blink
  | `Block, false -> Esc.cursor_block
  | `Line, true -> Esc.cursor_line_blink
  | `Line, false -> Esc.cursor_line
  | `Underline, true -> Esc.cursor_underline_blink
  | `Underline, false -> Esc.cursor_underline

let hide_temporarily t w =
  (* Hide cursor if it's visible or in unknown state.
     When state is unknown, we hide to be safe during render. *)
  match t.last_visible with
  | Some false -> () (* Already hidden, nothing to do *)
  | Some true | None ->
      Esc.emit Esc.hide_cursor w;
      t.last_visible <- Some false

let emit t ~row_offset w =
  (* 1. Visibility Check *)
  if not t.visible then (
    (* Hide cursor if visible or unknown *)
    match t.last_visible with
    | Some false -> () (* Already hidden *)
    | Some true | None ->
        Esc.emit Esc.hide_cursor w;
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
       Esc.cursor_position ~row:r ~col:c w);

    (* 3. Style Changes *)
    let style_changed =
      match (t.last_style, t.last_blink) with
      | Some s, Some b when s = t.style && b = t.blinking -> false
      | _ -> true
    in
    if style_changed then (
      Esc.emit (cursor_style_seq t.style t.blinking) w;
      t.last_style <- Some t.style;
      t.last_blink <- Some t.blinking);

    (* 4. Color Changes *)
    if t.color <> t.last_color then (
      (match t.color with
      | Some (r, g, b) -> Esc.emit (Esc.cursor_color ~r ~g ~b) w
      | None ->
          Esc.emit Esc.reset_cursor_color w;
          Esc.emit Esc.reset_cursor_color_fallback w);
      t.last_color <- t.color);

    (* 5. Ensure Visible (if it was hidden by hide_temporarily or unknown) *)
    match t.last_visible with
    | Some true -> () (* Already visible *)
    | Some false | None ->
        Esc.emit Esc.show_cursor w;
        t.last_visible <- Some true)
