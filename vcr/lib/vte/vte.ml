open Ansi

type style = {
  bold : bool;
  faint : bool; (* Corresponds to Ansi.style.`Dim` *)
  italic : bool;
  underline : bool;
  double_underline : bool;
  fg : Ansi.color;
  bg : Ansi.color;
  reversed : bool;
  link : string option;
  strikethrough : bool;
  overline : bool;
  blink : bool;
}

type cell = { char : Uchar.t; style : style }
type cursor = { mutable row : int; mutable col : int; mutable visible : bool }

let default_style =
  {
    bold = false;
    faint = false;
    italic = false;
    underline = false;
    double_underline = false;
    fg = Default;
    bg = Default;
    reversed = false;
    link = None;
    strikethrough = false;
    overline = false;
    blink = false;
  }

let empty_cell = None

(* The `grid` and `cursor` fields are changed to be non-mutable.
   Their *contents* are mutable, but the record fields themselves are never
   re-assigned, which is what the original warning was about. *)
type t = {
  grid : cell option array array;
  scrollback : cell option array Ring_buffer.t;
  scrollback_size : int;
  cursor : cursor;
  mutable gfx_state : style;
  parser : Parser.t;
  mutable title : string;
  mutable dirty : bool;  (** Set when terminal state changes *)
  (* Alternate screen buffer support *)
  alt_grid : cell option array array;
  mutable in_alternate : bool;
  mutable scroll_top : int;  (** Top of scroll region (0-based) *)
  mutable scroll_bottom : int;  (** Bottom of scroll region (0-based) *)
  mutable cursor_saved : (int * int * bool) option;  (** Saved cursor state *)
  mutable gfx_state_saved : style option;  (** Saved graphics state *)
  mutable origin_mode : bool;  (** DECOM - Origin mode *)
}

let rows (t : t) = Array.length t.grid
let cols (t : t) = if rows t = 0 then 0 else Array.length t.grid.(0)

let create ?(scrollback = 1000) ~rows ~cols () =
  let grid = Array.make_matrix rows cols empty_cell in
  let alt_grid = Array.make_matrix rows cols empty_cell in
  let empty_row = Array.make cols empty_cell in
  {
    grid;
    scrollback = Ring_buffer.create scrollback empty_row;
    scrollback_size = scrollback;
    cursor = { row = 0; col = 0; visible = true };
    gfx_state = default_style;
    parser = Parser.create ();
    title = "";
    dirty = false;
    alt_grid;
    in_alternate = false;
    scroll_top = 0;
    scroll_bottom = rows - 1;
    cursor_saved = None;
    gfx_state_saved = None;
    origin_mode = false;
  }

let clamp ~min ~max v = if v < min then min else if v > max then max else v

(* Dirty flag management *)
let is_dirty t = t.dirty
let clear_dirty t = t.dirty <- false
let mark_dirty t = t.dirty <- true

let set_cursor t ~row ~col =
  let row = clamp ~min:0 ~max:(rows t - 1) row in
  let col = clamp ~min:0 ~max:(cols t - 1) col in
  if t.cursor.row <> row || t.cursor.col <> col then (
    t.cursor.row <- row;
    t.cursor.col <- col;
    mark_dirty t)

let set_cursor_visible (t : t) visible =
  if t.cursor.visible <> visible then (
    t.cursor.visible <- visible;
    mark_dirty t)

let clear_line t row from_col =
  for i = from_col to cols t - 1 do
    t.grid.(row).(i) <- empty_cell
  done;
  if from_col < cols t then mark_dirty t

let clear_screen t =
  for r = 0 to rows t - 1 do
    for c = 0 to cols t - 1 do
      t.grid.(r).(c) <- empty_cell
    done
  done;
  mark_dirty t

(* Alternate screen buffer support *)
let switch_to_alternate t save_cursor =
  if not t.in_alternate then (
    (* Save cursor and graphics state if requested *)
    if save_cursor then (
      t.cursor_saved <- Some (t.cursor.row, t.cursor.col, t.cursor.visible);
      t.gfx_state_saved <- Some t.gfx_state);

    (* Swap grids *)
    for r = 0 to rows t - 1 do
      for c = 0 to cols t - 1 do
        let temp = t.grid.(r).(c) in
        t.grid.(r).(c) <- t.alt_grid.(r).(c);
        t.alt_grid.(r).(c) <- temp
      done
    done;

    (* Clear the alternate screen *)
    clear_screen t;

    (* Reset scroll region to full screen *)
    t.scroll_top <- 0;
    t.scroll_bottom <- rows t - 1;

    t.in_alternate <- true;
    mark_dirty t)

let switch_to_main t restore_cursor =
  if t.in_alternate then (
    (* Swap grids back *)
    for r = 0 to rows t - 1 do
      for c = 0 to cols t - 1 do
        let temp = t.grid.(r).(c) in
        t.grid.(r).(c) <- t.alt_grid.(r).(c);
        t.alt_grid.(r).(c) <- temp
      done
    done;

    (* Restore cursor and graphics state if saved *)
    (if restore_cursor then
       match t.cursor_saved with
       | Some (row, col, visible) ->
           t.cursor.row <- row;
           t.cursor.col <- col;
           t.cursor.visible <- visible;
           t.cursor_saved <- None
       | None -> (
           ();

           match t.gfx_state_saved with
           | Some saved_state ->
               t.gfx_state <- saved_state;
               t.gfx_state_saved <- None
           | None -> ()));

    t.in_alternate <- false;
    mark_dirty t)

let scroll_up (vte : t) n =
  if n > 0 then (
    let region_height = vte.scroll_bottom - vte.scroll_top + 1 in
    let actual_n = min n region_height in

    (* Add lines to scrollback only if scrolling from top of screen in main buffer *)
    if (not vte.in_alternate) && vte.scroll_top = 0 && vte.scrollback_size > 0
    then
      for i = 0 to actual_n - 1 do
        if vte.scroll_top + i < Array.length vte.grid then
          Ring_buffer.push vte.scrollback
            (Array.copy vte.grid.(vte.scroll_top + i))
      done;

    (* Shift lines up within scroll region *)
    for r = vte.scroll_top to vte.scroll_bottom - actual_n do
      vte.grid.(r) <- vte.grid.(r + actual_n)
    done;

    (* Clear bottom lines of scroll region *)
    for r = vte.scroll_bottom - actual_n + 1 to vte.scroll_bottom do
      vte.grid.(r) <- Array.make (cols vte) empty_cell
    done;

    mark_dirty vte)

let scroll_down (t : t) n =
  if n > 0 then (
    let region_height = t.scroll_bottom - t.scroll_top + 1 in
    let actual_n = min n region_height in

    (* Shift lines down within scroll region *)
    for r = t.scroll_bottom downto t.scroll_top + actual_n do
      t.grid.(r) <- t.grid.(r - actual_n)
    done;

    (* Clear top lines of scroll region *)
    for r = t.scroll_top to t.scroll_top + actual_n - 1 do
      t.grid.(r) <- Array.make (cols t) empty_cell
    done;

    mark_dirty t)

let advance_cursor (t : t) =
  let { row; col; _ } = t.cursor in
  if col + 1 >= cols t then
    (* Check if we're at the bottom of the scroll region *)
    if row >= t.scroll_bottom then (
      scroll_up t 1;
      set_cursor t ~row:t.scroll_bottom ~col:0)
    else set_cursor t ~row:(row + 1) ~col:0
  else set_cursor t ~row ~col:(col + 1)

let put_char (t : t) c =
  let { row; col; _ } = t.cursor in
  if row < rows t && col < cols t then (
    t.grid.(row).(col) <- Some { char = c; style = t.gfx_state };
    mark_dirty t);
  advance_cursor t

let apply_sgr_attr state (attr : Ansi.attr) =
  match attr with
  | `Reset -> default_style
  | `Bold -> { state with bold = true }
  | `Dim -> { state with faint = true }
  | `Italic -> { state with italic = true }
  | `Underline -> { state with underline = true }
  | `Double_underline -> { state with double_underline = true }
  | `Reverse -> { state with reversed = true }
  | `Fg fg -> { state with fg }
  | `Bg bg -> { state with bg }
  | `Blink -> { state with blink = true }
  | `Strikethrough -> { state with strikethrough = true }
  | `Overline -> { state with overline = true }
  | `Conceal | `Framed | `Encircled -> state (* Not visually implemented *)

let handle_control t (ctrl : Parser.control) =
  match ctrl with
  | CUU n -> set_cursor t ~row:(t.cursor.row - n) ~col:t.cursor.col
  | CUD n -> set_cursor t ~row:(t.cursor.row + n) ~col:t.cursor.col
  | CUF n -> set_cursor t ~row:t.cursor.row ~col:(t.cursor.col + n)
  | CUB n -> set_cursor t ~row:t.cursor.row ~col:(t.cursor.col - n)
  | CNL n -> set_cursor t ~row:(t.cursor.row + n) ~col:0
  | CPL n -> set_cursor t ~row:(t.cursor.row - n) ~col:0
  | CHA n -> set_cursor t ~row:t.cursor.row ~col:(n - 1)
  | VPA n -> set_cursor t ~row:(n - 1) ~col:t.cursor.col
  | CUP (r, c) -> set_cursor t ~row:(r - 1) ~col:(c - 1)
  | ED n -> (
      match n with
      | 0 ->
          clear_line t t.cursor.row t.cursor.col;
          for r = t.cursor.row + 1 to rows t - 1 do
            clear_line t r 0
          done
      | 1 ->
          for r = 0 to t.cursor.row - 1 do
            clear_line t r 0
          done;
          for c = 0 to t.cursor.col do
            t.grid.(t.cursor.row).(c) <- empty_cell
          done;
          if t.cursor.col >= 0 then mark_dirty t
      | 2 | 3 ->
          clear_screen t;
          set_cursor t ~row:0 ~col:0
      | _ -> ())
  | EL n -> (
      match n with
      | 0 -> clear_line t t.cursor.row t.cursor.col
      | 1 ->
          for c = 0 to t.cursor.col do
            t.grid.(t.cursor.row).(c) <- empty_cell
          done;
          if t.cursor.col >= 0 then mark_dirty t
      | 2 -> clear_line t t.cursor.row 0
      | _ -> ())
  | OSC (0, title) | OSC (2, title) ->
      t.title <- title;
      mark_dirty t
  | Hyperlink (Some (_, uri)) ->
      t.gfx_state <- { t.gfx_state with link = Some uri };
      mark_dirty t
  | Hyperlink None ->
      t.gfx_state <- { t.gfx_state with link = None };
      mark_dirty t
  | Reset ->
      t.gfx_state <- default_style;
      clear_screen t;
      set_cursor t ~row:0 ~col:0;
      mark_dirty t
  | Unknown csi -> (
      (* Handle DECSET/DECRST and other unrecognized sequences *)
      match csi with
      | s when String.starts_with ~prefix:"CSI[?1049h" s ->
          (* Enter alternate screen with cursor save *)
          switch_to_alternate t true
      | s when String.starts_with ~prefix:"CSI[?1049l" s ->
          (* Exit alternate screen with cursor restore *)
          switch_to_main t true
      | s
        when String.starts_with ~prefix:"CSI[?47h" s
             || String.starts_with ~prefix:"CSI[?1047h" s ->
          (* Enter alternate screen *)
          switch_to_alternate t (String.starts_with ~prefix:"CSI[?1047h" s)
      | s
        when String.starts_with ~prefix:"CSI[?47l" s
             || String.starts_with ~prefix:"CSI[?1047l" s ->
          (* Exit alternate screen *)
          switch_to_main t (String.starts_with ~prefix:"CSI[?1047l" s)
      | s when String.starts_with ~prefix:"CSI[?25h" s ->
          (* DECTCEM - Show cursor *)
          set_cursor_visible t true
      | s when String.starts_with ~prefix:"CSI[?25l" s ->
          (* DECTCEM - Hide cursor *)
          set_cursor_visible t false
      | s when String.starts_with ~prefix:"CSI[?6h" s ->
          (* DECOM - Set origin mode *)
          t.origin_mode <- true;
          set_cursor t ~row:t.scroll_top ~col:0
      | s when String.starts_with ~prefix:"CSI[?6l" s ->
          (* DECOM - Reset origin mode *)
          t.origin_mode <- false;
          set_cursor t ~row:0 ~col:0
      | s when String.starts_with ~prefix:"CSI[s" s ->
          (* DECSC - Save cursor *)
          t.cursor_saved <- Some (t.cursor.row, t.cursor.col, t.cursor.visible);
          t.gfx_state_saved <- Some t.gfx_state
      | s when String.starts_with ~prefix:"CSI[u" s -> (
          (* DECRC - Restore cursor *)
          (match t.cursor_saved with
          | Some (row, col, visible) ->
              t.cursor.row <- row;
              t.cursor.col <- col;
              t.cursor.visible <- visible;
              t.cursor_saved <- None
          | None -> ());
          match t.gfx_state_saved with
          | Some saved_state ->
              t.gfx_state <- saved_state;
              t.gfx_state_saved <- None
          | None -> ())
      | s -> (
          (* Try to parse DECSTBM (set scroll region) *)
          let re = Str.regexp "CSI\\[\\([0-9]*\\);\\([0-9]*\\)r" in
          try
            if Str.string_match re s 0 then (
              let top =
                try int_of_string (Str.matched_group 1 s) with _ -> 1
              in
              let bottom =
                try int_of_string (Str.matched_group 2 s) with _ -> rows t
              in
              t.scroll_top <- max 0 (top - 1);
              t.scroll_bottom <- min (rows t - 1) (bottom - 1);
              (* DECSTBM homes the cursor *)
              set_cursor t
                ~row:(if t.origin_mode then t.scroll_top else 0)
                ~col:0)
          with _ -> ()))
  | OSC (_, _) -> ()

let handle_text t text =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String text) in
  let rec decode_loop () =
    match Uutf.decode decoder with
    | `Uchar u ->
        (match Uchar.to_int u with
        | 0x0a ->
            if t.cursor.row >= t.scroll_bottom then scroll_up t 1
            else (
              t.cursor.row <- t.cursor.row + 1;
              mark_dirty t);
            t.cursor.col <- 0;
            mark_dirty t
        | 0x0d -> set_cursor t ~row:t.cursor.row ~col:0
        | 0x08 -> set_cursor t ~row:t.cursor.row ~col:(t.cursor.col - 1)
        | 0x09 ->
            let next_tab = ((t.cursor.col / 8) + 1) * 8 in
            if next_tab < cols t then
              set_cursor t ~row:t.cursor.row ~col:next_tab
            else set_cursor t ~row:t.cursor.row ~col:(cols t - 1)
        | _ -> put_char t u);
        decode_loop ()
    | `Malformed _ -> decode_loop ()
    | `Await -> assert false
    | `End -> ()
  in
  decode_loop ()

let handle_token t = function
  | Parser.Text s -> handle_text t s
  | Parser.SGR attrs ->
      let new_state = List.fold_left apply_sgr_attr t.gfx_state attrs in
      if new_state <> t.gfx_state then (
        t.gfx_state <- new_state;
        mark_dirty t)
  | Parser.Control ctrl -> handle_control t ctrl

let feed t bytes ofs len =
  let tokens = Parser.feed t.parser bytes ofs len in
  List.iter (handle_token t) tokens

let to_string_grid t =
  let buffer = Buffer.create (rows t * (cols t + 1)) in
  for r = 0 to rows t - 1 do
    for c = 0 to cols t - 1 do
      match t.grid.(r).(c) with
      | None -> Buffer.add_char buffer ' '
      | Some cell -> Buffer.add_utf_8_uchar buffer cell.char
    done;
    if r < rows t - 1 then Buffer.add_char buffer '\n'
  done;
  let lines = String.split_on_char '\n' (Buffer.contents buffer) in
  let trim_right s =
    let len = String.length s in
    let rec find_end i =
      if i < 0 || s.[i] <> ' ' then i + 1 else find_end (i - 1)
    in
    let end_pos = find_end (len - 1) in
    if end_pos = len then s else String.sub s 0 end_pos
  in
  String.concat "\n" (List.map trim_right lines)

let cursor_pos (t : t) = (t.cursor.row, t.cursor.col)
let is_cursor_visible (t : t) = t.cursor.visible
let title (t : t) = t.title

let reset t =
  (* Exit alternate screen if active *)
  if t.in_alternate then switch_to_main t false;
  clear_screen t;
  set_cursor t ~row:0 ~col:0;
  t.gfx_state <- default_style;
  Parser.reset t.parser;
  t.title <- "";
  Ring_buffer.clear t.scrollback;
  t.scroll_top <- 0;
  t.scroll_bottom <- rows t - 1;
  t.origin_mode <- false;
  mark_dirty t

let get_cell t ~row ~col =
  if row >= 0 && row < rows t && col >= 0 && col < cols t then
    t.grid.(row).(col)
  else None
