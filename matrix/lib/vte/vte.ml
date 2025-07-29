open Ansi

type cursor = { mutable row : int; mutable col : int; mutable visible : bool }

(* The `grid` and `cursor` fields are changed to be non-mutable.
   Their *contents* are mutable, but the record fields themselves are never
   re-assigned, which is what the original warning was about. *)
type t = {
  grid : Grid.t;
  scrollback : Grid.Cell.t option array Ring_buffer.t;
  scrollback_size : int;
  cursor : cursor;
  mutable gfx_state : Grid.Cell.style;
  parser : Parser.t;
  mutable title : string;
  mutable dirty : bool;  (** Set when terminal state changes *)
  (* Alternate screen buffer support *)
  alt_grid : Grid.t;
  mutable in_alternate : bool;
  mutable scroll_top : int;  (** Top of scroll region (0-based) *)
  mutable scroll_bottom : int;  (** Bottom of scroll region (0-based) *)
  mutable cursor_saved : (int * int * bool) option;  (** Saved cursor state *)
  mutable gfx_state_saved : Grid.Cell.style option;  (** Saved graphics state *)
  mutable origin_mode : bool;  (** DECOM - Origin mode *)
  mutable auto_wrap_mode : bool;  (** DECAWM - Auto wrap mode *)
  mutable cursor_key_mode : bool;  (** DECCKM - Cursor key application mode *)
  mutable insert_mode : bool;  (** IRM - Insert/Replace mode *)
  mutable auto_newline_mode : bool;  (** LNM - Automatic newline *)
}

let rows (t : t) = Grid.rows t.grid
let cols (t : t) = Grid.cols t.grid

let create ?(scrollback = 1000) ~rows ~cols () =
  let grid = Grid.create ~rows ~cols in
  let alt_grid = Grid.create ~rows ~cols in
  let empty_row = Grid.make_empty_row ~cols in
  {
    grid;
    scrollback = Ring_buffer.create scrollback empty_row;
    scrollback_size = scrollback;
    cursor = { row = 0; col = 0; visible = true };
    gfx_state = Grid.Cell.default_style;
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
    auto_wrap_mode = true;
    cursor_key_mode = false;
    insert_mode = false;
    auto_newline_mode = false;
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
  Grid.clear_line t.grid row from_col;
  if from_col < cols t then mark_dirty t

let clear_screen t =
  Grid.clear t.grid;
  mark_dirty t

(* Alternate screen buffer support *)
let switch_to_alternate t save_cursor =
  if not t.in_alternate then (
    (* Save cursor and graphics state if requested *)
    if save_cursor then (
      t.cursor_saved <- Some (t.cursor.row, t.cursor.col, t.cursor.visible);
      t.gfx_state_saved <- Some t.gfx_state);

    (* Swap grids *)
    Grid.swap (t.grid, t.alt_grid);

    (* Clear the alternate screen *)
    clear_screen t;
    
    (* Reset cursor to top-left *)
    set_cursor t ~row:0 ~col:0;

    (* Reset scroll region to full screen *)
    t.scroll_top <- 0;
    t.scroll_bottom <- rows t - 1;

    t.in_alternate <- true;
    mark_dirty t)

let switch_to_main t restore_cursor =
  if t.in_alternate then (
    (* Swap grids back *)
    Grid.swap (t.grid, t.alt_grid);

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
        if vte.scroll_top + i < rows vte then
          Ring_buffer.push vte.scrollback
            (Grid.copy_row vte.grid (vte.scroll_top + i))
      done;

    (* Shift lines up within scroll region *)
    for r = vte.scroll_top to vte.scroll_bottom - actual_n do
      let row_to_copy = Grid.copy_row vte.grid (r + actual_n) in
      Grid.set_row vte.grid r row_to_copy
    done;

    (* Clear bottom lines of scroll region *)
    for r = vte.scroll_bottom - actual_n + 1 to vte.scroll_bottom do
      Grid.set_row vte.grid r (Grid.make_empty_row ~cols:(cols vte))
    done;

    mark_dirty vte)

let scroll_down (t : t) n =
  if n > 0 then (
    let region_height = t.scroll_bottom - t.scroll_top + 1 in
    let actual_n = min n region_height in

    (* Shift lines down within scroll region *)
    for r = t.scroll_bottom downto t.scroll_top + actual_n do
      let row_to_copy = Grid.copy_row t.grid (r - actual_n) in
      Grid.set_row t.grid r row_to_copy
    done;

    (* Clear top lines of scroll region *)
    for r = t.scroll_top to t.scroll_top + actual_n - 1 do
      Grid.set_row t.grid r (Grid.make_empty_row ~cols:(cols t))
    done;

    mark_dirty t)

let advance_cursor (t : t) =
  let { row; col; _ } = t.cursor in
  if col + 1 >= cols t then (
    (* Check if we're at the bottom of the scroll region *)
    if t.auto_wrap_mode then (
      if row >= t.scroll_bottom then (
        scroll_up t 1;
        set_cursor t ~row:t.scroll_bottom ~col:0)
      else set_cursor t ~row:(row + 1) ~col:0)
    (* else stay at the last column when auto_wrap is disabled *)
  ) else 
    set_cursor t ~row ~col:(col + 1)

let put_char (t : t) c =
  let { row; col; _ } = t.cursor in

  (* Calculate character width first *)
  let code = Uchar.to_int c in
  let width =
    if
      (code >= 0x1100 && code <= 0x115F)
      (* Hangul Jamo *)
      || (code >= 0x2E80 && code <= 0x9FFF)
      (* CJK *)
      || (code >= 0xAC00 && code <= 0xD7A3)
      (* Hangul Syllables *)
      || (code >= 0xF900 && code <= 0xFAFF)
      (* CJK Compatibility *)
      || (code >= 0xFE30 && code <= 0xFE6F)
      (* CJK Compatibility Forms *)
      || (code >= 0xFF00 && code <= 0xFF60)
      ||
      (* Fullwidth Forms *)
      (code >= 0xFFE0 && code <= 0xFFE6 (* Fullwidth Forms *))
    then 2
    else if code = 0 || (code >= 0x0300 && code <= 0x036F (* Combining marks *))
    then 0
    else 1
  in


  (* Place character if within bounds *)
  if row < rows t && col < cols t then (
    (* In insert mode, shift existing characters to the right *)
    if t.insert_mode && col < cols t - 1 then (
      (* Copy the line and shift it right *)
      let line = Grid.copy_row t.grid row in
      for i = cols t - 1 downto col + 1 do
        if i > 0 then line.(i) <- line.(i - 1)
      done;
      Grid.set_row t.grid row line;
      mark_dirty t);

    let glyph = Buffer.create 4 in
    Buffer.add_utf_8_uchar glyph c;
    Grid.set t.grid ~row ~col
      (Some { Grid.Cell.glyph = Buffer.contents glyph; width; attrs = t.gfx_state });
    mark_dirty t);

  (* Handle cursor advancement based on character width *)
  if width = 0 then
    (* Combining character - don't advance cursor *)
    ()
  else if width = 2 then (
    (* Wide character - advance cursor twice *)
    advance_cursor t;
    if t.cursor.col < cols t then (
      Grid.set t.grid ~row:t.cursor.row ~col:t.cursor.col None;
      mark_dirty t);
    advance_cursor t)
  else (
    (* Normal character - advance cursor once *)
    advance_cursor t)

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
          Grid.clear_rect t.grid ~row_start:t.cursor.row ~row_end:t.cursor.row
            ~col_start:0 ~col_end:t.cursor.col;
          if t.cursor.col >= 0 then mark_dirty t
      | 2 | 3 ->
          clear_screen t;
          set_cursor t ~row:0 ~col:0
      | _ -> ())
  | EL n -> (
      match n with
      | 0 -> clear_line t t.cursor.row t.cursor.col
      | 1 ->
          Grid.clear_rect t.grid ~row_start:t.cursor.row ~row_end:t.cursor.row
            ~col_start:0 ~col_end:t.cursor.col;
          if t.cursor.col >= 0 then mark_dirty t
      | 2 -> clear_line t t.cursor.row 0
      | _ -> ())
  | IL n ->
      (* Insert n blank lines at cursor position, shifting content down *)
      let n = max 1 n in
      let current_row = t.cursor.row in
      if current_row >= t.scroll_top && current_row <= t.scroll_bottom then (
        (* Shift lines down from current position *)
        for r = t.scroll_bottom downto current_row + n do
          if r - n >= current_row then
            let row_to_copy = Grid.copy_row t.grid (r - n) in
            Grid.set_row t.grid r row_to_copy
        done;
        (* Clear the inserted lines *)
        for r = current_row to min (current_row + n - 1) t.scroll_bottom do
          Grid.set_row t.grid r (Grid.make_empty_row ~cols:(cols t))
        done;
        mark_dirty t)
  | DL n ->
      (* Delete n lines at cursor position, shifting content up *)
      let n = max 1 n in
      let current_row = t.cursor.row in
      if current_row >= t.scroll_top && current_row <= t.scroll_bottom then (
        (* Shift lines up from below deletion point *)
        for r = current_row to t.scroll_bottom - n do
          if r + n <= t.scroll_bottom then
            let row_to_copy = Grid.copy_row t.grid (r + n) in
            Grid.set_row t.grid r row_to_copy
        done;
        (* Clear the bottom lines *)
        for r = max current_row (t.scroll_bottom - n + 1) to t.scroll_bottom do
          Grid.set_row t.grid r (Grid.make_empty_row ~cols:(cols t))
        done;
        mark_dirty t)
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
      t.gfx_state <- Grid.Cell.default_style;
      clear_screen t;
      set_cursor t ~row:0 ~col:0;
      mark_dirty t
  | DECSC ->
      (* ESC 7 - Save cursor *)
      t.cursor_saved <- Some (t.cursor.row, t.cursor.col, t.cursor.visible);
      t.gfx_state_saved <- Some t.gfx_state
  | DECRC -> (
      (* ESC 8 - Restore cursor *)
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
      | s when String.starts_with ~prefix:"CSI[?7h" s ->
          (* DECAWM - Set auto wrap mode *)
          t.auto_wrap_mode <- true
      | s when String.starts_with ~prefix:"CSI[?7l" s ->
          (* DECAWM - Reset auto wrap mode *)
          t.auto_wrap_mode <- false
      | s when String.starts_with ~prefix:"CSI[?1h" s ->
          (* DECCKM - Set cursor key to application mode *)
          t.cursor_key_mode <- true
      | s when String.starts_with ~prefix:"CSI[?1l" s ->
          (* DECCKM - Set cursor key to cursor mode *)
          t.cursor_key_mode <- false
      | s when String.starts_with ~prefix:"CSI[4h" s ->
          (* IRM - Set insert mode *)
          t.insert_mode <- true
      | s when String.starts_with ~prefix:"CSI[4l" s ->
          (* IRM - Reset insert mode *)
          t.insert_mode <- false
      | s when String.starts_with ~prefix:"CSI[20h" s ->
          (* LNM - Set automatic newline mode *)
          t.auto_newline_mode <- true
      | s when String.starts_with ~prefix:"CSI[20l" s ->
          (* LNM - Reset automatic newline mode *)
          t.auto_newline_mode <- false
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
            (* Line Feed *)
            if t.cursor.row >= t.scroll_bottom then scroll_up t 1
            else (
              t.cursor.row <- t.cursor.row + 1;
              mark_dirty t);
            (* Most terminals treat LF as LF+CR by default *)
            t.cursor.col <- 0;
            mark_dirty t
        | 0x0d ->
            (* Carriage Return *)
            set_cursor t ~row:t.cursor.row ~col:0
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
      let new_state = List.fold_left Grid.Cell.apply_sgr_attr t.gfx_state attrs in
      if new_state <> t.gfx_state then (
        t.gfx_state <- new_state;
        mark_dirty t)
  | Parser.Control ctrl -> handle_control t ctrl

let feed t bytes ofs len =
  let tokens = Parser.feed t.parser bytes ofs len in
  List.iter (handle_token t) tokens

let to_string_grid t = Grid.to_string t.grid
let cursor_pos (t : t) = (t.cursor.row, t.cursor.col)
let is_cursor_visible (t : t) = t.cursor.visible
let is_cursor_key_mode t = t.cursor_key_mode
let is_insert_mode t = t.insert_mode
let is_auto_newline_mode t = t.auto_newline_mode
let is_auto_wrap_mode t = t.auto_wrap_mode
let title (t : t) = t.title

let reset t =
  (* Exit alternate screen if active *)
  if t.in_alternate then switch_to_main t false;
  clear_screen t;
  set_cursor t ~row:0 ~col:0;
  t.gfx_state <- Grid.Cell.default_style;
  Parser.reset t.parser;
  t.title <- "";
  Ring_buffer.clear t.scrollback;
  t.scroll_top <- 0;
  t.scroll_bottom <- rows t - 1;
  t.origin_mode <- false;
  t.auto_wrap_mode <- true;
  t.cursor_key_mode <- false;
  t.insert_mode <- false;
  t.auto_newline_mode <- false;
  mark_dirty t

let resize t ~rows:new_rows ~cols:new_cols =
  (* Resize both grids *)
  Grid.resize t.grid ~rows:new_rows ~cols:new_cols;
  Grid.resize t.alt_grid ~rows:new_rows ~cols:new_cols;
  
  (* Adjust cursor position if needed *)
  if t.cursor.row >= new_rows then t.cursor.row <- new_rows - 1;
  if t.cursor.col >= new_cols then t.cursor.col <- new_cols - 1;
  
  (* Adjust scroll region *)
  t.scroll_bottom <- new_rows - 1;
  if t.scroll_top >= new_rows then t.scroll_top <- 0;
  
  (* Mark as dirty *)
  mark_dirty t

let get_cell t ~row ~col = Grid.get t.grid ~row ~col
let get_grid t = t.grid
