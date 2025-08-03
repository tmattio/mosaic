open Ansi

type cursor = { mutable row : int; mutable col : int; mutable visible : bool }

(* The `grid` and `cursor` fields are changed to be non-mutable.
   Their *contents* are mutable, but the record fields themselves are never
   re-assigned, which is what the original warning was about. *)
type t = {
  grid : Grid.t;
  scrollback : Grid.Cell.t array Ring_buffer.t;
  scrollback_size : int;
  cursor : cursor;
  mutable gfx_state : Ansi.Style.t;
  parser : Parser.t;
  mutable title : string;
  mutable dirty : bool;  (** Set when terminal state changes *)
  (* Alternate screen buffer support *)
  alt_grid : Grid.t;
  mutable in_alternate : bool;
  mutable scroll_top : int;  (** Top of scroll region (0-based) *)
  mutable scroll_bottom : int;  (** Bottom of scroll region (0-based) *)
  mutable cursor_saved : (int * int * bool) option;  (** Saved cursor state *)
  mutable gfx_state_saved : Ansi.Style.t option;  (** Saved graphics state *)
  mutable origin_mode : bool;  (** DECOM - Origin mode *)
  mutable auto_wrap_mode : bool;  (** DECAWM - Auto wrap mode *)
  mutable cursor_key_mode : bool;  (** DECCKM - Cursor key application mode *)
  mutable insert_mode : bool;  (** IRM - Insert/Replace mode *)
  mutable auto_newline_mode : bool;  (** LNM - Automatic newline *)
}

let rows (t : t) = Grid.rows t.grid
let cols (t : t) = Grid.cols t.grid

let create ?(scrollback = 1000) ~rows ~cols () =
  let grid = Grid.create ~rows ~cols () in
  let alt_grid = Grid.create ~rows ~cols () in
  let empty_row = Grid.make_empty_row ~cols in
  {
    grid;
    scrollback = Ring_buffer.create scrollback empty_row;
    scrollback_size = scrollback;
    cursor = { row = 0; col = 0; visible = true };
    gfx_state = Ansi.Style.default;
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
  (* Allow column to be at cols for pending wrap state *)
  let col = clamp ~min:0 ~max:(cols t) col in
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

let advance_cursor ?(by = 1) (t : t) =
  let { row; col; _ } = t.cursor in
  let new_col = col + by in
  if new_col >= cols t then
    if t.auto_wrap_mode then
      (* Wrap to next line *)
      if row >= t.scroll_bottom then (
        scroll_up t 1;
        set_cursor t ~row:t.scroll_bottom ~col:(new_col - cols t))
      else set_cursor t ~row:(row + 1) ~col:(new_col - cols t)
    else
      (* Stay at the last column when auto_wrap is disabled *)
      set_cursor t ~row ~col:(cols t - 1)
  else set_cursor t ~row ~col:new_col

let put_text (t : t) text =
  let { row; col; _ } = t.cursor in

  (* Handle pending wrap state *)
  let row, col =
    if col >= cols t && t.auto_wrap_mode then
      if
        (* We're in pending wrap state - wrap to next line *)
        row >= t.scroll_bottom
      then (
        scroll_up t 1;
        (t.scroll_bottom, 0))
      else (row + 1, 0)
    else (row, col)
  in

  (* Update cursor position if we wrapped *)
  if row <> t.cursor.row || col <> t.cursor.col then set_cursor t ~row ~col;

  (* Place text if within bounds *)
  if row < rows t && col < cols t then (
    (* Handle insert mode - shift existing characters to the right *)
    (if t.insert_mode && String.length text > 0 then
       (* We need to calculate the width of the text we're about to insert *)
       (* For insert mode, we need to estimate the width of the text
         This is approximate - we'd need full grapheme segmentation for accuracy *)
       let text_width =
         let rec count_width i acc =
           if i >= String.length text then acc
           else
             let c = Char.code text.[i] in
             if c < 0x80 then count_width (i + 1) (acc + 1) (* ASCII *)
             else if c >= 0xC0 then count_width (i + 1) (acc + 1)
               (* Start of UTF-8 sequence *)
             else count_width (i + 1) acc (* Continuation byte *)
         in
         count_width 0 0
       in
       if text_width > 0 && col + text_width < cols t then (
         (* Copy the line and shift it right by text_width *)
         let line = Grid.copy_row t.grid row in
         let grid_cols = cols t in
         (* Shift characters to the right, starting from the end *)
         for i = grid_cols - 1 downto col + text_width do
           if i - text_width >= 0 && i - text_width < Array.length line then
             line.(i) <- line.(i - text_width)
         done;
         (* Clear the space where we'll insert the new text *)
         for i = col to min (col + text_width - 1) (grid_cols - 1) do
           line.(i) <- Grid.Cell.empty
         done;
         Grid.set_row t.grid row line;
         mark_dirty t));

    (* Write text, handling wrapping at the VTE level *)
    let text_len = String.length text in
    if text_len > 0 then
      (* Process text in chunks that fit on the current line *)
      let rec write_text start_pos =
        if start_pos < text_len then
          let { row; col; _ } = t.cursor in
          let remaining_cols = cols t - col in

          if remaining_cols > 0 then (
            (* Write as much as fits on the current line *)
            (* This is approximate - we'd need full grapheme segmentation for accuracy *)
            let chunk_len = min remaining_cols (text_len - start_pos) in
            let chunk = String.sub text start_pos chunk_len in
            let width =
              Grid.set_text t.grid ~row ~col ~text:chunk ~attrs:t.gfx_state
                ~east_asian_context:false
            in
            mark_dirty t;

            (* Advance cursor by the width written *)
            if width > 0 then (
              advance_cursor ~by:width t;
              (* Continue with remaining text *)
              write_text (start_pos + chunk_len)))
          else if t.auto_wrap_mode then (
            (* No room on current line - wrap to next *)
            if row >= t.scroll_bottom then (
              scroll_up t 1;
              set_cursor t ~row:t.scroll_bottom ~col:0)
            else set_cursor t ~row:(row + 1) ~col:0;
            (* Continue writing on new line *)
            write_text start_pos)
      in
      write_text 0)

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
  | Hyperlink (Some (_, _uri)) ->
      (* TODO: Hyperlink support needs to be reimplemented at the Grid storage level *)
      mark_dirty t
  | Hyperlink None ->
      (* TODO: Hyperlink support needs to be reimplemented at the Grid storage level *)
      mark_dirty t
  | Reset ->
      t.gfx_state <- Ansi.Style.default;
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
  let handle_control_char code =
    match code with
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
        if next_tab < cols t then set_cursor t ~row:t.cursor.row ~col:next_tab
        else set_cursor t ~row:t.cursor.row ~col:(cols t - 1)
    | 0x0e | 0x0f ->
        (* SO (Shift Out) and SI (Shift In) - character set switching *)
        (* For now, ignore these as we don't support alternate character sets *)
        ()
    | _ -> ()
  in

  (* Process text, separating control characters from printable text *)
  let len = String.length text in
  let text_start = ref 0 in

  let flush_text i =
    (if i > !text_start then
       let substring = String.sub text !text_start (i - !text_start) in
       put_text t substring);
    text_start := i + 1
  in

  for i = 0 to len - 1 do
    let code = Char.code text.[i] in
    if code < 0x20 || code = 0x7F then (
      (* Control character - flush any pending text and handle it *)
      flush_text i;
      handle_control_char code)
  done;

  (* Flush any remaining text *)
  flush_text len

let handle_token t = function
  | Parser.Text s -> handle_text t s
  | Parser.SGR attrs ->
      let new_state =
        List.fold_left Ansi.Style.apply_sgr_attr t.gfx_state attrs
      in
      if new_state <> t.gfx_state then (
        t.gfx_state <- new_state;
        mark_dirty t)
  | Parser.Control ctrl -> handle_control t ctrl

let feed t bytes ofs len =
  let tokens = Parser.feed t.parser bytes ofs len in
  List.iter (handle_token t) tokens

let feed_string t str =
  let bytes = Bytes.of_string str in
  feed t bytes 0 (Bytes.length bytes)

let grid t = t.grid
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
  t.gfx_state <- Ansi.Style.default;
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
