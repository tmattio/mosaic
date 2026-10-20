module Grid = Matrix_grid
module Text = Matrix_text

type cursor = { mutable row : int; mutable col : int; mutable visible : bool }
(* Internal cursor state *)

type saved_cursor = {
  saved_row : int;
  saved_col : int;
  saved_style : Ansi.Style.t;
  saved_origin : bool;
  saved_charsets : char array;
  saved_gl : int;
}
(* What DECSC saves. Visibility is not part of it, as in xterm. *)

(* What DECRC restores when nothing was saved. *)
let home_cursor =
  {
    saved_row = 0;
    saved_col = 0;
    saved_style = Ansi.Style.default;
    saved_origin = false;
    saved_charsets = Array.make 4 'B';
    saved_gl = 0;
  }

type style_run = { start_byte : int; style : Ansi.Style.t }
type scrollback_line = { text : string; styles : style_run array }

let empty_scrollback_line = { text = ""; styles = [||] }

type scrollback = {
  lines : scrollback_line array; (* Ring buffer of compressed lines *)
  mutable head : int;
  mutable count : int;
  capacity : int;
}
(* Scrollback ring buffer with compressed storage *)

type scroll_region = { mutable top : int; mutable bottom : int }
(* Scroll region bounds *)

module Int_set = Set.Make (Int)

(* Store complete styles, including hyperlinks, only when they change. Spaces
   can be omitted only when they carry no visible decoration or link. *)
let compress_row ~default_bg grid row cols =
  let buf = Buffer.create (cols * 2) in
  let styles = ref [] in
  let current_fg = ref Ansi.Color.default_fg in
  let current_bg = ref Ansi.Color.default_bg in
  let current_attrs = ref 0 in
  let current_link = ref "" in
  let content_end = ref 0 in
  for col = 0 to cols - 1 do
    let idx = (row * cols) + col in
    if not (Grid.is_continuation grid idx) then begin
      let text =
        if Grid.is_empty grid idx then " " else Grid.get_text grid idx
      in
      let fg = Grid.get_fg grid idx in
      let bg = Grid.get_bg grid idx in
      let attrs = Grid.get_attrs grid idx in
      let link = Grid.get_link grid idx in
      if
        !styles = []
        || (not (Ansi.Color.equal fg !current_fg))
        || (not (Ansi.Color.equal bg !current_bg))
        || attrs <> !current_attrs || link <> !current_link
      then begin
        styles :=
          { start_byte = Buffer.length buf; style = Grid.get_style grid idx }
          :: !styles;
        current_fg := fg;
        current_bg := bg;
        current_attrs := attrs;
        current_link := link
      end;
      Buffer.add_string buf text;
      if
        text <> " "
        || Ansi.Color.Packed.alpha (Ansi.Color.Packed.encode bg) > 0
           && not (Ansi.Color.equal bg default_bg)
        || attrs <> 0 || link <> ""
      then content_end := Buffer.length buf
    end
  done;
  let text = Buffer.sub buf 0 !content_end in
  let rec trim_runs = function
    | run :: rest when run.start_byte >= !content_end -> trim_runs rest
    | runs -> runs
  in
  { text; styles = Array.of_list (List.rev (trim_runs !styles)) }

let decompress_line line grid row =
  let col = ref 0 in
  let width_method = Grid.width_method grid in
  for i = 0 to Array.length line.styles - 1 do
    let run = line.styles.(i) in
    let stop =
      if i + 1 = Array.length line.styles then String.length line.text
      else line.styles.(i + 1).start_byte
    in
    if !col < Grid.width grid then begin
      let text = String.sub line.text run.start_byte (stop - run.start_byte) in
      Grid.draw_text grid ~x:!col ~y:row ~style:run.style ~text;
      col := !col + Text.measure ~width_method ~tab_width:2 text
    end
  done

type dirty_state = {
  mutable dirty : bool;
  mutable dirty_rows : Int_set.t;
  mutable dirty_cursor : bool;
}

type t = {
  (* Screen grids *)
  primary : Grid.t;
  alternate : Grid.t;
  mutable active_grid : Grid.t;
  (* Dimensions *)
  mutable rows : int;
  mutable cols : int;
  mutable tabs : bool array;
  (* Cursor state *)
  cursor : cursor;
  (* One saved cursor per screen *)
  mutable saved_primary : saved_cursor;
  mutable saved_alternate : saved_cursor;
  (* Graphics state *)
  mutable style : Ansi.Style.t;
  (* Parser *)
  parser : Ansi.Parser.t;
  (* Text was the last thing printed, so REP has a character to repeat *)
  mutable repeatable : bool;
  (* Terminal state *)
  mutable title : string;
  (* Bytes owed to the application, and the device attributes it is told *)
  replies : Buffer.t;
  da1 : string;
  da2 : string;
  dirty_state : dirty_state;
  (* Scrollback *)
  scrollback : scrollback option;
  (* Scroll region *)
  scroll_region : scroll_region;
  (* Terminal modes *)
  mutable origin_mode : bool;
  mutable auto_wrap_mode : bool;
  mutable cursor_key_mode : bool;
  mutable insert_mode : bool;
  (* Character sets: the designations of G0 to G3, and the one invoked into
     GL *)
  charsets : char array;
  mutable gl : int;
  mutable bracketed_paste : bool;
  default_fg : Ansi.Color.t;
  default_bg : Ansi.Color.t;
}
(* VTE instance *)

(* Tab stops every eight columns, as at power-up. *)
let default_tabs cols = Array.init cols (fun i -> i > 0 && i mod 8 = 0)

let create ?(scrollback = 10000) ?width_method ?(default_fg = Ansi.Color.white)
    ?(default_bg = Ansi.Color.black) ?(da1 = "\027[?1;2c")
    ?(da2 = "\027[>0;0;0c") ~rows ~cols () =
  let rows = max 1 rows in
  let cols = max 1 cols in
  let primary = Grid.create ~width:cols ~height:rows ?width_method () in
  let alternate = Grid.create ~width:cols ~height:rows ?width_method () in
  let scrollback_buffer =
    if scrollback <= 0 then None
    else
      let lines = Array.make scrollback empty_scrollback_line in
      Some { lines; head = 0; count = 0; capacity = scrollback }
  in
  Grid.clear ~color:default_bg primary;
  Grid.clear ~color:default_bg alternate;
  {
    primary;
    alternate;
    active_grid = primary;
    rows;
    cols;
    tabs = default_tabs cols;
    cursor = { row = 0; col = 0; visible = true };
    saved_primary = home_cursor;
    saved_alternate = home_cursor;
    style = Ansi.Style.default;
    parser = Ansi.Parser.create ();
    repeatable = false;
    title = "";
    replies = Buffer.create 16;
    da1;
    da2;
    dirty_state =
      { dirty = false; dirty_rows = Int_set.empty; dirty_cursor = false };
    scrollback = scrollback_buffer;
    scroll_region = { top = 0; bottom = rows - 1 };
    origin_mode = false;
    auto_wrap_mode = true;
    cursor_key_mode = false;
    insert_mode = false;
    charsets = Array.make 4 'B';
    gl = 0;
    bracketed_paste = false;
    default_fg;
    default_bg;
  }

(* Efficient destructive erase using the new Grid API: replace region with
   spaces, with BG matching current style (or default). The colour keeps its
   palette index, so a theme can recolour the fill as it does text. *)
let erase_region t ~x ~y ~width ~height =
  if width <= 0 || height <= 0 then ()
  else
    let base_bg =
      match t.style.Ansi.Style.bg with Some c -> c | None -> t.default_bg
    in
    let base_bg =
      match Ansi.Color.intent base_bg with
      | Default -> t.default_bg
      | Indexed _ | Rgb -> base_bg
    in
    Grid.fill_rect t.active_grid ~x ~y ~width ~height ~color:base_bg

(* Dirty tracking helpers *)
let mark_dirty t = t.dirty_state.dirty <- true

let mark_row_dirty t row =
  if row >= 0 && row < t.rows then (
    t.dirty_state.dirty <- true;
    t.dirty_state.dirty_rows <- Int_set.add row t.dirty_state.dirty_rows)

let mark_rows_dirty t start_row end_row =
  for row = start_row to end_row do
    mark_row_dirty t row
  done

let mark_cursor_dirty t =
  t.dirty_state.dirty <- true;
  t.dirty_state.dirty_cursor <- true

let grapheme_start_and_width grid row col =
  let width = Grid.width grid in
  if width = 0 then (0, 1)
  else
    let row_start = row * width in
    let col = min (max col 0) (width - 1) in
    let idx = ref (row_start + col) in
    while !idx >= row_start && Grid.is_continuation grid !idx do
      idx := !idx - 1
    done;
    let start_idx = !idx in
    let start_col = start_idx - row_start in
    let w = max 1 (Grid.cell_width grid start_idx) in
    (start_col, w)

(* DCH and ICH act from the cursor cell. On the right half of a wide
   character, blank the character first, as xterm does. *)
let edit_start t row =
  let start, w = grapheme_start_and_width t.active_grid row t.cursor.col in
  if start < t.cursor.col then erase_region t ~x:start ~y:row ~width:w ~height:1;
  t.cursor.col

let rows t = t.rows
let cols t = t.cols

let resize t ~rows ~cols =
  let rows = max 1 rows in
  let cols = max 1 cols in
  if t.rows = rows && t.cols = cols then ()
  else (
    Grid.resize t.primary ~width:cols ~height:rows;
    Grid.resize t.alternate ~width:cols ~height:rows;
    (* Resizing leaves existing content intact; terminals expect a clear grid
       after a size change so applications repaint. Explicitly clear both
       buffers. *)
    Grid.clear ~color:t.default_bg t.primary;
    Grid.clear ~color:t.default_bg t.alternate;

    (* Scrollback doesn't need resize - it's already compressed text *)
    (* Width changes are handled automatically during decompression *)
    (match t.scrollback with
    | Some _ -> () (* Compressed lines adapt to any width *)
    | None -> ());

    t.rows <- rows;
    t.cols <- cols;
    let tabs = default_tabs cols in
    Array.blit t.tabs 0 tabs 0 (min cols (Array.length t.tabs));
    t.tabs <- tabs;

    (* Clamp cursor *)
    if t.cursor.row >= rows then t.cursor.row <- rows - 1;
    if t.cursor.col >= cols then t.cursor.col <- cols - 1;

    (* Adjust scroll region *)
    if t.scroll_region.top >= rows then t.scroll_region.top <- 0;
    t.scroll_region.bottom <- rows - 1;

    mark_rows_dirty t 0 (rows - 1))

let grid t = t.active_grid
let title t = t.title

let take_replies t =
  let s = Buffer.contents t.replies in
  Buffer.clear t.replies;
  s

let is_dirty t = t.dirty_state.dirty

let clear_dirty t =
  t.dirty_state.dirty <- false;
  t.dirty_state.dirty_rows <- Int_set.empty;
  t.dirty_state.dirty_cursor <- false

let dirty_rows t = Int_set.elements t.dirty_state.dirty_rows
let is_cursor_dirty t = t.dirty_state.dirty_cursor
let is_alternate_screen t = t.active_grid == t.alternate
let cursor_pos t = (t.cursor.row, t.cursor.col)
let cursor_visible t = t.cursor.visible

let set_cursor_pos t ~row ~col =
  let row = max 0 (min row (t.rows - 1)) in
  (* Allow cursor at t.cols for autowrap - it wraps on next character *)
  let col = max 0 (min col t.cols) in
  if t.cursor.row <> row || t.cursor.col <> col then (
    t.cursor.row <- row;
    t.cursor.col <- col;
    mark_cursor_dirty t)

(* A cursor at column [cols] is the pending wrap left by a write to the last
   column. Cursor motion, line feeds and edits act from the last column and
   leave it there, as xterm resets its wrap flag. *)
let clear_pending_wrap t =
  if t.cursor.col >= t.cols then
    set_cursor_pos t ~row:t.cursor.row ~col:(t.cols - 1)

(* Explicit motion stops at the last column: only printing can leave the
   cursor in the pending-wrap position. *)
let move_cursor t ~row ~col = set_cursor_pos t ~row ~col:(min col (t.cols - 1))

(* In origin mode (DECOM) CUP and VPA rows count from the top margin and stay
   inside the scroll region. *)
let origin_row t row =
  if t.origin_mode then
    max t.scroll_region.top
      (min t.scroll_region.bottom (t.scroll_region.top + row))
  else row

let set_cursor_visible t visible =
  if t.cursor.visible <> visible then (
    t.cursor.visible <- visible;
    mark_cursor_dirty t)

(* DEC Special Graphics ([ESC ( 0]), the line drawing set. *)
let dec_graphics = function
  | '`' -> Some "\u{25c6}"
  | 'a' -> Some "\u{2592}"
  | 'b' -> Some "\u{2409}"
  | 'c' -> Some "\u{240c}"
  | 'd' -> Some "\u{240d}"
  | 'e' -> Some "\u{240a}"
  | 'f' -> Some "\u{00b0}"
  | 'g' -> Some "\u{00b1}"
  | 'h' -> Some "\u{2424}"
  | 'i' -> Some "\u{240b}"
  | 'j' -> Some "\u{2518}"
  | 'k' -> Some "\u{2510}"
  | 'l' -> Some "\u{250c}"
  | 'm' -> Some "\u{2514}"
  | 'n' -> Some "\u{253c}"
  | 'o' -> Some "\u{23ba}"
  | 'p' -> Some "\u{23bb}"
  | 'q' -> Some "\u{2500}"
  | 'r' -> Some "\u{23bc}"
  | 's' -> Some "\u{23bd}"
  | 't' -> Some "\u{251c}"
  | 'u' -> Some "\u{2524}"
  | 'v' -> Some "\u{2534}"
  | 'w' -> Some "\u{252c}"
  | 'x' -> Some "\u{2502}"
  | 'y' -> Some "\u{2264}"
  | 'z' -> Some "\u{2265}"
  | '{' -> Some "\u{03c0}"
  | '|' -> Some "\u{2260}"
  | '}' -> Some "\u{00a3}"
  | '~' -> Some "\u{00b7}"
  | _ -> None

(* The national replacement sets for British ([ESC ( A]) and German
   ([ESC ( K]). *)
let british = function '#' -> Some "\u{00a3}" | _ -> None

let german = function
  | '@' -> Some "\u{00a7}"
  | '[' -> Some "\u{00c4}"
  | '\\' -> Some "\u{00d6}"
  | ']' -> Some "\u{00dc}"
  | '{' -> Some "\u{00e4}"
  | '|' -> Some "\u{00f6}"
  | '}' -> Some "\u{00fc}"
  | '~' -> Some "\u{00df}"
  | _ -> None

(* Map text through the set invoked into GL; any other set is ASCII. *)
let translate_charset t text =
  let map =
    match t.charsets.(t.gl) with
    | '0' -> Some dec_graphics
    | 'A' -> Some british
    | 'K' -> Some german
    | _ -> None
  in
  match map with
  | None -> text
  | Some map ->
      let buf = Buffer.create (String.length text * 3) in
      String.iter
        (fun c ->
          match map c with
          | Some s -> Buffer.add_string buf s
          | None -> Buffer.add_char buf c)
        text;
      Buffer.contents buf

let scrollback_capacity t =
  match t.scrollback with None -> 0 | Some sb -> sb.capacity

let scrollback_size t =
  if t.active_grid != t.primary then 0
  else match t.scrollback with None -> 0 | Some sb -> sb.count

let scrollback_lines t =
  if t.active_grid != t.primary then []
  else
    match t.scrollback with
    | None -> []
    | Some sb when sb.count = 0 -> []
    | Some sb ->
        let lines = ref [] in
        let start_idx =
          if sb.count < sb.capacity then 0
          else (sb.head + sb.capacity - sb.count) mod sb.capacity
        in
        for i = 0 to sb.count - 1 do
          let idx = (start_idx + i) mod sb.capacity in
          let line = sb.lines.(idx) in
          let stop = ref (String.length line.text) in
          while !stop > 0 && line.text.[!stop - 1] = ' ' do
            decr stop
          done;
          let text =
            if !stop = String.length line.text then line.text
            else String.sub line.text 0 !stop
          in
          lines := text :: !lines
        done;
        List.rev !lines

let render_with_scrollback t ~offset dst =
  (* Only works on primary screen with scrollback *)
  if t.active_grid != t.primary then Grid.blit ~src:t.active_grid ~dst
  else
    match t.scrollback with
    | None -> Grid.blit ~src:t.active_grid ~dst
    | Some sb ->
        let offset = max 0 (min offset sb.count) in
        let dst_height = Grid.height dst in

        (* Clear destination grid up-front so we don't need per-row clearing. *)
        Grid.clear ~color:t.default_bg dst;

        (* Calculate how many lines come from scrollback vs screen *)
        let scrollback_lines = min offset dst_height in
        let screen_lines = dst_height - scrollback_lines in

        (* Render scrollback lines by decompressing them. Row [i] shows the
           line [offset - i] positions above the live screen, so the oldest
           visible line is at the top and the newest sits adjacent to the
           screen. *)
        for i = 0 to scrollback_lines - 1 do
          (* Distance above the screen, as a 0-based newest-first index. *)
          let scrollback_offset = offset - 1 - i in
          let scrollback_idx =
            if sb.count < sb.capacity then
              (* Not wrapped yet - simple offset from head *)
              sb.count - 1 - scrollback_offset
            else
              (* Wrapped - use modulo arithmetic *)
              (sb.head - 1 - scrollback_offset + sb.capacity) mod sb.capacity
          in
          let line = sb.lines.(scrollback_idx) in
          decompress_line line dst i
        done;

        (* Render visible screen lines *)
        if screen_lines > 0 then
          Grid.blit_region ~src:t.active_grid ~dst ~src_x:0 ~src_y:0
            ~width:t.cols ~height:(min screen_lines t.rows) ~dst_x:0
            ~dst_y:scrollback_lines

let cursor_key_mode t = t.cursor_key_mode
let insert_mode t = t.insert_mode
let auto_wrap_mode t = t.auto_wrap_mode
let bracketed_paste_mode t = t.bracketed_paste
let origin_mode t = t.origin_mode

(* IL and DL act only on a cursor inside the scroll region. *)
let in_scroll_region t =
  t.cursor.row >= t.scroll_region.top && t.cursor.row <= t.scroll_region.bottom

(* Save a row to scrollback *)
let push_to_scrollback t row =
  match t.scrollback with
  | None -> ()
  | Some _ when t.active_grid != t.primary -> ()
  | Some sb ->
      (* Compress the row and store it *)
      let compressed =
        compress_row ~default_bg:t.default_bg t.active_grid row t.cols
      in
      sb.lines.(sb.head) <- compressed;
      sb.head <- (sb.head + 1) mod sb.capacity;
      if sb.count < sb.capacity then sb.count <- sb.count + 1

let clear_scrollback t =
  match t.scrollback with
  | None -> ()
  | Some sb ->
      Array.fill sb.lines 0 sb.capacity empty_scrollback_line;
      sb.head <- 0;
      sb.count <- 0

(* Lines a scroll brings in take the current background when the style sets
   one (bce), as erased cells do. *)
let fill_scrolled_in t ~y ~height =
  match t.style.Ansi.Style.bg with
  | Some c when Ansi.Color.intent c <> Default ->
      erase_region t ~x:0 ~y ~width:t.cols ~height
  | Some _ | None -> ()

let scroll_up t n =
  if n <= 0 then ()
  else
    let region_height = t.scroll_region.bottom - t.scroll_region.top + 1 in
    let n = min n region_height in

    (* Save scrolled rows to scrollback if at screen top *)
    if t.active_grid == t.primary && t.scroll_region.top = 0 then
      for i = 0 to n - 1 do
        push_to_scrollback t (t.scroll_region.top + i)
      done;

    (* Use Grid's optimized scroll operation *)
    Grid.scroll t.active_grid ~top:t.scroll_region.top
      ~bottom:t.scroll_region.bottom n;
    fill_scrolled_in t ~y:(t.scroll_region.bottom - n + 1) ~height:n;

    mark_rows_dirty t t.scroll_region.top t.scroll_region.bottom

let scroll_down t n =
  if n <= 0 then ()
  else
    let n = min n (t.scroll_region.bottom - t.scroll_region.top + 1) in

    (* Use Grid's optimized scroll operation *)
    Grid.scroll t.active_grid ~top:t.scroll_region.top
      ~bottom:t.scroll_region.bottom (-n);
    fill_scrolled_in t ~y:t.scroll_region.top ~height:n;

    mark_rows_dirty t t.scroll_region.top t.scroll_region.bottom

(* Blank the wide character whose right half is at [col], so a column shift
   that cuts it leaves no half behind. *)
let blank_cut_wide t row col =
  if
    col > 0 && col < t.cols
    && Grid.is_continuation t.active_grid ((row * t.cols) + col)
  then
    let start, w = grapheme_start_and_width t.active_grid row col in
    erase_region t ~x:start ~y:row ~width:w ~height:1

(* Delete [n] cells (n > 0) or insert [-n] blank cells (n < 0) at column [x]
   of every row of the scroll region, for SL, SR, DECIC and DECDC. Like IL and
   DL, these act only on a cursor inside the region. *)
let shift_columns t ~x n =
  let top = t.scroll_region.top and bottom = t.scroll_region.bottom in
  let span = min (abs n) (t.cols - x) in
  if t.cursor.row >= top && t.cursor.row <= bottom && span > 0 then (
    for row = top to bottom do
      blank_cut_wide t row x;
      let keep = t.cols - x - span in
      if n > 0 then (
        blank_cut_wide t row (x + span);
        if keep > 0 then
          Grid.blit_region ~src:t.active_grid ~dst:t.active_grid
            ~src_x:(x + span) ~src_y:row ~width:keep ~height:1 ~dst_x:x
            ~dst_y:row;
        erase_region t ~x:(t.cols - span) ~y:row ~width:span ~height:1)
      else (
        blank_cut_wide t row (t.cols - span);
        if keep > 0 then
          Grid.blit_region ~src:t.active_grid ~dst:t.active_grid ~src_x:x
            ~src_y:row ~width:keep ~height:1 ~dst_x:(x + span) ~dst_y:row;
        erase_region t ~x ~y:row ~width:span ~height:1)
    done;
    mark_rows_dirty t top bottom)

(* Move down one row for LF/auto-wrap: scroll only when the cursor sits on the
   region bottom; below the region (legal via CUP after DECSTBM) advance toward
   the screen bottom without touching the region. Preserves the column. *)
let advance_row t =
  if t.cursor.row = t.scroll_region.bottom then scroll_up t 1
  else if t.cursor.row < t.rows - 1 then
    set_cursor_pos t ~row:(t.cursor.row + 1) ~col:t.cursor.col

(* The row [n] up or down for CUU, CUD, CPL and CNL. Moving up stops at the
   top margin from at or below it, moving down at the bottom margin from at or
   above it, and otherwise at the screen edge. *)
let row_up t n =
  let top =
    if t.cursor.row >= t.scroll_region.top then t.scroll_region.top else 0
  in
  max top (t.cursor.row - n)

let row_down t n =
  let bottom =
    if t.cursor.row <= t.scroll_region.bottom then t.scroll_region.bottom
    else t.rows - 1
  in
  min bottom (t.cursor.row + n)

(* Write printable text using Grid and Text APIs. We manually segment the input
   string into pieces that fit in the current line, using an ASCII fast path and
   falling back to grapheme segmentation for complex text. *)
let put_text t text =
  let text = translate_charset t text in
  let resolve_color ~default color =
    match Ansi.Color.intent color with
    | Default -> default
    | Indexed _ | Rgb -> color
  in
  let fg =
    Option.value t.style.Ansi.Style.fg ~default:t.default_fg
    |> resolve_color ~default:t.default_fg
  in
  let bg =
    Option.value t.style.Ansi.Style.bg ~default:t.default_bg
    |> resolve_color ~default:t.default_bg
  in
  let style = t.style |> Ansi.Style.fg fg |> Ansi.Style.bg bg in
  let width_method = Grid.width_method t.active_grid in
  (* A run beginning with zero-width graphemes continues the grapheme drawn
     by an earlier write (its base sits in the preceding cell): merge the
     marks into that cell, as real terminals do, so a grapheme cluster split
     across writes renders exactly like one delivered whole. *)
  let text =
    if String.length text = 0 || t.cursor.col = 0 || t.cursor.row >= t.rows then
      text
    else begin
      let zero_width_stop = ref 0 in
      (try
         Text.iter_graphemes
           (fun ~offset ~len ->
             if
               Text.width_at ~width_method ~tab_width:2 text ~byte_offset:offset
               = 0
             then zero_width_stop := offset + len
             else raise Exit)
           text
       with Exit -> ());
      let stop = !zero_width_stop in
      if stop = 0 then text
      else begin
        let row = t.cursor.row in
        let x = ref (t.cursor.col - 1) in
        let idx = ref ((row * t.cols) + !x) in
        while !x > 0 && Grid.is_continuation t.active_grid !idx do
          decr x;
          decr idx
        done;
        let prev = Grid.get_text t.active_grid !idx in
        if prev = "" || Grid.is_continuation t.active_grid !idx then text
        else begin
          Grid.draw_text t.active_grid ~x:!x ~y:row
            ~text:(prev ^ String.sub text 0 stop)
            ~style:(Grid.get_style t.active_grid !idx);
          mark_row_dirty t row;
          String.sub text stop (String.length text - stop)
        end
      end
    end
  in
  if text = "" then ()
  else if (not t.insert_mode) && String.length text <= t.cols - t.cursor.col
  then begin
    (* Printable UTF-8 never occupies more columns than bytes. This bounded
       fit check keeps short writes allocation-light without rescanning suffixes. *)
    Grid.draw_text t.active_grid ~x:t.cursor.col ~y:t.cursor.row ~text ~style;
    mark_row_dirty t t.cursor.row;
    let width = Text.measure ~width_method ~tab_width:2 text in
    set_cursor_pos t ~row:t.cursor.row ~col:(t.cursor.col + width)
  end
  else
    (* Accumulate one contiguous row slice at a time. No suffix is copied or
     remeasured: every display unit is visited once before Grid draws it. *)
    let start = ref 0 in
    let stop = ref 0 in
    let columns = ref 0 in
    let flush () =
      if !columns > 0 then begin
        let segment =
          if !start = 0 && !stop = String.length text then text
          else String.sub text !start (!stop - !start)
        in
        Grid.draw_text t.active_grid ~x:t.cursor.col ~y:t.cursor.row
          ~text:segment ~style;
        mark_row_dirty t t.cursor.row;
        set_cursor_pos t ~row:t.cursor.row ~col:(t.cursor.col + !columns);
        columns := 0
      end
    in
    let emit ~offset ~len ~width =
      if width = 0 then
        begin if !columns > 0 then stop := offset + len
        else if t.cursor.col > 0 then begin
          let x = ref (min (t.cols - 1) (t.cursor.col - 1)) in
          while
            !x > 0
            && Grid.is_continuation t.active_grid ((t.cursor.row * t.cols) + !x)
          do
            decr x
          done;
          let idx = (t.cursor.row * t.cols) + !x in
          Grid.draw_text t.active_grid ~x:!x ~y:t.cursor.row
            ~text:(Grid.get_text t.active_grid idx ^ String.sub text offset len)
            ~style:(Grid.get_style t.active_grid idx);
          mark_row_dirty t t.cursor.row
        end
        end
      else begin
        if t.insert_mode || t.cursor.col + !columns + width > t.cols then
          flush ();
        if t.cursor.col >= t.cols && t.auto_wrap_mode then begin
          advance_row t;
          set_cursor_pos t ~row:t.cursor.row ~col:0
        end;
        if
          (not t.insert_mode)
          && width > t.cols - t.cursor.col
          && t.auto_wrap_mode && t.cursor.col > 0
        then begin
          advance_row t;
          set_cursor_pos t ~row:t.cursor.row ~col:0
        end;
        if t.insert_mode || width > t.cols - t.cursor.col then begin
          let x = min t.cursor.col (t.cols - 1) in
          let inserted = min width (t.cols - t.cursor.col) in
          if t.insert_mode && inserted > 0 then begin
            Grid.blit_region ~src:t.active_grid ~dst:t.active_grid ~src_x:x
              ~src_y:t.cursor.row
              ~width:(t.cols - x - inserted)
              ~height:1 ~dst_x:(x + inserted) ~dst_y:t.cursor.row;
            erase_region t ~x ~y:t.cursor.row ~width:inserted ~height:1
          end;
          Grid.draw_text t.active_grid ~x ~y:t.cursor.row
            ~text:(String.sub text offset len)
            ~style;
          mark_row_dirty t t.cursor.row;
          if t.insert_mode && inserted > 0 then
            set_cursor_pos t ~row:t.cursor.row ~col:(x + inserted)
        end
        else begin
          if !columns = 0 then start := offset;
          stop := offset + len;
          columns := !columns + width
        end
      end
    in
    (match width_method with
    | `Unicode | `Unicode_wide ->
        Text.iter_grapheme_info ~width_method ~tab_width:2 emit text
    | `Wcwidth when t.insert_mode ->
        Text.iter_grapheme_info ~width_method ~tab_width:2 emit text
    | `Wcwidth ->
        let offset = ref 0 in
        while !offset < String.length text do
          let decoded = String.get_utf_8_uchar text !offset in
          let len = Uchar.utf_decode_length decoded in
          emit ~offset:!offset ~len
            ~width:
              (Text.uchar_width ~tab_width:2 (Uchar.utf_decode_uchar decoded));
          offset := !offset + len
        done);
    flush ()

(* The next tab stop after [col], or the last column. *)
let next_tab t col =
  let rec go x =
    if x >= t.cols - 1 then t.cols - 1 else if t.tabs.(x) then x else go (x + 1)
  in
  go (col + 1)

(* The previous tab stop before [col], or the first column. *)
let prev_tab t col =
  let rec go x = if x <= 0 then 0 else if t.tabs.(x) then x else go (x - 1) in
  go (col - 1)

(* Handle control characters *)
let handle_control_char t code =
  match code with
  | 0x07 -> () (* BEL *)
  | 0x08 ->
      (* BS: pure cursor movement, one column left. Never erases — overstrike
         sequences (nroff bold "c\bc") rely on the cell surviving. *)
      clear_pending_wrap t;
      if t.cursor.col > 0 then
        set_cursor_pos t ~row:t.cursor.row ~col:(t.cursor.col - 1)
  | 0x09 ->
      (* HT: a pending wrap stays pending, as in xterm. *)
      if t.cursor.col < t.cols then
        set_cursor_pos t ~row:t.cursor.row ~col:(next_tab t t.cursor.col)
  | 0x0A | 0x0B | 0x0C ->
      (* LF, VT, FF: down one row, same column (VT100). ONLCR translation is
         the tty driver's job; curses cursor-motion optimizations depend on
         bare LF not implying CR. *)
      clear_pending_wrap t;
      advance_row t
  | 0x0D -> (* CR *) set_cursor_pos t ~row:t.cursor.row ~col:0
  | 0x0E -> (* SO *) t.gl <- 1
  | 0x0F -> (* SI *) t.gl <- 0
  | _ -> ()

let handle_text t text =
  let len = String.length text in
  let text_start = ref 0 in

  let flush_text i =
    if i > !text_start then (
      put_text t (String.sub text !text_start (i - !text_start));
      t.repeatable <- true);
    text_start := i + 1
  in

  for i = 0 to len - 1 do
    let code = Char.code text.[i] in
    if code < 0x20 || code = 0x7F then (
      flush_text i;
      t.repeatable <- false;
      handle_control_char t code)
  done;
  flush_text len

(* REP prints the whole cell before the cursor again, combining marks
   included, as xterm does. *)
let repeat_preceding t n =
  let row = t.cursor.row in
  let x = ref (min t.cursor.col t.cols - 1) in
  while !x > 0 && Grid.is_continuation t.active_grid ((row * t.cols) + !x) do
    decr x
  done;
  if t.repeatable && !x >= 0 then
    let cell = Grid.get_text t.active_grid ((row * t.cols) + !x) in
    if cell <> "" then (
      let n = min n (t.rows * t.cols) in
      let buf = Buffer.create (n * String.length cell) in
      for _ = 1 to n do
        Buffer.add_string buf cell
      done;
      put_text t (Buffer.contents buf))

let apply_sgr_attr style attr =
  match attr with
  | `Reset -> Ansi.Style.make ?link:style.Ansi.Style.link ()
  | `Bold -> Ansi.Style.with_bold true style
  | `Dim -> Ansi.Style.with_dim true style
  | `Italic -> Ansi.Style.with_italic true style
  | `Underline -> Ansi.Style.with_underline true style
  | `Double_underline -> Ansi.Style.with_double_underline true style
  | `Blink -> Ansi.Style.with_blink true style
  | `Inverse -> Ansi.Style.with_inverse true style
  | `Hidden -> Ansi.Style.with_hidden true style
  | `Strikethrough -> Ansi.Style.with_strikethrough true style
  | `Overline -> Ansi.Style.with_overline true style
  | `Framed -> Ansi.Style.with_framed true style
  | `Encircled -> Ansi.Style.with_encircled true style
  | `No_bold -> Ansi.Style.with_bold false style
  | `No_dim -> Ansi.Style.with_dim false style
  | `No_italic -> Ansi.Style.with_italic false style
  | `No_underline -> Ansi.Style.with_underline false style
  | `No_blink -> Ansi.Style.with_blink false style
  | `No_inverse -> Ansi.Style.with_inverse false style
  | `No_hidden -> Ansi.Style.with_hidden false style
  | `No_strikethrough -> Ansi.Style.with_strikethrough false style
  | `No_overline -> Ansi.Style.with_overline false style
  | `No_framed -> Ansi.Style.with_framed false style
  | `No_encircled -> Ansi.Style.with_encircled false style
  | `Fg color -> Ansi.Style.fg color style
  | `Bg color -> Ansi.Style.bg color style

(* DECSC and DECRC act on the saved cursor of the active screen. *)
let decsc t =
  let saved =
    {
      saved_row = t.cursor.row;
      saved_col = t.cursor.col;
      saved_style = t.style;
      saved_origin = t.origin_mode;
      saved_charsets = Array.copy t.charsets;
      saved_gl = t.gl;
    }
  in
  if t.active_grid == t.alternate then t.saved_alternate <- saved
  else t.saved_primary <- saved

let decrc t =
  let saved =
    if t.active_grid == t.alternate then t.saved_alternate else t.saved_primary
  in
  t.origin_mode <- saved.saved_origin;
  Array.blit saved.saved_charsets 0 t.charsets 0 4;
  t.gl <- saved.saved_gl;
  let row =
    if t.origin_mode then
      max t.scroll_region.top (min t.scroll_region.bottom saved.saved_row)
    else saved.saved_row
  in
  set_cursor_pos t ~row ~col:saved.saved_col;
  t.style <- saved.saved_style

let switch_to_alternate t save_cursor =
  if t.active_grid != t.alternate then (
    if save_cursor then decsc t;
    t.active_grid <- t.alternate;
    Grid.clear ~color:t.default_bg t.active_grid;
    t.scroll_region.top <- 0;
    t.scroll_region.bottom <- t.rows - 1;
    mark_rows_dirty t 0 (t.rows - 1))

let switch_to_primary t restore_cursor =
  if t.active_grid == t.alternate then (
    t.active_grid <- t.primary;
    if restore_cursor then decrc t;
    mark_rows_dirty t 0 (t.rows - 1))

(* RIS: everything but the parser, which [reset] also resets. *)
let hard_reset t =
  if t.active_grid == t.alternate then switch_to_primary t false;
  Grid.clear ~color:t.default_bg t.primary;
  Grid.clear ~color:t.default_bg t.alternate;
  set_cursor_pos t ~row:0 ~col:0;
  set_cursor_visible t true;
  t.style <- Ansi.Style.default;
  t.saved_primary <- home_cursor;
  t.saved_alternate <- home_cursor;
  t.title <- "";

  (* Clear scrollback *)
  clear_scrollback t;

  t.scroll_region.top <- 0;
  t.scroll_region.bottom <- t.rows - 1;
  t.origin_mode <- false;
  t.auto_wrap_mode <- true;
  t.cursor_key_mode <- false;
  t.insert_mode <- false;
  Array.fill t.charsets 0 4 'B';
  t.gl <- 0;
  t.bracketed_paste <- false;
  t.tabs <- default_tabs t.cols;
  mark_rows_dirty t 0 (t.rows - 1)

(* DECSTR, as xterm's soft reset: the modes, SGR, the margins, the character
   sets and the saved cursor return to their defaults, and the screen and the
   cursor position stay. *)
let soft_reset t =
  set_cursor_visible t true;
  t.insert_mode <- false;
  t.origin_mode <- false;
  t.auto_wrap_mode <- true;
  t.cursor_key_mode <- false;
  t.style <- Ansi.Style.default;
  t.scroll_region.top <- 0;
  t.scroll_region.bottom <- t.rows - 1;
  Array.fill t.charsets 0 4 'B';
  t.gl <- 0;
  if t.active_grid == t.alternate then t.saved_alternate <- home_cursor
  else t.saved_primary <- home_cursor

(* ANSI modes ([CSI Pm h/l]). *)
let set_ansi_mode t mode set =
  match mode with 4 -> t.insert_mode <- set | _ -> ()

(* DEC private modes ([CSI ? Pm h/l]). *)
let set_dec_mode t mode set =
  match mode with
  | 1 -> t.cursor_key_mode <- set
  | 6 ->
      t.origin_mode <- set;
      set_cursor_pos t ~row:(origin_row t 0) ~col:0
  | 7 -> t.auto_wrap_mode <- set
  | 25 -> set_cursor_visible t set
  | 47 | 1047 | 1049 ->
      if set then switch_to_alternate t (mode = 1049)
      else switch_to_primary t (mode = 1049)
  | 2004 -> t.bracketed_paste <- set
  | _ -> ()

(* The 1-based cursor position CPR reports: a pending wrap reports the last
   column, and origin mode counts rows from the top margin, as in xterm. *)
let cursor_report t =
  let top = if t.origin_mode then t.scroll_region.top else 0 in
  (t.cursor.row - top + 1, min t.cursor.col (t.cols - 1) + 1)

let handle_control t ctrl =
  (match ctrl with
  | Ansi.Parser.(
      ( CUU _ | CUD _ | CUF _ | CUB _ | VPA _ | ED _ | EL _ | IL _ | DL _
      | DCH _ | ICH _ | RI )) ->
      clear_pending_wrap t
  | _ -> ());
  match ctrl with
  | Ansi.Parser.CUU n -> set_cursor_pos t ~row:(row_up t n) ~col:t.cursor.col
  | Ansi.Parser.CUD n -> set_cursor_pos t ~row:(row_down t n) ~col:t.cursor.col
  | Ansi.Parser.CUF n -> move_cursor t ~row:t.cursor.row ~col:(t.cursor.col + n)
  | Ansi.Parser.CUB n ->
      set_cursor_pos t ~row:t.cursor.row ~col:(t.cursor.col - n)
  | Ansi.Parser.CNL n -> set_cursor_pos t ~row:(row_down t n) ~col:0
  | Ansi.Parser.CPL n -> set_cursor_pos t ~row:(row_up t n) ~col:0
  | Ansi.Parser.CHA n -> move_cursor t ~row:t.cursor.row ~col:(n - 1)
  | Ansi.Parser.VPA n ->
      set_cursor_pos t ~row:(origin_row t (n - 1)) ~col:t.cursor.col
  | Ansi.Parser.CUP (r, c) ->
      move_cursor t ~row:(origin_row t (r - 1)) ~col:(c - 1)
  | Ansi.Parser.ED n -> (
      match n with
      | 0 ->
          (* Clear from cursor to end (destructive erase) on default/theme bg *)
          erase_region t ~x:t.cursor.col ~y:t.cursor.row
            ~width:(t.cols - t.cursor.col) ~height:1;
          erase_region t ~x:0 ~y:(t.cursor.row + 1) ~width:t.cols
            ~height:(t.rows - (t.cursor.row + 1));
          mark_rows_dirty t t.cursor.row (t.rows - 1)
      | 1 ->
          (* Clear from start to cursor (destructive erase) *)
          erase_region t ~x:0 ~y:0 ~width:t.cols ~height:t.cursor.row;
          erase_region t ~x:0 ~y:t.cursor.row ~width:(t.cursor.col + 1)
            ~height:1;
          mark_rows_dirty t 0 t.cursor.row
      | 2 ->
          (* Clear entire screen *)
          erase_region t ~x:0 ~y:0 ~width:t.cols ~height:t.rows;
          mark_rows_dirty t 0 (t.rows - 1)
      | 3 -> clear_scrollback t
      | _ -> ())
  | Ansi.Parser.ECH n ->
      clear_pending_wrap t;
      erase_region t ~x:t.cursor.col ~y:t.cursor.row
        ~width:(min (max n 1) (t.cols - t.cursor.col))
        ~height:1;
      mark_row_dirty t t.cursor.row
  | Ansi.Parser.EL n ->
      (match n with
      | 0 ->
          erase_region t ~x:t.cursor.col ~y:t.cursor.row
            ~width:(t.cols - t.cursor.col) ~height:1
      | 1 ->
          erase_region t ~x:0 ~y:t.cursor.row ~width:(t.cursor.col + 1)
            ~height:1
      | 2 -> erase_region t ~x:0 ~y:t.cursor.row ~width:t.cols ~height:1
      | _ -> ());
      mark_row_dirty t t.cursor.row
  | Ansi.Parser.IND ->
      clear_pending_wrap t;
      advance_row t
  | Ansi.Parser.NEL ->
      advance_row t;
      set_cursor_pos t ~row:t.cursor.row ~col:0
  | Ansi.Parser.IL n ->
      (* Insert lines *)
      if n > 0 && in_scroll_region t then (
        let insert_count = min n (t.scroll_region.bottom - t.cursor.row + 1) in
        for row = t.scroll_region.bottom downto t.cursor.row + insert_count do
          Grid.blit_region ~src:t.active_grid ~dst:t.active_grid ~src_x:0
            ~src_y:(row - insert_count) ~width:t.cols ~height:1 ~dst_x:0
            ~dst_y:row
        done;
        erase_region t ~x:0 ~y:t.cursor.row ~width:t.cols ~height:insert_count;
        mark_rows_dirty t t.cursor.row t.scroll_region.bottom;
        set_cursor_pos t ~row:t.cursor.row ~col:0)
  | Ansi.Parser.DL n ->
      (* Delete lines *)
      if n > 0 && in_scroll_region t then (
        let delete_count = min n (t.scroll_region.bottom - t.cursor.row + 1) in
        for row = t.cursor.row to t.scroll_region.bottom - delete_count do
          Grid.blit_region ~src:t.active_grid ~dst:t.active_grid ~src_x:0
            ~src_y:(row + delete_count) ~width:t.cols ~height:1 ~dst_x:0
            ~dst_y:row
        done;
        erase_region t ~x:0
          ~y:(t.scroll_region.bottom - delete_count + 1)
          ~width:t.cols ~height:delete_count;
        mark_rows_dirty t t.cursor.row t.scroll_region.bottom;
        set_cursor_pos t ~row:t.cursor.row ~col:0)
  | Ansi.Parser.DCH n ->
      (* Delete characters *)
      if n > 0 then
        let row = t.cursor.row in
        if row >= 0 && row < t.rows then
          let start_col = edit_start t row in
          if start_col < t.cols then
            let delete_span =
              let rec loop col remaining acc =
                if col >= t.cols || remaining <= 0 then acc
                else
                  let start, w =
                    grapheme_start_and_width t.active_grid row col
                  in
                  let next_col = start + w in
                  loop next_col (remaining - w) (acc + w)
              in
              let span = loop start_col n 0 in
              min span (t.cols - start_col)
            in
            if delete_span > 0 then (
              let src_x = start_col + delete_span in
              let move_width = t.cols - src_x in
              if move_width > 0 then
                Grid.blit_region ~src:t.active_grid ~dst:t.active_grid ~src_x
                  ~src_y:row ~width:move_width ~height:1 ~dst_x:start_col
                  ~dst_y:row;
              erase_region t ~x:(t.cols - delete_span) ~y:row ~width:delete_span
                ~height:1;
              mark_row_dirty t row)
  | Ansi.Parser.ICH n ->
      (* Insert characters: shift content right by insert_span, clear the gap.
         Mirror of DCH which shifts left. Grid.blit_region handles overlapping
         same-grid blits by iterating right-to-left when src_x < dst_x. *)
      if n > 0 then
        let row = t.cursor.row in
        if row >= 0 && row < t.rows then
          let start_col = edit_start t row in
          if start_col < t.cols then
            let insert_span = min n (t.cols - start_col) in
            if insert_span > 0 then (
              let move_width = t.cols - start_col - insert_span in
              if move_width > 0 then (
                Grid.blit_region ~src:t.active_grid ~dst:t.active_grid
                  ~src_x:start_col ~src_y:row ~width:move_width ~height:1
                  ~dst_x:(start_col + insert_span) ~dst_y:row;
                (* If a wide character at the right edge got truncated by the
                   shift, erase the orphaned cells. *)
                let right_start, right_w =
                  grapheme_start_and_width t.active_grid row (t.cols - 1)
                in
                if right_start + right_w > t.cols then
                  erase_region t ~x:right_start ~y:row
                    ~width:(t.cols - right_start) ~height:1);
              erase_region t ~x:start_col ~y:row ~width:insert_span ~height:1;
              mark_row_dirty t row)
  | Ansi.Parser.DECSTBM (top, bottom) ->
      let top = max 1 (min top t.rows) in
      let bottom = if bottom <= 0 then t.rows else max 1 (min bottom t.rows) in
      if top < bottom then (
        t.scroll_region.top <- top - 1;
        t.scroll_region.bottom <- bottom - 1;
        set_cursor_pos t ~row:(origin_row t 0) ~col:0)
  | Ansi.Parser.SU n -> scroll_up t n
  | Ansi.Parser.DECALN ->
      (* As xterm: reset the margins, fill with E in the default rendition,
         and home the cursor. *)
      t.scroll_region.top <- 0;
      t.scroll_region.bottom <- t.rows - 1;
      let style =
        Ansi.Style.default |> Ansi.Style.fg t.default_fg
        |> Ansi.Style.bg t.default_bg
      in
      let line = String.make t.cols 'E' in
      for row = 0 to t.rows - 1 do
        Grid.draw_text t.active_grid ~x:0 ~y:row ~text:line ~style
      done;
      mark_rows_dirty t 0 (t.rows - 1);
      set_cursor_pos t ~row:0 ~col:0
  | Ansi.Parser.SL n -> shift_columns t ~x:0 n
  | Ansi.Parser.SR n -> shift_columns t ~x:0 (-n)
  | Ansi.Parser.DECIC n ->
      clear_pending_wrap t;
      shift_columns t ~x:t.cursor.col (-n)
  | Ansi.Parser.DECDC n ->
      clear_pending_wrap t;
      shift_columns t ~x:t.cursor.col n
  | Ansi.Parser.SD n -> scroll_down t n
  | Ansi.Parser.VPR n ->
      clear_pending_wrap t;
      set_cursor_pos t ~row:(t.cursor.row + n) ~col:t.cursor.col
  | Ansi.Parser.OSC (0, title) | Ansi.Parser.OSC (2, title) -> t.title <- title
  | Ansi.Parser.OSC _ -> () (* Ignore other OSC *)
  | Ansi.Parser.CHT n ->
      if t.cursor.col < t.cols then (
        let col = ref t.cursor.col in
        for _ = 1 to min n t.cols do
          col := next_tab t !col
        done;
        set_cursor_pos t ~row:t.cursor.row ~col:!col)
  | Ansi.Parser.CBT n ->
      if t.cursor.col < t.cols then (
        let col = ref t.cursor.col in
        for _ = 1 to min n t.cols do
          col := prev_tab t !col
        done;
        set_cursor_pos t ~row:t.cursor.row ~col:!col)
  | Ansi.Parser.HTS ->
      if t.cursor.col < t.cols then t.tabs.(t.cursor.col) <- true
  | Ansi.Parser.TBC 0 ->
      if t.cursor.col < t.cols then t.tabs.(t.cursor.col) <- false
  | Ansi.Parser.TBC 3 -> Array.fill t.tabs 0 t.cols false
  | Ansi.Parser.TBC _ -> ()
  | Ansi.Parser.DCS _ | Ansi.Parser.APC _ | Ansi.Parser.PM _ | Ansi.Parser.SOS _
    ->
      ()
  | Ansi.Parser.DA -> Buffer.add_string t.replies t.da1
  | Ansi.Parser.DA2 -> Buffer.add_string t.replies t.da2
  | Ansi.Parser.DSR 5 -> Buffer.add_string t.replies "\027[0n"
  | Ansi.Parser.DSR 6 ->
      let row, col = cursor_report t in
      Printf.bprintf t.replies "\027[%d;%dR" row col
  | Ansi.Parser.DECXCPR ->
      let row, col = cursor_report t in
      Printf.bprintf t.replies "\027[?%d;%d;1R" row col
  | Ansi.Parser.DSR _ -> ()
  | Ansi.Parser.Hyperlink payload -> (
      match payload with
      | None -> t.style <- Ansi.Style.unlink t.style
      | Some (_, url) -> t.style <- Ansi.Style.hyperlink url t.style)
  | Ansi.Parser.DECSC -> decsc t
  | Ansi.Parser.DECRC -> decrc t
  | Ansi.Parser.SCS (g, c) -> t.charsets.(g) <- c
  | Ansi.Parser.LS g -> t.gl <- g
  | Ansi.Parser.RI ->
      if t.cursor.row = t.scroll_region.top then scroll_down t 1
      else set_cursor_pos t ~row:(t.cursor.row - 1) ~col:t.cursor.col
  | Ansi.Parser.Reset -> hard_reset t
  | Ansi.Parser.DECSTR -> soft_reset t
  | Ansi.Parser.SM modes -> List.iter (fun m -> set_ansi_mode t m true) modes
  | Ansi.Parser.RM modes -> List.iter (fun m -> set_ansi_mode t m false) modes
  | Ansi.Parser.DECSET modes -> List.iter (fun m -> set_dec_mode t m true) modes
  | Ansi.Parser.DECRST modes ->
      List.iter (fun m -> set_dec_mode t m false) modes
  | Ansi.Parser.REP _ | Ansi.Parser.Unknown _ -> ()

let handle_token t token =
  match token with
  | Ansi.Parser.Text s -> handle_text t s
  | Ansi.Parser.SGR attrs ->
      t.repeatable <- false;
      let new_style = List.fold_left apply_sgr_attr t.style attrs in
      if new_style <> t.style then (
        t.style <- new_style;
        mark_dirty t)
  | Ansi.Parser.Control (Ansi.Parser.REP n) -> repeat_preceding t n
  | Ansi.Parser.Control ctrl ->
      t.repeatable <- false;
      handle_control t ctrl

let feed t bytes ofs len =
  Ansi.Parser.feed t.parser bytes ~off:ofs ~len (handle_token t)

let feed_string t str =
  let bytes = Bytes.unsafe_of_string str in
  feed t bytes 0 (String.length str)

let reset t =
  hard_reset t;
  Ansi.Parser.reset t.parser

let to_string t =
  let grid = t.active_grid in
  let rows = t.rows in
  let cols = t.cols in
  let lines = ref [] in

  for row = 0 to rows - 1 do
    let buf = Buffer.create cols in
    let col = ref 0 in
    while !col < cols do
      let idx = (row * cols) + !col in
      if Grid.is_empty grid idx then (
        Buffer.add_char buf ' ';
        incr col)
      else if Grid.is_continuation grid idx then
        (* Part of a wide grapheme already emitted. *)
        incr col
      else
        let s = Grid.get_text grid idx in
        if s = "" then (
          Buffer.add_char buf ' ';
          incr col)
        else (
          Buffer.add_string buf s;
          let w = Grid.cell_width grid idx in
          let adv = if w <= 0 then 1 else w in
          col := !col + adv)
    done;
    let line = Buffer.contents buf in
    lines := line :: !lines
  done;
  String.concat "\n" (List.rev !lines)
