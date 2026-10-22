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
  saved_protect : bool;
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
    saved_protect = false;
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

(* An OSC is kept only when the VTE performs it: [Osc_number n] while its
   number is read (-1 before the first digit), [Osc_text n] once it is known
   to be kept, and [Osc_skip] otherwise. *)
type osc = Osc_skip | Osc_number of int | Osc_text of int

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
  (* Parser, and the OSC in progress *)
  parser : Ansi.Parser.t;
  mutable osc : osc;
  osc_text : Buffer.t;
  (* Text was the last thing printed, so REP has a character to repeat *)
  mutable repeatable : bool;
  (* The last grapheme printed did not fit at the right margin and was
     dropped, so marks that follow it in a later write are dropped too *)
  mutable dropped : bool;
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
  mutable reverse_wrap : bool;
  mutable cursor_key_mode : bool;
  mutable insert_mode : bool;
  (* Character sets: the designations of G0 to G3, and the one invoked into
     GL *)
  charsets : char array;
  mutable gl : int;
  (* DECSCA: the characters written next are protected *)
  mutable protect : bool;
  (* Selective erasures spare protected cells, from a DECSCA to a reset *)
  mutable selective : bool;
  (* The protected cells of each screen, a byte a cell, kept from the first
     DECSCA 1 to RIS *)
  mutable tracked : bool;
  mutable protected_primary : Bytes.t;
  mutable protected_alternate : Bytes.t;
  (* The rows of each screen that auto-wrap continued on the next row *)
  mutable wrapped_primary : Bytes.t;
  mutable wrapped_alternate : Bytes.t;
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
    osc = Osc_skip;
    osc_text = Buffer.create 64;
    repeatable = false;
    dropped = false;
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
    reverse_wrap = false;
    cursor_key_mode = false;
    insert_mode = false;
    charsets = Array.make 4 'B';
    gl = 0;
    protect = false;
    selective = false;
    tracked = false;
    protected_primary = Bytes.make (rows * cols) '\000';
    protected_alternate = Bytes.make (rows * cols) '\000';
    wrapped_primary = Bytes.make rows '\000';
    wrapped_alternate = Bytes.make rows '\000';
    bracketed_paste = false;
    default_fg;
    default_bg;
  }

let protected_cells t =
  if t.active_grid == t.alternate then t.protected_alternate
  else t.protected_primary

let wrapped_rows t =
  if t.active_grid == t.alternate then t.wrapped_alternate
  else t.wrapped_primary

(* Mark [row] as continued on the next row by auto-wrap, or not. *)
let set_wrapped t row w =
  if row >= 0 && row < t.rows then
    Bytes.set (wrapped_rows t) row (if w then '\001' else '\000')

let is_wrapped t row = row >= 0 && Bytes.get (wrapped_rows t) row <> '\000'

(* Clear the wrap mark of [height] rows from [y], which an erasure blanked. *)
let unwrap_rows t ~y ~height =
  for row = y to y + height - 1 do
    set_wrapped t row false
  done

(* Mark [width] cells from [x] on row [y] protected or not. *)
let set_protected t ~x ~y ~width p =
  let x = max 0 x in
  let width = min width (t.cols - x) in
  if t.tracked && width > 0 && y >= 0 && y < t.rows then
    Bytes.fill (protected_cells t)
      ((y * t.cols) + x)
      width
      (if p then '\001' else '\000')

(* Copy [width] cells, with their protection. *)
let copy_cells t ~src_x ~src_y ~dst_x ~dst_y ~width =
  Grid.blit_region ~src:t.active_grid ~dst:t.active_grid ~src_x ~src_y ~width
    ~height:1 ~dst_x ~dst_y;
  if t.tracked then
    let cells = protected_cells t in
    Bytes.blit cells
      ((src_y * t.cols) + src_x)
      cells
      ((dst_y * t.cols) + dst_x)
      width

(* Efficient destructive erase using the new Grid API: replace region with
   spaces, with BG matching current style (or default). The colour keeps its
   palette index, so a theme can recolour the fill as it does text. *)
let erase_region t ~x ~y ~width ~height =
  if width > 0 && height > 0 then (
    let base_bg =
      match t.style.Ansi.Style.bg with Some c -> c | None -> t.default_bg
    in
    let base_bg =
      match Ansi.Color.intent base_bg with
      | Default -> t.default_bg
      | Indexed _ | Rgb -> base_bg
    in
    Grid.fill_rect t.active_grid ~x ~y ~width ~height ~color:base_bg;
    for row = y to y + height - 1 do
      set_protected t ~x ~y:row ~width false
    done)

(* Erase as [erase_region]. A selective erasure (DECSED, DECSEL) spares the
   cells DECSCA protects, as xterm does from a DECSCA to the next reset. *)
let erase_cells t ~selective ~x ~y ~width ~height =
  if not (selective && t.selective && t.tracked) then
    erase_region t ~x ~y ~width ~height
  else
    let cells = protected_cells t in
    for row = y to y + height - 1 do
      let base = row * t.cols in
      let col = ref x in
      while !col < x + width do
        let start = !col in
        while !col < x + width && Bytes.get cells (base + !col) = '\000' do
          incr col
        done;
        erase_region t ~x:start ~y:row ~width:(!col - start) ~height:1;
        while !col < x + width && Bytes.get cells (base + !col) <> '\000' do
          incr col
        done
      done
    done

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
    t.protected_primary <- Bytes.make (rows * cols) '\000';
    t.protected_alternate <- Bytes.make (rows * cols) '\000';
    t.wrapped_primary <- Bytes.make rows '\000';
    t.wrapped_alternate <- Bytes.make rows '\000';
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

(* The national replacement sets: the characters each replaces, and their
   replacements in order. *)
let national = function
  | 'A' -> Some ("#", "£")
  | '4' -> Some ("#@[\\]{|}~", "£¾ĳ½|¨ƒ¼´")
  | 'C' | '5' -> Some ("[\\]^`{|}~", "ÄÖÅÜéäöåü")
  | 'R' -> Some ("#@[\\]{|}~", "£à°ç§éùè¨")
  | 'Q' -> Some ("@[\\]^`{|}~", "àâçêîôéùèû")
  | 'K' -> Some ("@[\\]{|}~", "§ÄÖÜäöüß")
  | 'Y' -> Some ("#@[\\]`{|}~", "£§°çéùàòèì")
  | 'E' | '6' -> Some ("@[\\]^`{|}~", "ÄÆØÅÜäæøåü")
  | 'Z' -> Some ("#@[\\]{|}", "£§¡Ñ¿°ñç")
  | 'H' | '7' -> Some ("@[\\]^`{|}~", "ÉÄÖÅÜéäöåü")
  | '=' -> Some ("#@[\\]^_`{|}~", "ùàéçêîèôäöüû")
  | _ -> None

(* The [n]th character of [s] from byte [i]. *)
let rec nth_char s i n =
  let len = Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
  if n = 0 then String.sub s i len else nth_char s (i + len) (n - 1)

(* Map text through the set invoked into GL; any other set is ASCII. *)
let translate_charset t text =
  let map =
    match t.charsets.(t.gl) with
    | '0' -> Some dec_graphics
    | set ->
        Option.map
          (fun (chars, by) c ->
            Option.map (nth_char by 0) (String.index_opt chars c))
          (national set)
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

(* Move [rows] of [stride] bytes from [top] to [bottom] by [n] rows, up for
   [n > 0] and down for [n < 0], and clear the rows the move vacates. *)
let shift_rows bytes ~stride ~top ~bottom n =
  let k = abs n in
  let keep = (bottom - top + 1 - k) * stride in
  if n > 0 then (
    Bytes.blit bytes ((top + k) * stride) bytes (top * stride) keep;
    Bytes.fill bytes ((bottom - k + 1) * stride) (k * stride) '\000')
  else (
    Bytes.blit bytes (top * stride) bytes ((top + k) * stride) keep;
    Bytes.fill bytes (top * stride) (k * stride) '\000')

(* Move the wrap marks and the protection of the scroll region's rows by [n]
   rows, as [shift_region] moves the cells. *)
let scroll_marks t n =
  let top = t.scroll_region.top and bottom = t.scroll_region.bottom in
  shift_rows (wrapped_rows t) ~stride:1 ~top ~bottom n;
  if t.tracked then shift_rows (protected_cells t) ~stride:t.cols ~top ~bottom n

(* Lines a scroll brings in take the current background when the style sets
   one (bce), as erased cells do. *)
let fill_scrolled_in t ~y ~height =
  match t.style.Ansi.Style.bg with
  | Some c when Ansi.Color.intent c <> Default ->
      erase_region t ~x:0 ~y ~width:t.cols ~height
  | Some _ | None -> ()

(* Move the scroll region's rows up by [n] rows, or down for [n < 0]. The
   rows brought in are transparent, then [fill_scrolled_in] colours them. *)
let shift_region t n =
  let top = t.scroll_region.top and bottom = t.scroll_region.bottom in
  Grid.shift t.active_grid
    { x = 0; y = top; width = t.cols; height = bottom - top + 1 }
    ~dx:0 ~dy:(-n) ~color:Ansi.Color.transparent

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

    shift_region t n;
    scroll_marks t n;
    fill_scrolled_in t ~y:(t.scroll_region.bottom - n + 1) ~height:n;

    mark_rows_dirty t t.scroll_region.top t.scroll_region.bottom

let scroll_down t n =
  if n <= 0 then ()
  else
    let n = min n (t.scroll_region.bottom - t.scroll_region.top + 1) in

    shift_region t (-n);
    scroll_marks t (-n);
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
          copy_cells t ~src_x:(x + span) ~src_y:row ~width:keep ~dst_x:x
            ~dst_y:row;
        erase_region t ~x:(t.cols - span) ~y:row ~width:span ~height:1)
      else (
        blank_cut_wide t row (t.cols - span);
        if keep > 0 then
          copy_cells t ~src_x:x ~src_y:row ~width:keep ~dst_x:(x + span)
            ~dst_y:row;
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
      else if t.repeatable && t.dropped then
        String.sub text stop (String.length text - stop)
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
  if text <> "" then t.dropped <- false;
  if text = "" then ()
  else if (not t.insert_mode) && String.length text <= t.cols - t.cursor.col
  then begin
    (* Printable UTF-8 never occupies more columns than bytes. This bounded
       fit check keeps short writes allocation-light without rescanning suffixes. *)
    Grid.draw_text t.active_grid ~x:t.cursor.col ~y:t.cursor.row ~text ~style;
    mark_row_dirty t t.cursor.row;
    let width = Text.measure ~width_method ~tab_width:2 text in
    set_protected t ~x:t.cursor.col ~y:t.cursor.row ~width t.protect;
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
        set_protected t ~x:t.cursor.col ~y:t.cursor.row ~width:!columns
          t.protect;
        mark_row_dirty t t.cursor.row;
        set_cursor_pos t ~row:t.cursor.row ~col:(t.cursor.col + !columns);
        columns := 0
      end
    in
    let emit_unit ~offset ~len ~width =
      if width = 0 then
        begin if !columns > 0 then stop := offset + len
        else if t.dropped then ()
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
          set_wrapped t t.cursor.row true;
          advance_row t;
          set_cursor_pos t ~row:t.cursor.row ~col:0
        end;
        if
          (not t.insert_mode)
          && width > t.cols - t.cursor.col
          && t.auto_wrap_mode && t.cursor.col > 0
        then begin
          set_wrapped t t.cursor.row true;
          advance_row t;
          set_cursor_pos t ~row:t.cursor.row ~col:0
        end;
        if t.insert_mode || width > t.cols - t.cursor.col then begin
          let x = min t.cursor.col (t.cols - 1) in
          let inserted = min width (t.cols - t.cursor.col) in
          t.dropped <- width > t.cols - x;
          if t.insert_mode && inserted > 0 then begin
            copy_cells t ~src_x:x ~src_y:t.cursor.row
              ~width:(t.cols - x - inserted)
              ~dst_x:(x + inserted) ~dst_y:t.cursor.row;
            erase_region t ~x ~y:t.cursor.row ~width:inserted ~height:1
          end;
          Grid.draw_text t.active_grid ~x ~y:t.cursor.row
            ~text:(String.sub text offset len)
            ~style;
          set_protected t ~x ~y:t.cursor.row
            ~width:(min width (t.cols - x))
            t.protect;
          mark_row_dirty t t.cursor.row;
          if t.insert_mode && inserted > 0 then
            set_cursor_pos t ~row:t.cursor.row ~col:(x + inserted)
        end
        else begin
          t.dropped <- false;
          if !columns = 0 then start := offset;
          stop := offset + len;
          columns := !columns + width
        end
      end
    in
    (* Text.iter_grapheme_info skips graphemes of no width. Those met in the
       run, such as U+200B, join the cell before them, as in xterm. *)
    let next = ref 0 in
    let emit ~offset ~len ~width =
      if offset > !next then
        emit_unit ~offset:!next ~len:(offset - !next) ~width:0;
      next := offset + len;
      emit_unit ~offset ~len ~width
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
    if !next < String.length text then
      emit_unit ~offset:!next ~len:(String.length text - !next) ~width:0;
    flush ()

(* A Prepend character joins the character after it into its grapheme, which
   a later write cannot join. xterm writes each character as it comes, so
   write the text after a Prepend character apart: a read split there then
   draws the screen a whole one does. *)
let put_run t text =
  let len = String.length text in
  let rec go start i =
    if i >= len then (
      if start = 0 then put_text t text
      else if start < len then put_text t (String.sub text start (len - start)))
    else if Char.code (String.unsafe_get text i) < 0xD8 then
      (* ASCII, a continuation byte, or a character below U+0600 *)
      go start (i + 1)
    else
      let d = String.get_utf_8_uchar text i in
      let next = i + Uchar.utf_decode_length d in
      if next < len && Text.is_prepend (Uchar.utf_decode_uchar d) then (
        put_text t (String.sub text start (next - start));
        go next next)
      else go start next
  in
  go 0 0

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

(* BS and CUB move [n] columns left. With reverse-wrap (mode 45) and
   auto-wrap set, a pending wrap takes the first step and a move past the
   first column continues from the last column of the row above, while that
   row wrapped onto this one, as in xterm. *)
let cursor_back t n =
  let rev = t.reverse_wrap && t.auto_wrap_mode in
  let row = ref t.cursor.row and col = ref (min t.cursor.col (t.cols - 1)) in
  let count = ref n in
  if n > 0 then if rev && t.cursor.col >= t.cols then decr count else decr col;
  let rec go () =
    if !col < 0 && not (rev && is_wrapped t (!row - 1)) then col := 0
    else (
      if !col < 0 then (
        decr row;
        col := t.cols - 1);
      decr count;
      if !count > 0 then (
        decr col;
        go ()))
  in
  go ();
  set_cursor_pos t ~row:!row ~col:!col

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
      put_run t (Buffer.contents buf))

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
      saved_protect = t.protect;
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
  t.protect <- saved.saved_protect;
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
    Bytes.fill t.protected_alternate 0
      (Bytes.length t.protected_alternate)
      '\000';
    Bytes.fill t.wrapped_alternate 0 t.rows '\000';
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
  t.reverse_wrap <- false;
  t.cursor_key_mode <- false;
  t.insert_mode <- false;
  Array.fill t.charsets 0 4 'B';
  t.gl <- 0;
  t.protect <- false;
  t.selective <- false;
  t.tracked <- false;
  Bytes.fill t.protected_primary 0 (Bytes.length t.protected_primary) '\000';
  Bytes.fill t.protected_alternate 0 (Bytes.length t.protected_alternate) '\000';
  Bytes.fill t.wrapped_primary 0 t.rows '\000';
  Bytes.fill t.wrapped_alternate 0 t.rows '\000';
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
  t.reverse_wrap <- false;
  t.cursor_key_mode <- false;
  t.style <- Ansi.Style.default;
  t.scroll_region.top <- 0;
  t.scroll_region.bottom <- t.rows - 1;
  Array.fill t.charsets 0 4 'B';
  t.gl <- 0;
  t.protect <- false;
  t.selective <- false;
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
  | 45 -> t.reverse_wrap <- set
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

(* ED and DECSED. *)
let erase_display t ~selective n =
  let erase = erase_cells t ~selective in
  match n with
  | 0 ->
      erase ~x:t.cursor.col ~y:t.cursor.row ~width:(t.cols - t.cursor.col)
        ~height:1;
      erase ~x:0 ~y:(t.cursor.row + 1) ~width:t.cols
        ~height:(t.rows - (t.cursor.row + 1));
      unwrap_rows t ~y:t.cursor.row ~height:(t.rows - t.cursor.row);
      mark_rows_dirty t t.cursor.row (t.rows - 1)
  | 1 ->
      erase ~x:0 ~y:0 ~width:t.cols ~height:t.cursor.row;
      erase ~x:0 ~y:t.cursor.row ~width:(t.cursor.col + 1) ~height:1;
      unwrap_rows t ~y:0 ~height:t.cursor.row;
      mark_rows_dirty t 0 t.cursor.row
  | 2 ->
      erase ~x:0 ~y:0 ~width:t.cols ~height:t.rows;
      unwrap_rows t ~y:0 ~height:t.rows;
      mark_rows_dirty t 0 (t.rows - 1)
  | 3 -> clear_scrollback t
  | _ -> ()

(* EL and DECSEL. *)
let erase_line t ~selective n =
  let erase ~x ~width =
    erase_cells t ~selective ~x ~y:t.cursor.row ~width ~height:1
  in
  (match n with
  | 0 ->
      erase ~x:t.cursor.col ~width:(t.cols - t.cursor.col);
      set_wrapped t t.cursor.row false
  | 1 -> erase ~x:0 ~width:(t.cursor.col + 1)
  | 2 -> erase ~x:0 ~width:t.cols
  | _ -> ());
  mark_row_dirty t t.cursor.row

(* Control functions, one each, named as in ECMA-48 and the DEC manuals. A
   count [n] is at least 1. *)

let bel _t = ()

(* BS: one column left. Never erases: overstrike sequences (nroff bold
   "c\bc") rely on the cell surviving. *)
let bs t = cursor_back t 1

(* HT: a pending wrap stays pending, as in xterm. *)
let ht t =
  if t.cursor.col < t.cols then
    set_cursor_pos t ~row:t.cursor.row ~col:(next_tab t t.cursor.col)

(* LF, VT and FF: down one row, same column (VT100). ONLCR translation is the
   tty driver's job; curses cursor-motion optimizations depend on bare LF not
   implying CR. *)
let lf t =
  clear_pending_wrap t;
  advance_row t

let cr t = set_cursor_pos t ~row:t.cursor.row ~col:0

(* SO, SI, LS2 and LS3 invoke G1, G0, G2 and G3 into GL. *)
let ls t g = t.gl <- g

let ind t =
  clear_pending_wrap t;
  advance_row t

let nel t =
  advance_row t;
  set_cursor_pos t ~row:t.cursor.row ~col:0

let hts t = if t.cursor.col < t.cols then t.tabs.(t.cursor.col) <- true

let ri t =
  clear_pending_wrap t;
  if t.cursor.row = t.scroll_region.top then scroll_down t 1
  else set_cursor_pos t ~row:(t.cursor.row - 1) ~col:t.cursor.col

let cuu t n =
  clear_pending_wrap t;
  set_cursor_pos t ~row:(row_up t n) ~col:t.cursor.col

let cud t n =
  clear_pending_wrap t;
  set_cursor_pos t ~row:(row_down t n) ~col:t.cursor.col

let cuf t n =
  clear_pending_wrap t;
  move_cursor t ~row:t.cursor.row ~col:(t.cursor.col + n)

let cub t n = cursor_back t n
let cnl t n = set_cursor_pos t ~row:(row_down t n) ~col:0
let cpl t n = set_cursor_pos t ~row:(row_up t n) ~col:0
let cha t n = move_cursor t ~row:t.cursor.row ~col:(n - 1)

let vpa t n =
  clear_pending_wrap t;
  set_cursor_pos t ~row:(origin_row t (n - 1)) ~col:t.cursor.col

(* VPR: down [n] rows, ignoring the margins. *)
let vpr t n =
  clear_pending_wrap t;
  set_cursor_pos t ~row:(t.cursor.row + n) ~col:t.cursor.col

let cup t row col = move_cursor t ~row:(origin_row t (row - 1)) ~col:(col - 1)

let move_tabs t n next =
  if t.cursor.col < t.cols then (
    let col = ref t.cursor.col in
    for _ = 1 to min n t.cols do
      col := next t !col
    done;
    set_cursor_pos t ~row:t.cursor.row ~col:!col)

let cht t n = move_tabs t n next_tab
let cbt t n = move_tabs t n prev_tab

let tbc t n =
  match n with
  | 0 -> if t.cursor.col < t.cols then t.tabs.(t.cursor.col) <- false
  | 3 -> Array.fill t.tabs 0 t.cols false
  | _ -> ()

let ed t n =
  clear_pending_wrap t;
  erase_display t ~selective:false n

let decsed t n =
  clear_pending_wrap t;
  erase_display t ~selective:true n

let el t n =
  clear_pending_wrap t;
  erase_line t ~selective:false n

let decsel t n =
  clear_pending_wrap t;
  erase_line t ~selective:true n

let ech t n =
  clear_pending_wrap t;
  erase_region t ~x:t.cursor.col ~y:t.cursor.row
    ~width:(min n (t.cols - t.cursor.col))
    ~height:1;
  set_wrapped t t.cursor.row false;
  mark_row_dirty t t.cursor.row

let decsca t n =
  t.selective <- true;
  match n with
  | 0 | 2 -> t.protect <- false
  | 1 ->
      t.protect <- true;
      t.tracked <- true
  | _ -> ()

let il t n =
  clear_pending_wrap t;
  if in_scroll_region t then (
    let insert_count = min n (t.scroll_region.bottom - t.cursor.row + 1) in
    for row = t.scroll_region.bottom downto t.cursor.row + insert_count do
      copy_cells t ~src_x:0 ~src_y:(row - insert_count) ~width:t.cols ~dst_x:0
        ~dst_y:row;
      set_wrapped t row (is_wrapped t (row - insert_count))
    done;
    erase_region t ~x:0 ~y:t.cursor.row ~width:t.cols ~height:insert_count;
    unwrap_rows t ~y:t.cursor.row ~height:insert_count;
    mark_rows_dirty t t.cursor.row t.scroll_region.bottom;
    set_cursor_pos t ~row:t.cursor.row ~col:0)

let dl t n =
  clear_pending_wrap t;
  if in_scroll_region t then (
    let delete_count = min n (t.scroll_region.bottom - t.cursor.row + 1) in
    for row = t.cursor.row to t.scroll_region.bottom - delete_count do
      copy_cells t ~src_x:0 ~src_y:(row + delete_count) ~width:t.cols ~dst_x:0
        ~dst_y:row;
      set_wrapped t row (is_wrapped t (row + delete_count))
    done;
    erase_region t ~x:0
      ~y:(t.scroll_region.bottom - delete_count + 1)
      ~width:t.cols ~height:delete_count;
    unwrap_rows t
      ~y:(t.scroll_region.bottom - delete_count + 1)
      ~height:delete_count;
    mark_rows_dirty t t.cursor.row t.scroll_region.bottom;
    set_cursor_pos t ~row:t.cursor.row ~col:0)

let dch t n =
  clear_pending_wrap t;
  let row = t.cursor.row in
  if row >= 0 && row < t.rows then
    let start_col = edit_start t row in
    if start_col < t.cols then
      let delete_span = min n (t.cols - start_col) in
      if delete_span > 0 then (
        let src_x = start_col + delete_span in
        blank_cut_wide t row src_x;
        let move_width = t.cols - src_x in
        if move_width > 0 then
          copy_cells t ~src_x ~src_y:row ~width:move_width ~dst_x:start_col
            ~dst_y:row;
        erase_region t ~x:(t.cols - delete_span) ~y:row ~width:delete_span
          ~height:1;
        set_wrapped t row false;
        mark_row_dirty t row)

(* ICH shifts the content right by the span and clears the gap, the mirror
   of DCH. Grid.blit_region handles overlapping same-grid blits by iterating
   right-to-left when src_x < dst_x. *)
let ich t n =
  clear_pending_wrap t;
  let row = t.cursor.row in
  if row >= 0 && row < t.rows then
    let start_col = edit_start t row in
    if start_col < t.cols then
      let insert_span = min n (t.cols - start_col) in
      if insert_span > 0 then (
        let move_width = t.cols - start_col - insert_span in
        if move_width > 0 then (
          copy_cells t ~src_x:start_col ~src_y:row ~width:move_width
            ~dst_x:(start_col + insert_span) ~dst_y:row;
          (* A wide character cut at the right edge by the shift is
             erased. *)
          let right_start, right_w =
            grapheme_start_and_width t.active_grid row (t.cols - 1)
          in
          if right_start + right_w > t.cols then
            erase_region t ~x:right_start ~y:row ~width:(t.cols - right_start)
              ~height:1);
        erase_region t ~x:start_col ~y:row ~width:insert_span ~height:1;
        mark_row_dirty t row)

let decstbm t top bottom =
  let top = min top t.rows in
  let bottom = min bottom t.rows in
  if top < bottom then (
    t.scroll_region.top <- top - 1;
    t.scroll_region.bottom <- bottom - 1;
    set_cursor_pos t ~row:(origin_row t 0) ~col:0)

let su t n = scroll_up t n
let sd t n = scroll_down t n

(* As xterm: reset the margins, fill with E in the default rendition, and
   home the cursor. *)
let decaln t =
  t.scroll_region.top <- 0;
  t.scroll_region.bottom <- t.rows - 1;
  let style =
    Ansi.Style.default |> Ansi.Style.fg t.default_fg
    |> Ansi.Style.bg t.default_bg
  in
  let line = String.make t.cols 'E' in
  for row = 0 to t.rows - 1 do
    Grid.draw_text t.active_grid ~x:0 ~y:row ~text:line ~style;
    set_protected t ~x:0 ~y:row ~width:t.cols false
  done;
  mark_rows_dirty t 0 (t.rows - 1);
  set_cursor_pos t ~row:0 ~col:0

let sl t n = shift_columns t ~x:0 n
let sr t n = shift_columns t ~x:0 (-n)

let decic t n =
  clear_pending_wrap t;
  shift_columns t ~x:t.cursor.col (-n)

let decdc t n =
  clear_pending_wrap t;
  shift_columns t ~x:t.cursor.col n

let rep t n = repeat_preceding t n

let sgr t p =
  let style = Ansi.Style.apply_sgr p t.style in
  if not (Ansi.Style.equal style t.style) then (
    t.style <- style;
    mark_dirty t)

let da t = Buffer.add_string t.replies t.da1
let da2 t = Buffer.add_string t.replies t.da2

let dsr t n =
  match n with
  | 5 -> Buffer.add_string t.replies "\027[0n"
  | 6 ->
      let row, col = cursor_report t in
      Printf.bprintf t.replies "\027[%d;%dR" row col
  | _ -> ()

let decxcpr t =
  let row, col = cursor_report t in
  Printf.bprintf t.replies "\027[?%d;%d;1R" row col

let ris t = hard_reset t
let decstr t = soft_reset t

(* SM, RM, DECSET and DECRST: every mode listed, absent ones skipped. *)
let set_modes t p set_mode set =
  for i = 0 to Ansi.Parser.Params.count p - 1 do
    Option.iter (fun mode -> set_mode t mode set) (Ansi.Parser.Params.value p i)
  done

let sm t p = set_modes t p set_ansi_mode true
let rm t p = set_modes t p set_ansi_mode false
let decset t p = set_modes t p set_dec_mode true
let decrst t p = set_modes t p set_dec_mode false

(* SCS: designate the 94-character set named by [final] as G[g]. *)
let scs t g final = t.charsets.(g) <- final

(* OSC 0 and 2 set the title, OSC 8 opens a hyperlink, or closes it when the
   URI is empty: [8 ; params ; URI]. *)
let osc_title t text = t.title <- text

let osc_hyperlink t text =
  match String.index_opt text ';' with
  | None -> ()
  | Some i ->
      if i + 1 = String.length text then t.style <- Ansi.Style.unlink t.style
      else
        let uri = String.sub text (i + 1) (String.length text - i - 1) in
        t.style <- Ansi.Style.hyperlink uri t.style

(* Dispatch: a function is named by the private marker, the intermediates
   and the final byte (ECMA-48 §5.4; ECMA-35 §13.1). Counts and positions
   read 0 as their default (Params.pn); selectors do not (Params.ps). *)

let control t c =
  match c with
  | '\x07' -> bel t
  | '\x08' -> bs t
  | '\x09' -> ht t
  | '\x0a' | '\x0b' | '\x0c' -> lf t
  | '\x0d' -> cr t
  | '\x0e' -> ls t 1
  | '\x0f' -> ls t 0
  | '\x84' -> ind t
  | '\x85' -> nel t
  | '\x88' -> hts t
  | '\x8d' -> ri t
  | _ -> ()

let esc t intermediates final =
  match (intermediates, final) with
  | "", 'c' -> ris t
  | "", '7' -> decsc t
  | "", '8' -> decrc t
  | "", 'n' -> ls t 2
  | "", 'o' -> ls t 3
  | "#", '8' -> decaln t
  | "(", f -> scs t 0 f
  | ")", f -> scs t 1 f
  | "*", f -> scs t 2 f
  | "+", f -> scs t 3 f
  | _ -> ()

let csi t (s : Ansi.Parser.sequence) =
  let p = s.params in
  let pn i = Ansi.Parser.Params.pn p i ~default:1 in
  let ps i = Ansi.Parser.Params.ps p i ~default:0 in
  match (s.marker, s.intermediates, s.final) with
  | None, "", 'A' -> cuu t (pn 0)
  | None, "", 'B' -> cud t (pn 0)
  | None, "", ('C' | 'a') -> cuf t (pn 0) (* HPR is CUF on this device *)
  | None, "", 'D' -> cub t (pn 0)
  | None, "", 'E' -> cnl t (pn 0)
  | None, "", 'F' -> cpl t (pn 0)
  | None, "", ('G' | '`') -> cha t (pn 0) (* HPA is CHA on this device *)
  | None, "", 'd' -> vpa t (pn 0)
  | None, "", 'e' -> vpr t (pn 0)
  | None, "", ('H' | 'f') -> cup t (pn 0) (pn 1) (* HVP is CUP *)
  | None, "", 'I' -> cht t (pn 0)
  | None, "", 'Z' -> cbt t (pn 0)
  | None, "", 'g' -> tbc t (ps 0)
  | None, "", 'J' -> ed t (ps 0)
  | None, "", 'K' -> el t (ps 0)
  | None, "", 'X' -> ech t (pn 0)
  | None, "", 'L' -> il t (pn 0)
  | None, "", 'M' -> dl t (pn 0)
  | None, "", 'P' -> dch t (pn 0)
  | None, "", '@' -> ich t (pn 0)
  | None, "", 'r' ->
      decstbm t (pn 0) (Ansi.Parser.Params.pn p 1 ~default:t.rows)
  | None, "", 'S' -> su t (pn 0)
  | None, "", 'T' -> sd t (pn 0)
  | None, "", 'm' -> sgr t p
  | None, "", 'c' when ps 0 = 0 -> da t
  | None, "", 'n' -> dsr t (ps 0)
  | None, "", 's' -> decsc t (* SCOSC *)
  | None, "", 'u' -> decrc t (* SCORC *)
  | None, "", 'h' -> sm t p
  | None, "", 'l' -> rm t p
  | Some '?', "", 'h' -> decset t p
  | Some '?', "", 'l' -> decrst t p
  | Some '?', "", 'J' -> decsed t (ps 0)
  | Some '?', "", 'K' -> decsel t (ps 0)
  | Some '?', "", 'n' when ps 0 = 6 -> decxcpr t
  | Some '>', "", 'c' when ps 0 = 0 -> da2 t
  | None, "!", 'p' -> decstr t
  | None, "\"", 'q' -> decsca t (ps 0)
  | None, " ", '@' -> sl t (pn 0)
  | None, " ", 'A' -> sr t (pn 0)
  | None, "'", '}' -> decic t (pn 0)
  | None, "'", '~' -> decdc t (pn 0)
  | _ -> ()

(* Strings. Only an OSC the VTE performs is kept, and only up to
   [osc_budget] bytes; a longer one is dropped whole, as xterm drops an
   over-long string. It is performed when it ends with ST or BEL, never when
   it is cut short. *)

let osc_budget = 65536

let osc_drop t =
  t.osc <- Osc_skip;
  Buffer.reset t.osc_text

let osc_keep t s off =
  let len = String.length s - off in
  if Buffer.length t.osc_text + len > osc_budget then osc_drop t
  else Buffer.add_substring t.osc_text s off len

(* Ps, the number before the first [;], decides whether the rest is kept. *)
let rec osc_number t s i n =
  if i >= String.length s then t.osc <- Osc_number n
  else
    match s.[i] with
    | '0' .. '9' as c ->
        osc_number t s (i + 1) (min 1000 ((max n 0 * 10) + Char.code c - 48))
    | ';' when n = 0 || n = 2 || n = 8 ->
        t.osc <- Osc_text n;
        osc_keep t s (i + 1)
    | _ -> osc_drop t

let open_string t (o : Ansi.Parser.opening) =
  match o with Osc -> t.osc <- Osc_number (-1) | _ -> t.osc <- Osc_skip

let string_data t s =
  match t.osc with
  | Osc_skip -> ()
  | Osc_number n -> osc_number t s 0 n
  | Osc_text _ -> osc_keep t s 0

let close_string t ending =
  (match (t.osc, ending) with
  | Osc_text (0 | 2), (`St | `Bel) -> osc_title t (Buffer.contents t.osc_text)
  | Osc_text 8, (`St | `Bel) -> osc_hyperlink t (Buffer.contents t.osc_text)
  | _ -> ());
  osc_drop t

let handle_token t token =
  match token with
  | Ansi.Parser.Text s ->
      put_run t s;
      t.repeatable <- true
  | Csi { marker = None; intermediates = ""; final = 'b'; params } ->
      (* REP repeats the last character printed, so it keeps [repeatable]. *)
      rep t (Ansi.Parser.Params.pn params 0 ~default:1)
  | Csi s ->
      t.repeatable <- false;
      csi t s
  | Control c ->
      t.repeatable <- false;
      control t c
  | Esc { intermediates; final } ->
      t.repeatable <- false;
      esc t intermediates final
  | Open o ->
      t.repeatable <- false;
      open_string t o
  | Data s -> string_data t s
  | Close ending -> close_string t ending

let feed t bytes ofs len =
  Ansi.Parser.feed t.parser bytes ~off:ofs ~len (handle_token t)

let feed_string t str =
  let bytes = Bytes.unsafe_of_string str in
  feed t bytes 0 (String.length str)

let reset t =
  hard_reset t;
  Ansi.Parser.reset t.parser;
  osc_drop t

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
