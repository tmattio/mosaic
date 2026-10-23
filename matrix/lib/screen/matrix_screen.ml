(* matrix/lib/screen/matrix_screen.ml *)

(* Allocation-conscious terminal screen with direct buffer mutation. *)

module Grid = Matrix_grid
open StdLabels
module Hit_grid = Hit_grid

(* --- Types & Metrics --- *)

type stats_state = {
  mutable frame_count : int;
  mutable total_cells : int;
  mutable total_bytes : int;
}

type stats = { frame_count : int; total_cells : int; total_bytes : int }

type frame_metrics = {
  frame_count : int;
  cells : int;
  bytes : int;
  cursor_visible : bool;
}

type cursor = {
  position : (int * int) option;
  style : Ansi.cursor_style;
  blinking : bool;
  color : (int * int * int) option;
  visible : bool;
}

(* Lifecycle of the next buffer between presents. [Stale] holds previously
   presented cells after a swap and must be cleared before the buffer is
   observed or mutated; [Pristine] is cleared, and drawn into only through
   {!next_grid}; [Touched] has been built into or had a hit region added. The
   swap defers its clear to the next observation so a frame pays for exactly
   one clear no matter how many layers (the runtime handing the grid out, a
   renderer building into it, the swap) take part in it. *)
type next_state = Stale | Pristine | Touched
type render_mode = [ `Diff | `Full ]

(* All state the diff loop threads through its recursion, retained on the
   screen so a render allocates nothing. The emit path re-points the mutable
   fields at each frame's inputs before running the loop. *)
type diff_ctx = {
  mutable mode : render_mode;
  mutable prev : Grid.t; (* Diff baseline; never consulted in [`Full]. *)
  mutable curr : Grid.t;
  mutable out : Ansi.writer;
  sgr : Ansi.Sgr_state.t;
  mutable scratch : bytes; (* Grapheme byte scratch; high-water growth. *)
  mutable row_offset : int;
  mutable width : int;
  mutable height : int;
  mutable prev_width : int;
  mutable prev_height : int;
  mutable explicit_width : bool;
  mutable explicit_cursor_positioning : bool;
  mutable hyperlinks : bool;
  mutable units : Matrix_text.Segmenter.t;
      (* Display units of [curr]'s width method, to tell where the terminal
         would join two cells. *)
  mutable units_method : Matrix_text.width_method;
  mutable joint : bytes; (* The previous cell's scalars, for [units]. *)
}

(* screen state - mutable internal state for maximum performance *)
type t = {
  stats : stats_state;
  (* The last frame's [frame_metrics], materialized only on demand. *)
  mutable m_cells : int;
  mutable m_bytes : int;
  mutable m_cursor_visible : bool;
  mutable e_cells : int;
  mutable e_output_len : int;
  mutable e_presented_height : int;
  (* Desired cursor state. The runtime owns terminal cursor emission. *)
  mutable cursor : cursor;
  (* Buffers. [current] is the last committed terminal state. [next] is the
     frame being built. [scroll_baseline] is scratch space for the
     scroll-adjusted previous state used while emitting a scroll-hinted diff. *)
  mutable current : Grid.t;
  mutable next : Grid.t;
  (* [false] after a resize: [current] no longer says what the terminal
     shows, so the next render paints every cell. *)
  mutable current_valid : bool;
  mutable next_state : next_state;
  scroll_baseline : Grid.t;
  mutable hit_current : Hit_grid.t;
  mutable hit_next : Hit_grid.t;
  (* State *)
  sgr_state : Ansi.Sgr_state.t;
  mutable row_offset : int;
  diff : diff_ctx;
  (* Retained ANSI output capacity; grows only past its high-water mark.
     [render_writer] is used only by the string-returning convenience API. *)
  render_writer : Ansi.writer;
  (* Capabilities: what the program asks for and what the terminal takes.
     Whether a render writes OSC 66 or repositions the cursor is derived from
     these in [emit_frame] alone. *)
  mutable prefer_explicit_width : bool;
  mutable explicit_width_capable : bool;
  mutable explicit_cursor_positioning_capable : bool;
  mutable hyperlinks_capable : bool;
}

(* --- Constants & Inline Helpers --- *)

let[@inline] clamp_height grid height = max 0 (min (Grid.height grid) height)

(* --- Core Rendering Logic ---

   The diff reads cells with [Grid.Unsafe], which skips the index check. Every
   index it reads is [y * width + x] for a grid of that width with [x] below
   the width and [y] below the grid's height: [emit_frame] sets [c.width] and
   [c.prev_width] to the grids' widths and clamps [c.height] and
   [c.prev_height] to their heights, and the loops stop at those bounds. *)

let[@inline] cell_changed (c : diff_ctx) y x idx curr_width =
  if curr_width <= 0 then false
  else
    match c.mode with
    | `Full -> true
    | `Diff ->
        if y >= c.prev_height || x >= c.prev_width then true
        else
          not
            (Grid.Unsafe.cells_equal c.prev ((y * c.prev_width) + x) c.curr idx)

(* The columns cell [idx] takes when written: its grapheme's width, [1] for an
   empty cell, which shows as a space as [Grid.to_text] writes it, and [0] for
   a continuation cell, which the run writes with its start cell. *)
let[@inline] cell_columns (c : diff_ctx) idx =
  let w = Grid.Unsafe.cell_width c.curr idx in
  if w > 0 || not (Grid.Unsafe.is_empty c.curr idx) then w else 1

let[@inline] cell_link (c : diff_ctx) idx =
  if c.hyperlinks then Grid.Unsafe.get_link c.curr idx else ""

let[@inline] ascii cp = cp land lnot 0x7f = 0

(* [buf], or a larger buffer when [buf] is shorter than [len]. *)
let[@inline] room buf len =
  if len <= Bytes.length buf then buf
  else Bytes.create (max (Bytes.length buf * 2) len)

let[@inline] first_scalar buf =
  Uchar.utf_decode_uchar (Bytes.get_utf_8_uchar buf 0)

let last_scalar buf n =
  let rec start i =
    if i > 0 && Char.code (Bytes.unsafe_get buf i) land 0xC0 = 0x80 then
      start (i - 1)
    else i
  in
  Uchar.utf_decode_uchar (Bytes.get_utf_8_uchar buf (start (n - 1)))

(* Writes the cell's scalars: [n] bytes in [c.scratch], or [cp] when it is
   ASCII. For wide graphemes, uses explicit_width (OSC 66) when available,
   otherwise falls back to cursor repositioning to prevent column drift in
   terminals that miscalculate grapheme widths. Emitters are called saturated
   so no closures are built. *)
let write_cell (c : diff_ctx) ~y ~x ~cell_width ~cp ~n =
  let w = c.out in
  if ascii cp then
    Ansi.Writer.write_char w (if cp = 0 then ' ' else Char.chr cp)
  else if n <= 0 then Ansi.Writer.write_char w ' '
  else if c.explicit_width && cell_width >= 2 then
    Ansi.explicit_width_bytes ~width:cell_width ~bytes:c.scratch ~off:0 ~len:n w
  else begin
    Ansi.Writer.write_subbytes w c.scratch 0 n;
    (* Fallback: reposition cursor after wide graphemes to prevent drift in
       terminals that support cursor addressing but not OSC 66. *)
    if c.explicit_cursor_positioning && cell_width >= 2 then
      let next_x = x + cell_width in
      if next_x < c.width then
        Ansi.cursor_position ~row:(c.row_offset + y + 1) ~col:(next_x + 1) w
  end

(* Adds the [n] bytes of scalars in [buf] from [pos] to [units]. *)
let rec add_scalars units buf pos n =
  if pos < n then begin
    let d = Bytes.get_utf_8_uchar buf pos in
    ignore (Matrix_text.Segmenter.add units (Uchar.utf_decode_uchar d) : bool);
    add_scalars units buf (pos + Uchar.utf_decode_length d) n
  end

(* Whether a cell whose first scalar is [first] would join cell [prev],
   written just before it and ending with the scalar [last]: whether the
   terminal, cutting the scalars it receives into display units as the
   grid's width method does, would make one unit of their graphemes, as of a
   letter and an emoji modifier, or of a Prepend character and a letter. A
   control function, which ends the unit in progress, keeps two joining cells
   apart.

   [prev] starts a unit, by itself or after such a control, so the units
   depend on its scalars only. Most pairs of scalars never join, which
   [Segmenter.may_continue] tells from the pair; the others go through
   [c.units]. *)
let joins (c : diff_ctx) ~prev ~prev_cp ~last first =
  Matrix_text.Segmenter.may_continue c.units_method last first
  &&
  let n =
    if prev_cp >= 0 then 0
    else (
      c.joint <- room c.joint (Grid.Unsafe.cell_text_length c.curr prev);
      Grid.Unsafe.blit_cell_text c.curr prev c.joint ~pos:0)
  in
  Matrix_text.Segmenter.reset c.units;
  if n > 0 then add_scalars c.units c.joint 0 n
  else ignore (Matrix_text.Segmenter.add c.units last : bool);
  not (Matrix_text.Segmenter.add c.units first)

(* Write consecutive changed cells, return the new x position. Cells written =
   new_x - start_x. [prev] is the cell written just before, of code point
   [prev_cp], width [prev_width] and last scalar [last], or [-1] after a
   cursor motion. *)
let rec write_run (c : diff_ctx) y x ~prev ~prev_cp ~prev_width ~last =
  if x >= c.width then x
  else
    let idx = (y * c.width) + x in
    let cell_width = cell_columns c idx in
    if cell_width <= 0 then x
    else if not (cell_changed c y x idx cell_width) then x
    else begin
      let cp = Grid.Unsafe.get_codepoint c.curr idx in
      (* The cell's scalars: [n] bytes in [c.scratch], or [cp] when it is
         ASCII; a space when there are none. *)
      let n =
        if ascii cp then 0
        else (
          c.scratch <- room c.scratch (Grid.Unsafe.cell_text_length c.curr idx);
          Grid.Unsafe.blit_cell_text c.curr idx c.scratch ~pos:0)
      in
      let simple = cp >= 0 || n <= 0 in
      let first =
        if simple then Uchar.unsafe_of_int (if cp < 0x20 then 0x20 else cp)
        else first_scalar c.scratch
      in
      let final = if simple then first else last_scalar c.scratch n in
      if
        prev >= 0
        (* Two ASCII cells never join. *)
        && (not (ascii cp && ascii prev_cp))
        (* A control stands between a wide cell and its neighbors when it
           is written in OSC 66, or followed by a cursor motion. *)
        && (not
              (prev_width >= 2
               && (c.explicit_width || c.explicit_cursor_positioning)
              || (c.explicit_width && cell_width >= 2)))
        && joins c ~prev ~prev_cp ~last first
      then Ansi.cursor_position ~row:(c.row_offset + y + 1) ~col:(x + 1) c.out;
      (* Emit style/color through zero-alloc accessors. *)
      Ansi.Sgr_state.update c.sgr c.out
        ~fg:(Grid.Unsafe.get_fg c.curr idx)
        ~bg:(Grid.Unsafe.get_bg c.curr idx)
        ~attrs:(Grid.Unsafe.get_attrs c.curr idx)
        ~link:(cell_link c idx);
      write_cell c ~y ~x ~cell_width ~cp ~n;
      write_run c y (x + cell_width) ~prev:idx ~prev_cp:cp
        ~prev_width:cell_width ~last:final
    end

(* Process columns in a row, return total cells updated in this row. *)
let rec process_cols (c : diff_ctx) y x row_cells =
  if x >= c.width then row_cells
  else
    let idx = (y * c.width) + x in
    let cell_width = cell_columns c idx in
    (* A continuation cell outside a run would belong to no start cell, which
       a grid never holds. *)
    if cell_width <= 0 then process_cols c y (x + 1) row_cells
    else if cell_changed c y x idx cell_width then begin
      (* Move cursor to start of changed run, then write it. *)
      Ansi.cursor_position ~row:(c.row_offset + y + 1) ~col:(x + 1) c.out;
      let start_x = x in
      let new_x =
        write_run c y x ~prev:(-1) ~prev_cp:0 ~prev_width:0 ~last:Uchar.min
      in
      (* Close active hyperlink. SGR state is preserved across the gap so the
         next run on this row skips re-emission when unchanged. *)
      Ansi.Sgr_state.close_link c.sgr c.out;
      process_cols c y new_x (row_cells + (new_x - start_x))
    end
    else process_cols c y (x + cell_width) row_cells

(* Process all rows, accumulate total cells updated. *)
let rec process_rows (c : diff_ctx) y total_cells =
  if y >= c.height then total_cells
  else process_rows c (y + 1) (total_cells + process_cols c y 0 0)

(* The hot loop. Scans the grid, diffs against the previous frame, and emits
   sequences while reusing the retained context and scratch buffers. *)
let run_diff (c : diff_ctx) =
  Ansi.Sgr_state.reset c.sgr;
  let total = process_rows c 0 0 in
  Ansi.Sgr_state.close_link c.sgr c.out;
  if total > 0 then Ansi.emit Ansi.reset c.out;
  Ansi.Sgr_state.reset c.sgr;
  total

(* --- Frame Lifecycle --- *)

let[@inline] swap_buffers r =
  let old_current = r.current in
  r.current <- r.next;
  r.next <- old_current;
  let old_hit_current = r.hit_current in
  r.hit_current <- r.hit_next;
  r.hit_next <- old_hit_current;
  (* Defer the clear of the swapped-in buffers to their next observation. *)
  r.next_state <- Stale

let refresh_next r =
  Grid.clear r.next;
  Hit_grid.clear r.hit_next;
  r.next_state <- Pristine

let[@inline] ensure_next_fresh r = if r.next_state = Stale then refresh_next r

let clear_unpresented_rows r presented_height =
  let height = Grid.height r.next in
  if presented_height < height then (
    let y = max 0 presented_height in
    let h = height - y in
    Grid.clear_rect r.next ~x:0 ~y ~width:(Grid.width r.next) ~height:h;
    Hit_grid.add r.hit_next ~x:0 ~y ~width:(Grid.width r.next) ~height:h
      ~id:Hit_grid.empty_id)

let finalize_frame r =
  (* Swap buffers; [next] is cleared to provide a fresh canvas for the
     builder. *)
  swap_buffers r;

  (* Update Stats *)
  r.stats.frame_count <- r.stats.frame_count + 1;
  r.stats.total_cells <- r.stats.total_cells + r.e_cells;
  r.stats.total_bytes <- r.stats.total_bytes + r.e_output_len;

  (* Snapshot Metrics *)
  r.m_cells <- r.e_cells;
  r.m_bytes <- r.e_output_len;
  r.m_cursor_visible <- r.cursor.visible

let presented_height r height = clamp_height r.next height

(* --- Input / Cursor Handling --- *)

(* --- Public API --- *)

type scroll_hint = { top : int; bottom : int; delta : int }
type viewport = { y : int; height : int }

let normalize_scroll_hint ~row_offset ~height ~current hint =
  let { top; bottom; delta } = hint in
  let current_height = Grid.height current in
  let render_height = max 0 (min current_height height) in
  let top = max 0 top in
  let bottom = min (render_height - 1) bottom in
  (* A shift by the region's height or more keeps none of its rows, so the
     diff repaints them all anyway; xterm 398 also blanks the rows outside
     the region on such an SU. *)
  if delta = 0 || top >= bottom || abs delta >= bottom - top + 1 then None
  else Some (top, bottom, delta, max 0 row_offset)

let apply_scroll_hint ~(writer : Ansi.writer) ~row_offset ~height ~current hint
    =
  match normalize_scroll_hint ~row_offset ~height ~current hint with
  | None -> ()
  | Some (top, bottom, delta, row_offset) ->
      (* Shift the previous buffer to match the hardware scroll. After this,
         the diff loop sees only the newly-revealed edge rows as changes. *)
      Grid.shift current
        {
          x = 0;
          y = top;
          width = Grid.width current;
          height = bottom - top + 1;
        }
        ~dx:0 ~dy:(-delta) ~color:Ansi.Color.transparent;
      (* Tell the terminal to perform the same shift via DECSTBM. *)
      Ansi.emit
        (Ansi.set_scrolling_region
           ~top:(row_offset + top + 1)
           ~bottom:(row_offset + bottom + 1))
        writer;
      if delta > 0 then Ansi.emit (Ansi.scroll_up ~n:delta) writer
      else Ansi.emit (Ansi.scroll_down ~n:(-delta)) writer;
      Ansi.emit Ansi.reset_scrolling_region writer;
      Ansi.cursor_position ~row:(row_offset + 1) ~col:1 writer

(* In [`Full] mode the returned grid is never consulted; [r.current] stands
   in so the caller needs no option. *)
let prepare_diff_baseline (r : t) ~mode ~scroll_hint ~row_offset ~height
    ~(writer : Ansi.writer) =
  match (mode, scroll_hint) with
  | `Full, _ -> r.current
  | `Diff, None -> r.current
  | `Diff, Some hint ->
      (* Scroll hints describe terminal-side movement. Apply them to a
         temporary baseline so output failures cannot corrupt [current]. *)
      Grid.blit ~src:r.current ~dst:r.scroll_baseline;
      apply_scroll_hint ~writer ~row_offset ~height ~current:r.scroll_baseline
        hint;
      r.scroll_baseline

let emit_frame (r : t) ~(mode : render_mode) ~scroll_hint ~viewport
    ~(writer : Ansi.writer) =
  let row_offset =
    match viewport with None -> max 0 r.row_offset | Some v -> max 0 v.y
  in
  let height =
    match viewport with
    | None -> Grid.height r.next
    | Some v -> clamp_height r.next v.height
  in
  let output_start = Ansi.Writer.len writer in
  let c = r.diff in
  c.mode <- mode;
  c.curr <- r.next;
  c.out <- writer;
  c.row_offset <- row_offset;
  c.width <- Grid.width r.next;
  c.height <- clamp_height r.next height;
  c.explicit_width <- r.prefer_explicit_width && r.explicit_width_capable;
  c.explicit_cursor_positioning <-
    (not c.explicit_width) && r.explicit_cursor_positioning_capable;
  c.hyperlinks <- r.hyperlinks_capable;
  let width_method = Grid.width_method r.next in
  if width_method <> c.units_method then (
    c.units <- Matrix_text.Segmenter.create width_method;
    c.units_method <- width_method);
  let prev =
    prepare_diff_baseline r ~mode ~scroll_hint ~row_offset ~height ~writer
  in
  c.prev <- prev;
  c.prev_width <- Grid.width prev;
  c.prev_height <- min (Grid.height prev) c.height;
  let cells = run_diff c in
  r.e_cells <- cells;
  r.e_output_len <- Ansi.Writer.len writer - output_start;
  r.e_presented_height <- presented_height r height

let commit_frame r =
  clear_unpresented_rows r r.e_presented_height;
  finalize_frame r

let render_into ?(full = false) ?scroll_hint ?viewport frame writer =
  (* A render with no build since the last one renders the blank frame the
     last one left, so the clear the swap deferred happens now. *)
  ensure_next_fresh frame;
  let mode = if full || not frame.current_valid then `Full else `Diff in
  (try emit_frame frame ~mode ~scroll_hint ~viewport ~writer
   with exn ->
     let bt = Printexc.get_raw_backtrace () in
     frame.diff.out <- frame.render_writer;
     Ansi.Sgr_state.reset frame.sgr_state;
     Printexc.raise_with_backtrace exn bt);
  frame.diff.out <- frame.render_writer;
  frame.current_valid <- true;
  commit_frame frame

let render ?full ?scroll_hint ?viewport frame =
  let writer = frame.render_writer in
  Ansi.Writer.reset_pos writer;
  render_into ?full ?scroll_hint ?viewport frame writer;
  Ansi.Writer.contents writer

(* Creation & Management *)

let create ?width_method ?(cursor_visible = true) ?(explicit_width = false) () =
  let w_method = match width_method with Some m -> m | None -> `Unicode in

  (* All buffers share one grapheme store and link registry so the diff can
     compare complex cells and hyperlinks by handle across frames. *)
  let current = Grid.create ~width:1 ~height:1 ~width_method:w_method () in
  let sgr_state = Ansi.Sgr_state.create () in
  let render_writer = Ansi.Writer.create 0 in
  {
    stats = { frame_count = 0; total_cells = 0; total_bytes = 0 };
    m_cells = 0;
    m_bytes = 0;
    m_cursor_visible = cursor_visible;
    e_cells = 0;
    e_output_len = 0;
    e_presented_height = 0;
    cursor =
      {
        position = None;
        style = `Block;
        blinking = false;
        color = None;
        visible = cursor_visible;
      };
    current;
    next = Grid.create_like current ~width:1 ~height:1;
    current_valid = true;
    next_state = Pristine;
    scroll_baseline = Grid.create_like current ~width:1 ~height:1;
    hit_current = Hit_grid.create ~width:1 ~height:1;
    hit_next = Hit_grid.create ~width:1 ~height:1;
    sgr_state;
    row_offset = 0;
    prefer_explicit_width = explicit_width;
    explicit_width_capable = true;
    explicit_cursor_positioning_capable = false;
    hyperlinks_capable = true;
    diff =
      {
        mode = `Diff;
        prev = current;
        curr = current;
        out = render_writer;
        sgr = sgr_state;
        (* Large enough for any grapheme. *)
        scratch = Bytes.create 1024;
        row_offset = 0;
        width = 0;
        height = 0;
        prev_width = 0;
        prev_height = 0;
        explicit_width = false;
        explicit_cursor_positioning = false;
        hyperlinks = true;
        units = Matrix_text.Segmenter.create w_method;
        units_method = w_method;
        joint = Bytes.create 64;
      };
    render_writer;
  }

let reset t =
  Grid.clear t.next;
  Grid.clear t.scroll_baseline;
  Hit_grid.clear t.hit_current;
  Hit_grid.clear t.hit_next;
  t.next_state <- Pristine;
  t.stats.frame_count <- 0;
  t.stats.total_cells <- 0;
  t.stats.total_bytes <- 0;
  Ansi.Sgr_state.reset t.sgr_state

let resize t ~width ~height =
  if width <= 0 || height <= 0 then
    invalid_arg "Matrix_screen.resize: width and height must be > 0";
  if width <> Grid.width t.current || height <> Grid.height t.current then (
    Grid.resize_clear t.current ~width ~height;
    Grid.resize_clear t.next ~width ~height;
    Grid.resize_clear t.scroll_baseline ~width ~height;
    (* Hit_grid.resize already clears unconditionally, no need to clear again *)
    Hit_grid.resize t.hit_current ~width ~height;
    Hit_grid.resize t.hit_next ~width ~height;
    t.next_state <- Pristine;
    t.current_valid <- false;
    t.cursor <-
      {
        t.cursor with
        position =
          Option.map
            (fun (x, y) -> (min (width - 1) x, min (height - 1) y))
            t.cursor.position;
      })

(* Each build owns the complete frame: the next grid and hit grid are
   guaranteed cleared (or freshly resized) before [f] runs, so a second
   build before a present replaces the superseded pass rather than
   compositing over it. A [Pristine] buffer skips the redundant clear. *)
let internal_build t ~width ~height f =
  if width <= 0 || height <= 0 then (
    if t.next_state <> Pristine then refresh_next t;
    t)
  else (
    if width <> Grid.width t.next || height <> Grid.height t.next then
      resize t ~width ~height
    else if t.next_state <> Pristine then refresh_next t;
    t.next_state <- Touched;
    f t.next t.hit_next;
    t)

let build t ~width ~height f =
  ignore (internal_build t ~width ~height (fun grid hits -> f grid hits) : t)

(* Handing out the next buffers applies any pending post-present clear and
   marks them mutated, so the observable contract — the next buffer is clear
   after a render — holds without an eager clear in the swap. *)
let next_grid frame =
  ensure_next_fresh frame;
  frame.next

let current_grid frame = frame.current

let next_hit_grid frame =
  ensure_next_fresh frame;
  frame.hit_next

let query_hit frame ~x ~y = Hit_grid.get frame.hit_current ~x ~y
let row_offset t = t.row_offset
let set_row_offset t offset = t.row_offset <- max 0 offset

let invalidate_presented t =
  (* Clear the current buffer so diff sees all cells as changed. This maintains
     the invariant: current = what's on terminal. After erasing the terminal
     region, the terminal is "blank", so current should also be blank. *)
  Grid.clear t.current

let active_height (t : t) =
  ensure_next_fresh t;
  Grid.active_height t.next

let stats t =
  {
    frame_count = t.stats.frame_count;
    total_cells = t.stats.total_cells;
    total_bytes = t.stats.total_bytes;
  }

let last_metrics t =
  {
    frame_count = t.stats.frame_count;
    cells = t.m_cells;
    bytes = t.m_bytes;
    cursor_visible = t.m_cursor_visible;
  }

let clamp_byte v = max 0 (min 255 v)

let set_cursor t cursor =
  let color =
    Option.map
      (fun (r, g, b) -> (clamp_byte r, clamp_byte g, clamp_byte b))
      cursor.color
  in
  (* Store the normalized state so [cursor] reads allocate nothing. *)
  let position =
    match cursor.position with
    | Some (x, y) when x < 0 || y < 0 -> Some (max 0 x, max 0 y)
    | p -> p
  in
  t.cursor <- { cursor with position; color }

let cursor t = t.cursor

let apply_capabilities r ~explicit_width ~explicit_cursor_positioning
    ~hyperlinks ~color_depth =
  r.explicit_width_capable <- explicit_width;
  r.explicit_cursor_positioning_capable <- explicit_cursor_positioning;
  r.hyperlinks_capable <- hyperlinks;
  Ansi.Sgr_state.set_color_depth r.sgr_state color_depth

let set_explicit_width t flag = t.prefer_explicit_width <- flag

let set_width_method (t : t) (method_ : Matrix_text.width_method) =
  Grid.set_width_method t.current method_;
  Grid.set_width_method t.scroll_baseline method_;
  Grid.set_width_method t.next method_

let width_method t = Grid.width_method t.next

let add_hit_region frame ~x ~y ~width ~height ~id =
  ensure_next_fresh frame;
  frame.next_state <- Touched;
  Hit_grid.add frame.hit_next ~x ~y ~width ~height ~id
