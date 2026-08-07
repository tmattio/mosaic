(* matrix/lib/screen.ml *)

(* Allocation-conscious terminal screen with direct buffer mutation. *)

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
  frame_time_ms : float;
  interval_ms : float;
  reset_ms : float;
  cursor_visible : bool;
  timestamp_s : float;
}

(* Mutable mirrors of the public [frame_metrics] and of the per-frame emit
   results. Floats live in all-float records so per-frame stores stay unboxed;
   the public records are materialized only on demand. *)
type metrics_floats = {
  mutable m_frame_time_ms : float;
  mutable m_interval_ms : float;
  mutable m_reset_ms : float;
  mutable m_timestamp_s : float;
}

type emit_floats = {
  mutable e_now : float;
  mutable e_delta_seconds : float;
  mutable e_elapsed_ms : float;
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
   observed or mutated; [Pristine] is cleared and untouched; [Touched] has
   been handed out or drawn into. The swap defers its clear to the next
   observation so a frame pays for exactly one clear no matter how many
   layers (runtime prepare, renderer build, swap) take part in it. *)
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
}

(* screen state - mutable internal state for maximum performance *)
type t = {
  (* Configuration *)
  clock : unit -> float;
  stats : stats_state;
  metrics_f : metrics_floats;
  mutable m_cells : int;
  mutable m_bytes : int;
  mutable m_cursor_visible : bool;
  emit_f : emit_floats;
  mutable e_cells : int;
  mutable e_output_len : int;
  mutable e_presented_height : int;
  (* The last cursor state given to [set_cursor], pre-normalized, so reads
     allocate nothing. *)
  mutable cursor_snapshot : cursor;
  (* Buffers. [current] is the last committed terminal state. [next] is the
     frame being built. [scroll_baseline] is scratch space for the
     scroll-adjusted previous state used while emitting a scroll-hinted diff. *)
  mutable current : Grid.t;
  mutable next : Grid.t;
  mutable next_state : next_state;
  scroll_baseline : Grid.t;
  mutable hit_current : Hit_grid.t;
  mutable hit_next : Hit_grid.t;
  (* State *)
  cursor : Cursor_state.t;
  sgr_state : Ansi.Sgr_state.t;
  mutable row_offset : int;
  diff : diff_ctx;
  (* Retained ANSI output capacity; grows only past its high-water mark.
     [render_writer] wraps [render_bytes] and is rebuilt only when it grows. *)
  mutable render_bytes : bytes;
  mutable render_writer : Ansi.writer;
  (* Clock time of the last committed frame; nan before the first frame. *)
  mutable last_render_time : float;
  (* Capabilities *)
  mutable prefer_explicit_width : bool;
  mutable explicit_width_capable : bool;
  mutable use_explicit_width : bool;
  mutable use_explicit_cursor_positioning : bool;
  mutable hyperlinks_capable : bool;
  (* Post-processing *)
  mutable post_process_fns : (int * (Grid.t -> delta:float -> unit)) list;
  mutable post_process_cache : (Grid.t -> delta:float -> unit) list;
  mutable post_process_dirty : bool;
  mutable next_effect_id : int;
}

(* --- Constants & Inline Helpers --- *)

let[@inline] width_step w = if w <= 0 then 1 else w
let[@inline] clamp_height grid height = max 0 (min (Grid.height grid) height)

(* Avoid a first-frame retry for ordinary terminals without imposing the old
   2 MiB reservation on every standalone or offscreen Screen. *)
let initial_render_buffer_size = 16 * 1024

(* --- Core Rendering Logic --- *)

let[@inline] cell_changed (c : diff_ctx) y x idx curr_width =
  if curr_width <= 0 then false
  else
    match c.mode with
    | `Full -> true
    | `Diff ->
        if y >= c.prev_height || x >= c.prev_width then true
        else not (Grid.cells_equal c.prev ((y * c.prev_width) + x) c.curr idx)

let[@inline] cell_link (c : diff_ctx) idx =
  if c.hyperlinks then
    Grid.hyperlink_url_direct c.curr (Grid.get_link c.curr idx)
  else ""

(* Writes a grid cell's content to the output buffer. For wide graphemes, uses
   explicit_width (OSC 66) when available, otherwise falls back to cursor
   repositioning to prevent column drift in terminals that miscalculate
   grapheme widths. Emitters are called saturated so no closures are built. *)
let write_cell (c : diff_ctx) ~y ~x ~cell_width idx =
  let w = c.out in
  let cell = Grid.get_cell c.curr idx in
  if Grid.Cell.is_empty cell || Grid.Cell.is_continuation cell then
    Ansi.Writer.write_char w ' '
  else if Grid.Cell.is_inline cell && Grid.Cell.codepoint cell < 128 then
    Ansi.Writer.write_char w (Char.chr (Grid.Cell.codepoint cell))
  else
    let len = Grid.cell_text_length c.curr idx in
    if len <= 0 then Ansi.Writer.write_char w ' '
    else begin
      if len > Bytes.length c.scratch then
        c.scratch <- Bytes.create (max (Bytes.length c.scratch * 2) len);
      let written = Grid.blit_cell_text c.curr idx c.scratch ~pos:0 in
      if written <= 0 then Ansi.Writer.write_char w ' '
      else if c.explicit_width && cell_width >= 2 then
        Ansi.explicit_width_bytes ~width:cell_width ~bytes:c.scratch ~off:0
          ~len:written w
      else begin
        Ansi.Writer.write_subbytes w c.scratch 0 written;
        (* Fallback: reposition cursor after wide graphemes to prevent drift
           in terminals that support cursor addressing but not OSC 66. *)
        if c.explicit_cursor_positioning && cell_width >= 2 then
          let next_x = x + cell_width in
          if next_x < c.width then
            Ansi.cursor_position ~row:(c.row_offset + y + 1) ~col:(next_x + 1) w
      end
    end

(* Write consecutive changed cells, return the new x position. Cells written =
   new_x - start_x. *)
let rec write_run (c : diff_ctx) y x =
  if x >= c.width then x
  else
    let idx = (y * c.width) + x in
    let cell_width = Grid.cell_width c.curr idx in
    let step = width_step cell_width in
    if cell_width <= 0 then x
    else if not (cell_changed c y x idx cell_width) then x
    else begin
      (* Emit style/color through zero-alloc accessors. *)
      Ansi.Sgr_state.update c.sgr c.out ~fg:(Grid.get_fg c.curr idx)
        ~bg:(Grid.get_bg c.curr idx)
        ~attrs:(Grid.get_attrs c.curr idx)
        ~link:(cell_link c idx);
      write_cell c ~y ~x ~cell_width idx;
      write_run c y (x + step)
    end

(* Zero-width cell: the run writer never visits it, so defensively clear it in
   the cell's own colors when it differs from the previous frame. In a
   well-formed grid the scan steps over the continuation cells of wide spans,
   so this only fires for null cells (and malformed orphan continuations). *)
let clear_stale_zero_width (c : diff_ctx) y x idx =
  if c.mode = `Diff && y < c.prev_height && x < c.prev_width then
    let prev_idx = (y * c.prev_width) + x in
    if not (Grid.cells_equal c.prev prev_idx c.curr idx) then begin
      Ansi.cursor_position ~row:(c.row_offset + y + 1) ~col:(x + 1) c.out;
      Ansi.Sgr_state.update c.sgr c.out ~fg:(Grid.get_fg c.curr idx)
        ~bg:(Grid.get_bg c.curr idx)
        ~attrs:(Grid.get_attrs c.curr idx)
        ~link:(cell_link c idx);
      Ansi.Writer.write_char c.out ' ';
      Ansi.Sgr_state.close_link c.sgr c.out
    end

(* Process columns in a row, return total cells updated in this row. *)
let rec process_cols (c : diff_ctx) y x row_cells =
  if x >= c.width then row_cells
  else
    let idx = (y * c.width) + x in
    let cell_width = Grid.cell_width c.curr idx in
    if cell_width <= 0 then begin
      clear_stale_zero_width c y x idx;
      process_cols c y (x + 1) row_cells
    end
    else if cell_changed c y x idx cell_width then begin
      (* Move cursor to start of changed run, then write it. *)
      Ansi.cursor_position ~row:(c.row_offset + y + 1) ~col:(x + 1) c.out;
      let start_x = x in
      let new_x = write_run c y x in
      (* Close active hyperlink. SGR state is preserved across the gap so the
         next run on this row skips re-emission when unchanged. *)
      Ansi.Sgr_state.close_link c.sgr c.out;
      process_cols c y new_x (row_cells + (new_x - start_x))
    end
    else process_cols c y (x + width_step cell_width) row_cells

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

let post_processes r =
  if r.post_process_dirty then (
    r.post_process_cache <- List.rev_map ~f:snd r.post_process_fns;
    r.post_process_dirty <- false);
  r.post_process_cache

let prepare_frame r =
  let now = r.clock () in
  let prev = r.last_render_time in
  let delta_seconds =
    if Float.is_nan prev then 0.
    else
      let delta = now -. prev in
      if delta <= 0. then 0. else delta
  in
  let delta_ms = delta_seconds *. 1000. in
  List.iter ~f:(fun fn -> fn r.next ~delta:delta_ms) (post_processes r);
  r.emit_f.e_now <- now;
  r.emit_f.e_delta_seconds <- delta_seconds

let finalize_frame r =
  let t_reset_start = r.clock () in

  (* Swap buffers; [next] is cleared to provide a fresh canvas for the
     builder. *)
  swap_buffers r;

  let t_reset_end = r.clock () in
  let reset_ms = (t_reset_end -. t_reset_start) *. 1000. in

  (* Update Stats *)
  r.stats.frame_count <- r.stats.frame_count + 1;
  r.stats.total_cells <- r.stats.total_cells + r.e_cells;
  r.stats.total_bytes <- r.stats.total_bytes + r.e_output_len;

  (* Snapshot Metrics *)
  r.metrics_f.m_frame_time_ms <- r.emit_f.e_elapsed_ms;
  r.metrics_f.m_interval_ms <- r.emit_f.e_delta_seconds *. 1000.;
  r.metrics_f.m_reset_ms <- reset_ms;
  r.metrics_f.m_timestamp_s <- r.emit_f.e_now;
  r.m_cells <- r.e_cells;
  r.m_bytes <- r.e_output_len;
  r.m_cursor_visible <- Cursor_state.is_visible r.cursor

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
  if delta = 0 || top >= bottom then None
  else
    let region_h = bottom - top + 1 in
    let delta =
      if delta > 0 then min delta region_h else max delta (-region_h)
    in
    Some (top, bottom, delta, max 0 row_offset)

let apply_scroll_hint ~(writer : Ansi.writer) ~row_offset ~height ~current hint
    =
  match normalize_scroll_hint ~row_offset ~height ~current hint with
  | None -> ()
  | Some (top, bottom, delta, row_offset) ->
      (* Shift the previous buffer to match the hardware scroll. After this,
         the diff loop sees only the newly-revealed edge rows as changes. *)
      Grid.scroll current ~top ~bottom delta;
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
  let render_start = r.clock () in
  let c = r.diff in
  c.mode <- mode;
  c.curr <- r.next;
  c.out <- writer;
  c.row_offset <- row_offset;
  c.width <- Grid.width r.next;
  c.height <- clamp_height r.next height;
  c.explicit_width <- r.use_explicit_width;
  c.explicit_cursor_positioning <- r.use_explicit_cursor_positioning;
  c.hyperlinks <- r.hyperlinks_capable;
  let prev =
    prepare_diff_baseline r ~mode ~scroll_hint ~row_offset ~height ~writer
  in
  c.prev <- prev;
  c.prev_width <- Grid.width prev;
  c.prev_height <- min (Grid.height prev) c.height;
  let cells =
    try run_diff c
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      Ansi.Sgr_state.reset r.sgr_state;
      Printexc.raise_with_backtrace exn bt
  in
  r.emit_f.e_elapsed_ms <- (r.clock () -. render_start) *. 1000.;
  r.e_cells <- cells;
  r.e_output_len <- Ansi.Writer.len writer;
  r.e_presented_height <- presented_height r height

let commit_frame r =
  r.last_render_time <- r.emit_f.e_now;
  clear_unpresented_rows r r.e_presented_height;
  finalize_frame r

let emit_to_bytes frame ~mode ~scroll_hint ~viewport bytes =
  let writer = Ansi.Writer.make bytes in
  emit_frame frame ~mode ~scroll_hint ~viewport ~writer

let grow_capacity current required =
  if required > Sys.max_string_length then raise_notrace Ansi.Writer.Buffer_full;
  let doubled =
    if current > Sys.max_string_length / 2 then Sys.max_string_length
    else current * 2
  in
  let capacity = max required doubled in
  if capacity <= current then raise_notrace Ansi.Writer.Buffer_full;
  capacity

let emit_to_render_bytes frame ~mode ~scroll_hint ~viewport =
  let writer = frame.render_writer in
  Ansi.Writer.reset_pos writer;
  try emit_frame frame ~mode ~scroll_hint ~viewport ~writer
  with Ansi.Writer.Buffer_full ->
    let counter = Ansi.Writer.make_counting () in
    emit_frame frame ~mode ~scroll_hint ~viewport ~writer:counter;
    let capacity =
      grow_capacity (Bytes.length frame.render_bytes) frame.e_output_len
    in
    frame.render_bytes <- Bytes.create capacity;
    frame.render_writer <- Ansi.Writer.make frame.render_bytes;
    emit_frame frame ~mode ~scroll_hint ~viewport ~writer:frame.render_writer

let render_to_bytes ?(full = false) ?scroll_hint ?viewport frame bytes =
  let mode = if full then `Full else `Diff in
  prepare_frame frame;
  emit_to_bytes frame ~mode ~scroll_hint ~viewport bytes;
  commit_frame frame;
  frame.e_output_len

let render ?(full = false) ?scroll_hint ?viewport frame =
  let mode = if full then `Full else `Diff in
  prepare_frame frame;
  emit_to_render_bytes frame ~mode ~scroll_hint ~viewport;
  commit_frame frame;
  Bytes.sub_string frame.render_bytes ~pos:0 ~len:frame.e_output_len

let render_to_buffer ?(full = false) ?scroll_hint ?viewport frame buffer =
  let mode = if full then `Full else `Diff in
  prepare_frame frame;
  emit_to_render_bytes frame ~mode ~scroll_hint ~viewport;
  commit_frame frame;
  Buffer.add_subbytes buffer frame.render_bytes 0 frame.e_output_len

(* Creation & Management *)

let create ?width_method ?respect_alpha ?(cursor_visible = true)
    ?(explicit_width = false) ?(clock = Unix.gettimeofday) () =
  let w_method = match width_method with Some m -> m | None -> `Unicode in
  let r_alpha = match respect_alpha with Some r -> r | None -> false in

  (* All buffers share one grapheme store and link registry so the diff can
     compare complex cells and hyperlinks by handle across frames. *)
  let current =
    Grid.create ~width:1 ~height:1 ~width_method:w_method ~respect_alpha:r_alpha
      ()
  in
  let sgr_state = Ansi.Sgr_state.create () in
  let render_bytes = Bytes.create initial_render_buffer_size in
  let render_writer = Ansi.Writer.make render_bytes in
  let t =
    {
      clock;
      stats = { frame_count = 0; total_cells = 0; total_bytes = 0 };
      metrics_f =
        {
          m_frame_time_ms = 0.;
          m_interval_ms = 0.;
          m_reset_ms = 0.;
          m_timestamp_s = 0.;
        };
      m_cells = 0;
      m_bytes = 0;
      m_cursor_visible = cursor_visible;
      emit_f = { e_now = 0.; e_delta_seconds = 0.; e_elapsed_ms = 0. };
      e_cells = 0;
      e_output_len = 0;
      e_presented_height = 0;
      cursor_snapshot =
        {
          position = None;
          style = `Block;
          blinking = false;
          color = None;
          visible = cursor_visible;
        };
      current;
      next = Grid.create_like current ~width:1 ~height:1;
      next_state = Pristine;
      scroll_baseline = Grid.create_like current ~width:1 ~height:1;
      hit_current = Hit_grid.create ~width:0 ~height:0;
      hit_next = Hit_grid.create ~width:0 ~height:0;
      cursor = Cursor_state.create ();
      sgr_state;
      row_offset = 0;
      post_process_fns = [];
      post_process_cache = [];
      post_process_dirty = false;
      next_effect_id = 0;
      prefer_explicit_width = explicit_width;
      explicit_width_capable = true;
      use_explicit_width = explicit_width;
      use_explicit_cursor_positioning = false;
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
        };
      render_bytes;
      render_writer;
      last_render_time = Float.nan;
    }
  in
  Cursor_state.set_visible t.cursor cursor_visible;
  t

let reset t =
  Grid.clear t.next;
  Grid.clear t.scroll_baseline;
  Hit_grid.clear t.hit_current;
  Hit_grid.clear t.hit_next;
  t.next_state <- Pristine;
  t.last_render_time <- Float.nan;
  t.stats.frame_count <- 0;
  t.stats.total_cells <- 0;
  t.stats.total_bytes <- 0;
  Cursor_state.reset t.cursor;
  Ansi.Sgr_state.reset t.sgr_state

let resize t ~width ~height =
  if width <= 0 || height <= 0 then
    invalid_arg "Screen.resize: width and height must be > 0";
  if width <> Grid.width t.current || height <> Grid.height t.current then (
    Grid.resize_clear t.current ~width ~height;
    Grid.resize_clear t.next ~width ~height;
    Grid.resize_clear t.scroll_baseline ~width ~height;
    (* Hit_grid.resize already clears unconditionally, no need to clear again *)
    Hit_grid.resize t.hit_current ~width ~height;
    Hit_grid.resize t.hit_next ~width ~height;
    t.next_state <- Pristine;
    Cursor_state.clamp_to_bounds t.cursor ~max_row:height ~max_col:width;
    let s = Cursor_state.snapshot t.cursor in
    t.cursor_snapshot <-
      {
        t.cursor_snapshot with
        position =
          (if s.has_position then Some (max 0 (s.col - 1), max 0 (s.row - 1))
           else None);
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
  frame.next_state <- Touched;
  frame.next

let current_grid frame = frame.current

let next_hit_grid frame =
  ensure_next_fresh frame;
  frame.next_state <- Touched;
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
    frame_time_ms = t.metrics_f.m_frame_time_ms;
    interval_ms = t.metrics_f.m_interval_ms;
    reset_ms = t.metrics_f.m_reset_ms;
    cursor_visible = t.m_cursor_visible;
    timestamp_s = t.metrics_f.m_timestamp_s;
  }

let clamp_byte v = max 0 (min 255 v)

let set_cursor t cursor =
  (match cursor.position with
  | None -> Cursor_state.clear_position t.cursor
  | Some (x, y) ->
      Cursor_state.set_position t.cursor ~row:(max 0 y + 1) ~col:(max 0 x + 1));
  Cursor_state.set_visible t.cursor cursor.visible;
  Cursor_state.set_style t.cursor ~style:cursor.style ~blinking:cursor.blinking;
  let color =
    Option.map
      (fun (r, g, b) -> (clamp_byte r, clamp_byte g, clamp_byte b))
      cursor.color
  in
  Cursor_state.set_color t.cursor color;
  (* Store the normalized state so [cursor] reads allocate nothing. *)
  let position =
    match cursor.position with
    | Some (x, y) when x < 0 || y < 0 -> Some (max 0 x, max 0 y)
    | p -> p
  in
  t.cursor_snapshot <- { cursor with position; color }

let cursor t = t.cursor_snapshot

let apply_capabilities r ~explicit_width ~explicit_cursor_positioning
    ~hyperlinks ~color_depth =
  r.explicit_width_capable <- explicit_width;
  r.use_explicit_width <- r.prefer_explicit_width && explicit_width;
  r.use_explicit_cursor_positioning <-
    (not r.use_explicit_width) && explicit_cursor_positioning;
  r.hyperlinks_capable <- hyperlinks;
  Ansi.Sgr_state.set_color_depth r.sgr_state color_depth

let set_explicit_width t flag =
  t.prefer_explicit_width <- flag;
  t.use_explicit_width <- flag && t.explicit_width_capable

let set_width_method (t : t) (method_ : Text.width_method) =
  Grid.set_width_method t.current method_;
  Grid.set_width_method t.scroll_baseline method_;
  Grid.set_width_method t.next method_

type effect_id = int

let post_process f frame =
  let id = frame.next_effect_id in
  frame.next_effect_id <- id + 1;
  frame.post_process_fns <- (id, f) :: frame.post_process_fns;
  frame.post_process_dirty <- true;
  id

let remove_post_process id frame =
  frame.post_process_fns <-
    List.filter ~f:(fun (eid, _) -> eid <> id) frame.post_process_fns;
  frame.post_process_dirty <- true

let clear_post_processes frame =
  frame.post_process_fns <- [];
  frame.post_process_cache <- [];
  frame.post_process_dirty <- false

let add_hit_region frame ~x ~y ~width ~height ~id =
  ensure_next_fresh frame;
  frame.next_state <- Touched;
  Hit_grid.add frame.hit_next ~x ~y ~width ~height ~id
