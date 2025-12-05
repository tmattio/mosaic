(* matrix/lib/screen.ml *)

(* High-performance terminal screen with functional API.
   Zero-allocation frame building through direct buffer mutation. *)

open StdLabels
module Hit_grid = Hit_grid
module Pool = Glyph
module Esc = Ansi.Escape

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
  overall_frame_ms : float;
  frame_callback_ms : float;
  stdout_ms : float;
  mouse_enabled : bool;
  cursor_visible : bool;
  timestamp_s : float;
}

type input_state = {
  mutable mouse_enabled : bool;
  mutable applied_mouse_enabled : bool;
}

(* screen state - mutable internal state for maximum performance *)
type t = {
  (* Configuration *)
  glyph_pool : Pool.pool;
  stats : stats_state;
  mutable last_metrics : frame_metrics;
  (* Buffers (Double Buffering) *)
  mutable current : Grid.t;
  mutable next : Grid.t;
  mutable hit_current : Hit_grid.t;
  mutable hit_next : Hit_grid.t;
  (* State *)
  input : input_state;
  cursor : Cursor_state.t;
  mutable row_offset : int;
  mutable scratch_bytes : bytes;
  mutable last_render_time : float option;
  (* Capabilities *)
  mutable prefer_explicit_width : bool;
  mutable explicit_width_capable : bool;
  mutable use_explicit_width : bool;
  mutable hyperlinks_capable : bool;
  (* Post-processing *)
  mutable post_process_fns : (Grid.t -> delta:float -> unit) list;
  mutable post_process_cache : (Grid.t -> delta:float -> unit) list;
  mutable post_process_dirty : bool;
}

(* --- Constants & Inline Helpers --- *)

let[@inline] width_step w = if w <= 0 then 1 else w

(* --- Writer Logic --- *)

(* Writes a grid cell's content to the output buffer. *)
let[@inline] add_code_to_writer ~explicit_width ~cell_width pool
    (scratch : bytes ref) (w : Esc.writer) grid idx =
  let code = Grid.get_code grid idx in

  if Int32.equal code 0l || Grid.is_continuation grid idx then
    Esc.emit (Esc.char ' ') w
  else if Pool.is_simple code then
    (* Fast Path: ASCII *)
    let u = Int32.to_int code in
    if u < 128 then Esc.emit (Esc.char (Char.chr u)) w
    else Esc.emit (Esc.utf8 u) w
  else
    (* Complex Grapheme *)
    let len = Pool.length pool code in
    if len <= 0 then Esc.emit (Esc.char ' ') w
    else (
      (* Resize scratch buffer if needed *)
      if len > Bytes.length !scratch then
        scratch := Bytes.create (max (Bytes.length !scratch * 2) len);

      (* Zero-alloc copy from Pool -> Scratch *)
      let written = Pool.blit pool code !scratch 0 in

      if written <= 0 then Esc.emit (Esc.char ' ') w
      else if explicit_width && cell_width >= 2 then
        Esc.emit
          (Esc.explicit_width_bytes ~width:cell_width ~bytes:!scratch ~off:0
             ~len:written)
          w
      else Esc.emit (Esc.bytes !scratch ~off:0 ~len:written) w)

(* --- Core Rendering Logic --- *)

type render_mode = [ `Diff | `Full ]

(* The hot loop. Scans grid, checks dirty flags, diffs against previous frame, emits sequences. *)
let render_generic ~pool ~row_offset ~use_explicit_width ~use_hyperlinks ~mode
    ~writer ~scratch ~prev ~curr =
  let width = Grid.width curr in
  let height = Grid.height curr in
  let row_offset = max 0 row_offset in

  (* Diff predicate using the specialized zero-alloc comparator *)
  let cell_changed =
    match prev with
    | None -> fun _ _ -> true
    | Some p ->
        fun prev_idx curr_idx -> not (Grid.cells_equal p prev_idx curr curr_idx)
  in

  let prev_width = match prev with None -> 0 | Some p -> Grid.width p in
  let prev_height = match prev with None -> 0 | Some p -> Grid.height p in

  let is_cell_changed y x idx curr_width =
    if curr_width <= 0 then false
    else
      match mode with
      | `Full -> true
      | `Diff ->
          if y >= prev_height || x >= prev_width then true
          else
            let prev_idx = (y * prev_width) + x in
            cell_changed prev_idx idx
  in

  (* SGR State Tracking *)
  let style = Ansi.Sgr_state.create () in
  let active_link = ref None in
  let cells_updated = ref 0 in

  let close_active_link () =
    match !active_link with
    | None -> ()
    | Some _ ->
        Esc.emit Esc.hyperlink_end writer;
        active_link := None
  in

  let set_active_link link =
    match (!active_link, link) with
    | Some a, Some b when String.equal a b -> ()
    | _ -> (
        close_active_link ();
        match link with
        | None -> ()
        | Some url when use_hyperlinks ->
            Esc.emit (Esc.hyperlink_start ~url) writer;
            active_link := Some url
        | Some _ -> ())
  in

  (* --- Main Render Loop --- *)
  for y = 0 to height - 1 do
    let x = ref 0 in
    while !x < width do
      let idx = (y * width) + !x in
      let curr_width = Grid.cell_width curr idx in

      if curr_width <= 0 then incr x
      else if is_cell_changed y !x idx curr_width then (
        (* Move cursor to start of changed run *)
        let target_row = row_offset + y + 1 in
        Esc.emit (Esc.cursor_position ~row:target_row ~col:(!x + 1)) writer;

        (* Inner loop: Write consecutive changed cells *)
        let rec write_run x_acc =
          if x_acc >= width then x_acc
          else
            let idx = (y * width) + x_acc in
            let curr_width = Grid.cell_width curr idx in
            let step = width_step curr_width in

            if curr_width <= 0 then x_acc
            else if not (is_cell_changed y x_acc idx curr_width) then x_acc
            else
              (* Emit Style/Color using zero-alloc accessors *)
              let attrs = Grid.get_attrs curr idx in
              let link = Grid.hyperlink_url curr (Grid.get_link curr idx) in
              set_active_link link;

              (* Individual component access to avoid tuple allocation *)
              let fg_r = Grid.get_fg_r curr idx in
              let fg_g = Grid.get_fg_g curr idx in
              let fg_b = Grid.get_fg_b curr idx in
              let fg_a = Grid.get_fg_a curr idx in

              let bg_r = Grid.get_bg_r curr idx in
              let bg_g = Grid.get_bg_g curr idx in
              let bg_b = Grid.get_bg_b curr idx in
              let bg_a = Grid.get_bg_a curr idx in

              Ansi.Sgr_state.update style writer ~fg_r ~fg_g ~fg_b ~fg_a ~bg_r
                ~bg_g ~bg_b ~bg_a ~attrs;

              (* Emit Content *)
              add_code_to_writer ~explicit_width:use_explicit_width
                ~cell_width:curr_width pool scratch writer curr idx;

              cells_updated := !cells_updated + step;
              write_run (x_acc + step)
        in
        let new_x = write_run !x in
        (* Reset style at end of run to prevent bleed to skipped cells *)
        Ansi.Sgr_state.reset style;
        x := new_x)
      else x := !x + width_step curr_width
    done
  done;

  close_active_link ();
  Ansi.Sgr_state.reset style;
  !cells_updated

(* --- Frame Lifecycle --- *)

let[@inline] swap_buffers r =
  let old_current = r.current in
  r.current <- r.next;
  r.next <- old_current;
  let old_hit_current = r.hit_current in
  r.hit_current <- r.hit_next;
  r.hit_next <- old_hit_current;
  Hit_grid.clear r.hit_next;
  Grid.clear r.next

let post_processes r =
  if r.post_process_dirty then (
    r.post_process_cache <- List.rev r.post_process_fns;
    r.post_process_dirty <- false);
  r.post_process_cache

let prepare_frame r =
  let now = Unix.gettimeofday () in
  let delta_seconds =
    match r.last_render_time with
    | None ->
        r.last_render_time <- Some now;
        0.
    | Some prev ->
        let delta = now -. prev in
        r.last_render_time <- Some now;
        if delta <= 0. then 0. else delta
  in
  let delta_ms = delta_seconds *. 1000. in
  List.iter ~f:(fun fn -> fn r.next ~delta:delta_ms) (post_processes r);
  (now, delta_seconds)

let finalize_frame r ~now ~delta_seconds ~elapsed_ms ~cells ~output_len =
  let t_reset_start = Unix.gettimeofday () in

  (* Swap buffers; [next] is cleared to provide a fresh canvas for the builder. *)
  swap_buffers r;

  let t_reset_end = Unix.gettimeofday () in
  let reset_ms = (t_reset_end -. t_reset_start) *. 1000. in

  (* Update Stats *)
  r.stats.frame_count <- r.stats.frame_count + 1;
  r.stats.total_cells <- r.stats.total_cells + cells;
  r.stats.total_bytes <- r.stats.total_bytes + output_len;

  (* Snapshot Metrics *)
  let next_m =
    {
      frame_count = r.stats.frame_count;
      cells;
      bytes = output_len;
      frame_time_ms = elapsed_ms;
      interval_ms = delta_seconds *. 1000.;
      reset_ms;
      overall_frame_ms = 0.;
      frame_callback_ms = 0.;
      stdout_ms = 0.;
      mouse_enabled = r.input.mouse_enabled;
      cursor_visible = Cursor_state.is_visible r.cursor;
      timestamp_s = now;
    }
  in
  r.last_metrics <- next_m

(* --- Input / Cursor Handling --- *)

let mouse_enable_sequences =
  [
    Esc.mouse_tracking_on;
    Esc.mouse_button_tracking_on;
    Esc.mouse_motion_on;
    Esc.mouse_sgr_mode_on;
  ]

let mouse_disable_sequences =
  [
    Esc.mouse_motion_off;
    Esc.mouse_button_tracking_off;
    Esc.mouse_tracking_off;
    Esc.mouse_sgr_mode_off;
  ]

let emit_input_side_effects r (w : Esc.writer) =
  if r.input.mouse_enabled <> r.input.applied_mouse_enabled then (
    let sequences =
      if r.input.mouse_enabled then mouse_enable_sequences
      else mouse_disable_sequences
    in
    List.iter ~f:(fun seq -> Esc.emit seq w) sequences;
    r.input.applied_mouse_enabled <- r.input.mouse_enabled)

let emit_prefix r (w : Esc.writer) =
  emit_input_side_effects r w;
  Esc.emit Esc.sync_output_on w;
  Cursor_state.hide_temporarily r.cursor w

let emit_suffix r (w : Esc.writer) =
  Cursor_state.emit r.cursor ~row_offset:r.row_offset w;
  Esc.emit Esc.sync_output_off w

(* --- Public API --- *)

let submit r ~(mode : render_mode) ~(writer : Esc.writer) =
  let now, delta_seconds = prepare_frame r in
  emit_prefix r writer;

  let scratch = ref r.scratch_bytes in
  let render_start = Unix.gettimeofday () in
  let prev = match mode with `Diff -> Some r.current | `Full -> None in

  let cells =
    render_generic ~pool:r.glyph_pool ~row_offset:r.row_offset
      ~use_explicit_width:r.use_explicit_width
      ~use_hyperlinks:r.hyperlinks_capable ~mode ~writer ~scratch ~prev
      ~curr:r.next
  in

  let elapsed_ms = (Unix.gettimeofday () -. render_start) *. 1000. in
  r.scratch_bytes <- !scratch;

  emit_suffix r writer;
  let output_len = Esc.len writer in

  finalize_frame r ~now ~delta_seconds ~elapsed_ms ~cells ~output_len

let render_to_bytes ?(full = false) frame bytes =
  let writer = Esc.make bytes in
  let mode = if full then `Full else `Diff in
  submit frame ~mode ~writer;
  Esc.len writer

let render ?(full = false) frame =
  let bytes = Bytes.create 65536 in
  let len = render_to_bytes ~full frame bytes in
  Bytes.sub_string bytes ~pos:0 ~len

let glyph_pool t = t.glyph_pool

(* Creation & Management *)

let create ?glyph_pool ?width_method ?respect_alpha ?(mouse_enabled = true)
    ?(cursor_visible = true) ?(explicit_width = false) () =
  let glyph_pool =
    match glyph_pool with Some p -> p | None -> Pool.create_pool ()
  in
  let w_method = match width_method with Some m -> m | None -> `Unicode in
  let r_alpha = match respect_alpha with Some r -> r | None -> false in

  let t =
    {
      glyph_pool;
      stats = { frame_count = 0; total_cells = 0; total_bytes = 0 };
      last_metrics =
        {
          frame_count = 0;
          cells = 0;
          bytes = 0;
          frame_time_ms = 0.;
          interval_ms = 0.;
          reset_ms = 0.;
          overall_frame_ms = 0.;
          frame_callback_ms = 0.;
          stdout_ms = 0.;
          mouse_enabled;
          cursor_visible;
          timestamp_s = 0.;
        };
      current =
        Grid.create ~width:1 ~height:1 ~glyph_pool ~width_method:w_method
          ~respect_alpha:r_alpha ();
      next =
        Grid.create ~width:1 ~height:1 ~glyph_pool ~width_method:w_method
          ~respect_alpha:r_alpha ();
      hit_current = Hit_grid.create ~width:0 ~height:0;
      hit_next = Hit_grid.create ~width:0 ~height:0;
      input = { mouse_enabled; applied_mouse_enabled = false };
      cursor = Cursor_state.create ();
      row_offset = 0;
      post_process_fns = [];
      post_process_cache = [];
      post_process_dirty = false;
      prefer_explicit_width = explicit_width;
      explicit_width_capable = true;
      use_explicit_width = explicit_width;
      hyperlinks_capable = true;
      scratch_bytes = Bytes.create 256;
      last_render_time = None;
    }
  in
  Cursor_state.set_visible t.cursor cursor_visible;
  t

let reset t =
  Grid.clear t.next;
  Hit_grid.clear t.hit_current;
  Hit_grid.clear t.hit_next;
  t.last_render_time <- None;
  t.stats.frame_count <- 0;
  t.stats.total_cells <- 0;
  t.stats.total_bytes <- 0;
  Cursor_state.reset t.cursor

let resize t ~width ~height =
  Grid.resize t.current ~width ~height;
  Grid.resize t.next ~width ~height;
  Hit_grid.resize t.hit_current ~width ~height;
  Hit_grid.resize t.hit_next ~width ~height;
  Hit_grid.clear t.hit_current;
  Hit_grid.clear t.hit_next;
  if width > 0 && height > 0 then
    Cursor_state.clamp_to_bounds t.cursor ~max_row:height ~max_col:width

let internal_build t ~width ~height f =
  if width <= 0 || height <= 0 then (
    Hit_grid.clear t.hit_next;
    t)
  else (
    if width <> Grid.width t.next || height <> Grid.height t.next then
      resize t ~width ~height;
    Hit_grid.clear t.hit_next;
    f t.next t.hit_next;
    t)

let build t ~width ~height f =
  internal_build t ~width ~height (fun grid hits -> f grid hits)

let grid frame = frame.next
let hit_grid frame = frame.hit_next
let query_hit frame ~x ~y = Hit_grid.get frame.hit_current ~x ~y
let row_offset t = t.row_offset
let set_row_offset t offset = t.row_offset <- max 0 offset
let active_height (t : t) = Grid.active_height t.next

let stats t =
  {
    frame_count = t.stats.frame_count;
    total_cells = t.stats.total_cells;
    total_bytes = t.stats.total_bytes;
  }

let last_metrics t = t.last_metrics

let record_runtime_metrics t ~frame_callback_ms ~overall_frame_ms ~stdout_ms =
  let m = t.last_metrics in
  let new_m = { m with frame_callback_ms; overall_frame_ms; stdout_ms } in
  t.last_metrics <- new_m

let set_mouse_enabled t enabled = t.input.mouse_enabled <- enabled
let set_cursor_visible t visible = Cursor_state.set_visible t.cursor visible

let set_cursor_position t ~row ~col =
  Cursor_state.set_position t.cursor ~row ~col

let clear_cursor_position t = Cursor_state.clear_position t.cursor

let set_cursor_style t ~style ~blinking =
  Cursor_state.set_style t.cursor ~style ~blinking

let clamp_byte v = max 0 (min 255 v)

let set_cursor_color t ~r ~g ~b =
  Cursor_state.set_color t.cursor
    (Some (clamp_byte r, clamp_byte g, clamp_byte b))

let reset_cursor_color t = Cursor_state.set_color t.cursor None

let apply_capabilities r ~explicit_width ~hyperlinks =
  r.explicit_width_capable <- explicit_width;
  r.use_explicit_width <- r.prefer_explicit_width && explicit_width;
  r.hyperlinks_capable <- hyperlinks

let set_explicit_width t flag =
  t.prefer_explicit_width <- flag;
  t.use_explicit_width <- flag && t.explicit_width_capable

let set_width_method (t : t) (method_ : Glyph.width_method) =
  Grid.set_width_method t.current method_;
  Grid.set_width_method t.next method_

let post_process f frame =
  frame.post_process_fns <- f :: frame.post_process_fns;
  frame.post_process_dirty <- true;
  frame

let remove_post_process f frame =
  frame.post_process_fns <-
    List.filter ~f:(fun e -> not (e == f)) frame.post_process_fns;
  frame.post_process_dirty <- true;
  frame

let clear_post_processes frame =
  frame.post_process_fns <- [];
  frame.post_process_cache <- [];
  frame.post_process_dirty <- false;
  frame

let add_hit_region frame ~x ~y ~width ~height ~id =
  Hit_grid.add frame.hit_next ~x ~y ~width ~height ~id;
  frame
