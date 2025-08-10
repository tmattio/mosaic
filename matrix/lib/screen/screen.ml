(* Performance counters *)
module Perf = struct
  type counter = {
    mutable bytes_allocated : int;
    mutable cells_diffed : int;
    mutable frames_rendered : int;
    mutable patches_generated : int;
    mutable cache_hits : int;
    mutable cache_misses : int;
  }

  let global_counter =
    {
      bytes_allocated = 0;
      cells_diffed = 0;
      frames_rendered = 0;
      patches_generated = 0;
      cache_hits = 0;
      cache_misses = 0;
    }

  let reset () =
    global_counter.bytes_allocated <- 0;
    global_counter.cells_diffed <- 0;
    global_counter.frames_rendered <- 0;
    global_counter.patches_generated <- 0;
    global_counter.cache_hits <- 0;
    global_counter.cache_misses <- 0

  let get () =
    {
      bytes_allocated = global_counter.bytes_allocated;
      cells_diffed = global_counter.cells_diffed;
      frames_rendered = global_counter.frames_rendered;
      patches_generated = global_counter.patches_generated;
      cache_hits = global_counter.cache_hits;
      cache_misses = global_counter.cache_misses;
    }
end

module Viewport = struct
  type t = { row : int; col : int; width : int; height : int }

  let make ~row ~col ~width ~height =
    if width < 0 || height < 0 then
      invalid_arg "Viewport.make: negative width/height";
    { row; col; width; height }

  let full ~rows ~cols = { row = 0; col = 0; width = cols; height = rows }

  let contains { row; col; width; height } ~row:r ~col:c =
    r >= row && r < row + height && c >= col && c < col + width

  let intersect a b =
    let row' = max a.row b.row in
    let col' = max a.col b.col in
    let row_end = min (a.row + a.height) (b.row + b.height) in
    let col_end = min (a.col + a.width) (b.col + b.width) in
    let height' = row_end - row' in
    let width' = col_end - col' in
    if width' <= 0 || height' <= 0 then None
    else Some { row = row'; col = col'; width = width'; height = height' }

  (* Pretty-printing and equality *)

  let pp ppf v =
    Fmt.pf ppf "@[<h>Viewport {row=%d; col=%d; width=%d; height=%d}@]" v.row
      v.col v.width v.height

  let equal v1 v2 =
    v1.row = v2.row && v1.col = v2.col && v1.width = v2.width
    && v1.height = v2.height
end

module G = Grid
module C = G.Cell

type patch =
  | Run of {
      row : int;
      col : int;
      text : string;
      style : Ansi.Style.t;
      width : int;
    }
  | Clear_region of { row : int; col : int; width : int; height : int }
  | Clear_line of { row : int; from_col : int }
  | Clear_screen

(* Pretty-printing and equality for patches *)

let pp_patch ppf = function
  | Run { row; col; text; style; width } ->
      Fmt.pf ppf "@[<h>Run {row=%d; col=%d; text=%S; style=%a; width=%d}@]" row
        col text Ansi.Style.pp style width
  | Clear_region { row; col; width; height } ->
      Fmt.pf ppf "@[<h>Clear_region {row=%d; col=%d; width=%d; height=%d}@]" row
        col width height
  | Clear_line { row; from_col } ->
      Fmt.pf ppf "@[<h>Clear_line {row=%d; from_col=%d}@]" row from_col
  | Clear_screen -> Fmt.string ppf "Clear_screen"

let patch_equal p1 p2 =
  match (p1, p2) with
  | Run r1, Run r2 ->
      r1.row = r2.row && r1.col = r2.col && r1.text = r2.text
      && Ansi.Style.equal r1.style r2.style
      && r1.width = r2.width
  | Clear_region r1, Clear_region r2 ->
      r1.row = r2.row && r1.col = r2.col && r1.width = r2.width
      && r1.height = r2.height
  | Clear_line l1, Clear_line l2 -> l1.row = l2.row && l1.from_col = l2.from_col
  | Clear_screen, Clear_screen -> true
  | _ -> false

type t = {
  front : G.t;
  back : G.t;
  (* lazily initialised on first frame if caller forgets begin_frame *)
  mutable frame_started : bool;
  mutable cursor : (int * int) option;
  mutable needs_full_redraw : bool;
}
(** Screen type – two grids of identical size. *)

let create ~rows ~cols ?(east_asian_context = false) ?style () =
  let g1 = G.create ~rows ~cols ~east_asian_context () in
  let g2 = G.create ~rows ~cols ~east_asian_context () in
  (* Initialize with the given style if provided *)
  Option.iter
    (fun s ->
      G.clear ~style:s g1;
      G.clear ~style:s g2)
    style;
  {
    front = g1;
    back = g2;
    frame_started = false;
    cursor = None;
    needs_full_redraw = false;
  }

let rows t = G.rows t.front
let cols t = G.cols t.front

let resize t ~rows ~cols =
  G.resize t.front ~rows ~cols;
  G.resize t.back ~rows ~cols;
  t.needs_full_redraw <- true

let with_viewport t _viewport f =
  (* The viewport is handled by the drawing functions themselves *)
  f t

let iter_rect { Viewport.row; col; width; height } f =
  for r = row to row + height - 1 do
    for c = col to col + width - 1 do
      f r c
    done
  done

let clear ?viewport t =
  match viewport with
  | None ->
      (* Clear entire screen if no viewport specified *)
      G.clear t.back
  | Some vp ->
      with_viewport t vp @@ fun t' ->
      G.with_updates t'.back (fun grid ->
          iter_rect vp (fun r c -> G.set grid ~row:r ~col:c None))

let clear_rect ?viewport t ~row_start ~row_end ~col_start ~col_end =
  let extra =
    Viewport.make ~row:row_start ~col:col_start
      ~width:(col_end - col_start + 1)
      ~height:(row_end - row_start + 1)
  in
  let viewport =
    match viewport with
    | Some c -> Viewport.intersect c extra
    | None -> Some extra
  in
  clear ?viewport t

let clear_line ?viewport t ~row ~col =
  let extra = Viewport.make ~row ~col ~width:(cols t - col) ~height:1 in
  let viewport =
    match viewport with
    | Some c -> Viewport.intersect c extra
    | None -> Some extra
  in
  clear ?viewport t

let set_grapheme ?viewport t ~row ~col ~glyph ~attrs =
  match viewport with
  | None -> G.set_grapheme t.back ~row ~col ~glyph ~attrs
  | Some vp ->
      with_viewport t vp @@ fun t' ->
      if Viewport.contains vp ~row ~col then
        G.set_grapheme t'.back ~row ~col ~glyph ~attrs

let set_text ?viewport t ~row ~col ~text ~attrs =
  (* When viewport is provided, row/col are relative to the viewport *)
  match viewport with
  | None ->
      (* Check for negative positions or out of bounds *)
      if row < 0 || col < 0 || row >= G.rows t.back || col >= G.cols t.back then
        (0, 0)
      else
        let cols_advanced = G.set_text t.back ~row ~col ~text ~attrs in
        let lines_written = if cols_advanced > 0 then 1 else 0 in
        (lines_written, cols_advanced)
  | Some vp ->
      (* Check if row is out of viewport *)
      if row < 0 || row >= vp.Viewport.height then (0, 0)
      else if col < 0 then
        (* Negative col - skip beginning of text *)
        let skip_chars = -col in
        if skip_chars >= String.length text then (0, 0)
          (* All text is before viewport *)
        else
          let visible_text =
            String.sub text skip_chars (String.length text - skip_chars)
          in
          let abs_row = vp.Viewport.row + row in
          let abs_col =
            vp.Viewport.col
            (* Start at viewport's column 0 *)
          in
          let max_width = vp.Viewport.width in
          let cols_advanced =
            G.set_text t.back ~row:abs_row ~col:abs_col ~text:visible_text
              ~attrs ~max_width
          in
          let lines_written = if cols_advanced > 0 then 1 else 0 in
          (lines_written, cols_advanced)
      else if col >= vp.Viewport.width then (0, 0)
        (* Starting position is beyond viewport *)
      else
        (* Normal case - position is within viewport *)
        let abs_row = vp.Viewport.row + row in
        let abs_col = vp.Viewport.col + col in
        let max_width = vp.Viewport.width - col in
        let max_width = max 0 max_width in
        let cols_advanced =
          G.set_text t.back ~row:abs_row ~col:abs_col ~text ~attrs ~max_width
        in
        let lines_written = if cols_advanced > 0 then 1 else 0 in
        (lines_written, min cols_advanced max_width)

let set_multiline_text ?viewport t ~row ~col ~text ~attrs =
  let lines = String.split_on_char '\n' text in
  let max_cols = ref 0 in
  let max_row =
    match viewport with None -> max_int | Some vp -> vp.Viewport.height
  in
  let rec loop idx = function
    | [] -> idx
    | hd :: tl ->
        if idx >= max_row then idx (* Stop if we've reached viewport height *)
        else
          let _lines, cols =
            set_text ?viewport t ~row:(row + idx) ~col ~text:hd ~attrs
          in
          if cols > 0 then max_cols := max !max_cols cols;
          loop (idx + 1) tl
  in
  let lines_written = loop 0 lines in
  (lines_written, !max_cols)

let back t =
  if not t.frame_started then
    failwith "Screen.back: Must call begin_frame before accessing back buffer";
  t.back

let front t = t.front

let begin_frame t =
  (* Copy front buffer to back buffer for incremental updates *)
  let rows = G.rows t.front in
  let cols = G.cols t.front in
  let src_rect = { G.row = 0; col = 0; width = cols; height = rows } in
  G.blit ~src:t.front ~src_rect ~dst:t.back ~dst_pos:(0, 0);
  (* Clear dirty bits after copying - this is our new baseline *)
  ignore (G.flush_damage t.back);
  t.frame_started <- true

let present t =
  let dirty_regions =
    if t.needs_full_redraw then
      [
        {
          G.min_row = 0;
          max_row = G.rows t.back - 1;
          min_col = 0;
          max_col = G.cols t.back - 1;
        };
      ]
    else G.diff_regions t.front t.back
  in
  t.needs_full_redraw <- false;
  G.swap (t.front, t.back);
  (* After swap, copy front to back to maintain consistency for next frame *)
  let rows = G.rows t.front in
  let cols = G.cols t.front in
  let src_rect = { G.row = 0; col = 0; width = cols; height = rows } in
  G.blit ~src:t.front ~src_rect ~dst:t.back ~dst_pos:(0, 0);
  (* Flush damage to clear dirty bits after the copy *)
  ignore (G.flush_damage t.back);
  t.frame_started <- false;
  dirty_regions

let batch t f =
  begin_frame t;
  let result = f t in
  let _ = present t in
  result

let diff_cells t =
  let cells = G.diff_cells t.front t.back in
  Perf.global_counter.cells_diffed <-
    Perf.global_counter.cells_diffed + List.length cells;
  cells
  |> List.filter_map (fun (row, col) ->
         match G.get t.back ~row ~col with
         | Some cell when not (C.is_continuation cell) -> Some (row, col, cell)
         | _ -> None)

let flush_damage t = G.flush_damage t.back
let hash_front t = Hashtbl.hash (G.to_string t.front)
let hash_back t = Hashtbl.hash (G.to_string t.back)

let clone t =
  {
    front = G.copy t.front;
    back = G.copy t.back;
    frame_started = t.frame_started;
    cursor = t.cursor;
    needs_full_redraw = t.needs_full_redraw;
  }

let snapshot t =
  (* Return an immutable copy of the back buffer for background processing *)
  G.copy t.back

let copy_to ~src ~dst =
  (* Use the efficient blit operation to copy the entire grid *)
  let rows = min (G.rows src.back) (G.rows dst.back) in
  let cols = min (G.cols src.back) (G.cols dst.back) in
  G.blit ~src:src.back
    ~src_rect:{ row = 0; col = 0; width = cols; height = rows }
    ~dst:dst.back ~dst_pos:(0, 0)

(* Convert cells to run-length encoded patches - optimized with callback *)
let cells_to_patches_iter cells callback =
  (* Sort cells by row and column for proper run-length encoding *)
  let cells =
    List.sort
      (fun (r1, c1, _) (r2, c2, _) ->
        if r1 = r2 then compare c1 c2 else compare r1 r2)
      cells
  in

  let rec build_runs current_run = function
    | [] -> (
        match current_run with None -> () | Some run -> callback run)
    | (row, col, cell) :: rest -> (
        let text = C.get_text cell in
        let style = C.get_style cell in
        let width = C.width cell in

        match current_run with
        | None ->
            (* Start a new run if cell is not empty *)
            if C.is_empty cell then build_runs None rest
            else
              let run = Run { row; col; text; style; width } in
              build_runs (Some run) rest
        | Some
            (Run
               {
                 row = run_row;
                 col = run_col;
                 text = run_text;
                 style = run_style;
                 width = run_width;
               }) ->
            (* Check if we can extend the current run *)
            if
              row = run_row
              && col = run_col + run_width
              && Ansi.Style.equal style run_style
              && not (C.is_empty cell)
            then
              (* Extend the run *)
              let extended_run =
                Run
                  {
                    row = run_row;
                    col = run_col;
                    text = run_text ^ text;
                    style = run_style;
                    width = run_width + width;
                  }
              in
              build_runs (Some extended_run) rest
            else (
              (* Finish current run and maybe start a new one *)
              callback
                (Run
                   {
                     row = run_row;
                     col = run_col;
                     text = run_text;
                     style = run_style;
                     width = run_width;
                   });
              if C.is_empty cell then build_runs None rest
              else
                let new_run = Run { row; col; text; style; width } in
                build_runs (Some new_run) rest)
        | _ -> build_runs current_run rest)
  in

  build_runs None cells


let render t =
  Perf.global_counter.frames_rendered <- Perf.global_counter.frames_rendered + 1;
  if false then
    Printf.eprintf "render: front has content, back has content\n";
  let changes = diff_cells t in
  let total_cells = rows t * cols t in
  let changed_cells = List.length changes in

  (* Compute the largest continuous rectangle of changes *)
  let largest_rect_size =
    let changed_positions =
      List.map (fun (row, col, _) -> (row, col)) changes
    in
    match G.compute_update_regions changed_positions with
    | [] -> 0
    | regions ->
        List.fold_left
          (fun acc region ->
            let size =
              (region.G.max_row - region.G.min_row + 1)
              * (region.G.max_col - region.G.min_col + 1)
            in
            max acc size)
          0 regions
  in

  (* Heuristic: clear screen if either:
     - More than 30% of cells changed, OR
     - The largest continuous rectangle is more than 25% of the screen *)
  let patches = Queue.create () in
  let add_patch p = 
    Queue.push p patches;
    Perf.global_counter.patches_generated <- Perf.global_counter.patches_generated + 1
  in
  
  (* Debug: print change ratio *)
  if false then
    Printf.eprintf "Changed cells: %d/%d = %.2f%%, largest_rect: %d\n" 
      changed_cells total_cells 
      (float_of_int changed_cells /. float_of_int total_cells *. 100.0)
      largest_rect_size;
  
  if
    float_of_int changed_cells /. float_of_int total_cells > 0.3
    || largest_rect_size > total_cells / 4
  then (
    add_patch Clear_screen;
    (* Build runs for the entire back buffer - streaming without intermediate lists *)
    for row = 0 to rows t - 1 do
      let row_cells = ref [] in
      for col = 0 to cols t - 1 do
        match G.get t.back ~row ~col with
        | Some cell
          when (not (C.is_empty cell)) && not (C.is_continuation cell) ->
            row_cells := (row, col, cell) :: !row_cells
        | _ -> ()
      done;
      cells_to_patches_iter (List.rev !row_cells) add_patch
    done)
  else
    (* Build patches from changed cells only *)
    cells_to_patches_iter changes add_patch;
  
  Queue.fold (fun acc x -> x :: acc) [] patches |> List.rev

let render_to_string t =
  let buf = Buffer.create 1024 in
  let prev_style = ref None in
  for row = 0 to rows t - 1 do
    let line_buf = Buffer.create 80 in
    let line_prev_style = ref !prev_style in
    let last_non_empty_col = ref (-1) in

    (* First pass: find the last non-empty column *)
    for col = 0 to cols t - 1 do
      match G.get t.back ~row ~col with
      | Some cell when (not (C.is_empty cell)) && not (C.is_continuation cell)
        ->
          last_non_empty_col := col
      | _ -> ()
    done;

    (* Second pass: render the line *)
    let col = ref 0 in
    while !col < cols t do
      match G.get t.back ~row ~col:!col with
      | Some cell when (not (C.is_empty cell)) && not (C.is_continuation cell)
        ->
          let style = C.get_style cell in
          (* Collect consecutive cells with the same style *)
          let text_buf = Buffer.create 16 in
          let cells_consumed = ref 0 in
          let scan_col = ref !col in
          while !scan_col < cols t do
            match G.get t.back ~row ~col:!scan_col with
            | Some c
              when (not (C.is_empty c))
                   && (not (C.is_continuation c))
                   && Ansi.Style.equal (C.get_style c) style ->
                Buffer.add_string text_buf (C.get_text c);
                let width = C.width c in
                cells_consumed := !cells_consumed + width;
                scan_col := !scan_col + width
            | Some c when C.is_continuation c ->
                (* Should not happen if we're tracking width properly *)
                incr scan_col
            | _ ->
                (* Different style or empty - stop grouping *)
                scan_col := cols t + 1 (* Break inner loop *)
          done;

          (* Output the grouped text with style *)
          if !line_prev_style <> Some style then (
            let sgr = Ansi.Style.to_sgr ~prev_style:!line_prev_style style in
            Buffer.add_string line_buf sgr;
            line_prev_style := Some style);
          Buffer.add_string line_buf (Buffer.contents text_buf);
          col := !col + !cells_consumed
      | Some cell when C.is_continuation cell ->
          (* Skip continuation cells - should not normally happen if tracking properly *)
          incr col
      | _ ->
          (* Empty cell - only add space if we haven't reached the last non-empty column *)
          if !col <= !last_non_empty_col then (
            if
              !line_prev_style <> None
              && !line_prev_style <> Some Ansi.Style.default
            then (
              Buffer.add_string line_buf "\027[0m";
              line_prev_style := Some Ansi.Style.default);
            Buffer.add_char line_buf ' ');
          incr col
    done;

    (* If line ends with non-default style and there are only empty cells after last content, add reset *)
    (if
       !line_prev_style <> None
       && !line_prev_style <> Some Ansi.Style.default
       && !last_non_empty_col >= 0
     then
       (* Check if there are empty cells after the last non-empty cell *)
       let has_trailing_empty = !last_non_empty_col < cols t - 1 in
       if has_trailing_empty then (
         Buffer.add_string line_buf "\027[0m";
         line_prev_style := Some Ansi.Style.default));

    (* Add the line to the main buffer *)
    Buffer.add_buffer buf line_buf;
    prev_style := !line_prev_style;
    if row < rows t - 1 then Buffer.add_char buf '\n'
  done;
  Buffer.contents buf

let patch_to_sgr ?(prev_style = None) = function
  | Run { row; col; text; style; width = _ } ->
      let sgr = Ansi.Style.to_sgr ~prev_style style in
      Printf.sprintf "\027[%d;%dH%s%s" (row + 1) (col + 1) sgr text
  | Clear_region { row; col; width; height } ->
      let buf = Buffer.create 256 in
      for r = row to row + height - 1 do
        Buffer.add_string buf (Printf.sprintf "\027[%d;%dH" (r + 1) (col + 1));
        for _ = 0 to width - 1 do
          Buffer.add_char buf ' '
        done
      done;
      Buffer.contents buf
  | Clear_line { row; from_col } ->
      Printf.sprintf "\027[%d;%dH\027[K" (row + 1) (from_col + 1)
  | Clear_screen -> "\027[2J\027[H"

let patches_to_sgr patches =
  let buf = Buffer.create 1024 in
  let prev_style = ref None in
  List.iter
    (fun patch ->
      (* Special handling for transitions to default style *)
      let patch_str =
        match patch with
        | Run { row; col; text; style; _ }
          when style = Ansi.Style.default && !prev_style <> None
               && !prev_style <> Some Ansi.Style.default ->
            (* Use full reset when transitioning to default style *)
            Printf.sprintf "\027[%d;%dH\027[0m%s" (row + 1) (col + 1) text
        | _ -> patch_to_sgr ~prev_style:!prev_style patch
      in
      Buffer.add_string buf patch_str;
      (* Track the previous style for Run patches *)
      match patch with
      | Run { style; _ } -> prev_style := Some style
      | _ -> () (* Keep prev_style for continuity *))
    patches;
  Buffer.contents buf

let patches_to_sgr_synchronized patches =
  let buf = Buffer.create 1024 in
  (* Start synchronized update mode *)
  Buffer.add_string buf Ansi.synchronized_update_on;
  (* Add all patches *)
  Buffer.add_string buf (patches_to_sgr patches);
  (* End synchronized update mode *)
  Buffer.add_string buf Ansi.synchronized_update_off;
  Buffer.contents buf

let get_cursor t = t.cursor

let set_cursor t ~row ~col =
  if row >= 0 && row < rows t && col >= 0 && col < cols t then
    t.cursor <- Some (row, col)
  else t.cursor <- None

let render_viewport t viewport =
  let buffer = Buffer.create 256 in
  let vp = viewport in
  for
    r = vp.Viewport.row
    to min (vp.Viewport.row + vp.Viewport.height - 1) (rows t - 1)
  do
    let line_buf = Buffer.create vp.Viewport.width in
    let c = ref vp.Viewport.col in
    let max_c = min (vp.Viewport.col + vp.Viewport.width) (cols t) in
    while !c < max_c do
      match G.get t.back ~row:r ~col:!c with
      | Some cell when (not (C.is_empty cell)) && not (C.is_continuation cell)
        ->
          Buffer.add_string line_buf (C.get_text cell);
          (* skip continuation columns *)
          c := !c + C.width cell
      | Some cell when C.is_continuation cell ->
          (* Skip continuation cells *)
          incr c
      | _ ->
          Buffer.add_char line_buf ' ';
          incr c
    done;
    (* trim trailing spaces exactly like Grid.to_string *)
    let line = Buffer.contents line_buf in
    let len = String.length line in
    let rec find_end i =
      if i < 0 || line.[i] <> ' ' then i + 1 else find_end (i - 1)
    in
    let trimmed_len = find_end (len - 1) in
    Buffer.add_substring buffer line 0 trimmed_len;
    if r < min (vp.Viewport.row + vp.Viewport.height - 1) (rows t - 1) then
      Buffer.add_char buffer '\n'
  done;
  Buffer.contents buffer

let copy_viewport ~src ~dst ~src_viewport ~dst_row ~dst_col =
  let vp = src_viewport in
  (* Use blit for efficient rectangular copy *)
  G.blit ~src:src.back
    ~src_rect:
      {
        row = vp.Viewport.row;
        col = vp.Viewport.col;
        width = vp.Viewport.width;
        height = vp.Viewport.height;
      }
    ~dst:dst.back ~dst_pos:(dst_row, dst_col)
