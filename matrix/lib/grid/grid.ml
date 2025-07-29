module Cell = Cell

type rect = { row : int; col : int; width : int; height : int }
type t = { mutable cells : Cell.t option array array; dirty : rect list ref }

let create ~rows ~cols =
  { cells = Array.make_matrix rows cols Cell.empty; dirty = ref [] }

let rows grid = Array.length grid.cells
let cols grid = if rows grid = 0 then 0 else Array.length grid.cells.(0)

let get grid ~row ~col =
  if row >= 0 && row < rows grid && col >= 0 && col < cols grid then
    grid.cells.(row).(col)
  else None

let set grid ~row ~col cell =
  if row >= 0 && row < rows grid && col >= 0 && col < cols grid then (
    grid.cells.(row).(col) <- cell;
    grid.dirty := { row; col; width = 1; height = 1 } :: !(grid.dirty))

let clear grid =
  let nrows = rows grid in
  let ncols = cols grid in
  for r = 0 to nrows - 1 do
    for c = 0 to ncols - 1 do
      grid.cells.(r).(c) <- Cell.empty
    done
  done;
  grid.dirty := [ { row = 0; col = 0; width = ncols; height = nrows } ]

let clear_line grid row from_col =
  if row >= 0 && row < rows grid then (
    let ncols = cols grid in
    for i = from_col to ncols - 1 do
      grid.cells.(row).(i) <- Cell.empty
    done;
    if from_col < ncols then
      grid.dirty :=
        { row; col = from_col; width = ncols - from_col; height = 1 }
        :: !(grid.dirty))

let clear_rect grid ~row_start ~row_end ~col_start ~col_end =
  let row_start = max 0 row_start in
  let row_end = min (rows grid - 1) row_end in
  let col_start = max 0 col_start in
  let col_end = min (cols grid - 1) col_end in
  for r = row_start to row_end do
    for c = col_start to col_end do
      grid.cells.(r).(c) <- Cell.empty
    done
  done;
  if row_start <= row_end && col_start <= col_end then
    grid.dirty :=
      {
        row = row_start;
        col = col_start;
        width = col_end - col_start + 1;
        height = row_end - row_start + 1;
      }
      :: !(grid.dirty)

let copy_row grid row =
  if row >= 0 && row < rows grid then Array.copy grid.cells.(row) else [||]

let set_row grid row new_row =
  if row >= 0 && row < rows grid && Array.length new_row = cols grid then (
    grid.cells.(row) <- new_row;
    grid.dirty :=
      { row; col = 0; width = cols grid; height = 1 } :: !(grid.dirty))

let make_empty_row ~cols = Array.make cols Cell.empty

let flush_damage grid =
  let damage = !(grid.dirty) in
  grid.dirty := [];
  (damage, ())

let swap grids =
  let grid1, grid2 = grids in
  (* Just swap the cell arrays *)
  let temp = grid1.cells in
  grid1.cells <- grid2.cells;
  grid2.cells <- temp;
  (* Mark both grids as completely dirty *)
  let nrows1 = rows grid1 in
  let ncols1 = cols grid1 in
  let nrows2 = rows grid2 in
  let ncols2 = cols grid2 in
  grid1.dirty := [ { row = 0; col = 0; width = ncols1; height = nrows1 } ];
  grid2.dirty := [ { row = 0; col = 0; width = ncols2; height = nrows2 } ]

let resize grid ~rows:new_rows ~cols:new_cols =
  let old_rows = rows grid in
  let old_cols = cols grid in
  
  (* Create new cell array *)
  let new_cells = Array.make_matrix new_rows new_cols Cell.empty in
  
  (* Copy existing content *)
  let copy_rows = min old_rows new_rows in
  let copy_cols = min old_cols new_cols in
  for r = 0 to copy_rows - 1 do
    for c = 0 to copy_cols - 1 do
      new_cells.(r).(c) <- grid.cells.(r).(c)
    done
  done;
  
  (* Replace the cells array *)
  grid.cells <- new_cells;
  
  (* Mark entire grid as dirty *)
  grid.dirty := [ { row = 0; col = 0; width = new_cols; height = new_rows } ]

let to_string grid =
  let nrows = rows grid in
  let ncols = cols grid in
  let buffer = Buffer.create (nrows * (ncols + 1)) in
  for r = 0 to nrows - 1 do
    let c = ref 0 in
    while !c < ncols do
      match grid.cells.(r).(!c) with
      | None -> Buffer.add_char buffer ' '; incr c
      | Some cell -> 
          Buffer.add_string buffer cell.Cell.glyph;
          (* Skip continuation cells for wide characters *)
          c := !c + cell.Cell.width
    done;
    if r < nrows - 1 then Buffer.add_char buffer '\n'
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

(* Cell-level diff for efficient rendering *)

type dirty_region = {
  min_row : int;
  max_row : int;
  min_col : int;
  max_col : int;
}

(* Compute hash of a cell for comparison *)
let cell_hash = function
  | None -> 0
  | Some cell ->
      let open Cell in
      let glyph_hash = Hashtbl.hash cell.glyph in
      let style_hash =
        ((if cell.attrs.bold then 1 else 0) lsl 0)
        + ((if cell.attrs.italic then 1 else 0) lsl 1)
        + ((if cell.attrs.underline then 1 else 0) lsl 2)
        + ((if cell.attrs.strikethrough then 1 else 0) lsl 3)
        + ((if cell.attrs.reversed then 1 else 0) lsl 4)
        + ((if cell.attrs.blink then 1 else 0) lsl 5)
      in
      glyph_hash lxor style_hash

(* Compute hash of a row for quick comparison *)
let row_hash row =
  Array.fold_left (fun acc cell -> (acc * 31) + cell_hash cell) 0 row

(* Find dirty rows between two grids *)
let find_dirty_rows prev_grid curr_grid =
  let nrows = rows curr_grid in
  let dirty = Array.make nrows false in
  for r = 0 to nrows - 1 do
    dirty.(r) <- row_hash prev_grid.cells.(r) <> row_hash curr_grid.cells.(r)
  done;
  dirty

(* Convert dirty rows to minimal bounding regions *)
let compute_dirty_regions dirty_rows cols =
  let regions = ref [] in
  let rows = Array.length dirty_rows in
  let i = ref 0 in

  while !i < rows do
    if dirty_rows.(!i) then (
      let start_row = !i in
      while !i < rows && dirty_rows.(!i) do
        incr i
      done;
      let end_row = !i - 1 in

      (* For each dirty row range, find the column bounds *)
      regions :=
        {
          min_row = start_row;
          max_row = end_row;
          min_col = 0;
          max_col = cols - 1;
        }
        :: !regions)
    else incr i
  done;

  List.rev !regions

(* Find exact cell-level differences within a region *)
let find_cell_changes prev_grid curr_grid region =
  let changed_cells = ref [] in

  for row = region.min_row to region.max_row do
    for col = region.min_col to region.max_col do
      let prev_cell = prev_grid.cells.(row).(col) in
      let curr_cell = curr_grid.cells.(row).(col) in

      let cells_differ =
        match (prev_cell, curr_cell) with
        | None, None -> false
        | None, Some _ | Some _, None -> true
        | Some c1, Some c2 ->
            c1.Cell.glyph <> c2.Cell.glyph || c1.Cell.attrs <> c2.Cell.attrs
      in

      if cells_differ then changed_cells := (row, col) :: !changed_cells
    done
  done;

  List.rev !changed_cells

(* Compute minimal update regions from a list of changed cells *)
let compute_update_regions changed_cells =
  (* This is a simplified version - a more sophisticated algorithm
     could merge adjacent cells into larger rectangles *)
  match changed_cells with
  | [] -> []
  | cells ->
      let min_row = ref max_int in
      let max_row = ref min_int in
      let min_col = ref max_int in
      let max_col = ref min_int in

      List.iter
        (fun (row, col) ->
          min_row := min !min_row row;
          max_row := max !max_row row;
          min_col := min !min_col col;
          max_col := max !max_col col)
        cells;

      [
        {
          min_row = !min_row;
          max_row = !max_row;
          min_col = !min_col;
          max_col = !max_col;
        };
      ]

(* Main diff function *)
let diff prev_grid curr_grid =
  let cols = cols curr_grid in
  let dirty_rows = find_dirty_rows prev_grid curr_grid in
  let regions = compute_dirty_regions dirty_rows cols in

  (* For each dirty region, compute exact cell changes *)
  let all_changes =
    List.map
      (fun region ->
        let changed_cells = find_cell_changes prev_grid curr_grid region in
        (region, changed_cells))
      regions
  in

  (dirty_rows, all_changes)
