(** Grid diffing functionality for optimized rendering *)

type region = { min_row : int; max_row : int; min_col : int; max_col : int }

let empty_region =
  { min_row = max_int; max_row = min_int; min_col = max_int; max_col = min_int }

let is_empty_region r = r.min_row > r.max_row || r.min_col > r.max_col

let update_region r row col =
  {
    min_row = min r.min_row row;
    max_row = max r.max_row row;
    min_col = min r.min_col col;
    max_col = max r.max_col col;
  }

let cell_equal c1 c2 =
  match (c1, c2) with
  | None, None -> true
  | Some c1, Some c2 ->
      let open Vte.Cell in
      Uchar.equal c1.char c2.char
      && c1.style.bold = c2.style.bold
      && c1.style.italic = c2.style.italic
      && c1.style.underline = c2.style.underline
      && c1.style.fg = c2.style.fg && c1.style.bg = c2.style.bg
      && c1.style.reversed = c2.style.reversed
  | _ -> false

let diff_grids ~prev ~curr ~rows ~cols =
  let changed_region = ref empty_region in

  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      if not (cell_equal prev.(row).(col) curr.(row).(col)) then
        changed_region := update_region !changed_region row col
    done
  done;

  !changed_region
