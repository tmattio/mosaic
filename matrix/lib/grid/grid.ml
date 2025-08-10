module Cell = Cell

type rect = { row : int; col : int; width : int; height : int }

(* Columnar storage for cells using Bigarray for better memory locality *)
module Storage = struct
  (* Lightweight cell view for performance-critical operations *)
  type cell_view = {
    cell_type : int; (* 0=empty, 1=glyph, 2=continuation *)
    style : int64; (* Style bits *)
  }

  type t = {
    (* Text storage: use Bytes array for inline storage (up to 8 bytes per cell) *)
    mutable texts : Bytes.t array;
    (* Style storage: packed int64 *)
    mutable styles :
      (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array2.t;
    (* Width storage: 0=empty/continuation, 1-2=glyph width *)
    mutable widths :
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t;
    (* Cell type: 0=empty, 1=glyph, 2=continuation *)
    mutable types :
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t;
    (* String pool for deduplication *)
    string_pool : (string, int) Hashtbl.t;
    mutable pool_strings : string array;
    mutable pool_size : int;
    (* Link storage - ID to URL mapping *)
    link_table : (int, string) Hashtbl.t;
    mutable next_link_id : int;
  }

  let create rows cols =
    let styles =
      Bigarray.Array2.create Bigarray.int64 Bigarray.c_layout rows cols
    in
    let widths =
      Bigarray.Array2.create Bigarray.int8_unsigned Bigarray.c_layout rows cols
    in
    let types =
      Bigarray.Array2.create Bigarray.int8_unsigned Bigarray.c_layout rows cols
    in

    (* Initialize bigarrays to prevent garbage values *)
    Bigarray.Array2.fill styles (Ansi.Style.default :> int64);
    Bigarray.Array2.fill widths 0;
    Bigarray.Array2.fill types 0;

    {
      texts =
        Array.init rows (fun _ ->
            let bytes = Bytes.create (cols * 8) in
            (* Initialize all bytes to zero to ensure deterministic behavior *)
            Bytes.fill bytes 0 (Bytes.length bytes) '\000';
            bytes);
      styles;
      widths;
      types;
      string_pool = Hashtbl.create 1024;
      pool_strings = Array.make 1024 "";
      pool_size = 0;
      link_table = Hashtbl.create 64;
      next_link_id = 1;
    }

  let resize storage new_rows new_cols old_rows old_cols =
    let new_texts =
      Array.init new_rows (fun _ ->
          let bytes = Bytes.create (new_cols * 8) in
          (* Initialize all bytes to zero to ensure deterministic behavior *)
          Bytes.fill bytes 0 (Bytes.length bytes) '\000';
          bytes)
    in
    let new_styles =
      Bigarray.Array2.create Bigarray.int64 Bigarray.c_layout new_rows new_cols
    in
    let new_widths =
      Bigarray.Array2.create Bigarray.int8_unsigned Bigarray.c_layout new_rows
        new_cols
    in
    let new_types =
      Bigarray.Array2.create Bigarray.int8_unsigned Bigarray.c_layout new_rows
        new_cols
    in

    (* Initialize bigarrays to zero to prevent garbage values *)
    Bigarray.Array2.fill new_styles (Ansi.Style.default :> int64);
    Bigarray.Array2.fill new_widths 0;
    Bigarray.Array2.fill new_types 0;

    (* Copy existing data *)
    let copy_rows = min old_rows new_rows in
    let copy_cols = min old_cols new_cols in
    for r = 0 to copy_rows - 1 do
      (* Use unsafe operations for better performance in tight loops *)
      for c = 0 to copy_cols - 1 do
        Bigarray.Array2.unsafe_set new_types r c
          (Bigarray.Array2.unsafe_get storage.types r c);
        Bigarray.Array2.unsafe_set new_widths r c
          (Bigarray.Array2.unsafe_get storage.widths r c);
        Bigarray.Array2.unsafe_set new_styles r c
          (Bigarray.Array2.unsafe_get storage.styles r c)
      done;
      (* Copy text bytes in bulk for the entire row *)
      if copy_cols > 0 then (
        let bytes_to_copy = copy_cols * 8 in
        Bytes.blit storage.texts.(r) 0 new_texts.(r) 0 bytes_to_copy;
        (* Zero out any remaining bytes in partially-filled rows *)
        if copy_cols < new_cols then
          let start_offset = copy_cols * 8 in
          let bytes_to_clear = (new_cols - copy_cols) * 8 in
          Bytes.fill new_texts.(r) start_offset bytes_to_clear '\000')
    done;

    storage.texts <- new_texts;
    storage.styles <- new_styles;
    storage.widths <- new_widths;
    storage.types <- new_types

  let intern_string storage s =
    if String.length s <= 7 then
      (* Store inline - return negative to indicate inline storage *)
      -1
    else
      match Hashtbl.find_opt storage.string_pool s with
      | Some idx -> idx
      | None ->
          let idx = storage.pool_size in
          if idx >= Array.length storage.pool_strings then (
            (* Grow pool *)
            let new_size = Array.length storage.pool_strings * 2 in
            let new_pool = Array.make new_size "" in
            Array.blit storage.pool_strings 0 new_pool 0 storage.pool_size;
            storage.pool_strings <- new_pool);
          storage.pool_strings.(idx) <- s;
          Hashtbl.add storage.string_pool s idx;
          storage.pool_size <- idx + 1;
          idx

  let get_string storage row col =
    let base = col * 8 in
    if row < 0 || row >= Array.length storage.texts then ""
    else
      let texts = storage.texts.(row) in
      if base >= Bytes.length texts then ""
      else
        (* Check first byte to see if it's inline or pooled *)
        let b0 = Char.code (Bytes.get texts base) in
        if b0 = 0 then ""
        else if b0 = 255 then
          (* Pooled string - next 4 bytes are index *)
          let idx =
            Char.code (Bytes.get texts (base + 1))
            lor (Char.code (Bytes.get texts (base + 2)) lsl 8)
            lor (Char.code (Bytes.get texts (base + 3)) lsl 16)
            lor (Char.code (Bytes.get texts (base + 4)) lsl 24)
          in
          if idx >= 0 && idx < storage.pool_size then storage.pool_strings.(idx)
          else "" (* Invalid index, return empty string *)
        else
          (* Inline string - first byte is length, rest is string data *)
          let len = b0 in
          if len > 7 then "" (* Invalid length *)
          else
            let buf = Bytes.create len in
            for i = 0 to len - 1 do
              Bytes.set buf i (Bytes.get texts (base + 1 + i))
            done;
            Bytes.to_string buf

  let set_string storage row col s =
    let base = col * 8 in
    if row < 0 || row >= Array.length storage.texts then ()
    else
      let texts = storage.texts.(row) in
      if base + 7 >= Bytes.length texts then ()
      else
        let len = String.length s in
        if len = 0 then
          (* Clear all bytes *)
          Bytes.fill texts base 8 '\000'
        else if len <= 7 then (
          (* Inline storage - first byte is length *)
          Bytes.set texts base (Char.chr len);
          (* Use blit for faster copy *)
          Bytes.blit_string s 0 texts (base + 1) len;
          (* Clear remaining bytes *)
          if len < 7 then Bytes.fill texts (base + 1 + len) (7 - len) '\000')
        else
          (* Pool storage *)
          let idx = intern_string storage s in
          Bytes.set texts base '\255';
          (* Marker for pooled *)
          Bytes.set texts (base + 1) (Char.chr (idx land 0xFF));
          Bytes.set texts (base + 2) (Char.chr ((idx lsr 8) land 0xFF));
          Bytes.set texts (base + 3) (Char.chr ((idx lsr 16) land 0xFF));
          Bytes.set texts (base + 4) (Char.chr ((idx lsr 24) land 0xFF));
          Bytes.fill texts (base + 5) 3 '\000'

  let get_cell storage row col =
    let rows = Bigarray.Array2.dim1 storage.types in
    let cols = Bigarray.Array2.dim2 storage.types in
    if row < 0 || row >= rows || col < 0 || col >= cols then Cell.empty
    else
      let cell_type = Bigarray.Array2.get storage.types row col in
      match cell_type with
      | 0 ->
          (* Empty cell - check if it has a style *)
          let style_int64 = Bigarray.Array2.get storage.styles row col in
          let style = Ansi.Style.of_int64 style_int64 in
          if Ansi.Style.equal style Ansi.Style.default then Cell.empty
          else
            (* Empty cell with style - create a glyph with space to preserve style *)
            Cell.make_glyph " " ~style ~east_asian_context:false
      | 1 ->
          let text = get_string storage row col in
          let style =
            Ansi.Style.of_int64 (Bigarray.Array2.get storage.styles row col)
          in
          (* Note: links are handled at storage level, not cell level *)
          Cell.make_glyph text ~style ~east_asian_context:false
      | 2 ->
          let style =
            Ansi.Style.of_int64 (Bigarray.Array2.get storage.styles row col)
          in
          Cell.make_continuation ~style
      | _ -> Cell.empty

  (* Fast hash without allocation - directly hash the raw data *)
  let fast_hash storage row col =
    let rows = Bigarray.Array2.dim1 storage.types in
    let cols = Bigarray.Array2.dim2 storage.types in
    if row < 0 || row >= rows || col < 0 || col >= cols then 0
    else
      let typ = Bigarray.Array2.unsafe_get storage.types row col in
      let sty = Bigarray.Array2.unsafe_get storage.styles row col in
      let wid = Bigarray.Array2.unsafe_get storage.widths row col in
      (* Hash type, style, and width together *)
      let h1 = typ lxor (typ lsl 7) in
      let h2 = Int64.(to_int (logxor sty (shift_right_logical sty 32))) in
      let h3 = wid lxor (wid lsl 3) in
      let base_hash = h1 lxor h2 lxor h3 in
      (* For glyphs, also hash the text *)
      if typ = 1 then
        let text = get_string storage row col in
        base_hash lxor Hashtbl.hash text
      else base_hash

  let cell_hash storage row col = fast_hash storage row col

  let set_cell_with_old_hash storage row col cell =
    (* Get old cell hash before modifying *)
    let old_hash = cell_hash storage row col in

    (if Cell.is_empty cell then (
       Bigarray.Array2.set storage.types row col 0;
       Bigarray.Array2.set storage.widths row col 0;
       Bigarray.Array2.set storage.styles row col (Ansi.Style.default :> int64);
       set_string storage row col "")
     else if Cell.is_continuation cell then (
       let style = Cell.get_style cell in
       Bigarray.Array2.set storage.types row col 2;
       Bigarray.Array2.set storage.widths row col 0;
       Bigarray.Array2.set storage.styles row col (style :> int64);
       set_string storage row col "")
     else
       (* Glyph *)
       let text = Cell.get_text cell in
       let width = Cell.width cell in
       let style = Cell.get_style cell in
       Bigarray.Array2.set storage.types row col 1;
       Bigarray.Array2.set storage.widths row col width;
       Bigarray.Array2.set storage.styles row col (style :> int64);
       set_string storage row col text);

    (* Return old hash for incremental updates *)
    old_hash

  let set_cell storage row col cell =
    let _ = set_cell_with_old_hash storage row col cell in
    ()

  (* Fast cell view access without allocation *)
  let get_cell_view storage row col =
    let rows = Bigarray.Array2.dim1 storage.types in
    let cols = Bigarray.Array2.dim2 storage.types in
    if row < 0 || row >= rows || col < 0 || col >= cols then
      { cell_type = 0; style = (Ansi.Style.default :> int64) }
    else
      {
        cell_type = Bigarray.Array2.get storage.types row col;
        style = Bigarray.Array2.get storage.styles row col;
      }

  (* Fast cell comparison without full Cell construction *)
  let cells_equal storage1 row1 col1 storage2 row2 col2 =
    let v1 = get_cell_view storage1 row1 col1 in
    let v2 = get_cell_view storage2 row2 col2 in

    (* Quick type and style check - styles include link IDs *)
    if v1.cell_type <> v2.cell_type || v1.style <> v2.style then false
    else if v1.cell_type = 0 then true (* Both empty *)
    else if v1.cell_type = 2 then true
      (* Continuation cells - style already compared *)
    else
      (* Glyph cells - compare text bytes directly to avoid string allocation *)
      let base1 = col1 * 8 in
      let base2 = col2 * 8 in
      if
        row1 >= 0
        && row1 < Array.length storage1.texts
        && row2 >= 0
        && row2 < Array.length storage2.texts
      then
        let texts1 = storage1.texts.(row1) in
        let texts2 = storage2.texts.(row2) in
        (* Compare the 8 bytes directly *)
        let rec compare_bytes i =
          if i >= 8 then true
          else if Bytes.get texts1 (base1 + i) <> Bytes.get texts2 (base2 + i)
          then false
          else compare_bytes (i + 1)
        in
        compare_bytes 0
      else false
end

(* Compute hash of a row for quick comparison - using FNV-1a for better collision resistance *)
let row_hash storage row cols =
  (* FNV-1a constants for 32-bit *)
  let fnv_prime = 0x01000193 in
  let fnv_offset_basis = 0x811c9dc5 in

  let hash = ref fnv_offset_basis in
  for col = 0 to cols - 1 do
    let cell_hash = Storage.fast_hash storage row col in
    (* FNV-1a: XOR then multiply *)
    hash := !hash lxor cell_hash;
    hash := !hash * fnv_prime
  done;
  !hash

type t = {
  mutable storage : Storage.t;
  mutable rows : int;
  mutable cols : int;
  mutable row_hashes : int array; (* Cache for row hashes *)
  mutable cell_hashes : int array array;
      (* Cache individual cell hashes for incremental updates *)
  mutable dirty_rows : bool array; (* Track which rows are dirty *)
  east_asian_context : bool;
  mutable batch_updates : bool; (* Whether we're in a batch update *)
  mutable batch_dirty_rows : int list; (* Rows modified during batch *)
}

let create ~rows ~cols ?(east_asian_context = false) () =
  let storage = Storage.create rows cols in
  (* Initialize with empty cells by default (types array is already 0) *)
  let row_hashes = Array.make rows 0 in
  let cell_hashes = Array.init rows (fun _ -> Array.make cols 0) in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      cell_hashes.(r).(c) <- Storage.fast_hash storage r c
    done;
    row_hashes.(r) <- row_hash storage r cols
  done;
  let dirty_rows = Array.make rows false in
  {
    storage;
    rows;
    cols;
    row_hashes;
    cell_hashes;
    dirty_rows;
    east_asian_context;
    batch_updates = false;
    batch_dirty_rows = [];
  }

(* Update row hash incrementally for a single cell change *)
let update_row_hash_incremental grid row col =
  if row >= 0 && row < grid.rows && col >= 0 && col < grid.cols then (
    (* Update the cell hash cache *)
    let new_cell_hash = Storage.fast_hash grid.storage row col in
    grid.cell_hashes.(row).(col) <- new_cell_hash;
    (* With XXHash, we need to recompute the entire row hash *)
    (* This is still faster than the old FNV-1a approach *)
    grid.row_hashes.(row) <- row_hash grid.storage row grid.cols;
    grid.dirty_rows.(row) <- true)

(* Full row hash recomputation - used when incremental isn't possible *)
let update_row_hash grid row =
  if grid.batch_updates then (
    if
      (* Defer actual recomputation; just remember the row *)
      not (List.mem row grid.batch_dirty_rows)
    then grid.batch_dirty_rows <- row :: grid.batch_dirty_rows)
  else (
    (* Immediate recomputation when not batching *)
    for c = 0 to grid.cols - 1 do
      grid.cell_hashes.(row).(c) <- Storage.fast_hash grid.storage row c
    done;
    grid.row_hashes.(row) <- row_hash grid.storage row grid.cols)

let rows grid = grid.rows
let cols grid = grid.cols

let get grid ~row ~col =
  if row >= 0 && row < grid.rows && col >= 0 && col < grid.cols then
    Some (Storage.get_cell grid.storage row col)
  else None

let set grid ~row ~col cell =
  if row >= 0 && row < grid.rows && col >= 0 && col < grid.cols then (
    (match cell with
    | Some c -> Storage.set_cell grid.storage row col c
    | None -> Storage.set_cell grid.storage row col Cell.empty);
    (* Use incremental hash update for single cell change *)
    if grid.batch_updates then
      update_row_hash grid row (* Defer in batch mode *)
    else update_row_hash_incremental grid row col;
    (* Mark row as dirty *)
    grid.dirty_rows.(row) <- true)

let set_grapheme ?link ?east_asian_context grid ~row ~col ~glyph ~attrs =
  if row >= 0 && row < grid.rows && col >= 0 && col < grid.cols then (
    let east_asian_context =
      Option.value east_asian_context ~default:grid.east_asian_context
    in

    (* Handle link if provided *)
    let attrs =
      match link with
      | None -> attrs
      | Some url ->
          (* Add link to storage and get ID *)
          let link_id =
            (* Check if URL already exists *)
            let existing_id = ref None in
            Hashtbl.iter
              (fun id stored_url ->
                if stored_url = url then existing_id := Some id)
              grid.storage.link_table;
            match !existing_id with
            | Some id -> id
            | None ->
                (* Add new link *)
                let id = grid.storage.next_link_id in
                Hashtbl.add grid.storage.link_table id url;
                grid.storage.next_link_id <- grid.storage.next_link_id + 1;
                id
          in
          (* Encode link ID in style *)
          Ansi.Style.set_link_id attrs link_id
    in

    (* Check the width of the existing cell to clear orphan continuations *)
    let existing_cell = Storage.get_cell grid.storage row col in
    let old_width = Cell.width existing_cell in

    (* Handle alpha blending for RGBA colors *)
    let attrs, preserve_existing_glyph =
      let fg = Ansi.Style.fg attrs in
      let bg = Ansi.Style.bg attrs in
      let existing_style = Cell.get_style existing_cell in
      let existing_fg = Ansi.Style.fg existing_style in
      let existing_bg = Ansi.Style.bg existing_style in

      (* Check if we're only updating background (glyph is space) *)
      let is_background_only = glyph = " " in

      (* Blend foreground if it's RGBA *)
      let attrs =
        match fg with
        | Ansi.Style.RGBA _ ->
            let blended_fg = Ansi.Style.blend_colors ~src:fg ~dst:existing_fg in
            Ansi.Style.with_fg blended_fg attrs
        | _ -> attrs
      in

      (* Blend background if it's RGBA *)
      let attrs, should_preserve =
        match bg with
        | Ansi.Style.RGBA _ ->
            let blended_bg = Ansi.Style.blend_colors ~src:bg ~dst:existing_bg in
            (Ansi.Style.with_bg blended_bg attrs, is_background_only)
        | _ -> (attrs, false)
      in
      (attrs, should_preserve)
    in

    (* If we're only updating background with alpha, preserve existing glyph *)
    let final_glyph =
      if preserve_existing_glyph then
        let existing_text = Cell.get_text existing_cell in
        if existing_text <> "" then existing_text else glyph
      else glyph
    in

    let cell = Cell.make_glyph final_glyph ~style:attrs ~east_asian_context in
    let width = Cell.width cell in

    (* Check if we have enough space for the full width *)
    let available_width = grid.cols - col in

    if width > available_width then (
      (* Not enough space - replace with U+FFFD (replacement character) *)
      let replacement_cell =
        Cell.make_glyph "\xEF\xBF\xBD" ~style:attrs ~east_asian_context
      in
      Storage.set_cell grid.storage row col replacement_cell;

      (* Clear any remaining cells if replacement is narrower than original attempt *)
      if old_width > 1 then
        for i = 1 to min (old_width - 1) (grid.cols - col - 1) do
          Storage.set_cell grid.storage row (col + i) Cell.empty
        done)
    else (
      (* Enough space - set the cell normally *)
      Storage.set_cell grid.storage row col cell;

      (* For wide characters, set continuation cells *)
      if width > 1 then
        for i = 1 to width - 1 do
          Storage.set_cell grid.storage row (col + i)
            (Cell.make_continuation ~style:attrs)
        done;

      (* Clear any orphan continuation cells from the previous wider character *)
      if old_width > width then
        for i = width to min (old_width - 1) (grid.cols - col - 1) do
          Storage.set_cell grid.storage row (col + i) Cell.empty
        done);

    (* Update row hash - must recompute full row with FNV-1a *)
    update_row_hash grid row;

    (* Mark row as dirty *)
    grid.dirty_rows.(row) <- true)

let set_text ?link ?east_asian_context ?max_width grid ~row ~col ~text ~attrs =
  if String.length text = 0 then 0
  else
    let east_asian_context =
      Option.value east_asian_context ~default:grid.east_asian_context
    in
    let current_col = ref col in
    let total_width = ref 0 in
    let max_col =
      match max_width with
      | None -> grid.cols
      | Some w -> min grid.cols (col + w)
    in

    (* Fold over grapheme clusters in the text *)
    let folder () grapheme =
      if !current_col < max_col then
        let width =
          Ucwidth.string_width ~east_asian:east_asian_context grapheme
        in
        if width > 0 then (
          (* Normal width grapheme *)
          set_grapheme ?link grid ~row ~col:!current_col ~glyph:grapheme ~attrs
            ~east_asian_context;
          (* Only count the width that was actually written *)
          let actual_width = min width (max_col - !current_col) in
          current_col := !current_col + actual_width;
          total_width := !total_width + actual_width)
        else if width = 0 then
          (* Zero-width grapheme (combining characters, ZWJ, VS15/16, etc) *)
          (* Check if this is a combining mark character *)
          let is_combining_mark =
            match
              Uutf.decode (Uutf.decoder ~encoding:`UTF_8 (`String grapheme))
            with
            | `Uchar u ->
                let gc = Uucp.Gc.general_category u in
                gc = `Mn || gc = `Mc || gc = `Me
            | _ -> false
          in

          if is_combining_mark then (
            (* Combining mark fed as separate grapheme - advance cursor *)
            (if !current_col > col then
               (* Try to append to previous cell first *)
               let prev_col = !current_col - 1 in
               match Storage.get_cell grid.storage row prev_col with
               | cell
                 when (not (Cell.is_empty cell))
                      && not (Cell.is_continuation cell) ->
                   (* Append to previous cell's text *)
                   let prev_text = Cell.get_text cell in
                   let combined_text = prev_text ^ grapheme in
                   let combined_cell =
                     Cell.make_glyph combined_text ~style:(Cell.get_style cell)
                       ~east_asian_context
                   in
                   Storage.set_cell grid.storage row prev_col combined_cell
               | _ ->
                   (* No valid previous cell - place as standalone *)
                   set_grapheme ?link grid ~row ~col:!current_col
                     ~glyph:grapheme ~attrs ~east_asian_context);
            (* Advance cursor for separately-fed combining marks 
               This matches terminal behavior when combining marks are sent as separate characters *)
            current_col := !current_col + 1;
            total_width := !total_width + 1)
          else if !current_col > col then
            (* Other zero-width characters (ZWJ, VS, etc) - don't advance cursor *)
            let prev_col = !current_col - 1 in
            match Storage.get_cell grid.storage row prev_col with
            | cell
              when (not (Cell.is_empty cell)) && not (Cell.is_continuation cell)
              ->
                (* Append to previous cell's text *)
                let prev_text = Cell.get_text cell in
                let combined_text = prev_text ^ grapheme in
                let combined_cell =
                  Cell.make_glyph combined_text ~style:(Cell.get_style cell)
                    ~east_asian_context
                in
                Storage.set_cell grid.storage row prev_col combined_cell;
                (* Don't advance cursor for other zero-width chars *)
                ()
            | _ ->
                (* No previous cell or it's empty/continuation - skip this grapheme *)
                ()
      (* else: zero-width at start of line - skip it *)
    in

    Uuseg_string.fold_utf_8 `Grapheme_cluster folder () text;
    !total_width

let clear ?style grid =
  let nrows = grid.rows in
  let ncols = grid.cols in
  let style = Option.value style ~default:Ansi.Style.default in
  (* Clear all bigarrays in bulk - much faster than cell-by-cell *)
  for r = 0 to nrows - 1 do
    (* Use Bigarray.Array1 slice for efficient row clearing *)
    let row_slice_types = Bigarray.Array2.slice_left grid.storage.types r in
    let row_slice_widths = Bigarray.Array2.slice_left grid.storage.widths r in
    let row_slice_styles = Bigarray.Array2.slice_left grid.storage.styles r in
    Bigarray.Array1.fill row_slice_types 0;
    (* Still empty cells *)
    Bigarray.Array1.fill row_slice_widths 0;
    Bigarray.Array1.fill row_slice_styles (style :> int64);
    (* But with the given style *)
    (* Clear text storage for this row *)
    Bytes.fill grid.storage.texts.(r) 0 (ncols * 8) '\000';
    update_row_hash grid r;
    grid.dirty_rows.(r) <- true
  done

let clear_line grid row from_col =
  if row >= 0 && row < grid.rows then (
    let ncols = grid.cols in
    for i = from_col to ncols - 1 do
      Storage.set_cell grid.storage row i Cell.empty
    done;
    (* Update row hash *)
    update_row_hash grid row;
    if from_col < ncols then grid.dirty_rows.(row) <- true)

let clear_rect grid ~row_start ~row_end ~col_start ~col_end =
  let row_start = max 0 row_start in
  let row_end = min (grid.rows - 1) row_end in
  let col_start = max 0 col_start in
  let col_end = min (grid.cols - 1) col_end in
  for r = row_start to row_end do
    (* Clear cells in the range more efficiently using unsafe operations *)
    for c = col_start to col_end do
      Bigarray.Array2.unsafe_set grid.storage.types r c 0;
      Bigarray.Array2.unsafe_set grid.storage.widths r c 0;
      Bigarray.Array2.unsafe_set grid.storage.styles r c
        (Ansi.Style.default :> int64)
    done;
    (* Clear text bytes for the range *)
    if col_end >= col_start then (
      let start_byte = col_start * 8 in
      let num_bytes = (col_end - col_start + 1) * 8 in
      Bytes.fill grid.storage.texts.(r) start_byte num_bytes '\000';
      (* Update row hash for this row *)
      update_row_hash grid r;
      grid.dirty_rows.(r) <- true)
  done

let copy_row grid row =
  if row >= 0 && row < grid.rows then
    Array.init grid.cols (fun col -> Storage.get_cell grid.storage row col)
  else [||]

let set_row grid row new_row =
  if row >= 0 && row < grid.rows && Array.length new_row = grid.cols then (
    Array.iteri
      (fun col cell -> Storage.set_cell grid.storage row col cell)
      new_row;
    update_row_hash grid row;
    grid.dirty_rows.(row) <- true)

let make_empty_row ~cols = Array.make cols Cell.empty

let fill_space ?(style = Ansi.Style.default) grid =
  let nrows = grid.rows in
  let ncols = grid.cols in
  let style64 = (style :> int64) in
  for r = 0 to nrows - 1 do
    let row_types = Bigarray.Array2.slice_left grid.storage.types r in
    let row_widths = Bigarray.Array2.slice_left grid.storage.widths r in
    let row_styles = Bigarray.Array2.slice_left grid.storage.styles r in
    Bigarray.Array1.fill row_types 1;
    Bigarray.Array1.fill row_widths 1;
    Bigarray.Array1.fill row_styles style64;
    for c = 0 to ncols - 1 do
      Storage.set_string grid.storage r c " "
    done;
    update_row_hash grid r;
    grid.dirty_rows.(r) <- true
  done

let flush_damage grid =
  (* Convert dirty rows to rectangular regions *)
  let regions = ref [] in
  let rows = grid.rows in
  let i = ref 0 in

  while !i < rows do
    if grid.dirty_rows.(!i) then (
      let start_row = !i in
      while !i < rows && grid.dirty_rows.(!i) do
        grid.dirty_rows.(!i) <- false;
        (* Clear dirty flag *)
        incr i
      done;
      let end_row = !i - 1 in
      regions :=
        {
          row = start_row;
          col = 0;
          width = grid.cols;
          height = end_row - start_row + 1;
        }
        :: !regions)
    else incr i
  done;

  List.rev !regions

let swap grids =
  let grid1, grid2 = grids in
  (* Swap storage *)
  let temp_storage = grid1.storage in
  grid1.storage <- grid2.storage;
  grid2.storage <- temp_storage;
  (* Swap dimensions *)
  let temp_rows = grid1.rows in
  let temp_cols = grid1.cols in
  grid1.rows <- grid2.rows;
  grid1.cols <- grid2.cols;
  grid2.rows <- temp_rows;
  grid2.cols <- temp_cols;
  (* Swap row hashes *)
  let temp_hashes = grid1.row_hashes in
  grid1.row_hashes <- grid2.row_hashes;
  grid2.row_hashes <- temp_hashes;
  (* Swap dirty rows *)
  let temp_dirty = grid1.dirty_rows in
  grid1.dirty_rows <- grid2.dirty_rows;
  grid2.dirty_rows <- temp_dirty
(* Don't automatically mark all rows as dirty - let the client decide 
     what's dirty via diffing. This enables true minimal-update render loops. *)

let resize grid ~rows:new_rows ~cols:new_cols =
  let old_rows = grid.rows in
  let old_cols = grid.cols in

  (* Resize storage *)
  Storage.resize grid.storage new_rows new_cols old_rows old_cols;

  (* Update dimensions *)
  grid.rows <- new_rows;
  grid.cols <- new_cols;

  (* Check for cut wide characters at the new column boundary *)
  if new_cols < old_cols then
    for
      (* When shrinking columns, check if any wide characters are cut *)
      r = 0 to min new_rows old_rows - 1
    do
      if new_cols > 0 then
        (* Check the last column - if it's a wide character that would extend beyond boundary *)
        let last_col = new_cols - 1 in
        let cell = Storage.get_cell grid.storage r last_col in
        if Cell.is_glyph cell && Cell.width cell > 1 then
          (* This wide character would extend beyond the new boundary *)
          (* Clear the cell - don't leave half a double-width glyph *)
          Storage.set_cell grid.storage r last_col Cell.empty
        else if Cell.is_continuation cell then
          (* This is an orphaned continuation - clear both the lead and trail *)
          (* Find the start of the wide character by going backwards *)
          let rec find_start col =
            if col <= 0 then 0
            else
              let prev_cell = Storage.get_cell grid.storage r (col - 1) in
              if Cell.is_continuation prev_cell then find_start (col - 1)
              else col - 1
          in
          let start_col = find_start last_col in
          (* Clear all cells from the start of the wide char to the continuation *)
          for c = start_col to last_col do
            Storage.set_cell grid.storage r c Cell.empty
          done
    done;

  (* Resize cell hashes array *)
  let new_cell_hashes =
    Array.init new_rows (fun r ->
        Array.init new_cols (fun c ->
            if r < old_rows && c < old_cols then grid.cell_hashes.(r).(c)
            else Storage.fast_hash grid.storage r c))
  in
  grid.cell_hashes <- new_cell_hashes;

  (* Resize and update row hash cache *)
  let new_row_hashes = Array.make new_rows 0 in
  for r = 0 to new_rows - 1 do
    new_row_hashes.(r) <- row_hash grid.storage r new_cols
  done;
  grid.row_hashes <- new_row_hashes;

  (* Resize dirty rows array and mark all as dirty *)
  let new_dirty_rows = Array.make new_rows true in
  grid.dirty_rows <- new_dirty_rows

let blit ~src ~src_rect ~dst ~dst_pos =
  let src_row = src_rect.row in
  let src_col = src_rect.col in
  let width = src_rect.width in
  let height = src_rect.height in
  let dst_row, dst_col = dst_pos in

  (* Calculate the actual region to copy considering boundaries *)
  let src_row_start = max 0 src_row in
  let src_col_start = max 0 src_col in
  let src_row_end = min src.rows (src_row + height) in
  let src_col_end = min src.cols (src_col + width) in

  let dst_row_start = max 0 dst_row in
  let dst_col_start = max 0 dst_col in
  let dst_row_end = min dst.rows (dst_row + (src_row_end - src_row_start)) in
  let dst_col_end = min dst.cols (dst_col + (src_col_end - src_col_start)) in

  (* Calculate the actual dimensions to copy *)
  let copy_height =
    min (src_row_end - src_row_start) (dst_row_end - dst_row_start)
  in
  let copy_width =
    min (src_col_end - src_col_start) (dst_col_end - dst_col_start)
  in

  (* Copy the data row by row *)
  for i = 0 to copy_height - 1 do
    let src_r = src_row_start + i in
    let dst_r = dst_row_start + i in

    (* Use Bigarray slices for more efficient bulk copy *)
    if copy_width > 0 then (
      (* Get row slices *)
      let src_types_slice =
        Bigarray.Array2.slice_left src.storage.types src_r
      in
      let dst_types_slice =
        Bigarray.Array2.slice_left dst.storage.types dst_r
      in
      let src_widths_slice =
        Bigarray.Array2.slice_left src.storage.widths src_r
      in
      let dst_widths_slice =
        Bigarray.Array2.slice_left dst.storage.widths dst_r
      in
      let src_styles_slice =
        Bigarray.Array2.slice_left src.storage.styles src_r
      in
      let dst_styles_slice =
        Bigarray.Array2.slice_left dst.storage.styles dst_r
      in

      (* Create sub-arrays for the exact range we want to copy *)
      let src_sub_types =
        Bigarray.Array1.sub src_types_slice src_col_start copy_width
      in
      let dst_sub_types =
        Bigarray.Array1.sub dst_types_slice dst_col_start copy_width
      in
      let src_sub_widths =
        Bigarray.Array1.sub src_widths_slice src_col_start copy_width
      in
      let dst_sub_widths =
        Bigarray.Array1.sub dst_widths_slice dst_col_start copy_width
      in
      let src_sub_styles =
        Bigarray.Array1.sub src_styles_slice src_col_start copy_width
      in
      let dst_sub_styles =
        Bigarray.Array1.sub dst_styles_slice dst_col_start copy_width
      in

      (* Use Bigarray blit for efficient memory copy *)
      Bigarray.Array1.blit src_sub_types dst_sub_types;
      Bigarray.Array1.blit src_sub_widths dst_sub_widths;
      Bigarray.Array1.blit src_sub_styles dst_sub_styles);

    (* Copy text bytes for this row segment *)
    (if copy_width > 0 then
       let src_byte_offset = src_col_start * 8 in
       let dst_byte_offset = dst_col_start * 8 in
       let bytes_to_copy = copy_width * 8 in
       Bytes.blit src.storage.texts.(src_r) src_byte_offset
         dst.storage.texts.(dst_r) dst_byte_offset bytes_to_copy);

    (* Update row hash and mark as dirty for destination row *)
    update_row_hash dst dst_r;
    dst.dirty_rows.(dst_r) <- true
  done

let to_string grid =
  let nrows = grid.rows in
  let ncols = grid.cols in
  let buffer = Buffer.create (nrows * (ncols + 1)) in
  for r = 0 to nrows - 1 do
    let c = ref 0 in
    while !c < ncols do
      let cell = Storage.get_cell grid.storage r !c in
      if Cell.is_empty cell then (
        Buffer.add_char buffer ' ';
        incr c)
      else if Cell.is_continuation cell then
        (* Skip continuation cells *)
        incr c
      else
        (* Glyph cell *)
        let text = Cell.get_text cell in
        let width = Cell.width cell in
        Buffer.add_string buffer text;
        (* Skip continuation cells for wide characters *)
        c := !c + width
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

(* Find dirty rows between two grids *)
let find_dirty_rows prev_grid curr_grid =
  let nrows = curr_grid.rows in
  let prev_rows = prev_grid.rows in
  let dirty = Array.make nrows false in
  for r = 0 to nrows - 1 do
    if r < prev_rows then
      dirty.(r) <- prev_grid.row_hashes.(r) <> curr_grid.row_hashes.(r)
    else
      (* New rows that didn't exist in prev_grid are dirty *)
      dirty.(r) <- true
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

(* Merge overlapping or adjacent regions to reduce patch count *)
let merge_regions regions =
  match regions with
  | [] | [ _ ] -> regions
  | _ ->
      (* Sort regions by min_row, then min_col *)
      let sorted =
        List.sort
          (fun a b ->
            let row_cmp = compare a.min_row b.min_row in
            if row_cmp = 0 then compare a.min_col b.min_col else row_cmp)
          regions
      in

      (* Merge overlapping or adjacent regions *)
      let rec merge acc = function
        | [] -> List.rev acc
        | [ r ] -> List.rev (r :: acc)
        | r1 :: r2 :: rest ->
            (* Check if regions overlap or are adjacent *)
            (* Adjacent means they touch or are within 1 row of each other *)
            if
              r1.max_row + 1 >= r2.min_row
              && r1.min_col <= r2.max_col && r1.max_col >= r2.min_col
            then
              (* Merge the regions *)
              let merged =
                {
                  min_row = min r1.min_row r2.min_row;
                  max_row = max r1.max_row r2.max_row;
                  min_col = min r1.min_col r2.min_col;
                  max_col = max r1.max_col r2.max_col;
                }
              in
              merge acc (merged :: rest)
            else merge (r1 :: acc) (r2 :: rest)
      in
      merge [] sorted

(* Find exact cell-level differences within a region *)
let find_cell_changes prev_grid curr_grid region =
  let changed_cells = ref [] in

  for row = region.min_row to region.max_row do
    for col = region.min_col to region.max_col do
      (* Use fast cell comparison without allocation *)
      let cells_differ =
        not
          (Storage.cells_equal prev_grid.storage row col curr_grid.storage row
             col)
      in

      if cells_differ then changed_cells := (row, col) :: !changed_cells
    done
  done;

  List.rev !changed_cells

(* Compute minimal update regions from a list of changed cells *)
let compute_update_regions changed_cells =
  match changed_cells with
  | [] -> []
  | _ ->
      (* Sort cells by row then column for linear sweep *)
      let sorted_cells =
        List.sort
          (fun (r1, c1) (r2, c2) ->
            let row_cmp = compare r1 r2 in
            if row_cmp = 0 then compare c1 c2 else row_cmp)
          changed_cells
      in

      (* Linear sweep to build row intervals *)
      let rec build_row_intervals cells current_row start_col end_col intervals
          =
        match cells with
        | [] ->
            if current_row >= 0 then
              (current_row, start_col, end_col) :: intervals
            else intervals
        | (r, c) :: rest ->
            if current_row = r then
              (* Same row - decide whether to extend or start new interval *)
              if c <= end_col + 2 then
                (* Allow small gaps to be merged (up to 1 column gap) *)
                build_row_intervals rest r start_col (max end_col c) intervals
              else
                (* Gap too large - start new interval on same row *)
                build_row_intervals rest r c c
                  ((current_row, start_col, end_col) :: intervals)
            else
              (* New row *)
              let new_intervals =
                if current_row >= 0 then
                  (current_row, start_col, end_col) :: intervals
                else intervals
              in
              build_row_intervals rest r c c new_intervals
      in

      let row_intervals =
        match sorted_cells with
        | [] -> []
        | (r, c) :: rest -> build_row_intervals rest r c c [] |> List.rev
      in

      (* Merge adjacent row intervals into rectangles *)
      let rec merge_intervals intervals current_rect rects =
        match intervals with
        | [] -> (
            match current_rect with None -> rects | Some rect -> rect :: rects)
        | (row, start_col, end_col) :: rest -> (
            match current_rect with
            | None ->
                (* Start new rectangle *)
                let rect =
                  {
                    min_row = row;
                    max_row = row;
                    min_col = start_col;
                    max_col = end_col;
                  }
                in
                merge_intervals rest (Some rect) rects
            | Some rect ->
                if
                  row = rect.max_row + 1
                  && start_col = rect.min_col && end_col = rect.max_col
                then
                  (* Extend rectangle vertically *)
                  let extended_rect = { rect with max_row = row } in
                  merge_intervals rest (Some extended_rect) rects
                else
                  (* Start new rectangle *)
                  let new_rect =
                    {
                      min_row = row;
                      max_row = row;
                      min_col = start_col;
                      max_col = end_col;
                    }
                  in
                  merge_intervals rest (Some new_rect) (rect :: rects))
      in

      merge_intervals row_intervals None [] |> List.rev

(* Diff functions *)
let diff_rows prev_grid curr_grid =
  let nrows = curr_grid.rows in
  let prev_rows = prev_grid.rows in
  let dirty_rows = ref [] in
  for r = 0 to nrows - 1 do
    if r < prev_rows then (
      if prev_grid.row_hashes.(r) <> curr_grid.row_hashes.(r) then
        dirty_rows := r :: !dirty_rows)
    else
      (* New rows that didn't exist in prev_grid are dirty *)
      dirty_rows := r :: !dirty_rows
  done;
  List.rev !dirty_rows

let diff_regions prev_grid curr_grid =
  let dirty_rows_array = find_dirty_rows prev_grid curr_grid in
  let regions = compute_dirty_regions dirty_rows_array curr_grid.cols in
  merge_regions regions

let diff_cells prev_grid curr_grid =
  let regions = diff_regions prev_grid curr_grid in
  List.fold_left
    (fun acc region ->
      let cells = find_cell_changes prev_grid curr_grid region in
      List.rev_append cells acc)
    [] regions
  |> List.rev

let diff_regions_detailed prev_grid curr_grid =
  let regions = diff_regions prev_grid curr_grid in
  (* For each region, find the actual changed cells and create more granular regions *)
  List.fold_left
    (fun acc region ->
      let changed_cells = find_cell_changes prev_grid curr_grid region in
      if changed_cells = [] then acc
      else
        (* Compute minimal regions from the changed cells *)
        let cell_regions = compute_update_regions changed_cells in
        (* Return each region with its cells *)
        List.fold_left
          (fun acc2 cell_region ->
            let cells_in_region =
              List.filter
                (fun (r, c) ->
                  r >= cell_region.min_row && r <= cell_region.max_row
                  && c >= cell_region.min_col && c <= cell_region.max_col)
                changed_cells
            in
            (cell_region, cells_in_region) :: acc2)
          acc cell_regions)
    [] regions
  |> List.rev

let diff prev_grid curr_grid =
  (* Integrated diff function that combines row hash comparison 
     and dirty region computation for maximum efficiency *)
  let dirty_rows_array = find_dirty_rows prev_grid curr_grid in
  compute_dirty_regions dirty_rows_array curr_grid.cols

(* Create a deep copy of the grid *)
let copy grid =
  let new_storage = Storage.create grid.rows grid.cols in

  (* Copy each cell *)
  for r = 0 to grid.rows - 1 do
    for c = 0 to grid.cols - 1 do
      let cell = Storage.get_cell grid.storage r c in
      Storage.set_cell new_storage r c cell
    done
  done;

  (* Copy string pool *)
  Hashtbl.iter
    (fun k v -> Hashtbl.add new_storage.string_pool k v)
    grid.storage.string_pool;
  new_storage.pool_strings <- Array.copy grid.storage.pool_strings;
  new_storage.pool_size <- grid.storage.pool_size;

  (* Copy link table *)
  Hashtbl.iter
    (fun k v -> Hashtbl.add new_storage.link_table k v)
    grid.storage.link_table;
  new_storage.next_link_id <- grid.storage.next_link_id;

  {
    grid with
    storage = new_storage;
    row_hashes = Array.copy grid.row_hashes;
    dirty_rows = Array.make grid.rows false;
  }

let with_updates grid f =
  (* Save current state for potential rollback *)
  let backup = copy grid in

  (* Set batch mode *)
  grid.batch_updates <- true;
  grid.batch_dirty_rows <- [];

  (* Execute the function *)
  let result =
    try
      let res = f grid in
      (* Success - update all dirty row hashes at once *)
      List.iter
        (fun row ->
          grid.row_hashes.(row) <- row_hash grid.storage row grid.cols)
        grid.batch_dirty_rows;
      res
    with e ->
      (* Rollback changes on exception *)
      (* Copy storage back from backup *)
      Storage.resize backup.storage grid.rows grid.cols backup.rows backup.cols;
      grid.storage <- backup.storage;
      grid.cell_hashes <- Array.map Array.copy backup.cell_hashes;
      grid.row_hashes <- Array.copy backup.row_hashes;
      grid.dirty_rows <- Array.copy backup.dirty_rows;
      (* Restore batch mode state *)
      grid.batch_updates <- false;
      grid.batch_dirty_rows <- [];
      raise e
  in

  (* Clear batch mode *)
  grid.batch_updates <- false;
  grid.batch_dirty_rows <- [];
  result

(* Pretty-printing functions using Fmt *)

let pp_rect ppf rect =
  Fmt.pf ppf "@[<h>{row=%d; col=%d; width=%d; height=%d}@]" rect.row rect.col
    rect.width rect.height

let pp_dirty_region ppf region =
  Fmt.pf ppf "@[<h>{min_row=%d; max_row=%d; min_col=%d; max_col=%d}@]"
    region.min_row region.max_row region.min_col region.max_col

let pp ppf grid =
  Fmt.pf ppf
    "@[<v>Grid {@;\
     <0 2>@[<v>dimensions: %dx%d@ east_asian_context: %b@ non-empty cells:@ \
     @[<v>%a@]@]@,\
     }@]"
    grid.rows grid.cols grid.east_asian_context
    (fun ppf () ->
      for row = 0 to grid.rows - 1 do
        for col = 0 to grid.cols - 1 do
          match get grid ~row ~col with
          | Some cell when not (Cell.is_empty cell) ->
              Fmt.pf ppf "[%d,%d]: %a@ " row col Cell.pp cell
          | _ -> ()
        done
      done)
    ()

(* Equality functions *)

let equal_rect r1 r2 =
  r1.row = r2.row && r1.col = r2.col && r1.width = r2.width
  && r1.height = r2.height

let equal_dirty_region r1 r2 =
  r1.min_row = r2.min_row && r1.max_row = r2.max_row && r1.min_col = r2.min_col
  && r1.max_col = r2.max_col

let char_width ?(east_asian = false) uchar =
  Ucwidth.char_width ~east_asian uchar

let string_width ?(east_asian = false) str =
  Ucwidth.string_width ~east_asian str
