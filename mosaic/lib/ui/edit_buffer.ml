module Layout = Matrix.Text.Layout
module Source = Layout.Source

(* ───── Helpers ───── *)

let strip_newlines s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    let c = String.unsafe_get s !i in
    if c = '\r' then
      begin if !i + 1 < len && String.unsafe_get s (!i + 1) = '\n' then
        i := !i + 2
      else incr i
      end
    else if c = '\n' then incr i
    else begin
      Buffer.add_char buf c;
      incr i
    end
  done;
  if Buffer.length buf = len then s else Buffer.contents buf

let truncate_graphemes s max_graphemes =
  let source = Source.of_string s in
  if Source.length source <= max_graphemes then s
  else Source.slice source ~first:0 ~last:max_graphemes

(* ───── Types ───── *)

type snapshot = {
  content : string;
  cursor_pos : int;
  affinity : Layout.affinity;
  selection_anchor : int option;
}

type t = {
  mutable content : string;
  mutable cursor_pos : int;
  mutable affinity : Layout.affinity;
  mutable selection_anchor : int option;
  mutable max_length : int;
  mutable undo_stack : snapshot list;
  mutable redo_stack : snapshot list;
  mutable cache : Source.t option;
  tab_width : int;
  width_method : Matrix.Text.width_method;
}

(* ───── Cache ───── *)

let ensure_cache t =
  match t.cache with
  | Some c -> c
  | None ->
      let c = Source.of_string t.content in
      t.cache <- Some c;
      c

let source = ensure_cache
let invalidate_cache t = t.cache <- None
let find_line = Source.line_of_offset
let byte_offset_of_grapheme_in_content c i = Source.byte_offset c i

(* ───── Construction ───── *)

let create ?(max_length = 1000) ?(width_method = `Unicode) ?(tab_width = 2)
    initial =
  let max_length = Int.max 0 max_length in
  let tab_width = Int.max 1 tab_width in
  let content = truncate_graphemes initial max_length in
  let t =
    {
      content;
      cursor_pos = 0;
      affinity = Layout.Downstream;
      selection_anchor = None;
      max_length;
      undo_stack = [];
      redo_stack = [];
      cache = None;
      tab_width;
      width_method;
    }
  in
  let c = ensure_cache t in
  t.cursor_pos <- Source.length c;
  t

(* ───── Content ───── *)

let text t = t.content

let set_text t s =
  t.affinity <- Layout.Downstream;
  let content = truncate_graphemes s t.max_length in
  t.content <- content;
  invalidate_cache t;
  let c = ensure_cache t in
  t.cursor_pos <- Source.length c;
  t.selection_anchor <- None

let length t =
  let c = ensure_cache t in
  Source.length c

let display_width t =
  let c = ensure_cache t in
  Source.columns ~width_method:t.width_method ~tab_width:t.tab_width c ~first:0
    ~last:(Source.length c)

let is_empty t = String.length t.content = 0

(* ───── Line information ───── *)

let line_count t =
  let c = ensure_cache t in
  Source.line_count c

let cursor_line t =
  let c = ensure_cache t in
  find_line c t.cursor_pos

let cursor_col t =
  let c = ensure_cache t in
  let line = find_line c t.cursor_pos in
  t.cursor_pos - Source.line_start c line

(* ───── Cursor ───── *)

let cursor t = t.cursor_pos

let set_cursor t pos =
  t.affinity <- Layout.Downstream;
  let c = ensure_cache t in
  t.cursor_pos <- Int.max 0 (Int.min pos (Source.length c));
  t.selection_anchor <- None

let caret t = Layout.{ grapheme = t.cursor_pos; affinity = t.affinity }

let cursor_display_offset t =
  Source.columns ~width_method:t.width_method ~tab_width:t.tab_width
    (ensure_cache t) ~first:0 ~last:t.cursor_pos

(* ───── Selection ───── *)

let selection t =
  match t.selection_anchor with
  | None -> None
  | Some anchor ->
      let lo = Int.min anchor t.cursor_pos in
      let hi = Int.max anchor t.cursor_pos in
      if lo = hi then None else Some (lo, hi)

let has_selection t = Option.is_some (selection t)

let selected_text t =
  match selection t with
  | None -> ""
  | Some (lo, hi) ->
      let c = ensure_cache t in
      let byte_lo = byte_offset_of_grapheme_in_content c lo in
      let byte_hi = byte_offset_of_grapheme_in_content c hi in
      String.sub t.content byte_lo (byte_hi - byte_lo)

let clear_selection t = t.selection_anchor <- None

let select_all t =
  t.affinity <- Layout.Downstream;
  let c = ensure_cache t in
  t.selection_anchor <- Some 0;
  t.cursor_pos <- Source.length c

(* ───── Undo / Redo ───── *)

(* Each undo point snapshots the full content — O(document) retained per
   entry — so the history must be bounded: keep the newest entries and drop
   the oldest, as OpenTUI's rope trims its history (rope.zig max_undo_depth).
   Editing a large buffer in a long-lived widget stays bounded at
   O(max_undo_depth × document). *)
let max_undo_depth = 200

let take n lst =
  let rec go acc n = function
    | [] -> List.rev acc
    | _ when n <= 0 -> List.rev acc
    | x :: rest -> go (x :: acc) (n - 1) rest
  in
  go [] n lst

let save_undo t =
  let stack =
    if List.length t.undo_stack >= max_undo_depth then
      take (max_undo_depth - 1) t.undo_stack
    else t.undo_stack
  in
  t.undo_stack <-
    {
      content = t.content;
      cursor_pos = t.cursor_pos;
      affinity = t.affinity;
      selection_anchor = t.selection_anchor;
    }
    :: stack;
  t.redo_stack <- []

let undo t =
  match t.undo_stack with
  | [] -> false
  | snap :: rest ->
      let old_content = t.content in
      t.redo_stack <-
        {
          content = t.content;
          cursor_pos = t.cursor_pos;
          affinity = t.affinity;
          selection_anchor = t.selection_anchor;
        }
        :: t.redo_stack;
      t.undo_stack <- rest;
      t.content <- snap.content;
      t.cursor_pos <- snap.cursor_pos;
      t.affinity <- snap.affinity;
      t.selection_anchor <- snap.selection_anchor;
      invalidate_cache t;
      not (String.equal old_content snap.content)

let redo t =
  match t.redo_stack with
  | [] -> false
  | snap :: rest ->
      let old_content = t.content in
      t.undo_stack <-
        {
          content = t.content;
          cursor_pos = t.cursor_pos;
          affinity = t.affinity;
          selection_anchor = t.selection_anchor;
        }
        :: t.undo_stack;
      t.redo_stack <- rest;
      t.content <- snap.content;
      t.cursor_pos <- snap.cursor_pos;
      t.affinity <- snap.affinity;
      t.selection_anchor <- snap.selection_anchor;
      invalidate_cache t;
      not (String.equal old_content snap.content)

(* ───── Internal mutation ───── *)

let delete_grapheme_range t lo hi =
  let c = ensure_cache t in
  let byte_lo = byte_offset_of_grapheme_in_content c lo in
  let byte_hi = byte_offset_of_grapheme_in_content c hi in
  let total_len = String.length t.content in
  t.content <-
    String.sub t.content 0 byte_lo
    ^ String.sub t.content byte_hi (total_len - byte_hi);
  t.selection_anchor <- None;
  invalidate_cache t;
  t.cursor_pos <-
    Source.grapheme_of_byte (ensure_cache t) ~affinity:Downstream byte_lo;
  t.affinity <- Layout.Downstream

let delete_selection t =
  match selection t with
  | None -> false
  | Some (lo, hi) ->
      save_undo t;
      delete_grapheme_range t lo hi;
      true

(* ───── Max length ───── *)

let max_length t = t.max_length

let set_max_length t n =
  let n = Int.max 0 n in
  t.max_length <- n;
  let c = ensure_cache t in
  if Source.length c > n then begin
    let byte_end = byte_offset_of_grapheme_in_content c n in
    t.content <- String.sub t.content 0 byte_end;
    invalidate_cache t;
    let new_len = n in
    if t.cursor_pos > new_len then t.cursor_pos <- new_len;
    t.selection_anchor <- None
  end

(* ───── Editing ───── *)

let insert t inserted =
  let old = ensure_cache t in
  let first, last =
    match selection t with
    | Some range -> range
    | None -> (t.cursor_pos, t.cursor_pos)
  in
  let a = Source.byte_offset old first and b = Source.byte_offset old last in
  let prefix = String.sub t.content 0 a in
  let suffix = String.sub t.content b (String.length t.content - b) in
  let candidate = Source.of_string (prefix ^ inserted ^ suffix) in
  let candidate, inserted_bytes =
    if Source.length candidate <= t.max_length then
      (candidate, String.length inserted)
    else
      let available =
        max 0 (t.max_length - (Source.length old - (last - first)))
      in
      let admitted = truncate_graphemes inserted available in
      (Source.of_string (prefix ^ admitted ^ suffix), String.length admitted)
  in
  let content = Source.text candidate in
  if String.equal content t.content then false
  else begin
    save_undo t;
    t.content <- content;
    t.cache <- Some candidate;
    t.cursor_pos <-
      Source.grapheme_of_byte candidate ~affinity:Downstream (a + inserted_bytes);
    t.affinity <- Layout.Downstream;
    t.selection_anchor <- None;
    true
  end

let delete_backward t =
  if has_selection t then delete_selection t
  else if t.cursor_pos = 0 then false
  else begin
    save_undo t;
    delete_grapheme_range t (t.cursor_pos - 1) t.cursor_pos;
    true
  end

let delete_forward t =
  if has_selection t then delete_selection t
  else
    let c = ensure_cache t in
    if t.cursor_pos >= Source.length c then false
    else begin
      save_undo t;
      delete_grapheme_range t t.cursor_pos (t.cursor_pos + 1);
      true
    end

(* ───── Word boundaries ───── *)

let grapheme_contains_line_break t c i =
  let start = byte_offset_of_grapheme_in_content c i in
  let stop = byte_offset_of_grapheme_in_content c (i + 1) in
  let rec loop byte_offset =
    byte_offset < stop
    &&
    match String.unsafe_get t.content byte_offset with
    | '\n' | '\r' -> true
    | _ -> loop (byte_offset + 1)
  in
  loop start

let word_break_set t =
  let c = ensure_cache t in
  let s = Hashtbl.create 16 in
  Matrix.Text.iter_wrap_breaks
    (fun ~break_byte_offset ~next_byte_offset ~grapheme_offset ->
      ignore break_byte_offset;
      ignore next_byte_offset;
      Hashtbl.replace s grapheme_offset ())
    t.content;
  for i = 0 to Source.length c - 1 do
    if grapheme_contains_line_break t c i then Hashtbl.replace s i ()
  done;
  s

let next_word_boundary t =
  let c = ensure_cache t in
  let breaks = word_break_set t in
  let pos = ref t.cursor_pos in
  while !pos < Source.length c && not (Hashtbl.mem breaks !pos) do
    incr pos
  done;
  while !pos < Source.length c && Hashtbl.mem breaks !pos do
    incr pos
  done;
  !pos

let prev_word_boundary t =
  let breaks = word_break_set t in
  let pos = ref t.cursor_pos in
  while !pos > 0 && Hashtbl.mem breaks (!pos - 1) do
    decr pos
  done;
  while !pos > 0 && not (Hashtbl.mem breaks (!pos - 1)) do
    decr pos
  done;
  !pos

(* ───── Word deletion ───── *)

let delete_word_backward t =
  if has_selection t then delete_selection t
  else if t.cursor_pos = 0 then false
  else begin
    let pos = prev_word_boundary t in
    save_undo t;
    delete_grapheme_range t pos t.cursor_pos;
    true
  end

let delete_word_forward t =
  if has_selection t then delete_selection t
  else
    let c = ensure_cache t in
    if t.cursor_pos >= Source.length c then false
    else begin
      let pos = next_word_boundary t in
      save_undo t;
      delete_grapheme_range t t.cursor_pos pos;
      true
    end

let delete_to_line_start t =
  if has_selection t then delete_selection t
  else
    let c = ensure_cache t in
    let line = find_line c t.cursor_pos in
    let line_start = Source.line_start c line in
    if t.cursor_pos <= line_start then false
    else begin
      save_undo t;
      delete_grapheme_range t line_start t.cursor_pos;
      true
    end

let delete_to_line_end t =
  if has_selection t then delete_selection t
  else
    let c = ensure_cache t in
    let line = find_line c t.cursor_pos in
    let line_end =
      if line + 1 < Source.line_count c then Source.line_start c (line + 1) - 1
      else Source.length c
    in
    if t.cursor_pos >= line_end then false
    else begin
      save_undo t;
      delete_grapheme_range t t.cursor_pos line_end;
      true
    end

let delete_to_start t =
  if has_selection t then delete_selection t
  else if t.cursor_pos = 0 then false
  else begin
    save_undo t;
    delete_grapheme_range t 0 t.cursor_pos;
    true
  end

let delete_to_end t =
  if has_selection t then delete_selection t
  else
    let c = ensure_cache t in
    if t.cursor_pos >= Source.length c then false
    else begin
      save_undo t;
      delete_grapheme_range t t.cursor_pos (Source.length c);
      true
    end

let delete_line t =
  let c = ensure_cache t in
  if Source.length c = 0 then false
  else begin
    save_undo t;
    let line = find_line c t.cursor_pos in
    let start, stop =
      if Source.line_count c = 1 then (0, Source.length c)
      else if line + 1 < Source.line_count c then
        (Source.line_start c line, Source.line_start c (line + 1))
      else (Source.line_start c line - 1, Source.length c)
    in
    delete_grapheme_range t start stop;
    true
  end

(* ───── Cursor movement ───── *)

let update_cursor_with_selection t ~select new_pos =
  t.affinity <- Layout.Downstream;
  let old_pos = t.cursor_pos in
  if select then begin
    (match t.selection_anchor with
    | None -> t.selection_anchor <- Some old_pos
    | Some _ -> ());
    t.cursor_pos <- new_pos;
    (* Clear selection if anchor equals cursor *)
    match t.selection_anchor with
    | Some a when a = t.cursor_pos -> t.selection_anchor <- None
    | _ -> ()
  end
  else begin
    t.cursor_pos <- new_pos;
    t.selection_anchor <- None
  end;
  old_pos <> new_pos

let move_left ?(select = false) t =
  if (not select) && has_selection t then begin
    let lo, _ = Option.get (selection t) in
    t.cursor_pos <- lo;
    t.selection_anchor <- None;
    t.affinity <- Layout.Downstream;
    true
  end
  else if t.cursor_pos = 0 then false
  else update_cursor_with_selection t ~select (t.cursor_pos - 1)

let move_right ?(select = false) t =
  let c = ensure_cache t in
  if (not select) && has_selection t then begin
    let _, hi = Option.get (selection t) in
    t.cursor_pos <- hi;
    t.selection_anchor <- None;
    t.affinity <- Layout.Downstream;
    true
  end
  else if t.cursor_pos >= Source.length c then false
  else update_cursor_with_selection t ~select (t.cursor_pos + 1)

let move_word_forward ?(select = false) t =
  let c = ensure_cache t in
  if (not select) && has_selection t then begin
    let _, hi = Option.get (selection t) in
    t.cursor_pos <- hi;
    t.selection_anchor <- None;
    t.affinity <- Layout.Downstream;
    true
  end
  else if t.cursor_pos >= Source.length c then false
  else
    let target = next_word_boundary t in
    update_cursor_with_selection t ~select target

let move_word_backward ?(select = false) t =
  if (not select) && has_selection t then begin
    let lo, _ = Option.get (selection t) in
    t.cursor_pos <- lo;
    t.selection_anchor <- None;
    t.affinity <- Layout.Downstream;
    true
  end
  else if t.cursor_pos = 0 then false
  else
    let target = prev_word_boundary t in
    update_cursor_with_selection t ~select target

let move_home ?(select = false) t =
  if (not select) && has_selection t then begin
    t.cursor_pos <- 0;
    t.selection_anchor <- None;
    t.affinity <- Layout.Downstream;
    true
  end
  else if t.cursor_pos = 0 then false
  else update_cursor_with_selection t ~select 0

let move_end ?(select = false) t =
  let c = ensure_cache t in
  if (not select) && has_selection t then begin
    t.cursor_pos <- Source.length c;
    t.selection_anchor <- None;
    t.affinity <- Layout.Downstream;
    true
  end
  else if t.cursor_pos >= Source.length c then false
  else update_cursor_with_selection t ~select (Source.length c)

let move_line_start ?(select = false) t =
  let c = ensure_cache t in
  let line = find_line c t.cursor_pos in
  let target = Source.line_start c line in
  if (not select) && has_selection t then begin
    t.cursor_pos <- target;
    t.selection_anchor <- None;
    t.affinity <- Layout.Downstream;
    true
  end
  else if t.cursor_pos = target then false
  else update_cursor_with_selection t ~select target

let move_line_end ?(select = false) t =
  let c = ensure_cache t in
  let line = find_line c t.cursor_pos in
  let target =
    if line + 1 < Source.line_count c then Source.line_start c (line + 1) - 1
    else Source.length c
  in
  if (not select) && has_selection t then begin
    t.cursor_pos <- target;
    t.selection_anchor <- None;
    t.affinity <- Layout.Downstream;
    true
  end
  else if t.cursor_pos >= target then false
  else update_cursor_with_selection t ~select target

let set_cursor_offset ?(select = false) t pos =
  let c = ensure_cache t in
  let pos = Int.max 0 (Int.min pos (Source.length c)) in
  ignore (update_cursor_with_selection t ~select pos : bool)

(* ───── Offset conversions ───── *)

let line_of_offset t pos =
  let c = ensure_cache t in
  let pos = Int.max 0 (Int.min pos (Source.length c)) in
  find_line c pos

let col_of_offset t pos =
  let c = ensure_cache t in
  let pos = Int.max 0 (Int.min pos (Source.length c)) in
  let line = find_line c pos in
  pos - Source.line_start c line

let line_start t line =
  let c = ensure_cache t in
  let line = Int.max 0 (Int.min line (Source.line_count c - 1)) in
  Source.line_start c line

let line_end t line =
  let c = ensure_cache t in
  let line = Int.max 0 (Int.min line (Source.line_count c - 1)) in
  if line + 1 < Source.line_count c then Source.line_start c (line + 1) - 1
  else Source.length c

let byte_offset t ~grapheme =
  let c = ensure_cache t in
  byte_offset_of_grapheme_in_content c grapheme

let set_caret ?(select = false) t (caret : Layout.caret) =
  set_cursor_offset ~select t caret.grapheme;
  t.affinity <- caret.affinity
