open Ansi

type style = {
  bold : bool;
  faint : bool; (* Corresponds to Ansi.style.`Dim` *)
  italic : bool;
  underline : bool;
  double_underline : bool;
  fg : Ansi.color;
  bg : Ansi.color;
  reversed : bool;
  link : string option;
}

type cell = { char : Uchar.t; style : style }
type cursor = { mutable row : int; mutable col : int; mutable visible : bool }

let default_style =
  {
    bold = false;
    faint = false;
    italic = false;
    underline = false;
    double_underline = false;
    fg = Default;
    bg = Default;
    reversed = false;
    link = None;
  }

let empty_cell = None

(* The `grid` and `cursor` fields are changed to be non-mutable.
   Their *contents* are mutable, but the record fields themselves are never
   re-assigned, which is what the original warning was about. *)
type t = {
  grid : cell option array array;
  mutable scrollback : cell option list list;
  scrollback_size : int;
  cursor : cursor;
  mutable gfx_state : style;
  parser : Parser.t;
  mutable title : string;
}

let rows (t : t) = Array.length t.grid
let cols (t : t) = if rows t = 0 then 0 else Array.length t.grid.(0)

let create ?(scrollback = 1000) ~rows ~cols () =
  let grid = Array.make_matrix rows cols empty_cell in
  {
    grid;
    scrollback = [];
    scrollback_size = scrollback;
    cursor = { row = 0; col = 0; visible = true };
    gfx_state = default_style;
    parser = Parser.create ();
    title = "";
  }

let clamp ~min ~max v = if v < min then min else if v > max then max else v

let set_cursor t ~row ~col =
  let row = clamp ~min:0 ~max:(rows t - 1) row in
  let col = clamp ~min:0 ~max:(cols t - 1) col in
  t.cursor.row <- row;
  t.cursor.col <- col

let clear_line t row from_col =
  for i = from_col to cols t - 1 do
    t.grid.(row).(i) <- empty_cell
  done

let clear_screen t =
  for r = 0 to rows t - 1 do
    for c = 0 to cols t - 1 do
      t.grid.(r).(c) <- empty_cell
    done
  done

let scroll_up (vte : t) n =
  if n > 0 && n < rows vte then (
    (* Add lines to scrollback *)
    if vte.scrollback_size > 0 then (
      for i = 0 to n - 1 do
        vte.scrollback <- Array.to_list vte.grid.(i) :: vte.scrollback
      done;
      let scrollback_len = List.length vte.scrollback in
      if scrollback_len > vte.scrollback_size then
        vte.scrollback <-
          List.rev
            (List.to_seq vte.scrollback
            |> Seq.take vte.scrollback_size
            |> List.of_seq)
          |> List.rev);
    (* Shift grid up *)
    for r = 0 to rows vte - 1 - n do
      vte.grid.(r) <- vte.grid.(r + n)
    done;
    (* Clear bottom lines *)
    for r = rows vte - n to rows vte - 1 do
      vte.grid.(r) <- Array.make (cols vte) empty_cell
    done)
  else if n > 0 then clear_screen vte

let scroll_down (t : t) n =
  if n > 0 && n < rows t then (
    for r = rows t - 1 downto n do
      t.grid.(r) <- t.grid.(r - n)
    done;
    for r = 0 to n - 1 do
      t.grid.(r) <- Array.make (cols t) empty_cell
    done)
  else if n > 0 then clear_screen t

let advance_cursor (t : t) =
  let { row; col; _ } = t.cursor in
  if col + 1 >= cols t then
    if row + 1 >= rows t then (
      scroll_up t 1;
      set_cursor t ~row:(rows t - 1) ~col:0)
    else set_cursor t ~row:(row + 1) ~col:0
  else set_cursor t ~row ~col:(col + 1)

let put_char (t : t) c =
  let { row; col; _ } = t.cursor in
  if row < rows t && col < cols t then
    t.grid.(row).(col) <- Some { char = c; style = t.gfx_state };
  advance_cursor t

let apply_sgr_attr state (attr : Ansi.attr) =
  match attr with
  | `Reset -> default_style
  | `Bold -> { state with bold = true }
  | `Dim -> { state with faint = true }
  | `Italic -> { state with italic = true }
  | `Underline -> { state with underline = true }
  | `Double_underline -> { state with double_underline = true }
  | `Reverse -> { state with reversed = true }
  | `Fg fg -> { state with fg }
  | `Bg bg -> { state with bg }
  (* Other styles are not represented in our style type, so we ignore them. *)
  | `Blink | `Conceal | `Strikethrough | `Overline | `Framed | `Encircled ->
      state

let handle_control t (ctrl : Parser.control) =
  match ctrl with
  | CUU n -> set_cursor t ~row:(t.cursor.row - n) ~col:t.cursor.col
  | CUD n -> set_cursor t ~row:(t.cursor.row + n) ~col:t.cursor.col
  | CUF n -> set_cursor t ~row:t.cursor.row ~col:(t.cursor.col + n)
  | CUB n -> set_cursor t ~row:t.cursor.row ~col:(t.cursor.col - n)
  | CNL n -> set_cursor t ~row:(t.cursor.row + n) ~col:0
  | CPL n -> set_cursor t ~row:(t.cursor.row - n) ~col:0
  | CHA n -> set_cursor t ~row:t.cursor.row ~col:(n - 1)
  | VPA n -> set_cursor t ~row:(n - 1) ~col:t.cursor.col
  | CUP (r, c) -> set_cursor t ~row:(r - 1) ~col:(c - 1)
  | ED n -> (
      match n with
      | 0 ->
          clear_line t t.cursor.row t.cursor.col;
          for r = t.cursor.row + 1 to rows t - 1 do
            clear_line t r 0
          done
      | 1 ->
          for r = 0 to t.cursor.row - 1 do
            clear_line t r 0
          done;
          for c = 0 to t.cursor.col do
            t.grid.(t.cursor.row).(c) <- empty_cell
          done
      | 2 | 3 ->
          clear_screen t;
          set_cursor t ~row:0 ~col:0
      | _ -> ())
  | EL n -> (
      match n with
      | 0 -> clear_line t t.cursor.row t.cursor.col
      | 1 ->
          for c = 0 to t.cursor.col do
            t.grid.(t.cursor.row).(c) <- empty_cell
          done
      | 2 -> clear_line t t.cursor.row 0
      | _ -> ())
  | OSC (0, title) | OSC (2, title) -> t.title <- title
  | Hyperlink (Some (_, uri)) ->
      t.gfx_state <- { t.gfx_state with link = Some uri }
  | Hyperlink None -> t.gfx_state <- { t.gfx_state with link = None }
  | Reset ->
      t.gfx_state <- default_style;
      clear_screen t;
      set_cursor t ~row:0 ~col:0
  (* Note: The provided Ansi.Parser does not seem to emit tokens for
     Scroll Up/Down or Cursor Show/Hide. If it did, they would be handled here. *)
  | OSC (_, _) | Unknown _ -> ()

let handle_text t text =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String text) in
  let rec decode_loop () =
    match Uutf.decode decoder with
    | `Uchar u ->
        (match Uchar.to_int u with
        | 0x0a ->
            if t.cursor.row + 1 >= rows t then scroll_up t 1
            else t.cursor.row <- t.cursor.row + 1;
            t.cursor.col <- 0
        | 0x0d -> set_cursor t ~row:t.cursor.row ~col:0
        | 0x08 -> set_cursor t ~row:t.cursor.row ~col:(t.cursor.col - 1)
        | 0x09 ->
            let next_tab = ((t.cursor.col / 8) + 1) * 8 in
            if next_tab < cols t then
              set_cursor t ~row:t.cursor.row ~col:next_tab
            else set_cursor t ~row:t.cursor.row ~col:(cols t - 1)
        | _ -> put_char t u);
        decode_loop ()
    | `Malformed _ -> decode_loop ()
    | `Await -> assert false
    | `End -> ()
  in
  decode_loop ()

let handle_token t = function
  | Parser.Text s -> handle_text t s
  | Parser.SGR attrs ->
      t.gfx_state <- List.fold_left apply_sgr_attr t.gfx_state attrs
  | Parser.Control ctrl -> handle_control t ctrl

let feed t bytes ofs len =
  let tokens = Parser.feed t.parser bytes ofs len in
  List.iter (handle_token t) tokens

let to_string_grid t =
  let buffer = Buffer.create (rows t * (cols t + 1)) in
  for r = 0 to rows t - 1 do
    for c = 0 to cols t - 1 do
      match t.grid.(r).(c) with
      | None -> Buffer.add_char buffer ' '
      | Some cell -> Buffer.add_utf_8_uchar buffer cell.char
    done;
    if r < rows t - 1 then Buffer.add_char buffer '\n'
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

let cursor_pos (t : t) = (t.cursor.row, t.cursor.col)
let is_cursor_visible (t : t) = t.cursor.visible
let set_cursor_visible (t : t) visible = t.cursor.visible <- visible
let title (t : t) = t.title

let reset t =
  clear_screen t;
  set_cursor t ~row:0 ~col:0;
  t.gfx_state <- default_style;
  Parser.reset t.parser;
  t.title <- "";
  t.scrollback <- []

let get_cell t ~row ~col =
  if row >= 0 && row < rows t && col >= 0 && col < cols t then
    t.grid.(row).(col)
  else None
