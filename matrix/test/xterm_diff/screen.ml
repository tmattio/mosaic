(* The screen model both sides are reduced to, xterm's print-screen decoder,
   and the comparison.

   xterm's media copy ([CSI i]) with [printAttributes: 2] writes, for each row,
   [ESC # 5] (the line's DECDWL/DECDHL state), [SGR 0], the cells, [SGR 0], CR,
   LF; the screen ends with a form feed ([printerFormFeed: true]). See
   [printLine] in xterm's print.c. Three facts of that format shape the
   decoding:

   - An SGR is emitted before a cell iff the cell has attributes or its colours
     differ from the last emitted ones. The test compares the cell's attributes
     with [0], not with the last emitted attributes, so a plain cell after a
     reverse one gets no [SGR 0]. A cell with no SGR before it therefore has no
     attributes and default colours: decoding uses "the SGR just before the
     cell, else the default", never the running SGR state.
   - Trailing cells no character was drawn in (erased or never written) are not
     printed, whatever their colour; matrix has no such distinction, so both
     sides drop trailing spaces.
   - The SGR test is skipped for cells holding no character (code 0, which is
     how xterm stores the rows a scroll brings in, whatever their colour): they
     print as a space with no SGR. A space with no SGR before it therefore has
     unknown attributes, and is compared by its text only.
   - The second cell of a wide character holds U+FFFF, printed as is with
     [printRawChars: true]; combining characters follow their base. xterm can
     leave a U+FFFF whose first half was overwritten (it draws nothing there);
     it decodes as a space. *)

type color = Default | Index of int | Rgb of int * int * int

(* The attributes xterm's print-screen reports (xtermFormatSGR in misc.c). *)
let bold = 0x01
let underline = 0x02
let blink = 0x04
let inverse = 0x08
let invisible = 0x10
let faint = 0x20
let italic = 0x40
let strike = 0x80
let dbl_under = 0x100

type cell = {
  text : string;  (** The grapheme, [""] for the second cell of a wide one. *)
  cont : bool;
  attrs : int;
  fg : color;
  bg : color;
  known : bool;
      (** [false] when the print does not tell the attributes: see [decode]. *)
}

type t = { lines : cell array array; line_attrs : char array }

let blank =
  {
    text = " ";
    cont = false;
    attrs = 0;
    fg = Default;
    bg = Default;
    known = true;
  }

(* {1 Escaping} *)

let escape s =
  let b = Buffer.create (String.length s * 2) in
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string b "\\\\"
      | '\027' -> Buffer.add_string b "\\e"
      | ' ' .. '~' -> Buffer.add_char b c
      | c -> Printf.bprintf b "\\x%02x" (Char.code c))
    s;
  Buffer.contents b

let unescape s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i < n then
      if s.[i] = '\\' && i + 1 < n then (
        match s.[i + 1] with
        | 'e' ->
            Buffer.add_char b '\027';
            go (i + 2)
        | 'n' ->
            Buffer.add_char b '\n';
            go (i + 2)
        | 'r' ->
            Buffer.add_char b '\r';
            go (i + 2)
        | 't' ->
            Buffer.add_char b '\t';
            go (i + 2)
        | 'x' when i + 3 < n ->
            Buffer.add_char b
              (Char.chr (int_of_string ("0x" ^ String.sub s (i + 2) 2)));
            go (i + 4)
        | 'u' when i + 2 < n && s.[i + 2] = '{' ->
            let close = String.index_from s i '}' in
            let cp =
              int_of_string ("0x" ^ String.sub s (i + 3) (close - i - 3))
            in
            Buffer.add_utf_8_uchar b (Uchar.of_int cp);
            go (close + 1)
        | c ->
            Buffer.add_char b c;
            go (i + 2))
      else (
        Buffer.add_char b s.[i];
        go (i + 1))
  in
  go 0;
  Buffer.contents b

(* Shows non-ASCII as [\u{...}] for reading, controls as [\e] or [\xHH]. *)
let pretty s =
  let b = Buffer.create (String.length s * 2) in
  let n = String.length s in
  let rec go i =
    if i < n then (
      let c = s.[i] in
      if Char.code c < 0x80 then (
        Buffer.add_string b (escape (String.make 1 c));
        go (i + 1))
      else
        let d = String.get_utf_8_uchar s i in
        let len = Uchar.utf_decode_length d in
        if Uchar.utf_decode_is_valid d then
          Printf.bprintf b "\\u{%x}" (Uchar.to_int (Uchar.utf_decode_uchar d))
        else Buffer.add_string b (escape (String.sub s i len));
        go (i + len))
  in
  go 0;
  Buffer.contents b

(* {1 xterm's print-screen} *)

let apply_sgr (attrs, fg, bg) params =
  let field f =
    List.map
      (fun x -> if x = "" then -1 else try int_of_string x with _ -> -1)
      (String.split_on_char ':' f)
  in
  let st = ref (attrs, fg, bg) in
  let set_attr bit =
    let a, f, b = !st in
    st := (a lor bit, f, b)
  in
  let set_fg c =
    let a, _, b = !st in
    st := (a, c, b)
  in
  let set_bg c =
    let a, f, _ = !st in
    st := (a, f, c)
  in
  let ext = function
    | [ _; 5; n ] -> Some (Index n)
    | [ _; 2; _; r; g; b ] | [ _; 2; r; g; b ] -> Some (Rgb (r, g, b))
    | _ -> None
  in
  List.iter
    (fun f ->
      match field f with
      | [] | [ -1 ] | [ 0 ] -> st := (0, Default, Default)
      | [ 1 ] -> set_attr bold
      | [ 2 ] -> set_attr faint
      | [ 3 ] -> set_attr italic
      | [ 4 ] -> set_attr underline
      | [ 5 ] -> set_attr blink
      | [ 7 ] -> set_attr inverse
      | [ 8 ] -> set_attr invisible
      | [ 9 ] -> set_attr strike
      | [ 21 ] -> set_attr dbl_under
      | [ n ] when n >= 30 && n <= 37 -> set_fg (Index (n - 30))
      | [ n ] when n >= 90 && n <= 97 -> set_fg (Index (n - 90 + 8))
      | [ n ] when n >= 40 && n <= 47 -> set_bg (Index (n - 40))
      | [ n ] when n >= 100 && n <= 107 -> set_bg (Index (n - 100 + 8))
      | 38 :: _ as l -> Option.iter set_fg (ext l)
      | 48 :: _ as l -> Option.iter set_bg (ext l)
      | _ -> ())
    params;
  !st

(* [decode ~width ~rows dump] is the screen [dump] describes. [width cp] is
   xterm's own width of [cp], measured once per session (see
   [Xterm.calibrate]): it tells a combining character from a cell's base, which
   xterm prints the same way, and a wide character's second half from an
   orphan. *)
let decode ~width ~rows ~cols dump =
  let zero_width cp = width cp = 0 in
  let first_cp s =
    if s = "" then 0
    else Uchar.to_int (Uchar.utf_decode_uchar (String.get_utf_8_uchar s 0))
  in
  let lines = ref [] and line_attrs = ref [] in
  let row = ref [] and line_attr = ref '5' and pending = ref None in
  let prefix = ref false in
  let end_row () =
    lines := Array.of_list (List.rev !row) :: !lines;
    line_attrs := !line_attr :: !line_attrs;
    row := [];
    line_attr := '5';
    pending := None
  in
  let state () =
    match !pending with
    | Some st ->
        pending := None;
        (st, true)
    | None -> ((0, Default, Default), false)
  in
  let n = String.length dump in
  let rec go i =
    if i < n then
      match dump.[i] with
      | '\027' when i + 1 < n && dump.[i + 1] = '[' ->
          let j = String.index_from dump i 'm' in
          let params =
            String.split_on_char ';' (String.sub dump (i + 2) (j - i - 2))
          in
          (* The [SGR 0] after a row's line attribute is the row's prefix, sent
             whatever the first cell holds. *)
          if !prefix then prefix := false
          else pending := Some (apply_sgr (0, Default, Default) params);
          go (j + 1)
      | '\027' when i + 2 < n && dump.[i + 1] = '#' ->
          line_attr := dump.[i + 2];
          prefix := true;
          go (i + 3)
      | '\r' | '\x0e' | '\x0f' -> go (i + 1)
      | '\n' ->
          end_row ();
          go (i + 1)
      | '\x0c' -> ()
      | _ ->
          let d = String.get_utf_8_uchar dump i in
          let len = Uchar.utf_decode_length d in
          let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
          let text = String.sub dump i len in
          (if cp = 0xFFFF then
             let (attrs, fg, bg), _ = state () in
             match !row with
             | prev :: _ when (not prev.cont) && width (first_cp prev.text) = 2
               ->
                 row :=
                   { text = ""; cont = true; attrs; fg; bg; known = true }
                   :: !row
             | _ -> row := { blank with known = false } :: !row
           else
             let rec attach = function
               | c :: rest when c.cont -> (
                   match attach rest with
                   | Some rest -> Some (c :: rest)
                   | None -> None)
               | c :: rest -> Some ({ c with text = c.text ^ text } :: rest)
               | [] -> None
             in
             match if zero_width cp then attach !row else None with
             | Some r -> row := r
             | None ->
                 let (attrs, fg, bg), emitted = state () in
                 let known = emitted || text <> " " in
                 row := { text; cont = false; attrs; fg; bg; known } :: !row);
          go (i + len)
  in
  go 0;
  if !row <> [] then end_row ();
  let lines = Array.of_list (List.rev !lines) in
  let line_attrs = Array.of_list (List.rev !line_attrs) in
  let get a i d = if i < Array.length a then a.(i) else d in
  {
    lines =
      Array.init rows (fun r ->
          let l = get lines r [||] in
          if Array.length l > cols then Array.sub l 0 cols else l);
    line_attrs = Array.init rows (fun r -> get line_attrs r '5');
  }

(* {1 Comparison} *)

let trim cells =
  let n = ref (Array.length cells) in
  while !n > 0 && (not cells.(!n - 1).cont) && cells.(!n - 1).text = " " do
    decr n
  done;
  Array.sub cells 0 !n

(* The attributes that show on a space. *)
let visible_on_blank = inverse lor underline lor dbl_under lor strike

let cells_match a b =
  a.text = b.text && a.cont = b.cont
  && (a.cont
     ||
     if a.text = " " && not (a.known && b.known) then true
     else if a.text = " " then
       let va = a.attrs land visible_on_blank
       and vb = b.attrs land visible_on_blank in
       a.bg = b.bg && va = vb && (va = 0 || a.fg = b.fg)
     else a.attrs = b.attrs && a.fg = b.fg && a.bg = b.bg)

type kind = Crash | Text | Cursor | Attr | Reply | Line

let kind_name = function
  | Crash -> "crash"
  | Text -> "text"
  | Attr -> "attr"
  | Line -> "line"
  | Reply -> "reply"
  | Cursor -> "cursor"

type diff = { kind : kind; where : string; xterm : string; matrix : string }

let color_to_string = function
  | Default -> "default"
  | Index i -> string_of_int i
  | Rgb (r, g, b) -> Printf.sprintf "#%02x%02x%02x" r g b

let attrs_to_string a =
  let names =
    [
      (bold, "bold");
      (faint, "faint");
      (italic, "italic");
      (underline, "ul");
      (dbl_under, "dblul");
      (blink, "blink");
      (inverse, "rev");
      (invisible, "invis");
      (strike, "strike");
    ]
  in
  match List.filter (fun (b, _) -> a land b <> 0) names with
  | [] -> "-"
  | l -> String.concat "+" (List.map snd l)

let cell_to_string c =
  if c.cont then "<cont>"
  else
    Printf.sprintf "%S %s fg=%s bg=%s" (pretty c.text) (attrs_to_string c.attrs)
      (color_to_string c.fg) (color_to_string c.bg)

let compare_screens ~(xterm : t) ~(matrix : t) =
  let diffs = ref [] in
  Array.iteri
    (fun r xl ->
      let xl = trim xl and ml = trim matrix.lines.(r) in
      let n = max (Array.length xl) (Array.length ml) in
      let get a i = if i < Array.length a then a.(i) else blank in
      (* After a text difference the rest of the row is usually shifted: only
         the first one of a row is reported. *)
      let text_seen = ref false in
      for c = 0 to n - 1 do
        let x = get xl c and m = get ml c in
        if not (cells_match x m) then begin
          let kind =
            if x.text <> m.text || x.cont <> m.cont then Text else Attr
          in
          let report =
            (kind = Attr || not !text_seen) && List.length !diffs < 64
          in
          if kind = Text then text_seen := true;
          if report then
            diffs :=
              {
                kind;
                where = Printf.sprintf "row %d col %d" r c;
                xterm = cell_to_string x;
                matrix = cell_to_string m;
              }
              :: !diffs
        end
      done;
      if xterm.line_attrs.(r) <> matrix.line_attrs.(r) && Array.length xl > 0
      then
        diffs :=
          {
            kind = Line;
            where = Printf.sprintf "row %d" r;
            xterm = String.make 1 xterm.line_attrs.(r);
            matrix = String.make 1 matrix.line_attrs.(r);
          }
          :: !diffs)
    xterm.lines;
  List.rev !diffs

let render s =
  let b = Buffer.create 256 in
  Array.iteri
    (fun r l ->
      let l = trim l in
      Buffer.add_string b "  |";
      Array.iter (fun c -> if not c.cont then Buffer.add_string b c.text) l;
      Buffer.add_string b "|";
      let styled =
        Array.to_list l
        |> List.mapi (fun i c -> (i, c))
        |> List.filter (fun (_, c) ->
            (not c.cont) && (c.attrs <> 0 || c.fg <> Default || c.bg <> Default))
      in
      if styled <> [] then
        Printf.bprintf b "   %s"
          (String.concat " "
             (List.map
                (fun (i, c) ->
                  Printf.sprintf "%d:%s/%s/%s" i (attrs_to_string c.attrs)
                    (color_to_string c.fg) (color_to_string c.bg))
                styled));
      if s.line_attrs.(r) <> '5' then
        Printf.bprintf b "   ESC#%c" s.line_attrs.(r);
      ignore r;
      Buffer.add_char b '\n')
    s.lines;
  Buffer.contents b
