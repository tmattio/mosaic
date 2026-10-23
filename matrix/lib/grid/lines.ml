type weight = [ `Light | `Heavy | `Double ]

(* A cell is an int. Bits 0 to 7 hold the weights of its up, down, left and
   right arms, two bits each (0 for none, 1 light, 2 heavy, 3 double). Bits 8
   and 9 hold the direction of a line one cell long through it (across, down),
   and bits 10 and 11 that line's weight. The bits from 12 hold one more than
   the index of its style in [styles]. A cell no line passes through is 0. *)
type t = {
  width : int;
  height : int;
  cells : int array;
  mutable styles : Ansi.Style.t array;
  mutable n_styles : int;
}

let up = 0
let down = 2
let left = 4
let right = 6
let across = 0x100
let downward = 0x200
let lone_weight = 10
let style_shift = 12
let[@inline] field c k = (c lsr k) land 3
let weight_code = function `Light -> 1 | `Heavy -> 2 | `Double -> 3

let create ~width ~height =
  if width < 0 || height < 0 then
    invalid_arg
      (Printf.sprintf "Matrix_grid.Lines.create: size %dx%d" width height);
  {
    width;
    height;
    cells = Array.make (width * height) 0;
    styles = [||];
    n_styles = 0;
  }

let width t = t.width
let height t = t.height

let clear t =
  Array.fill t.cells 0 (Array.length t.cells) 0;
  t.n_styles <- 0

(* One more than the index of [s] in [t.styles], added if absent. A set holds
   few distinct styles, so a search is cheap, and after [clear] the styles of
   the next frame take the same slots without allocating. *)
let style_slot t s =
  let rec find i =
    if i = t.n_styles then begin
      if i = Array.length t.styles then begin
        let styles = Array.make (max 4 (2 * i)) s in
        Array.blit t.styles 0 styles 0 i;
        t.styles <- styles
      end;
      t.styles.(i) <- s;
      t.n_styles <- i + 1;
      i + 1
    end
    else
      let s' = t.styles.(i) in
      if s' == s || Ansi.Style.equal s' s then i + 1 else find (i + 1)
  in
  find 0

(* Adds the arms and lone line of [bits] to cell [(x, y)], each arm keeping
   the heavier of its weights. A new cell takes the style of slot [slot]. *)
let mark t ~x ~y ~slot bits =
  if x >= 0 && y >= 0 && x < t.width && y < t.height then begin
    let i = (y * t.width) + x in
    let c = t.cells.(i) in
    let c = if c = 0 then slot lsl style_shift else c in
    let heavier c k =
      let w = field bits k in
      if w > field c k then c land lnot (3 lsl k) lor (w lsl k) else c
    in
    t.cells.(i) <-
      heavier
        (heavier (heavier (heavier (heavier c up) down) left) right)
        lone_weight
      lor (bits land (across lor downward))
  end

(* A line of [length] cells from [(x, y)] along [dx, dy], whose cells have
   arms [back] and [forth] toward the line's start and end. *)
let line ~weight ~style t ~x ~y ~length ~dx ~dy ~back ~forth ~lone =
  if length > 0 then begin
    let w = weight_code weight and slot = style_slot t style in
    if length = 1 then mark t ~x ~y ~slot (lone lor (w lsl lone_weight))
    else
      for i = 0 to length - 1 do
        mark t
          ~x:(x + (i * dx))
          ~y:(y + (i * dy))
          ~slot
          ((if i > 0 then w lsl back else 0)
          lor if i < length - 1 then w lsl forth else 0)
      done
  end

let hline ?(weight = `Light) ?(style = Ansi.Style.default) t ~x ~y ~length =
  line ~weight ~style t ~x ~y ~length ~dx:1 ~dy:0 ~back:left ~forth:right
    ~lone:across

let vline ?(weight = `Light) ?(style = Ansi.Style.default) t ~x ~y ~length =
  line ~weight ~style t ~x ~y ~length ~dx:0 ~dy:1 ~back:up ~forth:down
    ~lone:downward

let arm ?(weight = `Light) ?(style = Ansi.Style.default) t ~x ~y dir =
  let k =
    match dir with `Up -> up | `Down -> down | `Left -> left | `Right -> right
  in
  mark t ~x ~y ~slot:(style_slot t style) (weight_code weight lsl k)

let code t idx = t.cells.(idx)
let style t c = t.styles.((c lsr style_shift) - 1)

(* The arms of a cell, its lone line given arms along its direction. *)
let arms c =
  let a = c land 0xff in
  if a <> 0 then a
  else
    let w = field c lone_weight in
    if c land downward <> 0 then (w lsl up) lor (w lsl down)
    else (w lsl left) lor (w lsl right)

(* ───── Glyph sets ───── *)

type set = {
  horizontal : string;
  vertical : string;
  top_left : string;
  top_right : string;
  bottom_left : string;
  bottom_right : string;
  top_t : string;
  bottom_t : string;
  left_t : string;
  right_t : string;
  cross : string;
}

type glyphs = Unicode | Rounded | Set of set

let ascii =
  Set
    {
      horizontal = "-";
      vertical = "|";
      top_left = "+";
      top_right = "+";
      bottom_left = "+";
      bottom_right = "+";
      top_t = "+";
      bottom_t = "+";
      left_t = "+";
      right_t = "+";
      cross = "+";
    }

let of_border (b : Border.t) =
  let s u =
    let buf = Bytes.create (Uchar.utf_8_byte_length u) in
    ignore (Bytes.set_utf_8_uchar buf 0 u);
    Bytes.unsafe_to_string buf
  in
  Set
    {
      horizontal = s b.horizontal;
      vertical = s b.vertical;
      top_left = s b.top_left;
      top_right = s b.top_right;
      bottom_left = s b.bottom_left;
      bottom_right = s b.bottom_right;
      top_t = s b.top_t;
      bottom_t = s b.bottom_t;
      left_t = s b.left_t;
      right_t = s b.right_t;
      cross = s b.cross;
    }

let glyph s c =
  let a = arms c in
  let has k = field a k <> 0 in
  match (has up, has down, has left, has right) with
  | _, _, false, false -> s.vertical
  | false, false, _, _ -> s.horizontal
  | false, true, false, true -> s.top_left
  | false, true, true, false -> s.top_right
  | true, false, false, true -> s.bottom_left
  | true, false, true, false -> s.bottom_right
  | false, true, true, true -> s.top_t
  | true, false, true, true -> s.bottom_t
  | true, true, false, true -> s.left_t
  | true, true, true, false -> s.right_t
  | true, true, true, true -> s.cross

(* Unicode's box-drawing characters, each with the weights of its up, down,
   left and right arms: '.' none, 'l' light or single, 'h' heavy, 'd'
   double. The half lines (U+2574 to U+257B) are left out, since a cell with
   arms on one axis is drawn whole, and so are the dashed and diagonal
   lines. *)
let box_drawing =
  [ "─ ..ll"; "━ ..hh"; "│ ll.."; "┃ hh.."; "┌ .l.l"; "┍ .l.h";
    "┎ .h.l"; "┏ .h.h"; "┐ .ll."; "┑ .lh."; "┒ .hl."; "┓ .hh.";
    "└ l..l"; "┕ l..h"; "┖ h..l"; "┗ h..h"; "┘ l.l."; "┙ l.h.";
    "┚ h.l."; "┛ h.h."; "├ ll.l"; "┝ ll.h"; "┞ hl.l"; "┟ lh.l";
    "┠ hh.l"; "┡ hl.h"; "┢ lh.h"; "┣ hh.h"; "┤ lll."; "┥ llh.";
    "┦ hll."; "┧ lhl."; "┨ hhl."; "┩ hlh."; "┪ lhh."; "┫ hhh.";
    "┬ .lll"; "┭ .lhl"; "┮ .llh"; "┯ .lhh"; "┰ .hll"; "┱ .hhl";
    "┲ .hlh"; "┳ .hhh"; "┴ l.ll"; "┵ l.hl"; "┶ l.lh"; "┷ l.hh";
    "┸ h.ll"; "┹ h.hl"; "┺ h.lh"; "┻ h.hh"; "┼ llll"; "┽ llhl";
    "┾ lllh"; "┿ llhh"; "╀ hlll"; "╁ lhll"; "╂ hhll"; "╃ hlhl";
    "╄ hllh"; "╅ lhhl"; "╆ lhlh"; "╇ hlhh"; "╈ lhhh"; "╉ hhhl";
    "╊ hhlh"; "╋ hhhh"; "═ ..dd"; "║ dd.."; "╒ .l.d"; "╓ .d.l";
    "╔ .d.d"; "╕ .ld."; "╖ .dl."; "╗ .dd."; "╘ l..d"; "╙ d..l";
    "╚ d..d"; "╛ l.d."; "╜ d.l."; "╝ d.d."; "╞ ll.d"; "╟ dd.l";
    "╠ dd.d"; "╡ lld."; "╢ ddl."; "╣ ddd."; "╤ .ldd"; "╥ .dll";
    "╦ .ddd"; "╧ l.dd"; "╨ d.ll"; "╩ d.dd"; "╪ lldd"; "╫ ddll";
    "╬ dddd"; "╼ ..lh"; "╽ lh.."; "╾ ..hl"; "╿ hl.." ]
[@@ocamlformat "disable"]

let rounded_corners = [ "╭ .l.l"; "╮ .ll."; "╰ l..l"; "╯ l.l." ]

(* The arms and character of an entry of the lists above. *)
let entry s =
  let d = String.get_utf_8_uchar s 0 in
  let n = Uchar.utf_decode_length d in
  let w i =
    match s.[n + 1 + i] with 'l' -> 1 | 'h' -> 2 | 'd' -> 3 | _ -> 0
  in
  ( (w 0 lsl up) lor (w 1 lsl down) lor (w 2 lsl left) lor (w 3 lsl right),
    Uchar.to_int (Uchar.utf_decode_uchar d) )

(* The character of every combination of arms, by the rules of the module's
   documentation: arms on one axis make its whole line, then where Unicode has
   no character, heavy becomes double beside a double arm and the arms of an
   axis take the heavier weight. Every such combination has a character, so
   the second lookup always succeeds. *)
let table corners =
  let exact = Array.make 256 (-1) in
  List.iter
    (fun s ->
      let a, u = entry s in
      exact.(a) <- u)
    (box_drawing @ corners);
  Array.init 256 (fun a ->
      let u = field a up and d = field a down in
      let l = field a left and r = field a right in
      let whole a b = if a = 0 then b else a in
      let u, d = if l = 0 && r = 0 then (whole u d, whole d u) else (u, d) in
      let l, r = if u = 0 && d = 0 then (whole l r, whole r l) else (l, r) in
      let pack u d l r =
        (u lsl up) lor (d lsl down) lor (l lsl left) lor (r lsl right)
      in
      let found = exact.(pack u d l r) in
      if found >= 0 then found
      else
        let double = u = 3 || d = 3 || l = 3 || r = 3 in
        let w x = if double && x = 2 then 3 else x in
        let u, d, l, r = (w u, w d, w l, w r) in
        let pair a b = if a > 0 && b > 0 then (max a b, max a b) else (a, b) in
        let u, d = pair u d and l, r = pair l r in
        exact.(pack u d l r))

let square = table []
let round = table rounded_corners

let scalar ~rounded c =
  Uchar.unsafe_of_int (if rounded then round else square).(arms c)
