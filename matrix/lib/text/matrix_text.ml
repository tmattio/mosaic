open StdLabels

type width_method = [ `Unicode | `Unicode_wide | `Wcwidth ]
type line_break_kind = [ `LF | `CR | `CRLF ]
type position = { byte_offset : int; units : int; columns_used : int }

let default_tab_width = 2

(* ASCII Helpers *)

let[@inline] normalize_tab_width w = if w <= 0 then default_tab_width else w

let check_sub name str ~pos ~len =
  let str_len = String.length str in
  if pos < 0 then invalid_arg (name ^ ": negative position");
  if len < 0 then invalid_arg (name ^ ": negative length");
  if pos > str_len || len > str_len - pos then
    invalid_arg (name ^ ": substring out of bounds")

(* Width of an ASCII byte (0-127). Tab returns tab_width, printable (0x20-0x7E)
   returns 1, control characters return 0. Two comparisons instead of a table
   lookup (which costs 3 dependent memory loads). *)
let[@inline] ascii_width ~tab_width b =
  if b = 0x09 then tab_width else if b >= 0x20 && b <= 0x7E then 1 else 0

(* Check if 4 consecutive bytes are all ASCII (< 128). Uses native int
   operations only — zero allocation on 64-bit OCaml. *)
let[@inline] is_ascii_4 str i =
  let c0 = Char.code (String.unsafe_get str i) in
  let c1 = Char.code (String.unsafe_get str (i + 1)) in
  let c2 = Char.code (String.unsafe_get str (i + 2)) in
  let c3 = Char.code (String.unsafe_get str (i + 3)) in
  c0 lor c1 lor c2 lor c3 < 128

let rec is_ascii_only_tail str len j =
  j >= len
  || Char.code (String.unsafe_get str j) < 128
     && is_ascii_only_tail str len (j + 1)

let rec is_ascii_only str len i =
  if i + 4 <= len then is_ascii_4 str i && is_ascii_only str len (i + 4)
  else is_ascii_only_tail str len i

(* Width Predicates *)

let[@inline] is_regional_indicator cp = cp >= 0x1F1E6 && cp <= 0x1F1FF
let[@inline] is_emoji_modifier cp = cp >= 0x1F3FB && cp <= 0x1F3FF

(* OpenTUI widens spacing marks with positive scalar width. Its width table
   zeroes Mc marks, unlike Uucp's tty hints. In Unicode 17, the remaining
   spacing marks are Thai SARA AM and Lao AM. *)
let[@inline] is_wide_spacing_mark cp = cp = 0x0E33 || cp = 0x0EB3

(* Codepoint Width *)

let[@inline] codepoint_width_wcwidth ~tab_width cp =
  if cp < 0x80 then
    if cp = 0x09 then tab_width else if cp < 32 || cp = 127 then 0 else 1
  else
    let w = Unicode.tty_width_hint (Uchar.unsafe_of_int cp) in
    if w = -1 then 0 else w

let uchar_width ~tab_width uchar =
  codepoint_width_wcwidth
    ~tab_width:(normalize_tab_width tab_width)
    (Uchar.to_int uchar)

(* Unicode-width state machine.

   The [`Unicode] and [`Unicode_wide] methods fold the scalars of a unit
   through a small state machine handling VS16 emoji promotion, emoji
   modifier sequences, regional-indicator pairs and spacing marks. The whole
   state is one immediate int: the unit's width in the bits above bit 3 and
   four flags below. The width bits are [0] while [ws_has_width] is unset,
   except in a TAB's unit, which [unit_step] makes [tab] wide without it.

   The initial state is [0]; [ws_step] handles the first codepoint of a unit
   like any other (from state [0] every branch reduces to the unit-start
   behaviour). *)

let ws_has_width = 1 (* cluster has a base width *)
let ws_ri_pair = 2 (* last codepoint was the first RI of a pair *)
let ws_emoji = 4 (* last codepoint was a narrow emoji *)
let ws_base = 8 (* last codepoint was a narrow emoji modifier base *)
let ws_last = ws_emoji lor ws_base
let[@inline] ws_make w flags = (w lsl 4) lor flags
let[@inline] ws_width state = state lsr 4

(* [ws_step cp cp_w state] advances the state machine by the codepoint [cp]
   whose individual display width is [cp_w]. *)
let[@inline] ws_step ~wide cp cp_w state =
  let flags = state land 15 in
  if cp = 0xFE0F then
    if flags land ws_emoji <> 0 && ws_width state = 1 then ws_make 2 flags
    else state
  else if is_regional_indicator cp then
    if flags land ws_ri_pair <> 0 then
      ws_make (ws_width state + cp_w) ws_has_width
    else
      let w = if flags land ws_has_width = 0 then cp_w else ws_width state in
      ws_make w (flags lor ws_has_width lor ws_ri_pair)
  else if
    wide
    && flags land ws_has_width <> 0
    && not (Unicode.is_zero_in_grapheme (Uchar.unsafe_of_int cp))
  then ws_make (max 2 (ws_width state)) (flags land lnot ws_last)
  else if flags land ws_has_width <> 0 && cp_w > 0 && is_wide_spacing_mark cp
  then ws_make (max 2 (ws_width state)) (flags land lnot ws_last)
  else if (not wide) && flags land ws_base <> 0 && is_emoji_modifier cp then
    ws_make 2 (flags land lnot ws_last)
  else if flags land ws_has_width = 0 && cp_w > 0 then
    let u = Uchar.unsafe_of_int cp in
    let emoji =
      if cp_w = 1 && Unicode.is_emoji u then
        if Unicode.is_emoji_modifier_base u then ws_last else ws_emoji
      else 0
    in
    ws_make cp_w (flags lor ws_has_width lor emoji)
  else state land lnot ws_last

(* UTF-8 decoding

   The folds below are slower with direct standard decoding (bench_text's
   position cases by 10 to 60%): even a cold decoder call spills loop state on
   valid input. Keep this inline decoder and test its malformed prefix
   consumption against the standard decoder. It returns the scalar and its byte length packed in one immediate
   int, [(cp lsl 3) lor len], so decoding allocates nothing. Bytes from
   [limit] on are not read: a scalar cut by [limit] is malformed. *)

let[@inline] is_cont_byte b = b land 0xC0 = 0x80
let[@inline] decoded_cp d = d lsr 3
let[@inline] decoded_len d = d land 7
let replacement_1 = (0xFFFD lsl 3) lor 1
let replacement_2 = (0xFFFD lsl 3) lor 2
let replacement_3 = (0xFFFD lsl 3) lor 3

let[@inline] decode_malformed str pos limit b0 =
  if b0 < 0xE0 || b0 > 0xF4 || pos + 1 >= limit then replacement_1
  else
    let b1 = Char.code (String.unsafe_get str (pos + 1)) in
    if
      (not (is_cont_byte b1))
      || (b0 = 0xE0 && b1 < 0xA0)
      || (b0 = 0xED && b1 >= 0xA0)
      || (b0 = 0xF0 && b1 < 0x90)
      || (b0 = 0xF4 && b1 >= 0x90)
    then replacement_1
    else if b0 < 0xF0 || pos + 2 >= limit then replacement_2
    else if is_cont_byte (Char.code (String.unsafe_get str (pos + 2))) then
      replacement_3
    else replacement_2

let[@inline] decode str pos limit =
  let b0 = Char.code (String.unsafe_get str pos) in
  if b0 < 0x80 then (b0 lsl 3) lor 1
  else if b0 land 0xE0 = 0xC0 then
    if pos + 1 < limit then
      let b1 = Char.code (String.unsafe_get str (pos + 1)) in
      if is_cont_byte b1 then
        let cp = ((b0 land 0x1F) lsl 6) lor (b1 land 0x3F) in
        if cp >= 0x80 then (cp lsl 3) lor 2
        else decode_malformed str pos limit b0
      else decode_malformed str pos limit b0
    else decode_malformed str pos limit b0
  else if b0 land 0xF0 = 0xE0 then
    if pos + 2 < limit then
      let b1 = Char.code (String.unsafe_get str (pos + 1)) in
      let b2 = Char.code (String.unsafe_get str (pos + 2)) in
      if is_cont_byte b1 && is_cont_byte b2 then
        let cp =
          ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F)
        in
        if cp >= 0x800 && (cp < 0xD800 || cp > 0xDFFF) then (cp lsl 3) lor 3
        else decode_malformed str pos limit b0
      else decode_malformed str pos limit b0
    else decode_malformed str pos limit b0
  else if b0 land 0xF8 = 0xF0 then
    if pos + 3 < limit then
      let b1 = Char.code (String.unsafe_get str (pos + 1)) in
      let b2 = Char.code (String.unsafe_get str (pos + 2)) in
      let b3 = Char.code (String.unsafe_get str (pos + 3)) in
      if is_cont_byte b1 && is_cont_byte b2 && is_cont_byte b3 then
        let cp =
          ((b0 land 0x07) lsl 18)
          lor ((b1 land 0x3F) lsl 12)
          lor ((b2 land 0x3F) lsl 6)
          lor (b3 land 0x3F)
        in
        if cp >= 0x10000 && cp <= 0x10FFFF then (cp lsl 3) lor 4
        else decode_malformed str pos limit b0
      else decode_malformed str pos limit b0
    else decode_malformed str pos limit b0
  else decode_malformed str pos limit b0

(* Display units

   A unit's state is one immediate int: the [ws_step] state of the unit in
   progress above bit 2, its kind in bits 1 and 2, and in bit 0 whether the
   last scalar started it. One transition, [unit_step], serves the streaming
   {!Segmenter} and the folds below, which keep the state in a loop variable
   so that measuring allocates nothing beyond the boundary state. *)

(* General_Category Cc: C0, DEL and C1. *)
let[@inline] is_control cp = cp < 0x20 || (cp >= 0x7F && cp < 0xA0)
let[@inline] is_printable_ascii b = b >= 0x20 && b < 0x7F
let kind_control = 1
let kind_text = 2
let[@inline] unit_ws u = u lsr 3
let[@inline] unit_kind u = (u lsr 1) land 3
let[@inline] unit_starts u = u land 1 <> 0
let[@inline] unit_width u = ws_width (unit_ws u)
let[@inline] unit_make ws kind = (ws lsl 3) lor (kind lsl 1) lor 1
let[@inline] unit_extend u ws = (ws lsl 3) lor (u land 6)

(* [unit_step seg ~wide ~wcwidth ~tab cp u] adds the scalar [cp] to the unit
   state [u]. A control is a unit of its own, of width [tab] for TAB and [0]
   otherwise. [`Wcwidth] starts a unit at each scalar of positive width;
   [seg] is unused then. *)
let[@inline] unit_step seg ~wide ~wcwidth ~tab cp u =
  if is_control cp then begin
    if not wcwidth then
      ignore
        (Uuseg_grapheme_cluster.check_boundary seg (Uchar.unsafe_of_int cp)
          : bool);
    unit_make (if cp = 0x09 then ws_make tab 0 else 0) kind_control
  end
  else if wcwidth then
    let w = codepoint_width_wcwidth ~tab_width:0 cp in
    if w > 0 then unit_make (ws_make w ws_has_width) kind_text
    else if unit_kind u <> kind_text then unit_make 0 kind_text
    else u land lnot 1
  else
    let bw =
      Uuseg_grapheme_cluster.check_boundary_with_width seg
        (Uchar.unsafe_of_int cp)
    in
    let cp_w = (bw land 3) - 1 in
    if bw land 4 <> 0 || unit_kind u <> kind_text then
      unit_make (ws_step ~wide cp cp_w 0) kind_text
    else unit_extend u (ws_step ~wide cp cp_w (unit_ws u))

(* The unit of one printable ASCII character: [ws_step] from [0], without its
   table lookup. The emoji among them are # * and the digits, which VS16 makes
   keycap bases. *)
let[@inline] ascii_unit b =
  if b = 0x23 || b = 0x2A || (b >= 0x30 && b <= 0x39) then
    unit_make (ws_make 1 (ws_has_width lor ws_emoji)) kind_text
  else unit_make (ws_make 1 ws_has_width) kind_text

(* [ascii_run seg ~wcwidth ~ascii_left u str pos limit] is the number of
   bytes from [pos] that are complete units of one printable ASCII character
   given the state [u] and [seg]: each starts a unit, and is followed before
   [limit] by another printable ASCII byte, which therefore starts the next.
   [ascii_left] tells that the last scalar was printable ASCII. A printable
   ASCII character after another always starts a unit (GB999); the unit in
   progress can absorb the first one only if it ends with a Prepend
   character (GB9b), which [seg] knows. With [final], [limit] ends the text,
   which completes a run that reaches it: the result is then [limit - pos].
   Reads but does not update the state. *)
let[@inline] printable_at str i =
  is_printable_ascii (Char.code (String.unsafe_get str i))

let[@inline] ascii_run ?(final = false) seg ~wcwidth ~ascii_left u str pos limit
    =
  if
    pos + 1 >= limit
    || (not (printable_at str pos))
    || (not (printable_at str (pos + 1)))
    || not
         (ascii_left || wcwidth
         || unit_kind u <> kind_text
         || Uuseg_grapheme_cluster.is_boundary seg
              (Uchar.unsafe_of_int (Char.code (String.unsafe_get str pos))))
  then 0
  else begin
    let j = ref (pos + 1) in
    while !j + 1 < limit && printable_at str (!j + 1) do
      incr j
    done;
    if final && !j + 1 = limit then limit - pos else !j - pos
  end

(* The state after a run of [k] bytes from [pos] found by [ascii_run], for a
   fold, which keeps [seg] up to date. *)
let[@inline] after_ascii_run seg ~wcwidth str pos k =
  let last = Char.code (String.unsafe_get str (pos + k - 1)) in
  if not wcwidth then
    ignore
      (Uuseg_grapheme_cluster.check_boundary seg (Uchar.unsafe_of_int last)
        : bool);
  ascii_unit last

(* Never mutated: [`Wcwidth] does not segment by UAX #29. *)
let no_seg = Uuseg_grapheme_cluster.create ()

let[@inline] unit_seg width_method =
  match width_method with
  | `Wcwidth -> no_seg
  | `Unicode | `Unicode_wide -> Uuseg_grapheme_cluster.create ()

module Segmenter = struct
  type t = {
    seg : Uuseg_grapheme_cluster.t;
    wide : bool;
    wcwidth : bool;
    (* The unit in progress; [0] before the first scalar. *)
    mutable unit : int;
    (* The last scalar was printable ASCII, which [seg] may not have seen:
       every printable ASCII character leaves [seg] in the same state. *)
    mutable ascii_left : bool;
  }

  let create m =
    {
      seg = unit_seg m;
      wide = m = `Unicode_wide;
      wcwidth = m = `Wcwidth;
      unit = 0;
      ascii_left = false;
    }

  let reset s =
    if not s.wcwidth then Uuseg_grapheme_cluster.reset s.seg;
    s.unit <- 0;
    s.ascii_left <- false

  let ascii_x = Uchar.of_int 0x78

  let[@inline] sync s =
    if s.ascii_left then begin
      Uuseg_grapheme_cluster.reset s.seg;
      ignore (Uuseg_grapheme_cluster.check_boundary s.seg ascii_x : bool);
      s.ascii_left <- false
    end

  let width s = unit_width s.unit

  let add s u =
    let cp = Uchar.to_int u in
    if s.ascii_left && is_printable_ascii cp then begin
      s.unit <- ascii_unit cp;
      true
    end
    else begin
      sync s;
      let u =
        unit_step s.seg ~wide:s.wide ~wcwidth:s.wcwidth ~tab:0 cp s.unit
      in
      s.unit <- u;
      if is_printable_ascii cp && not s.wcwidth then s.ascii_left <- true;
      unit_starts u
    end

  (* A control is a unit of its own. [`Wcwidth] continues a unit with each
     scalar of no width. *)
  let may_continue m u v =
    let cu = Uchar.to_int u and cv = Uchar.to_int v in
    (not (is_control cu || is_control cv))
    &&
    match m with
    | `Wcwidth -> codepoint_width_wcwidth ~tab_width:0 cv <= 0
    | `Unicode | `Unicode_wide -> Uuseg_grapheme_cluster.may_continue u v

  let ascii_run s str ~pos ~len =
    check_sub "Matrix_text.Segmenter.ascii_run" str ~pos ~len;
    let k =
      ascii_run s.seg ~wcwidth:s.wcwidth ~ascii_left:s.ascii_left s.unit str pos
        (pos + len)
    in
    if k > 0 then begin
      s.unit <- ascii_unit (Char.code (String.unsafe_get str (pos + k - 1)));
      if not s.wcwidth then s.ascii_left <- true
    end;
    k
end

(* Grapheme Segmentation *)

let rec find_boundary_loop seg str limit pos =
  if pos >= limit then limit
  else
    let d = String.get_utf_8_uchar str pos in
    let u = Uchar.utf_decode_uchar d in
    if Uuseg_grapheme_cluster.check_boundary seg u then pos
    else find_boundary_loop seg str limit (pos + Uchar.utf_decode_length d)

(* Find the next grapheme cluster boundary starting at [start]. Returns the byte
   offset after the grapheme cluster. *)
let next_boundary seg str start limit =
  if start >= limit then limit
  else (
    Uuseg_grapheme_cluster.reset seg;
    let d = String.get_utf_8_uchar str start in
    let u = Uchar.utf_decode_uchar d in
    let _ = Uuseg_grapheme_cluster.check_boundary seg u in
    find_boundary_loop seg str limit (start + Uchar.utf_decode_length d))

(* Grapheme Iteration *)

let rec iter_graphemes_ascii str len f i =
  if i >= len then ()
  else if
    Char.code (Stdlib.String.unsafe_get str i) = 0x0D
    && i + 1 < len
    && Stdlib.String.unsafe_get str (i + 1) = '\n'
  then (
    f ~offset:i ~len:2;
    iter_graphemes_ascii str len f (i + 2))
  else (
    f ~offset:i ~len:1;
    iter_graphemes_ascii str len f (i + 1))

let rec iter_graphemes_unicode seg str len f i start =
  if i >= len then (if start < len then f ~offset:start ~len:(len - start))
  else
    let d = Stdlib.String.get_utf_8_uchar str i in
    let u = Uchar.utf_decode_uchar d in
    let next = i + Uchar.utf_decode_length d in
    if Uuseg_grapheme_cluster.check_boundary seg u then (
      f ~offset:start ~len:(i - start);
      iter_graphemes_unicode seg str len f next i)
    else iter_graphemes_unicode seg str len f next start

let iter_graphemes f str =
  let len = Stdlib.String.length str in
  if len = 0 then ()
  else if is_ascii_only str len 0 then iter_graphemes_ascii str len f 0
  else
    let seg = Uuseg_grapheme_cluster.create () in
    let d = Stdlib.String.get_utf_8_uchar str 0 in
    let _ =
      Uuseg_grapheme_cluster.check_boundary seg (Uchar.utf_decode_uchar d)
    in
    iter_graphemes_unicode seg str len f (Uchar.utf_decode_length d) 0

(* The fold of {!Segmenter} that visits units of positive width. *)
let rec visit_units f seg ~wide ~wcwidth str len tab i start u =
  if i >= len then begin
    let w = unit_width u in
    if w > 0 then f ~offset:start ~len:(len - start) ~width:w
  end
  else
    let k = ascii_run ~final:true seg ~wcwidth ~ascii_left:false u str i len in
    if k > 0 then begin
      let w = unit_width u in
      if i > start && w > 0 then f ~offset:start ~len:(i - start) ~width:w;
      let ends = i + k = len in
      for j = i to if ends then len - 1 else i + k - 2 do
        f ~offset:j ~len:1 ~width:1
      done;
      if not ends then
        visit_units f seg ~wide ~wcwidth str len tab (i + k)
          (i + k - 1)
          (after_ascii_run seg ~wcwidth str i k)
    end
    else
      let d = decode str i len in
      let cp = decoded_cp d in
      let next = i + decoded_len d in
      let u' = unit_step seg ~wide ~wcwidth ~tab cp u in
      if unit_starts u' && i > start then begin
        let w = unit_width u in
        if w > 0 then f ~offset:start ~len:(i - start) ~width:w;
        visit_units f seg ~wide ~wcwidth str len tab next i u'
      end
      else visit_units f seg ~wide ~wcwidth str len tab next start u'

let iter_units ~width_method ~tab_width f str =
  let tab_width = normalize_tab_width tab_width in
  let len = Stdlib.String.length str in
  if len > 0 then
    visit_units f (unit_seg width_method)
      ~wide:(width_method = `Unicode_wide)
      ~wcwidth:(width_method = `Wcwidth) str len tab_width 0 0 0

(* Width-position helpers *)

let zero_position = { byte_offset = 0; units = 0; columns_used = 0 }

(* Top-level loops share fitting and positioning without allocating a closure
   for each query. *)
let rec find_pos_ascii str len tab_width include_start_before columns i count
    used =
  if i >= len then { byte_offset = len; units = count; columns_used = used }
  else
    let width = ascii_width ~tab_width (Char.code (String.unsafe_get str i)) in
    if width <= 0 then
      find_pos_ascii str len tab_width include_start_before columns (i + 1)
        count used
    else
      let next_used = used + width in
      if
        (include_start_before && used >= columns)
        || ((not include_start_before) && next_used > columns)
      then { byte_offset = i; units = count; columns_used = used }
      else
        find_pos_ascii str len tab_width include_start_before columns (i + 1)
          (count + 1) next_used

(* The fold of {!Segmenter} that fits units of positive width into
   [columns]. [start] begins the unit in progress, [u]. *)
(* A unit of width [w] after [used] columns fits [columns]: it ends within
   them, or with [isb], it starts within them. *)
let[@inline] fits ~isb ~columns used w =
  not ((isb && used >= columns) || ((not isb) && used + w > columns))

let rec find_units seg ~wide ~wcwidth str len tab isb columns i start count used
    u =
  if i >= len then
    let w = unit_width u in
    if w > 0 && start < len then
      if not (fits ~isb ~columns used w) then
        { byte_offset = start; units = count; columns_used = used }
      else { byte_offset = len; units = count + 1; columns_used = used + w }
    else { byte_offset = len; units = count; columns_used = used }
  else
    let k = ascii_run ~final:true seg ~wcwidth ~ascii_left:false u str i len in
    if k > 0 then
      let w = unit_width u in
      let closes = i > start && w > 0 in
      if closes && not (fits ~isb ~columns used w) then
        { byte_offset = start; units = count; columns_used = used }
      else
        let count, used =
          if closes then (count + 1, used + w) else (count, used)
        in
        (* Complete units of width 1: all [k] at the end of the text. *)
        let ends = i + k = len in
        let complete = if ends then k else k - 1 in
        let n = min complete (max 0 (columns - used)) in
        if n < complete || ends then
          { byte_offset = i + n; units = count + n; columns_used = used + n }
        else
          find_units seg ~wide ~wcwidth str len tab isb columns (i + k)
            (i + k - 1)
            (count + n) (used + n)
            (after_ascii_run seg ~wcwidth str i k)
    else
      let d = decode str i len in
      let cp = decoded_cp d in
      let next = i + decoded_len d in
      let u' = unit_step seg ~wide ~wcwidth ~tab cp u in
      if unit_starts u' && i > start then
        let w = unit_width u in
        if w <= 0 then
          find_units seg ~wide ~wcwidth str len tab isb columns next i count
            used u'
        else if not (fits ~isb ~columns used w) then
          { byte_offset = start; units = count; columns_used = used }
        else
          find_units seg ~wide ~wcwidth str len tab isb columns next i
            (count + 1) (used + w) u'
      else
        find_units seg ~wide ~wcwidth str len tab isb columns next start count
          used u'

let find_pos_units ~width_method ~tab_width ~include_start_before str ~columns =
  let len = Stdlib.String.length str in
  if len = 0 then zero_position
  else if is_ascii_only str len 0 then
    find_pos_ascii str len tab_width include_start_before columns 0 0 0
  else
    find_units (unit_seg width_method)
      ~wide:(width_method = `Unicode_wide)
      ~wcwidth:(width_method = `Wcwidth) str len tab_width include_start_before
      columns 0 0 0 0 0

let find_wrap_pos ~width_method ~tab_width str ~max_columns =
  find_pos_units ~width_method
    ~tab_width:(normalize_tab_width tab_width)
    ~include_start_before:false str ~columns:max_columns

let find_pos ~width_method ~tab_width ?(include_start_before = false) str
    ~columns =
  find_pos_units ~width_method
    ~tab_width:(normalize_tab_width tab_width)
    ~include_start_before str ~columns

(* The width of the unit that starts at [i]. *)
let rec first_unit_width seg ~wide ~wcwidth str len tab i u =
  if i >= len then unit_width u
  else
    let d = decode str i len in
    let cp = decoded_cp d in
    let u' = unit_step seg ~wide ~wcwidth ~tab cp u in
    if unit_starts u' && u <> 0 then unit_width u
    else first_unit_width seg ~wide ~wcwidth str len tab (i + decoded_len d) u'

let width_at ~width_method ~tab_width str ~byte_offset =
  let tab_width = normalize_tab_width tab_width in
  let len = Stdlib.String.length str in
  if byte_offset < 0 || byte_offset >= len then 0
  else
    let b = Char.code (String.unsafe_get str byte_offset) in
    if
      b < 0x80
      && (byte_offset + 1 = len || Char.code str.[byte_offset + 1] < 0x80)
    then ascii_width ~tab_width b
    else
      first_unit_width (unit_seg width_method)
        ~wide:(width_method = `Unicode_wide)
        ~wcwidth:(width_method = `Wcwidth) str len tab_width byte_offset 0

(* String Measurement *)

let rec measure_ascii_tail str len tab_width i total =
  if i >= len then total
  else
    let w =
      ascii_width ~tab_width (Char.code (Stdlib.String.unsafe_get str i))
    in
    measure_ascii_tail str len tab_width (i + 1) (total + w)

let rec measure_ascii str len tab_width i total =
  if i + 4 <= len && is_ascii_4 str i then
    let w0 =
      ascii_width ~tab_width (Char.code (Stdlib.String.unsafe_get str i))
    in
    let w1 =
      ascii_width ~tab_width (Char.code (Stdlib.String.unsafe_get str (i + 1)))
    in
    let w2 =
      ascii_width ~tab_width (Char.code (Stdlib.String.unsafe_get str (i + 2)))
    in
    let w3 =
      ascii_width ~tab_width (Char.code (Stdlib.String.unsafe_get str (i + 3)))
    in
    measure_ascii str len tab_width (i + 4) (total + w0 + w1 + w2 + w3)
  else measure_ascii_tail str len tab_width i total

(* The fold of {!Segmenter} that sums unit widths. *)
let rec measure_units seg ~wide ~wcwidth str limit tab i total u =
  if i >= limit then total + unit_width u
  else
    let k =
      ascii_run ~final:true seg ~wcwidth ~ascii_left:false u str i limit
    in
    if k > 0 && i + k = limit then total + unit_width u + k
    else if k > 0 then
      measure_units seg ~wide ~wcwidth str limit tab (i + k)
        (total + unit_width u + k - 1)
        (after_ascii_run seg ~wcwidth str i k)
    else
      let d = decode str i limit in
      let u' = unit_step seg ~wide ~wcwidth ~tab (decoded_cp d) u in
      measure_units seg ~wide ~wcwidth str limit tab
        (i + decoded_len d)
        (if unit_starts u' then total + unit_width u else total)
        u'

(* Under [`Wcwidth] a unit's width is that of its one scalar of positive
   width, so the sum over units is the sum over scalars. This loop is about
   10% faster than [measure_units] on non-ASCII text; a test checks that
   both agree. *)
let rec measure_scalars str limit tab i total =
  if i >= limit then total
  else
    let d = decode str i limit in
    let cp = decoded_cp d in
    let w =
      if cp = 0x09 then tab
      else if is_control cp then 0
      else codepoint_width_wcwidth ~tab_width:0 cp
    in
    measure_scalars str limit tab (i + decoded_len d) (total + w)

let measure ~width_method ~tab_width str =
  let tab_width = normalize_tab_width tab_width in
  let len = Stdlib.String.length str in
  if len = 0 then 0
  else if is_ascii_only str len 0 then measure_ascii str len tab_width 0 0
  else if width_method = `Wcwidth then measure_scalars str len tab_width 0 0
  else
    measure_units (unit_seg width_method)
      ~wide:(width_method = `Unicode_wide)
      ~wcwidth:(width_method = `Wcwidth) str len tab_width 0 0 0

let measure_sub ~width_method ~tab_width str ~pos ~len:sub_len =
  let tab_width = normalize_tab_width tab_width in
  if sub_len <= 0 then 0
  else (
    check_sub "Matrix_text.measure_sub" str ~pos ~len:sub_len;
    let end_pos = pos + sub_len in
    if is_ascii_only str end_pos pos then
      measure_ascii str end_pos tab_width pos 0
    else if width_method = `Wcwidth then
      measure_scalars str end_pos tab_width pos 0
    else
      measure_units (unit_seg width_method)
        ~wide:(width_method = `Unicode_wide)
        ~wcwidth:(width_method = `Wcwidth) str end_pos tab_width pos 0 0)

let grapheme_count str =
  let n = ref 0 in
  iter_graphemes (fun ~offset:_ ~len:_ -> incr n) str;
  !n

(* Text Segmentation (wrap breaks, line breaks) *)

let[@inline] is_ascii_wrap_break b =
  match b with
  | 0x20 | 0x09 | 0x2D | 0x2F | 0x5C | 0x2C | 0x3B | 0x3A | 0x21 | 0x3F | 0x28
  | 0x29 | 0x5B | 0x5D | 0x7B | 0x7D ->
      true
  | _ -> false

let[@inline] is_unicode_wrap_break cp =
  match cp with
  | 0x00A0 | 0x1680 | 0x202F | 0x205F | 0x3000 | 0x200B | 0x00AD | 0x2010
  | 0x3001 | 0x3002 | 0xFF01 | 0xFF1F ->
      true
  | cp when cp >= 0x2000 && cp <= 0x200A -> true
  | _ -> false

type word_class = Ascii_word | Cjk_word | Other

let[@inline] is_ascii_word_byte b =
  (b >= 0x61 && b <= 0x7A)
  || (b >= 0x41 && b <= 0x5A)
  || (b >= 0x30 && b <= 0x39)
  || b = 0x5F

let[@inline] is_cjk_word_codepoint cp =
  (cp >= 0x3400 && cp <= 0x4DBF)
  || (cp >= 0x4E00 && cp <= 0x9FFF)
  || (cp >= 0xF900 && cp <= 0xFAFF)
  || (cp >= 0x20000 && cp <= 0x2A6DF)
  || (cp >= 0x2A700 && cp <= 0x2B73F)
  || (cp >= 0x2B740 && cp <= 0x2B81F)
  || (cp >= 0x2B820 && cp <= 0x2CEAF)
  || (cp >= 0x2CEB0 && cp <= 0x2EBEF)
  || (cp >= 0x2EBF0 && cp <= 0x2EE5D)
  || (cp >= 0x2F800 && cp <= 0x2FA1F)
  || (cp >= 0x3040 && cp <= 0x309F)
  || (cp >= 0x30A0 && cp <= 0x30FF)
  || (cp >= 0x31F0 && cp <= 0x31FF)
  || (cp >= 0xFF66 && cp <= 0xFF9D)
  || (cp >= 0x1100 && cp <= 0x11FF)
  || (cp >= 0x3130 && cp <= 0x318F)
  || (cp >= 0xA960 && cp <= 0xA97F)
  || (cp >= 0xAC00 && cp <= 0xD7AF)
  || (cp >= 0xD7B0 && cp <= 0xD7FF)

let[@inline] classify_word cp =
  if cp <= 0x7F then if is_ascii_word_byte cp then Ascii_word else Other
  else if is_cjk_word_codepoint cp then Cjk_word
  else Other

let[@inline] is_cjk_ascii_transition prev curr =
  match (prev, curr) with
  | Cjk_word, Ascii_word | Ascii_word, Cjk_word -> true
  | _ -> false

let[@inline] first_codepoint s off =
  let d = Stdlib.String.get_utf_8_uchar s off in
  Uchar.to_int (Uchar.utf_decode_uchar d)

let iter_breaks ~intercharacter f s =
  let len = Stdlib.String.length s in
  let seg = Uuseg_grapheme_cluster.create () in
  let rec has_break i limit =
    if i >= limit then false
    else
      let b0 = Char.code (Stdlib.String.unsafe_get s i) in
      if b0 < 0x80 then is_ascii_wrap_break b0 || has_break (i + 1) limit
      else
        let d = Stdlib.String.get_utf_8_uchar s i in
        let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
        is_unicode_wrap_break cp
        || has_break (i + Uchar.utf_decode_length d) limit
  in
  (* [prev_byte_off < 0] means no previous grapheme; [prev_class] is then a
     dummy. Plain ints avoid boxing three options per grapheme. *)
  let rec loop byte_off g_off prev_byte_off prev_g_off prev_class =
    if byte_off >= len then ()
    else
      let next = next_boundary seg s byte_off len in
      let cp = first_codepoint s byte_off in
      let curr_class = classify_word cp in
      if
        prev_byte_off >= 0
        && (is_cjk_ascii_transition prev_class curr_class
           || intercharacter && prev_class = Cjk_word && curr_class = Cjk_word
              && cp <> 0x3099 && cp <> 0x309A && cp <> 0x30A0 && cp <> 0x30FB
              && measure_sub ~width_method:`Unicode ~tab_width:2 s
                   ~pos:prev_byte_off ~len:(byte_off - prev_byte_off)
                 > 0)
      then
        f ~break_byte_offset:prev_byte_off ~next_byte_offset:byte_off
          ~grapheme_offset:prev_g_off;
      if has_break byte_off next then
        f ~break_byte_offset:byte_off ~next_byte_offset:next
          ~grapheme_offset:g_off;
      loop next (g_off + 1) byte_off g_off curr_class
  in
  loop 0 0 (-1) (-1) Other

let iter_wrap_breaks f s = iter_breaks ~intercharacter:true f s
let iter_word_breaks f s = iter_breaks ~intercharacter:false f s

let iter_line_breaks f s =
  let len = Stdlib.String.length s in
  let rec loop i =
    if i < len then
      let b = Char.code (Stdlib.String.unsafe_get s i) in
      if b = 0x0D then
        if i + 1 < len && Char.code (Stdlib.String.unsafe_get s (i + 1)) = 0x0A
        then (
          f ~pos:(i + 1) ~kind:`CRLF;
          loop (i + 2))
        else (
          f ~pos:i ~kind:`CR;
          loop (i + 1))
      else if b = 0x0A then (
        f ~pos:i ~kind:`LF;
        loop (i + 1))
      else loop (i + 1)
  in
  loop 0

module Layout = struct
  type affinity = Upstream | Downstream
  type caret = { grapheme : int; affinity : affinity }
  type wrap = [ `None | `Char | `Word ]

  let clamp n x = max 0 (min n x)

  let lower_bound a first last value =
    let lo = ref first and hi = ref last in
    while !lo < !hi do
      let mid = !lo + ((!hi - !lo) / 2) in
      if a.(mid) < value then lo := mid + 1 else hi := mid
    done;
    !lo

  let upper_bound a first last value =
    let lo = ref first and hi = ref last in
    while !lo < !hi do
      let mid = !lo + ((!hi - !lo) / 2) in
      if a.(mid) <= value then lo := mid + 1 else hi := mid
    done;
    !lo

  (* [columns.(g)] is the cells before grapheme [g]. [next_visible.(g)] is the
     first grapheme at or after [g] that takes a cell; it exists only when a
     zero-width cluster other than a line terminator does. A terminator ends
     its line and so every row that could skip it, so without such a cluster
     every grapheme a row covers takes a cell and the skip is the identity. *)
  type metrics = { columns : int array; next_visible : int array option }

  module Source = struct
    type t = {
      text : string;
      offsets : int array;
      lines : int array;
      mutable breaks : bytes option;
      mutable metrics : (width_method * int * metrics) option;
    }

    let text t = t.text
    let length t = Array.length t.offsets - 1
    let line_count t = Array.length t.lines
    let byte_offset t g = t.offsets.(clamp (length t) g)

    let grapheme_of_byte t ~affinity byte =
      let byte = clamp (String.length t.text) byte in
      let i = lower_bound t.offsets 0 (length t + 1) byte in
      if affinity = Upstream && t.offsets.(i) <> byte then i - 1 else i

    let line_start t line = t.lines.(clamp (line_count t - 1) line)

    let line_end t line =
      let line = clamp (line_count t - 1) line in
      if line + 1 = line_count t then length t else t.lines.(line + 1) - 1

    let line_of_offset t g =
      max 0 (upper_bound t.lines 0 (line_count t) (clamp (length t) g) - 1)

    let of_string text =
      let n = grapheme_count text in
      let offsets = Array.make (n + 1) (String.length text) in
      let lines = ref [ 0 ] and i = ref 0 in
      iter_graphemes
        (fun ~offset ~len:_ ->
          offsets.(!i) <- offset;
          if text.[offset] = '\n' || text.[offset] = '\r' then
            lines := (!i + 1) :: !lines;
          incr i)
        text;
      {
        text;
        offsets;
        lines = Array.of_list (List.rev !lines);
        breaks = None;
        metrics = None;
      }

    let wrap_breaks t =
      match t.breaks with
      | Some breaks -> breaks
      | None ->
          let n = length t in
          let breaks = Bytes.make (n + 1) '\000' and g = ref 0 in
          iter_wrap_breaks
            (fun ~break_byte_offset:_ ~next_byte_offset ~grapheme_offset:_ ->
              while !g < n && t.offsets.(!g) < next_byte_offset do
                incr g
              done;
              if t.offsets.(!g) = next_byte_offset then
                Bytes.set breaks !g '\001')
            t.text;
          t.breaks <- Some breaks;
          breaks

    let metrics t ~width_method ~tab_width =
      let tab_width = max 1 tab_width in
      match t.metrics with
      | Some (m, w, metrics) when m = width_method && w = tab_width -> metrics
      | _ ->
          let n = length t in
          let columns = Array.make (n + 1) 0 in
          let has_zero = ref false in
          for g = 0 to n - 1 do
            let off = t.offsets.(g) in
            let width =
              measure_sub ~width_method ~tab_width t.text ~pos:off
                ~len:(t.offsets.(g + 1) - off)
            in
            let terminator = t.text.[off] = '\n' || t.text.[off] = '\r' in
            if width = 0 && not terminator then has_zero := true;
            columns.(g + 1) <- columns.(g) + width
          done;
          let next_visible =
            if not !has_zero then None
            else begin
              let next = Array.make (n + 1) n in
              for g = n - 1 downto 0 do
                next.(g) <-
                  (if columns.(g + 1) > columns.(g) then g else next.(g + 1))
              done;
              Some next
            end
          in
          let metrics = { columns; next_visible } in
          t.metrics <- Some (width_method, tab_width, metrics);
          metrics

    let columns ?(width_method = `Unicode) ?(tab_width = 2) t ~first ~last =
      let m = metrics t ~width_method ~tab_width in
      m.columns.(clamp (length t) last) - m.columns.(clamp (length t) first)

    let slice t ~first ~last =
      let first = byte_offset t first and last = byte_offset t last in
      if last <= first then "" else Stdlib.String.sub t.text first (last - first)
  end

  type row = {
    first : int;
    last : int;
    visible_last : int;
    columns : int;
    line : int;
    continuation : int;
  }

  type t = {
    source : Source.t;
    metrics : metrics;
    rows : row array;
    max_width : int;
  }

  (* Both measurement and retained geometry use this source-window walk. *)
  let build source (metrics : metrics) ~width ~wrap ~truncate emit =
    let p = metrics.columns in
    let breaks =
      if wrap = `Word then Source.wrap_breaks source else Bytes.empty
    in
    for line = 0 to Source.line_count source - 1 do
      let first = Source.line_start source line in
      let last = Source.line_end source line in
      if wrap = `None || width <= 0 || first = last then begin
        let natural = p.(last) - p.(first) in
        let visible_last =
          if wrap = `None && truncate && width > 0 && natural > width then
            max first
              (upper_bound p first (last + 1) (p.(first) + width - 1) - 1)
          else last
        in
        let columns =
          p.(visible_last) - p.(first) + if visible_last < last then 1 else 0
        in
        emit first last visible_last columns line 0
      end
      else begin
        let start = ref first and g = ref first and best = ref first in
        let continuation = ref 0 in
        while !g < last do
          if wrap = `Word && Bytes.get breaks !g <> '\000' && !g > !start then
            best := !g;
          let used = p.(!g) - p.(!start) in
          let next = p.(!g + 1) - p.(!start) in
          if next > width && used > 0 && p.(!g + 1) > p.(!g) then begin
            let stop = if wrap = `Word && !best > !start then !best else !g in
            emit !start stop stop (p.(stop) - p.(!start)) line !continuation;
            incr continuation;
            start := stop;
            g := stop;
            best := stop
          end
          else incr g
        done;
        emit !start last last (p.(last) - p.(!start)) line !continuation
      end
    done

  let create ?(width_method = `Unicode) ?(tab_width = 2) ?(truncate = false)
      source ~width ~wrap =
    let metrics = Source.metrics source ~width_method ~tab_width in
    let rows = Dynarray.create () and max_width = ref 0 in
    build source metrics ~width ~wrap ~truncate
      (fun first last visible_last columns line continuation ->
        max_width := max !max_width columns;
        Dynarray.add_last rows
          { first; last; visible_last; columns; line; continuation });
    { source; metrics; rows = Dynarray.to_array rows; max_width = !max_width }

  let measure ?(width_method = `Unicode) ?(tab_width = 2) ?(truncate = false)
      source ~width ~wrap =
    let metrics = Source.metrics source ~width_method ~tab_width in
    let count = ref 0 and max_width = ref 0 in
    build source metrics ~width ~wrap ~truncate (fun _ _ _ columns _ _ ->
        incr count;
        max_width := max !max_width columns);
    (!count, !max_width)

  let source t = t.source
  let row_count t = Array.length t.rows
  let max_width t = t.max_width
  let row t i = t.rows.(clamp (row_count t - 1) i)
  let row_start t i = (row t i).first
  let row_end t i = (row t i).last
  let row_width t i = (row t i).columns
  let logical_line t i = (row t i).line
  let continuation t i = (row t i).continuation

  let iter_rows t ~first ~count f =
    let first = clamp (row_count t) first in
    let count = max 0 (min count (row_count t - first)) in
    for i = first to first + count - 1 do
      let r = t.rows.(i) in
      f ~row:i ~logical_line:r.line ~continuation:r.continuation
    done

  let locate t { grapheme; affinity } =
    let g = clamp (Source.length t.source) grapheme in
    let lo = ref 0 and hi = ref (row_count t - 1) in
    while !lo < !hi do
      let mid = !lo + ((!hi - !lo + 1) / 2) in
      if t.rows.(mid).first <= g then lo := mid else hi := mid - 1
    done;
    let i =
      if
        affinity = Upstream && !lo > 0 && t.rows.(!lo).first = g
        && t.rows.(!lo - 1).last = g
      then !lo - 1
      else !lo
    in
    let r = t.rows.(i) in
    let col =
      if g = r.last && g > r.visible_last then r.columns
      else if g > r.visible_last then
        r.columns - if affinity = Upstream then 1 else 0
      else t.metrics.columns.(g) - t.metrics.columns.(r.first)
    in
    (i, col)

  let hit ?(affinity = Downstream) t ~row:i ~column =
    if i < 0 then { grapheme = 0; affinity = Downstream }
    else if i >= row_count t then
      { grapheme = Source.length t.source; affinity }
    else
      let r = t.rows.(i) and p = t.metrics.columns in
      (* The boundaries at [g]'s column: the earliest upstream, the latest
         downstream. *)
      let snap g =
        if affinity = Upstream then lower_bound p r.first (g + 1) p.(g)
        else upper_bound p g (r.visible_last + 1) p.(g) - 1
      in
      let column = max 0 column in
      let g =
        if r.visible_last < r.last && column >= r.columns - 1 then
          if column < r.columns then snap r.visible_last else r.last
        else
          let target = p.(r.first) + column in
          let next = lower_bound p r.first (r.visible_last + 1) target in
          snap
            (if next > r.visible_last then r.visible_last
             else if p.(next) = target || next = r.first then next
             else if target - p.(next - 1) < p.(next) - target then next - 1
             else next)
      in
      let affinity =
        if g = r.last && i + 1 < row_count t && t.rows.(i + 1).first = g then
          Upstream
        else if g = r.first then Downstream
        else affinity
      in
      { grapheme = g; affinity }

  let coverage t ~row:i ~column =
    let r = row t i in
    if i < 0 then (0, 0)
    else if i >= row_count t then
      let n = Source.length t.source in
      (n, n)
    else if column < 0 then (r.first, r.first)
    else if column >= r.columns then (r.last, r.last)
    else if r.visible_last < r.last && column = r.columns - 1 then
      (r.visible_last, r.last)
    else
      let p = t.metrics.columns in
      let g =
        max r.first
          (upper_bound p r.first (r.visible_last + 1) (p.(r.first) + column) - 1)
      in
      (g, min r.last (g + 1))

  let iter_cells t ~row:i ~first_column ~last_column f =
    if i >= 0 && i < row_count t && last_column > first_column then begin
      let r = t.rows.(i) and p = t.metrics.columns in
      let origin = p.(r.first) in
      let g =
        ref
          (max r.first
             (upper_bound p r.first (r.visible_last + 1)
                (origin + max 0 first_column)
             - 1))
      in
      let next g =
        match t.metrics.next_visible with None -> g | Some a -> a.(g)
      in
      g := next !g;
      while !g < r.visible_last && p.(!g) - origin < last_column do
        let column = p.(!g) - origin in
        let width = p.(!g + 1) - p.(!g) in
        if column + width > first_column then
          f ~first:!g ~last:(!g + 1) ~column ~width ~ellipsis:false;
        g := next (!g + 1)
      done;
      if
        r.visible_last < r.last
        && r.columns - 1 >= first_column
        && r.columns - 1 < last_column
      then
        f ~first:r.visible_last ~last:r.last ~column:(r.columns - 1) ~width:1
          ~ellipsis:true
    end
end
