open StdLabels

type width_method = [ `Unicode | `Unicode_wide | `Wcwidth ]
type line_break_kind = [ `LF | `CR | `CRLF ]
type position = { byte_offset : int; grapheme_count : int; columns_used : int }
type grapheme = { byte_offset : int; byte_length : int; width : int }

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

(* Detects Indic virama characters (U+094D, U+09CD, U+0A4D, U+0ACD, U+0B4D,
   U+0BCD, U+0C4D, U+0CCD, U+0D4D). The virama joins two consonants into a
   conjunct (e.g. क + ् + ष = क्ष) which may be wider than a single cell. We check
   specific virama codepoints rather than the broader GeneralCategory=Mn class
   because the conjunct-width logic should only fire for actual virama
   sequences, not for arbitrary combining marks like diacriticals. *)
let[@inline] is_virama cp = cp land 0x7F = 0x4D && cp >= 0x094D && cp <= 0x0D4D

let[@inline] is_devanagari_base cp =
  (cp >= 0x0915 && cp <= 0x0939) || (cp >= 0x0958 && cp <= 0x095F)

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

let[@inline] codepoint_width_unicode ~tab_width cp =
  if cp < 0x80 then
    if cp = 0x09 then tab_width else if cp < 32 || cp = 127 then -1 else 1
  else Unicode.tty_width_hint (Uchar.unsafe_of_int cp)

let uchar_width ~tab_width uchar =
  codepoint_width_wcwidth
    ~tab_width:(normalize_tab_width tab_width)
    (Uchar.to_int uchar)

(* Unicode-width state machine.

   The [`Unicode] width method folds a cluster's codepoints through a small
   state machine handling VS16 emoji promotion, regional-indicator pairs,
   spacing marks, and Indic virama conjuncts. The whole state is one immediate
   int: the accumulated cluster width in the bits above bit 2 and three flags below.
   Invariant: the width bits are [0] while [ws_has_width] is unset.

   The initial state is [0]; [ws_step] handles the first codepoint of a
   cluster like any other (from state [0] every branch reduces to the
   cluster-start behaviour). *)

let ws_has_width = 1 (* cluster has a base width *)
let ws_ri_pair = 2 (* last codepoint was the first RI of a pair *)
let ws_virama = 4 (* last codepoint was a virama *)
let[@inline] ws_make w flags = (w lsl 3) lor flags
let[@inline] ws_width state = state lsr 3

(* [ws_step cp cp_w state] advances the state machine by the codepoint [cp]
   whose individual display width is [cp_w]. *)
let[@inline] ws_step ~wide cp cp_w state =
  let flags = state land 7 in
  if cp = 0xFE0F then
    if flags land ws_has_width <> 0 && ws_width state = 1 then ws_make 2 flags
    else state
  else if is_virama cp then state lor ws_virama
  else if is_regional_indicator cp then
    if flags land ws_ri_pair <> 0 then
      ws_make (ws_width state + cp_w) ws_has_width
    else
      let w = if flags land ws_has_width = 0 then cp_w else ws_width state in
      ws_make w (flags lor ws_has_width lor ws_ri_pair land lnot ws_virama)
  else if
    wide
    && flags land ws_has_width <> 0
    && not (Unicode.is_zero_in_grapheme (Uchar.unsafe_of_int cp))
  then ws_make (max 2 (ws_width state)) (flags land lnot ws_virama)
  else if flags land ws_has_width <> 0 && cp_w > 0 && is_wide_spacing_mark cp
  then ws_make (max 2 (ws_width state)) (flags land lnot ws_virama)
  else if
    flags land ws_has_width <> 0
    && flags land ws_virama <> 0
    && is_devanagari_base cp
  then
    let add = if cp <> 0x0930 && cp_w > 0 then cp_w else 0 in
    ws_make (ws_width state + add) (flags lor ws_has_width land lnot ws_virama)
  else if flags land ws_has_width = 0 && cp_w > 0 then
    ws_make cp_w (flags lor ws_has_width land lnot ws_virama)
  else state land lnot ws_virama

let rec grapheme_width_unicode_loop ~wide str limit tab_width i state =
  if i >= limit then ws_width state
  else
    let d = String.get_utf_8_uchar str i in
    let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
    let cp_w = codepoint_width_unicode ~tab_width cp in
    grapheme_width_unicode_loop ~wide str limit tab_width
      (i + Uchar.utf_decode_length d)
      (ws_step ~wide cp cp_w state)

let rec grapheme_width_wcwidth_loop str limit tab_width i acc =
  if i >= limit then acc
  else
    let d = String.get_utf_8_uchar str i in
    let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
    let next = i + Uchar.utf_decode_length d in
    grapheme_width_wcwidth_loop str limit tab_width next
      (acc + codepoint_width_wcwidth ~tab_width cp)

let cluster_width ~method_ ~tab_width str off len =
  let limit = off + len in
  match method_ with
  | `Wcwidth -> grapheme_width_wcwidth_loop str limit tab_width off 0
  | `Unicode | `Unicode_wide ->
      grapheme_width_unicode_loop ~wide:(method_ = `Unicode_wide) str limit
        tab_width off 0

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

let iter_grapheme_info ~width_method ~tab_width f str =
  let tab_width = normalize_tab_width tab_width in
  let len = Stdlib.String.length str in
  if len = 0 then ()
  else
    let seg = Uuseg_grapheme_cluster.create () in

    let emit_ascii i =
      let b = Char.code (Stdlib.String.unsafe_get str i) in
      let w = ascii_width ~tab_width b in
      if w > 0 then f ~offset:i ~len:1 ~width:w
    in

    (* An ASCII byte is a complete cluster only when the next codepoint is
       also ASCII (or the string ends): a following non-ASCII codepoint can
       extend the cluster (combining marks, ZWJ, variation selectors). *)
    let ascii_cluster_at i =
      i + 1 >= len || Char.code (Stdlib.String.unsafe_get str (i + 1)) < 128
    in

    let rec loop i =
      if i >= len then ()
      else if i + 4 <= len && is_ascii_4 str i && ascii_cluster_at (i + 3) then (
        emit_ascii i;
        emit_ascii (i + 1);
        emit_ascii (i + 2);
        emit_ascii (i + 3);
        loop (i + 4))
      else
        let c = Stdlib.String.unsafe_get str i in
        if Char.code c < 128 && ascii_cluster_at i then (
          emit_ascii i;
          loop (i + 1))
        else
          let end_pos = next_boundary seg str i len in
          let clus_len = end_pos - i in
          let w =
            cluster_width ~method_:width_method ~tab_width str i clus_len
          in
          if w > 0 then (
            f ~offset:i ~len:clus_len ~width:w;
            loop end_pos)
          else loop end_pos
    in
    loop 0

(* Width-position helpers *)

let zero_position = { byte_offset = 0; grapheme_count = 0; columns_used = 0 }

(* The position/*-wcwidth benchmarks in matrix/bench/bench_text.ml are about
   60% slower with direct standard decoding. Even a cold decoder call spills
   loop state on valid input. Keep this inline decoder and test its malformed
   prefix consumption against the standard decoder. *)
let[@inline] is_cont_byte b = b land 0xC0 = 0x80
let replacement_1 = (0xFFFD, 1)
let replacement_2 = (0xFFFD, 2)
let replacement_3 = (0xFFFD, 3)

let[@inline] decode_malformed str pos b0 =
  let len = Stdlib.String.length str in
  if b0 < 0xE0 || b0 > 0xF4 || pos + 1 >= len then replacement_1
  else
    let b1 = Char.code (String.unsafe_get str (pos + 1)) in
    if
      (not (is_cont_byte b1))
      || (b0 = 0xE0 && b1 < 0xA0)
      || (b0 = 0xED && b1 >= 0xA0)
      || (b0 = 0xF0 && b1 < 0x90)
      || (b0 = 0xF4 && b1 >= 0x90)
    then replacement_1
    else if b0 < 0xF0 || pos + 2 >= len then replacement_2
    else if is_cont_byte (Char.code (String.unsafe_get str (pos + 2))) then
      replacement_3
    else replacement_2

let[@inline] decode_codepoint str pos =
  let len = Stdlib.String.length str in
  let b0 = Char.code (String.unsafe_get str pos) in
  if b0 < 0x80 then (b0, 1)
  else if b0 land 0xE0 = 0xC0 then
    if pos + 1 < len then
      let b1 = Char.code (String.unsafe_get str (pos + 1)) in
      if is_cont_byte b1 then
        let cp = ((b0 land 0x1F) lsl 6) lor (b1 land 0x3F) in
        if cp >= 0x80 then (cp, 2) else decode_malformed str pos b0
      else decode_malformed str pos b0
    else decode_malformed str pos b0
  else if b0 land 0xF0 = 0xE0 then
    if pos + 2 < len then
      let b1 = Char.code (String.unsafe_get str (pos + 1)) in
      let b2 = Char.code (String.unsafe_get str (pos + 2)) in
      if is_cont_byte b1 && is_cont_byte b2 then
        let cp =
          ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F)
        in
        if cp >= 0x800 && (cp < 0xD800 || cp > 0xDFFF) then (cp, 3)
        else decode_malformed str pos b0
      else decode_malformed str pos b0
    else decode_malformed str pos b0
  else if b0 land 0xF8 = 0xF0 then
    if pos + 3 < len then
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
        if cp >= 0x10000 && cp <= 0x10FFFF then (cp, 4)
        else decode_malformed str pos b0
      else decode_malformed str pos b0
    else decode_malformed str pos b0
  else decode_malformed str pos b0

(* Top-level loops share fitting and positioning without allocating a closure
   for each query. *)
let rec find_pos_ascii str len tab_width include_start_before columns i count
    used =
  if i >= len then
    { byte_offset = len; grapheme_count = count; columns_used = used }
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
      then { byte_offset = i; grapheme_count = count; columns_used = used }
      else
        find_pos_ascii str len tab_width include_start_before columns (i + 1)
          (count + 1) next_used

let rec find_pos_clusters seg str len width_method tab_width
    include_start_before columns offset count used =
  if offset >= len then
    { byte_offset = len; grapheme_count = count; columns_used = used }
  else
    let end_pos = next_boundary seg str offset len in
    let width =
      cluster_width ~method_:width_method ~tab_width str offset
        (end_pos - offset)
    in
    if width <= 0 then
      find_pos_clusters seg str len width_method tab_width include_start_before
        columns end_pos count used
    else
      let next_used = used + width in
      if
        (include_start_before && used >= columns)
        || ((not include_start_before) && next_used > columns)
      then { byte_offset = offset; grapheme_count = count; columns_used = used }
      else
        find_pos_clusters seg str len width_method tab_width
          include_start_before columns end_pos (count + 1) next_used

let find_pos_grapheme ~width_method ~tab_width ~include_start_before str
    ~columns =
  let len = Stdlib.String.length str in
  if len = 0 || columns <= 0 then zero_position
  else if is_ascii_only str len 0 then
    find_pos_ascii str len tab_width include_start_before columns 0 0 0
  else
    let seg = Uuseg_grapheme_cluster.create () in
    find_pos_clusters seg str len width_method tab_width include_start_before
      columns 0 0 0

let find_wrap_pos_wcwidth ~tab_width str ~max_columns =
  let len = Stdlib.String.length str in
  if len = 0 || max_columns <= 0 then zero_position
  else
    let rec loop pos count columns =
      if pos >= len then
        { byte_offset = len; grapheme_count = count; columns_used = columns }
      else
        let cp, cp_len = decode_codepoint str pos in
        let width = codepoint_width_wcwidth ~tab_width cp in
        if columns >= max_columns || columns + width > max_columns then
          { byte_offset = pos; grapheme_count = count; columns_used = columns }
        else loop (pos + cp_len) (count + 1) (columns + width)
    in
    loop 0 0 0

let find_pos_wcwidth ~tab_width ~include_start_before str ~columns =
  let len = Stdlib.String.length str in
  if len = 0 || columns <= 0 then zero_position
  else
    let rec loop pos count used =
      if pos >= len then
        { byte_offset = len; grapheme_count = count; columns_used = used }
      else
        let cp, cp_len = decode_codepoint str pos in
        let width = codepoint_width_wcwidth ~tab_width cp in
        let next_used = used + width in
        if
          (include_start_before && used >= columns)
          || ((not include_start_before) && next_used > columns)
        then { byte_offset = pos; grapheme_count = count; columns_used = used }
        else loop (pos + cp_len) (count + 1) next_used
    in
    loop 0 0 0

let find_wrap_pos ~width_method ~tab_width str ~max_columns =
  let tab_width = normalize_tab_width tab_width in
  match width_method with
  | `Wcwidth -> find_wrap_pos_wcwidth ~tab_width str ~max_columns
  | `Unicode | `Unicode_wide ->
      find_pos_grapheme ~width_method ~tab_width ~include_start_before:false str
        ~columns:max_columns

let find_pos ~width_method ~tab_width ?(include_start_before = false) str
    ~columns =
  let tab_width = normalize_tab_width tab_width in
  match width_method with
  | `Wcwidth -> find_pos_wcwidth ~tab_width ~include_start_before str ~columns
  | `Unicode | `Unicode_wide ->
      find_pos_grapheme ~width_method ~tab_width ~include_start_before str
        ~columns

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
      match width_method with
      | `Wcwidth ->
          let decoded = Stdlib.String.get_utf_8_uchar str byte_offset in
          codepoint_width_wcwidth ~tab_width
            (Uchar.to_int (Uchar.utf_decode_uchar decoded))
      | `Unicode | `Unicode_wide ->
          let seg = Uuseg_grapheme_cluster.create () in
          let end_pos = next_boundary seg str byte_offset len in
          cluster_width ~method_:width_method ~tab_width str byte_offset
            (end_pos - byte_offset)

let prev_grapheme_wcwidth ~tab_width str ~byte_offset =
  let len = Stdlib.String.length str in
  if byte_offset <= 0 || byte_offset > len then None
  else
    let rec loop pos last_offset last_len last_width =
      if pos >= byte_offset then
        if last_offset < 0 then None
        else
          Some
            {
              byte_offset = last_offset;
              byte_length = last_len;
              width = last_width;
            }
      else
        let cp, cp_len = decode_codepoint str pos in
        let width = codepoint_width_wcwidth ~tab_width cp in
        if width > 0 then loop (pos + cp_len) pos cp_len width
        else loop (pos + cp_len) last_offset last_len last_width
    in
    loop 0 (-1) 0 0

let prev_grapheme_segmented ~width_method ~tab_width str ~byte_offset =
  let len = Stdlib.String.length str in
  if byte_offset <= 0 || byte_offset > len then None
  else if is_ascii_only str len 0 then
    let rec loop i =
      if i < 0 then None
      else
        let width =
          ascii_width ~tab_width (Char.code (String.unsafe_get str i))
        in
        if width > 0 then Some { byte_offset = i; byte_length = 1; width }
        else loop (i - 1)
    in
    loop (byte_offset - 1)
  else
    let seg = Uuseg_grapheme_cluster.create () in
    let rec loop offset last_offset last_len =
      if offset >= len || offset >= byte_offset then
        if last_offset < 0 then None else Some (last_offset, last_len)
      else
        let end_pos = next_boundary seg str offset len in
        let grapheme_len = end_pos - offset in
        if end_pos >= byte_offset then Some (offset, grapheme_len)
        else loop end_pos offset grapheme_len
    in
    match loop 0 (-1) 0 with
    | None -> None
    | Some (byte_offset, byte_length) ->
        let width =
          cluster_width ~method_:width_method ~tab_width str byte_offset
            byte_length
        in
        Some { byte_offset; byte_length; width }

let prev_grapheme ~width_method ~tab_width str ~byte_offset =
  let tab_width = normalize_tab_width tab_width in
  match width_method with
  | `Wcwidth -> prev_grapheme_wcwidth ~tab_width str ~byte_offset
  | `Unicode | `Unicode_wide ->
      prev_grapheme_segmented ~width_method ~tab_width str ~byte_offset

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

let[@inline] decode_bounded str pos limit =
  let decoded = Stdlib.String.get_utf_8_uchar str pos in
  let remaining = limit - pos in
  if Uchar.utf_decode_length decoded <= remaining then decoded
  else Uchar.utf_decode_invalid remaining

let rec measure_wcwidth str len tab_width i total =
  if i >= len then total
  else
    let d = decode_bounded str i len in
    let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
    let w = codepoint_width_wcwidth ~tab_width cp in
    measure_wcwidth str len tab_width (i + Uchar.utf_decode_length d) (total + w)

(* Fused segmentation + width loop for Unicode width. [state] is a
   [ws_step] state; a boundary flushes the finished cluster's width into
   [total] and restarts the machine from [0]. *)
let rec measure_segmented ~wide seg str len tab_width i total state =
  if i >= len then total + ws_width state
  else
    let d = decode_bounded str i len in
    let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
    let next = i + Uchar.utf_decode_length d in
    (* Single property lookup yields both boundary decision and width. *)
    let bw =
      Uuseg_grapheme_cluster.check_boundary_with_width seg
        (Uchar.unsafe_of_int cp)
    in
    let cp_w = if cp = 0x09 then tab_width else (bw land 3) - 1 in
    if bw land 4 <> 0 then
      measure_segmented ~wide seg str len tab_width next
        (total + ws_width state)
        (ws_step ~wide cp cp_w 0)
    else
      measure_segmented ~wide seg str len tab_width next total
        (ws_step ~wide cp cp_w state)

let measure ~width_method ~tab_width str =
  let tab_width = normalize_tab_width tab_width in
  let len = Stdlib.String.length str in
  if len = 0 then 0
  else if is_ascii_only str len 0 then measure_ascii str len tab_width 0 0
  else
    match width_method with
    | `Wcwidth -> measure_wcwidth str len tab_width 0 0
    | `Unicode | `Unicode_wide ->
        let seg = Uuseg_grapheme_cluster.create () in
        measure_segmented
          ~wide:(width_method = `Unicode_wide)
          seg str len tab_width 0 0 0

let measure_sub ~width_method ~tab_width str ~pos ~len:sub_len =
  let tab_width = normalize_tab_width tab_width in
  if sub_len <= 0 then 0
  else (
    check_sub "Matrix_text.measure_sub" str ~pos ~len:sub_len;
    let end_pos = pos + sub_len in
    if is_ascii_only str end_pos pos then
      measure_ascii str end_pos tab_width pos 0
    else
      match width_method with
      | `Wcwidth -> measure_wcwidth str end_pos tab_width pos 0
      | `Unicode | `Unicode_wide ->
          let seg = Uuseg_grapheme_cluster.create () in
          measure_segmented
            ~wide:(width_method = `Unicode_wide)
            seg str end_pos tab_width pos 0 0)

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
              && cluster_width ~method_:`Unicode ~tab_width:2 s prev_byte_off
                   (byte_off - prev_byte_off)
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
              cluster_width ~method_:width_method ~tab_width t.text off
                (t.offsets.(g + 1) - off)
            in
            if width = 0 then has_zero := true;
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
      if g > r.visible_last then
        r.columns - if affinity = Upstream then 1 else 0
      else t.metrics.columns.(min g r.last) - t.metrics.columns.(r.first)
    in
    (i, col)

  let hit ?(affinity = Downstream) t ~row:i ~column =
    if i < 0 then { grapheme = 0; affinity = Downstream }
    else if i >= row_count t then
      { grapheme = Source.length t.source; affinity }
    else
      let r = t.rows.(i) and p = t.metrics.columns in
      let g =
        if r.visible_last < r.last && column >= r.columns - 1 then
          if column < r.columns then r.visible_last else r.last
        else
          let target = p.(r.first) + max 0 column in
          let next = lower_bound p r.first (r.visible_last + 1) target in
          if next > r.visible_last then r.visible_last
          else if p.(next) = target then
            if affinity = Upstream then next
            else upper_bound p next (r.visible_last + 1) target - 1
          else if next = r.first then next
          else if target - p.(next - 1) < p.(next) - target then next - 1
          else next
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
