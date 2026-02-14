open StdLabels

(* Types *)

type t = int
type width_method = [ `Unicode | `Wcwidth | `No_zwj ]

type pool = {
  mutable storage : bytes;
  mutable offsets : int array;
  mutable lengths : int array;
  mutable refcounts : int array;
  mutable generations : int array;
  mutable free_stack : int array;
  mutable free_count : int;
  mutable next_id : int;
  mutable storage_cursor : int;
}

(* Constants & Bit Layout *)

(* Bit layout (31 bits used, compatible with 32-bit OCaml): bit 30: grapheme
   flag bit 29: continuation flag bits 27-28: right_extent (2 bits) bits 25-26:
   left_extent (2 bits) bits 18-24: generation (7 bits) bits 0-17: index (18
   bits) → 262K max IDs *)

let flag_grapheme = 0x40000000
let flag_continuation = 0x20000000
let shift_right_extent = 27
let shift_left_extent = 25
let shift_generation = 18
let mask_generation = 0x7F
let mask_index = 0x3FFFF
let default_tab_width = 2
let initial_pool_ids = 4096
let initial_pool_bytes = 4096 * 8

(* ASCII Helpers *)

(* Pre-computed widths for 0-255. -1 indicates control characters (C0, DEL, C1).
   1 indicates printable (including Latin-1). *)
let ascii_width_table =
  let arr = Array.make 256 1 in
  for i = 0 to 31 do
    arr.(i) <- -1
  done;
  arr.(127) <- -1;
  for i = 128 to 159 do
    arr.(i) <- -1
  done;
  arr

let[@inline] normalize_tab_width w = if w <= 0 then default_tab_width else w

let[@inline] ascii_width ~tab_width b =
  if b = 0x09 then tab_width
  else
    let w = Array.unsafe_get ascii_width_table b in
    if w > 0 then w else 0

(* Check if string contains only ASCII bytes - processes 4 bytes at a time *)
let rec is_ascii_only_tail str len j =
  j >= len
  || Char.code (String.unsafe_get str j) < 128
     && is_ascii_only_tail str len (j + 1)

let rec is_ascii_only str len i =
  if i + 4 <= len then
    let c0 = Char.code (String.unsafe_get str i) in
    let c1 = Char.code (String.unsafe_get str (i + 1)) in
    let c2 = Char.code (String.unsafe_get str (i + 2)) in
    let c3 = Char.code (String.unsafe_get str (i + 3)) in
    c0 lor c1 lor c2 lor c3 < 128 && is_ascii_only str len (i + 4)
  else is_ascii_only_tail str len i

(* Check if string contains any non-ASCII bytes - processes 4 bytes at a time *)
let rec has_non_ascii_tail str len j =
  j < len
  && (Char.code (String.unsafe_get str j) >= 128
     || has_non_ascii_tail str len (j + 1))

let rec has_non_ascii str len i =
  if i + 4 <= len then
    let c0 = Char.code (String.unsafe_get str i) in
    let c1 = Char.code (String.unsafe_get str (i + 1)) in
    let c2 = Char.code (String.unsafe_get str (i + 2)) in
    let c3 = Char.code (String.unsafe_get str (i + 3)) in
    c0 lor c1 lor c2 lor c3 >= 128 || has_non_ascii str len (i + 4)
  else has_non_ascii_tail str len i

(* Width Predicates *)

let[@inline] is_regional_indicator cp = cp >= 0x1F1E6 && cp <= 0x1F1FF

let[@inline] is_virama cp =
  cp = 0x094D || cp = 0x09CD || cp = 0x0A4D || cp = 0x0ACD || cp = 0x0B4D
  || cp = 0x0BCD || cp = 0x0C4D || cp = 0x0CCD || cp = 0x0D4D

let[@inline] is_devanagari_base cp =
  (cp >= 0x0915 && cp <= 0x0939) || (cp >= 0x0958 && cp <= 0x095F)

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

let[@inline] codepoint_width ~method_ ~tab_width cp =
  match method_ with
  | `Wcwidth -> codepoint_width_wcwidth ~tab_width cp
  | `Unicode | `No_zwj -> codepoint_width_unicode ~tab_width cp

(* Grapheme Cluster Width (for a slice of string) *)

(* Flag bits for width state *)
let width_flag_has_width = 1
let width_flag_ri_pair = 2
let width_flag_virama = 4

let rec grapheme_width_unicode_loop str limit tab_width i width flags =
  if i >= limit then width
  else
    let d = String.get_utf_8_uchar str i in
    let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
    let next = i + Uchar.utf_decode_length d in
    let cp_width = codepoint_width_unicode ~tab_width cp in
    let has_width = flags land width_flag_has_width <> 0 in
    let is_ri_pair = flags land width_flag_ri_pair <> 0 in
    let has_virama = flags land width_flag_virama <> 0 in
    if cp = 0xFE0F then
      let new_width = if has_width && width = 1 then 2 else width in
      grapheme_width_unicode_loop str limit tab_width next new_width flags
    else if is_virama cp then
      grapheme_width_unicode_loop str limit tab_width next width
        (flags lor width_flag_virama)
    else if is_regional_indicator cp then
      if is_ri_pair then
        grapheme_width_unicode_loop str limit tab_width next (width + cp_width)
          (flags lor width_flag_has_width land lnot width_flag_ri_pair
         land lnot width_flag_virama)
      else
        let new_w = if not has_width then cp_width else width in
        grapheme_width_unicode_loop str limit tab_width next new_w
          (flags lor width_flag_has_width lor width_flag_ri_pair
         land lnot width_flag_virama)
    else if has_width && has_virama && is_devanagari_base cp then
      let add = if cp <> 0x0930 && cp_width > 0 then cp_width else 0 in
      grapheme_width_unicode_loop str limit tab_width next (width + add)
        (flags lor width_flag_has_width land lnot width_flag_virama)
    else if (not has_width) && cp_width > 0 then
      grapheme_width_unicode_loop str limit tab_width next cp_width
        (flags lor width_flag_has_width land lnot width_flag_virama)
    else
      grapheme_width_unicode_loop str limit tab_width next width
        (flags land lnot width_flag_virama)

let rec grapheme_width_wcwidth_loop str limit tab_width i acc =
  if i >= limit then if acc < 0 then 0 else acc
  else
    let d = String.get_utf_8_uchar str i in
    let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
    let next = i + Uchar.utf_decode_length d in
    let w = codepoint_width_wcwidth ~tab_width cp in
    grapheme_width_wcwidth_loop str limit tab_width next
      (acc + if w < 0 then 0 else w)

let grapheme_width ~method_ ~tab_width str off len =
  let limit = off + len in
  match method_ with
  | `Wcwidth -> grapheme_width_wcwidth_loop str limit tab_width off 0
  | `Unicode | `No_zwj ->
      grapheme_width_unicode_loop str limit tab_width off 0 0

(* Grapheme Segmentation *)

let rec find_boundary_loop seg str limit pos =
  if pos >= limit then limit
  else
    let d = String.get_utf_8_uchar str pos in
    let u = Uchar.utf_decode_uchar d in
    if Uuseg_grapheme_cluster.check_boundary seg u then pos
    else find_boundary_loop seg str limit (pos + Uchar.utf_decode_length d)

(* Find the next grapheme cluster boundary starting at [start]. Returns the byte
   offset after the grapheme cluster. When [ignore_zwj] is true, GB11 is
   disabled (no emoji ZWJ sequences). *)
let next_boundary seg ~ignore_zwj str start limit =
  if start >= limit then limit
  else (
    Uuseg_grapheme_cluster.reset seg;
    Uuseg_grapheme_cluster.set_ignore_zwj seg ignore_zwj;
    let d = String.get_utf_8_uchar str start in
    let u = Uchar.utf_decode_uchar d in
    let _ = Uuseg_grapheme_cluster.check_boundary seg u in
    find_boundary_loop seg str limit (start + Uchar.utf_decode_length d))

(* Pool Management *)

let create_pool () =
  {
    storage = Bytes.create initial_pool_bytes;
    offsets = Array.make initial_pool_ids 0;
    lengths = Array.make initial_pool_ids 0;
    refcounts = Array.make initial_pool_ids 0;
    generations = Array.make initial_pool_ids 0;
    free_stack = Array.make initial_pool_ids 0;
    free_count = 0;
    next_id = 1;
    storage_cursor = 0;
  }

let ensure_id_capacity pool =
  let cap = Array.length pool.offsets in
  if pool.next_id >= cap then (
    let new_cap = cap * 2 in
    if new_cap > mask_index + 1 then failwith "Glyph pool ID exhaustion";
    let resize arr def =
      let new_arr = Array.make new_cap def in
      Array.blit ~src:arr ~src_pos:0 ~dst:new_arr ~dst_pos:0 ~len:cap;
      new_arr
    in
    pool.offsets <- resize pool.offsets 0;
    pool.lengths <- resize pool.lengths 0;
    pool.refcounts <- resize pool.refcounts 0;
    pool.generations <- resize pool.generations 0;
    pool.free_stack <- resize pool.free_stack 0)

let ensure_storage_capacity pool needed =
  let cap = Bytes.length pool.storage in
  if pool.storage_cursor + needed > cap then (
    let new_cap = max (cap * 2) (pool.storage_cursor + needed) in
    let new_bytes = Bytes.create new_cap in
    Bytes.blit ~src:pool.storage ~src_pos:0 ~dst:new_bytes ~dst_pos:0
      ~len:pool.storage_cursor;
    pool.storage <- new_bytes)

let[@inline] next_free_id pool =
  if pool.free_count > 0 then (
    let i = pool.free_count - 1 in
    pool.free_count <- i;
    let id = Array.unsafe_get pool.free_stack i in
    let g = (Array.unsafe_get pool.generations id + 1) land mask_generation in
    Array.unsafe_set pool.generations id g;
    id)
  else
    let id = pool.next_id in
    pool.next_id <- id + 1;
    Array.unsafe_set pool.generations id 0;
    id

let[@inline] push_free pool idx =
  Array.unsafe_set pool.free_stack pool.free_count idx;
  pool.free_count <- pool.free_count + 1

let alloc_string pool str off len =
  ensure_id_capacity pool;
  let id = next_free_id pool in
  let cap = Array.unsafe_get pool.lengths id in
  let cursor =
    if cap >= len then Array.unsafe_get pool.offsets id
    else (
      ensure_storage_capacity pool len;
      let cur = pool.storage_cursor in
      pool.storage_cursor <- cur + len;
      cur)
  in
  Bytes.blit_string ~src:str ~src_pos:off ~dst:pool.storage ~dst_pos:cursor ~len;
  Array.unsafe_set pool.offsets id cursor;
  Array.unsafe_set pool.lengths id len;
  Array.unsafe_set pool.refcounts id 0;
  id

let alloc_codepoint pool u len =
  ensure_id_capacity pool;
  let id = next_free_id pool in
  let cap = Array.unsafe_get pool.lengths id in
  let cursor =
    if cap >= len then Array.unsafe_get pool.offsets id
    else (
      ensure_storage_capacity pool len;
      let cur = pool.storage_cursor in
      pool.storage_cursor <- cur + len;
      cur)
  in
  let dst = pool.storage in
  if len = 1 then Bytes.unsafe_set dst cursor (Char.chr u)
  else if len = 2 then (
    Bytes.unsafe_set dst cursor (Char.chr (0xc0 lor (u lsr 6)));
    Bytes.unsafe_set dst (cursor + 1) (Char.chr (0x80 lor (u land 0x3f))))
  else if len = 3 then (
    Bytes.unsafe_set dst cursor (Char.chr (0xe0 lor (u lsr 12)));
    Bytes.unsafe_set dst (cursor + 1)
      (Char.chr (0x80 lor ((u lsr 6) land 0x3f)));
    Bytes.unsafe_set dst (cursor + 2) (Char.chr (0x80 lor (u land 0x3f))))
  else (
    Bytes.unsafe_set dst cursor (Char.chr (0xf0 lor (u lsr 18)));
    Bytes.unsafe_set dst (cursor + 1)
      (Char.chr (0x80 lor ((u lsr 12) land 0x3f)));
    Bytes.unsafe_set dst (cursor + 2)
      (Char.chr (0x80 lor ((u lsr 6) land 0x3f)));
    Bytes.unsafe_set dst (cursor + 3) (Char.chr (0x80 lor (u land 0x3f))));
  Array.unsafe_set pool.offsets id cursor;
  Array.unsafe_set pool.lengths id len;
  Array.unsafe_set pool.refcounts id 0;
  id

let clear pool =
  pool.next_id <- 1;
  pool.storage_cursor <- 0;
  pool.free_count <- 0;
  Array.fill pool.offsets ~pos:0 ~len:(Array.length pool.offsets) 0;
  Array.fill pool.lengths ~pos:0 ~len:(Array.length pool.lengths) 0;
  Array.fill pool.refcounts ~pos:0 ~len:(Array.length pool.refcounts) 0;
  Array.fill pool.generations ~pos:0 ~len:(Array.length pool.generations) 0

(* Glyph Packing & Accessors *)

let[@inline] clamp_extent v = if v < 0 then 0 else if v > 3 then 3 else v

let[@inline] pack_start idx gen width =
  let w = if width < 1 then 1 else width in
  let right = if w > 4 then 3 else w - 1 in
  flag_grapheme
  lor (right lsl shift_right_extent)
  lor (gen lsl shift_generation) lor (idx land mask_index)

let[@inline] pack_continuation ~idx ~gen ~left ~right =
  flag_grapheme lor flag_continuation
  lor (clamp_extent left lsl shift_left_extent)
  lor (clamp_extent right lsl shift_right_extent)
  lor (gen lsl shift_generation) lor (idx land mask_index)

let[@inline] is_simple c = c land flag_grapheme = 0
let[@inline] is_start c = is_simple c || c land flag_continuation = 0

let[@inline] is_continuation c =
  (not (is_simple c)) && c land flag_continuation <> 0

let[@inline] is_empty c = c = 0
let[@inline] right_extent c = (c lsr shift_right_extent) land 3
let[@inline] left_extent c = (c lsr shift_left_extent) land 3
let[@inline] unpack_idx c = c land mask_index
let[@inline] unpack_gen c = (c lsr shift_generation) land mask_generation

let[@inline] validate_complex pool c =
  let idx = unpack_idx c in
  let gen = unpack_gen c in
  if
    idx > 0 && idx < pool.next_id
    && Array.unsafe_get pool.generations idx = gen
    && Array.unsafe_get pool.refcounts idx >= 0
  then idx
  else -1

let[@inline] width ?(tab_width = default_tab_width) c =
  let tab_width = normalize_tab_width tab_width in
  if is_empty c then 0
  else if is_simple c then ascii_width ~tab_width (c land 0xFF)
  else
    let l = left_extent c in
    let r = right_extent c in
    if is_continuation c then l + 1 + r else if l <> 0 then 0 else r + 1

(* Reference Counting *)

let incref pool c =
  if not (is_simple c) then
    let idx = validate_complex pool c in
    if idx >= 0 then
      Array.unsafe_set pool.refcounts idx
        (Array.unsafe_get pool.refcounts idx + 1)

let decref pool c =
  if not (is_simple c) then
    let idx = validate_complex pool c in
    if idx >= 0 then
      let rc = Array.unsafe_get pool.refcounts idx in
      if rc > 0 then (
        let rc' = rc - 1 in
        Array.unsafe_set pool.refcounts idx rc';
        if rc' = 0 then (
          Array.unsafe_set pool.refcounts idx (-1);
          push_free pool idx))
      else if rc = 0 then (
        Array.unsafe_set pool.refcounts idx (-1);
        push_free pool idx)

(* Data Retrieval *)

let blit pool c buf off =
  if is_simple c then
    let u = Uchar.unsafe_of_int c in
    let len = Uchar.utf_8_byte_length u in
    if len > Bytes.length buf - off then 0 else Bytes.set_utf_8_uchar buf off u
  else
    let idx = validate_complex pool c in
    if idx < 0 then 0
    else
      let len = Array.unsafe_get pool.lengths idx in
      if len > Bytes.length buf - off then 0
      else
        let src_off = Array.unsafe_get pool.offsets idx in
        Bytes.blit ~src:pool.storage ~src_pos:src_off ~dst:buf ~dst_pos:off ~len;
        len

let copy src_pool c dst_pool =
  if is_simple c then c
  else
    let idx = validate_complex src_pool c in
    if idx < 0 then 0
    else
      let len = Array.unsafe_get src_pool.lengths idx in
      let src_off = Array.unsafe_get src_pool.offsets idx in
      if src_off + len > Bytes.length src_pool.storage then 0
      else (
        ensure_id_capacity dst_pool;
        let dst_id = next_free_id dst_pool in
        let dst_gen = Array.unsafe_get dst_pool.generations dst_id in
        let cap = Array.unsafe_get dst_pool.lengths dst_id in
        let cursor =
          if cap >= len then Array.unsafe_get dst_pool.offsets dst_id
          else (
            ensure_storage_capacity dst_pool len;
            let cur = dst_pool.storage_cursor in
            dst_pool.storage_cursor <- cur + len;
            cur)
        in
        Bytes.blit ~src:src_pool.storage ~src_pos:src_off ~dst:dst_pool.storage
          ~dst_pos:cursor ~len;
        Array.unsafe_set dst_pool.offsets dst_id cursor;
        Array.unsafe_set dst_pool.lengths dst_id len;
        Array.unsafe_set dst_pool.refcounts dst_id 0;
        if is_continuation c then
          pack_continuation ~idx:dst_id ~gen:dst_gen ~left:(left_extent c)
            ~right:(right_extent c)
        else pack_start dst_id dst_gen (width c))

let to_string pool c =
  if is_simple c then (
    let u = Uchar.unsafe_of_int c in
    let len = Uchar.utf_8_byte_length u in
    let buf = Bytes.create len in
    ignore (Bytes.set_utf_8_uchar buf 0 u);
    Bytes.unsafe_to_string buf)
  else
    let idx = validate_complex pool c in
    if idx < 0 then ""
    else
      let len = Array.unsafe_get pool.lengths idx in
      let off = Array.unsafe_get pool.offsets idx in
      Bytes.sub_string pool.storage ~pos:off ~len

let length pool c =
  if is_simple c then Uchar.utf_8_byte_length (Uchar.unsafe_of_int c)
  else
    let idx = validate_complex pool c in
    if idx < 0 then 0 else Array.unsafe_get pool.lengths idx

(* Interning *)

(* Check ASCII and compute width in one pass. Returns -1 if non-ASCII found. *)
let rec ascii_width_loop_tail str limit tab_width i acc =
  if i >= limit then acc
  else
    let b = Char.code (String.unsafe_get str i) in
    if b >= 128 then -1
    else
      ascii_width_loop_tail str limit tab_width (i + 1)
        (acc + ascii_width ~tab_width b)

let rec ascii_width_loop str limit tab_width i acc =
  if i + 4 <= limit then
    let b0 = Char.code (String.unsafe_get str i) in
    let b1 = Char.code (String.unsafe_get str (i + 1)) in
    let b2 = Char.code (String.unsafe_get str (i + 2)) in
    let b3 = Char.code (String.unsafe_get str (i + 3)) in
    if b0 lor b1 lor b2 lor b3 >= 128 then -1
    else
      let w0 = ascii_width ~tab_width b0 in
      let w1 = ascii_width ~tab_width b1 in
      let w2 = ascii_width ~tab_width b2 in
      let w3 = ascii_width ~tab_width b3 in
      ascii_width_loop str limit tab_width (i + 4) (acc + w0 + w1 + w2 + w3)
  else ascii_width_loop_tail str limit tab_width i acc

let intern_core pool method_ tab_width precomputed_width off len str =
  if len = 0 then 0
  else if len = 1 then
    let b = Char.code (String.unsafe_get str off) in
    let w =
      match precomputed_width with
      | Some w -> w
      | None ->
          if b < 128 then ascii_width ~tab_width b
          else codepoint_width ~method_ ~tab_width b
    in
    if w <= 0 then 0 else b
  else
    match precomputed_width with
    | Some w ->
        if w <= 0 then 0
        else
          let idx = alloc_string pool str off len in
          pack_start idx (Array.unsafe_get pool.generations idx) w
    | None ->
        let first_b = Char.code (String.unsafe_get str off) in
        if first_b >= 128 then
          let w = grapheme_width ~method_ ~tab_width str off len in
          if w <= 0 then 0
          else
            let idx = alloc_string pool str off len in
            pack_start idx (Array.unsafe_get pool.generations idx) w
        else
          let limit = off + len in
          let ascii_w = ascii_width_loop str limit tab_width off 0 in
          if ascii_w >= 0 then
            if ascii_w = 0 then 0
            else
              let idx = alloc_string pool str off len in
              pack_start idx (Array.unsafe_get pool.generations idx) ascii_w
          else
            let w = grapheme_width ~method_ ~tab_width str off len in
            if w <= 0 then 0
            else
              let idx = alloc_string pool str off len in
              pack_start idx (Array.unsafe_get pool.generations idx) w

let intern pool ?(width_method = `Unicode) ?(tab_width = default_tab_width)
    ?width ?(off = 0) ?len str =
  let tab_width = normalize_tab_width tab_width in
  let len = match len with Some l -> l | None -> String.length str - off in
  intern_core pool width_method tab_width width off len str

let intern_char pool u =
  let tab_width = default_tab_width in
  if u < 128 then
    let w = ascii_width ~tab_width u in
    if w <= 0 then 0 else u
  else
    let len = if u < 0x800 then 2 else if u < 0x10000 then 3 else 4 in
    let w = codepoint_width ~method_:`Unicode ~tab_width u in
    if w <= 0 then 0
    else
      let idx = alloc_codepoint pool u len in
      pack_start idx (Array.unsafe_get pool.generations idx) w

(* Encoding (string -> glyph stream) *)

let encode_lazy pool ~width_method ~tab_width str on_grapheme =
  let tab_width = normalize_tab_width tab_width in

  (* Emit helper: builds a lazily allocated emitter for a complex grapheme. *)
  let make_complex_emitter ~str ~off ~len ~width =
    let cached = ref None in
    fun f ->
      let start, idx, gen =
        match !cached with
        | Some triple -> triple
        | None ->
            let idx = alloc_string pool str off len in
            let gen = Array.unsafe_get pool.generations idx in
            let start = pack_start idx gen width in
            let triple = (start, idx, gen) in
            cached := Some triple;
            triple
      in
      f start;
      if width > 1 then
        let max_span = min 4 width - 1 in
        for k = 1 to max_span do
          f (pack_continuation ~idx ~gen ~left:k ~right:(max_span - k))
        done
  in

  let rec loop seg method_ ignore_zwj str len i =
    if i >= len then ()
    else if
      i + 4 <= len
      &&
      let c0 = Char.code (String.unsafe_get str i) in
      let c1 = Char.code (String.unsafe_get str (i + 1)) in
      let c2 = Char.code (String.unsafe_get str (i + 2)) in
      let c3 = Char.code (String.unsafe_get str (i + 3)) in
      c0 lor c1 lor c2 lor c3 < 128
    then (
      let emit_ascii b =
        let w = ascii_width ~tab_width b in
        if w > 0 then on_grapheme ~width:w ~emit:(fun f -> f b)
      in
      emit_ascii (Char.code (String.unsafe_get str i));
      emit_ascii (Char.code (String.unsafe_get str (i + 1)));
      emit_ascii (Char.code (String.unsafe_get str (i + 2)));
      emit_ascii (Char.code (String.unsafe_get str (i + 3)));
      loop seg method_ ignore_zwj str len (i + 4))
    else
      let c = String.unsafe_get str i in
      if Char.code c < 128 then (
        let w = ascii_width ~tab_width (Char.code c) in
        if w > 0 then on_grapheme ~width:w ~emit:(fun f -> f (Char.code c));
        loop seg method_ ignore_zwj str len (i + 1))
      else
        let end_pos = next_boundary seg ~ignore_zwj str i len in
        let clus_len = end_pos - i in
        let w = grapheme_width ~method_ ~tab_width str i clus_len in
        if w > 0 then (
          let emit = make_complex_emitter ~str ~off:i ~len:clus_len ~width:w in
          on_grapheme ~width:w ~emit;
          loop seg method_ ignore_zwj str len end_pos)
        else loop seg method_ ignore_zwj str len end_pos
  in

  let seg = Uuseg_grapheme_cluster.create () in
  loop seg width_method (width_method = `No_zwj) str (String.length str) 0

let encode pool ~width_method ~tab_width str f =
  encode_lazy pool ~width_method ~tab_width str (fun ~width:_ ~emit -> emit f)

let iter_grapheme_info ~width_method ~tab_width str f =
  let tab_width = normalize_tab_width tab_width in
  let len = String.length str in
  if len = 0 then ()
  else
    let seg = Uuseg_grapheme_cluster.create () in
    let ignore_zwj = width_method = `No_zwj in

    let emit_ascii i =
      let b = Char.code (String.unsafe_get str i) in
      let w = ascii_width ~tab_width b in
      if w > 0 then f ~offset:i ~len:1 ~width:w
    in

    let rec loop i =
      if i >= len then ()
      else if
        i + 4 <= len
        &&
        let c0 = Char.code (String.unsafe_get str i) in
        let c1 = Char.code (String.unsafe_get str (i + 1)) in
        let c2 = Char.code (String.unsafe_get str (i + 2)) in
        let c3 = Char.code (String.unsafe_get str (i + 3)) in
        c0 lor c1 lor c2 lor c3 < 128
      then (
        emit_ascii i;
        emit_ascii (i + 1);
        emit_ascii (i + 2);
        emit_ascii (i + 3);
        loop (i + 4))
      else
        let c = String.unsafe_get str i in
        if Char.code c < 128 then (
          emit_ascii i;
          loop (i + 1))
        else
          let end_pos = next_boundary seg ~ignore_zwj str i len in
          let clus_len = end_pos - i in
          let w =
            grapheme_width ~method_:width_method ~tab_width str i clus_len
          in
          if w > 0 then (
            f ~offset:i ~len:clus_len ~width:w;
            loop end_pos)
          else loop end_pos
    in

    loop 0

(* Grapheme Iteration *)

let rec iter_graphemes_ascii str len f i =
  if i >= len then ()
  else if
    Char.code (String.unsafe_get str i) = 0x0D
    && i + 1 < len
    && String.unsafe_get str (i + 1) = '\n'
  then (
    f ~offset:i ~len:2;
    iter_graphemes_ascii str len f (i + 2))
  else (
    f ~offset:i ~len:1;
    iter_graphemes_ascii str len f (i + 1))

let rec iter_graphemes_unicode seg str len f i start =
  if i >= len then (if start < len then f ~offset:start ~len:(len - start))
  else
    let d = String.get_utf_8_uchar str i in
    let u = Uchar.utf_decode_uchar d in
    let next = i + Uchar.utf_decode_length d in
    if Uuseg_grapheme_cluster.check_boundary seg u then (
      f ~offset:start ~len:(i - start);
      iter_graphemes_unicode seg str len f next i)
    else iter_graphemes_unicode seg str len f next start

let iter_graphemes f str =
  let len = String.length str in
  if len = 0 then ()
  else if not (has_non_ascii str len 0) then iter_graphemes_ascii str len f 0
  else
    let seg = Uuseg_grapheme_cluster.create () in
    let d = String.get_utf_8_uchar str 0 in
    let _ =
      Uuseg_grapheme_cluster.check_boundary seg (Uchar.utf_decode_uchar d)
    in
    iter_graphemes_unicode seg str len f (Uchar.utf_decode_length d) 0

(* String Measurement *)

let rec measure_ascii_tail str len tab_width i total =
  if i >= len then total
  else
    let b = Char.code (String.unsafe_get str i) in
    let w = ascii_width ~tab_width b in
    measure_ascii_tail str len tab_width (i + 1) (total + if w > 0 then w else 0)

let rec measure_ascii str len tab_width i total =
  if i + 4 <= len then
    let b0 = Char.code (String.unsafe_get str i) in
    let b1 = Char.code (String.unsafe_get str (i + 1)) in
    let b2 = Char.code (String.unsafe_get str (i + 2)) in
    let b3 = Char.code (String.unsafe_get str (i + 3)) in
    let w0 = ascii_width ~tab_width b0 in
    let w1 = ascii_width ~tab_width b1 in
    let w2 = ascii_width ~tab_width b2 in
    let w3 = ascii_width ~tab_width b3 in
    let add0 = if w0 > 0 then w0 else 0 in
    let add1 = if w1 > 0 then w1 else 0 in
    let add2 = if w2 > 0 then w2 else 0 in
    let add3 = if w3 > 0 then w3 else 0 in
    measure_ascii str len tab_width (i + 4) (total + add0 + add1 + add2 + add3)
  else measure_ascii_tail str len tab_width i total

let rec measure_wcwidth str len tab_width i total =
  if i >= len then total
  else
    let d = String.get_utf_8_uchar str i in
    let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
    let w = codepoint_width_wcwidth ~tab_width cp in
    measure_wcwidth str len tab_width (i + Uchar.utf_decode_length d) (total + w)

(* Fused segmentation + width loop for Unicode/No_zwj methods *)
let rec measure_segmented seg str len tab_width i total g_w g_has g_ri g_vs
    g_vir =
  if i >= len then if g_has then total + g_w else total
  else
    let d = String.get_utf_8_uchar str i in
    let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
    let next = i + Uchar.utf_decode_length d in
    if Uuseg_grapheme_cluster.check_boundary seg (Uchar.unsafe_of_int cp) then
      let new_total = if g_has then total + g_w else total in
      let cp_w = codepoint_width_unicode ~tab_width cp in
      if cp = 0xFE0F then
        measure_segmented seg str len tab_width next new_total 0 false false
          true false
      else if is_virama cp then
        measure_segmented seg str len tab_width next new_total 0 false false
          false true
      else if is_regional_indicator cp then
        measure_segmented seg str len tab_width next new_total cp_w true true
          false false
      else if cp_w > 0 then
        measure_segmented seg str len tab_width next new_total cp_w true false
          false false
      else
        measure_segmented seg str len tab_width next new_total 0 false false
          false false
    else
      let is_vs16 = cp = 0xFE0F in
      if is_vs16 then
        let new_w = if g_has && g_w = 1 then 2 else g_w in
        measure_segmented seg str len tab_width next total new_w g_has g_ri true
          g_vir
      else if is_virama cp then
        measure_segmented seg str len tab_width next total g_w g_has g_ri g_vs
          true
      else if is_regional_indicator cp then
        let cp_w = codepoint_width_unicode ~tab_width cp in
        if g_ri then
          measure_segmented seg str len tab_width next total (g_w + cp_w) true
            false g_vs false
        else
          let new_w = if not g_has then cp_w else g_w in
          measure_segmented seg str len tab_width next total new_w true true
            g_vs false
      else if g_has && g_vir && is_devanagari_base cp then
        let cp_w = codepoint_width_unicode ~tab_width cp in
        let add = if cp <> 0x0930 && cp_w > 0 then cp_w else 0 in
        measure_segmented seg str len tab_width next total (g_w + add) true g_ri
          g_vs false
      else
        let cp_w = codepoint_width_unicode ~tab_width cp in
        if (not g_has) && cp_w > 0 then
          measure_segmented seg str len tab_width next total cp_w true g_ri g_vs
            false
        else
          measure_segmented seg str len tab_width next total g_w g_has g_ri g_vs
            false

let measure ~width_method ~tab_width str =
  let tab_width = normalize_tab_width tab_width in
  let len = String.length str in
  if len = 0 then 0
  else if is_ascii_only str len 0 then measure_ascii str len tab_width 0 0
  else
    match width_method with
    | `Wcwidth -> measure_wcwidth str len tab_width 0 0
    | `Unicode | `No_zwj ->
        let seg = Uuseg_grapheme_cluster.create () in
        Uuseg_grapheme_cluster.set_ignore_zwj seg (width_method = `No_zwj);
        let d = String.get_utf_8_uchar str 0 in
        let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
        let _ =
          Uuseg_grapheme_cluster.check_boundary seg (Uchar.unsafe_of_int cp)
        in
        let w = codepoint_width_unicode ~tab_width cp in
        measure_segmented seg str len tab_width
          (Uchar.utf_decode_length d)
          0
          (if w > 0 then w else 0)
          (w > 0) (is_regional_indicator cp) (cp = 0xFE0F) (is_virama cp)

(* Text Segmentation (wrap breaks, line breaks) *)

let[@inline] is_ascii_wrap_break b =
  match b with
  | 0x20 | 0x09 | 0x2D | 0x2F | 0x5C | 0x2E | 0x2C | 0x3B | 0x3A | 0x21 | 0x3F
  | 0x28 | 0x29 | 0x5B | 0x5D | 0x7B | 0x7D ->
      true
  | _ -> false

let[@inline] is_unicode_wrap_break cp =
  match cp with
  | 0x00A0 | 0x1680 | 0x202F | 0x205F | 0x3000 | 0x200B | 0x00AD | 0x2010 ->
      true
  | cp when cp >= 0x2000 && cp <= 0x200A -> true
  | _ -> false

let iter_wrap_breaks ?(width_method = `Unicode) f s =
  let len = String.length s in
  let ignore_zwj = width_method = `No_zwj in
  let seg = Uuseg_grapheme_cluster.create () in
  let rec has_break i limit =
    if i >= limit then false
    else
      let b0 = Char.code (String.unsafe_get s i) in
      if b0 < 0x80 then is_ascii_wrap_break b0 || has_break (i + 1) limit
      else
        let d = String.get_utf_8_uchar s i in
        let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
        is_unicode_wrap_break cp
        || has_break (i + Uchar.utf_decode_length d) limit
  in
  let rec loop byte_off g_off =
    if byte_off >= len then ()
    else
      let next = next_boundary seg ~ignore_zwj s byte_off len in
      if has_break byte_off next then f ~byte_offset:next ~grapheme_offset:g_off;
      loop next (g_off + 1)
  in
  loop 0 0

type line_break_kind = [ `LF | `CR | `CRLF ]

let iter_line_breaks f s =
  let len = String.length s in
  let rec loop i =
    if i >= len then ()
    else
      let b = Char.code (String.unsafe_get s i) in
      if b = 0x0A then (
        let kind =
          if i > 0 && Char.code (String.unsafe_get s (i - 1)) = 0x0D then `CRLF
          else `LF
        in
        if kind = `LF then f ~pos:i ~kind;
        loop (i + 1))
      else if b = 0x0D then
        if i + 1 < len && Char.code (String.unsafe_get s (i + 1)) = 0x0A then (
          f ~pos:(i + 1) ~kind:`CRLF;
          loop (i + 2))
        else (
          f ~pos:i ~kind:`CR;
          loop (i + 1))
      else loop (i + 1)
  in
  loop 0
