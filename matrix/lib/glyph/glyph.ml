open StdLabels

(* 
   Pre-computed widths for 0-255. 
   -1 indicates control characters (C0, DEL, C1).
   1 indicates printable (including Latin-1).
*)
let ascii_table =
  let arr = Array.make 256 1 in
  (* C0 Controls (0-31) *)
  for i = 0 to 31 do
    arr.(i) <- -1
  done;
  (* DEL (127) *)
  arr.(127) <- -1;
  (* C1 Controls (128-159) - often treated as control/zero-width in terminals *)
  for i = 128 to 159 do
    arr.(i) <- -1
  done;
  arr

let empty_string = ""
let default_tab_width = 2

let[@inline] normalize_tab_width tab_width =
  if tab_width <= 0 then default_tab_width else tab_width

let[@inline] ascii_display_width ~tab_width b =
  if b = 0x09 then tab_width
  else
    let w = Array.unsafe_get ascii_table b in
    if w > 0 then w else 0

(* --- Constants & Bit Layout --- *)

type t = int32
type width_method = [ `Unicode | `Wcwidth | `No_zwj ]

let flag_grapheme = 0x80000000l
let flag_continuation = 0x40000000l
let shift_right_extent = 28
let shift_left_extent = 26
let shift_generation = 19
let mask_generation = 0x7F
let mask_index = 0x7FFFF
let[@inline] clamp_extent v = if v < 0 then 0 else if v > 3 then 3 else v

(* Width Predicates *)
let[@inline] is_regional_indicator cp = cp >= 0x1F1E6 && cp <= 0x1F1FF

let[@inline] is_virama cp =
  cp = 0x094D
  || (cp >= 0x09CD && cp <= 0x09CD)
  || (cp >= 0x0A4D && cp <= 0x0A4D)
  || (cp >= 0x0ACD && cp <= 0x0ACD)
  || (cp >= 0x0B4D && cp <= 0x0B4D)
  || (cp >= 0x0BCD && cp <= 0x0BCD)
  || (cp >= 0x0C4D && cp <= 0x0C4D)
  || (cp >= 0x0CCD && cp <= 0x0CCD)
  || (cp >= 0x0D4D && cp <= 0x0D4D)

let[@inline] is_devanagari_ra cp = cp = 0x0930

let[@inline] is_devanagari_base cp =
  (cp >= 0x0915 && cp <= 0x0939) || (cp >= 0x0958 && cp <= 0x095F)

let[@inline] decode_utf8_bounded str i limit =
  let b0 = Char.code (String.unsafe_get str i) in
  if b0 land 0x80 = 0 then (b0, 1)
  else if b0 land 0xE0 = 0xC0 && i + 1 < limit then
    let b1 = Char.code (String.unsafe_get str (i + 1)) land 0x3F in
    (((b0 land 0x1F) lsl 6) lor b1, 2)
  else if b0 land 0xF0 = 0xE0 && i + 2 < limit then
    let b1 = Char.code (String.unsafe_get str (i + 1)) land 0x3F in
    let b2 = Char.code (String.unsafe_get str (i + 2)) land 0x3F in
    (((b0 land 0x0F) lsl 12) lor (b1 lsl 6) lor b2, 3)
  else if b0 land 0xF8 = 0xF0 && i + 3 < limit then
    let b1 = Char.code (String.unsafe_get str (i + 1)) land 0x3F in
    let b2 = Char.code (String.unsafe_get str (i + 2)) land 0x3F in
    let b3 = Char.code (String.unsafe_get str (i + 3)) land 0x3F in
    (((b0 land 0x07) lsl 18) lor (b1 lsl 12) lor (b2 lsl 6) lor b3, 4)
  else (b0, 1)

let char_width ~method_ ~tab_width cp =
  if cp < 0x80 then
    (* ASCII Fast Path *)
    if cp = 0x09 then tab_width
    else if cp < 32 || cp = 127 then
      match method_ with `Wcwidth -> 0 | _ -> -1
    else 1
  else
    match Grapheme_cluster.width_of_cp cp with
    | 1 -> 1
    | 0 -> 0
    | 2 -> 2
    | 3 -> ( match method_ with `Wcwidth -> 0 | _ -> -1)
    | _ -> 1

(* Fused width calculation *)
let calculate_width ~method_ ~tab_width str off len =
  match method_ with
  | `Wcwidth ->
      let limit = off + len in
      let rec loop i acc =
        if i >= limit then if acc < 0 then 0 else acc
        else
          let cp, clen = decode_utf8_bounded str i limit in
          let next = i + clen in
          let w = char_width ~method_ ~tab_width cp in
          loop next (acc + if w < 0 then 0 else w)
      in
      loop off 0
  | `Unicode | `No_zwj ->
      let limit = off + len in
      let rec loop i width has_width is_ri_pair has_vs16 has_indic_virama =
        if i >= limit then width
        else
          let cp, clen = decode_utf8_bounded str i limit in
          let next = i + clen in
          let cp_width = char_width ~method_:`Unicode ~tab_width cp in
          let is_vs16_now = cp = 0xFE0F in

          if is_vs16_now then
            let new_width = if has_width && width = 1 then 2 else width in
            loop next new_width has_width is_ri_pair true has_indic_virama
          else if is_virama cp then
            loop next width has_width is_ri_pair has_vs16 true
          else if is_regional_indicator cp then
            if is_ri_pair then
              loop next (width + cp_width) true false has_vs16 false
            else
              let new_w = if not has_width then cp_width else width in
              loop next new_w true true has_vs16 false
          else if has_width && has_indic_virama && is_devanagari_base cp then
            let add =
              if (not (is_devanagari_ra cp)) && cp_width > 0 then cp_width
              else 0
            in
            loop next (width + add) true is_ri_pair has_vs16 false
          else if (not has_width) && cp_width > 0 then
            loop next cp_width true is_ri_pair has_vs16 false
          else loop next width has_width is_ri_pair has_vs16 false
      in
      loop off 0 false false false false

let is_ascii_printable str =
  let len = String.length str in
  let tab_width = default_tab_width in
  let rec loop i =
    if i >= len then true
    else
      let b = Char.code (String.unsafe_get str i) in
      if b < 128 && ascii_display_width ~tab_width b > 0 then loop (i + 1)
      else false
  in
  loop 0

let encode_ascii str f =
  let len = String.length str in
  let tab_width = default_tab_width in
  for i = 0 to len - 1 do
    let b = Char.code (String.unsafe_get str i) in
    if b < 128 then
      let w = ascii_display_width ~tab_width b in
      if w > 0 then f (Int32.of_int b)
  done

(* --- Pool Implementation --- *)

type pool = {
  mutable storage : bytes;
  mutable offsets : int array;
  mutable lengths : int array;
  mutable borrowed_strings : string array;
  mutable refcounts : int array;
  mutable generations : int array;
  mutable free_list : int list;
  mutable next_id : int;
  mutable storage_cursor : int;
}

let initial_cap_ids = 4096
let initial_cap_bytes = 4096 * 8

let create_pool () =
  {
    storage = Bytes.create initial_cap_bytes;
    offsets = Array.make initial_cap_ids 0;
    lengths = Array.make initial_cap_ids 0;
    borrowed_strings = Array.make initial_cap_ids empty_string;
    refcounts = Array.make initial_cap_ids 0;
    generations = Array.make initial_cap_ids 0;
    free_list = [];
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
    pool.borrowed_strings <- resize pool.borrowed_strings empty_string;
    pool.refcounts <- resize pool.refcounts 0;
    pool.generations <- resize pool.generations 0)

let ensure_storage_capacity pool needed =
  let cap = Bytes.length pool.storage in
  if pool.storage_cursor + needed > cap then (
    let new_cap = max (cap * 2) (pool.storage_cursor + needed) in
    let new_bytes = Bytes.create new_cap in
    Bytes.blit ~src:pool.storage ~src_pos:0 ~dst:new_bytes ~dst_pos:0
      ~len:pool.storage_cursor;
    pool.storage <- new_bytes)

(* Helper: Get next free ID and generation *)
let next_free_id pool =
  match pool.free_list with
  | hd :: tl ->
      pool.free_list <- tl;
      let g = (Array.unsafe_get pool.generations hd + 1) land mask_generation in
      Array.unsafe_set pool.generations hd g;
      (hd, g, true)
  | [] ->
      let id = pool.next_id in
      pool.next_id <- id + 1;
      Array.unsafe_set pool.generations id 0;
      (id, 0, false)

(* Allocates a string slice into storage *)
let alloc pool str off len =
  ensure_id_capacity pool;
  let id, gen, reused = next_free_id pool in
  let cap =
    if reused && Array.unsafe_get pool.offsets id >= 0 then
      Array.unsafe_get pool.lengths id
    else 0
  in
  let cursor, next_cursor =
    if cap >= len then (Array.unsafe_get pool.offsets id, pool.storage_cursor)
    else (
      ensure_storage_capacity pool len;
      let cur = pool.storage_cursor in
      (cur, cur + len))
  in

  Bytes.blit_string ~src:str ~src_pos:off ~dst:pool.storage ~dst_pos:cursor ~len;

  Array.unsafe_set pool.offsets id cursor;
  Array.unsafe_set pool.lengths id len;
  Array.unsafe_set pool.borrowed_strings id empty_string;
  Array.unsafe_set pool.refcounts id 0;
  pool.storage_cursor <- next_cursor;
  (id, gen)

(* Allocates a string slice without copying. Caller must keep [str] alive. *)
let alloc_unowned pool str off len =
  ensure_id_capacity pool;
  let id, gen, _reused = next_free_id pool in
  Array.unsafe_set pool.offsets id (-off - 1);
  Array.unsafe_set pool.lengths id len;
  Array.unsafe_set pool.borrowed_strings id str;
  Array.unsafe_set pool.refcounts id 0;
  (id, gen)

(* Allocates a single unicode codepoint directly into storage (Zero Allocation) *)
let alloc_codepoint pool u len =
  ensure_id_capacity pool;
  let id, gen, reused = next_free_id pool in
  let cap =
    if reused && Array.unsafe_get pool.offsets id >= 0 then
      Array.unsafe_get pool.lengths id
    else 0
  in
  let cursor, next_cursor =
    if cap >= len then (Array.unsafe_get pool.offsets id, pool.storage_cursor)
    else (
      ensure_storage_capacity pool len;
      let cur = pool.storage_cursor in
      (cur, cur + len))
  in
  let dst = pool.storage in

  (* Direct UTF-8 Encode into storage *)
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
    (* len = 4 *)
    Bytes.unsafe_set dst cursor (Char.chr (0xf0 lor (u lsr 18)));
    Bytes.unsafe_set dst (cursor + 1)
      (Char.chr (0x80 lor ((u lsr 12) land 0x3f)));
    Bytes.unsafe_set dst (cursor + 2)
      (Char.chr (0x80 lor ((u lsr 6) land 0x3f)));
    Bytes.unsafe_set dst (cursor + 3) (Char.chr (0x80 lor (u land 0x3f))));

  Array.unsafe_set pool.offsets id cursor;
  Array.unsafe_set pool.lengths id len;
  Array.unsafe_set pool.borrowed_strings id empty_string;
  Array.unsafe_set pool.refcounts id 0;
  pool.storage_cursor <- next_cursor;
  (id, gen)

(* --- Packing --- *)

let[@inline] pack_start idx gen width =
  let w = if width < 1 then 1 else width in
  let right = if w > 4 then 3 else w - 1 in
  Int32.(
    logor flag_grapheme
      (logor
         (shift_left (of_int right) shift_right_extent)
         (logor
            (shift_left (of_int gen) shift_generation)
            (of_int (idx land mask_index)))))

let[@inline] pack_continuation ~idx ~gen ~left ~right =
  let l = clamp_extent left in
  let r = clamp_extent right in
  Int32.(
    logor flag_grapheme
      (logor flag_continuation
         (logor
            (shift_left (of_int l) shift_left_extent)
            (logor
               (shift_left (of_int r) shift_right_extent)
               (logor
                  (shift_left (of_int gen) shift_generation)
                  (of_int (idx land mask_index)))))))

(* --- Accessors --- *)

let[@inline] is_simple c = Int32.logand c flag_grapheme = 0l
let[@inline] is_start c = is_simple c || Int32.logand c flag_continuation = 0l

let[@inline] is_continuation c =
  (not (is_simple c)) && Int32.logand c flag_continuation <> 0l

let[@inline] is_empty c = c = 0l

let[@inline] right_extent c =
  Int32.(to_int (logand (shift_right_logical c shift_right_extent) 3l))

let[@inline] left_extent c =
  Int32.(to_int (logand (shift_right_logical c shift_left_extent) 3l))

let[@inline] unpack_complex c =
  if is_simple c then (0, 0)
  else
    let idx = Int32.(to_int (logand c (of_int mask_index))) in
    let gen =
      Int32.(
        to_int
          (logand
             (shift_right_logical c shift_generation)
             (of_int mask_generation)))
    in
    (idx, gen)

let[@inline] id c =
  if is_simple c then 0 else Int32.(to_int (logand c (of_int mask_index)))

let[@inline] width ?(tab_width = default_tab_width) c =
  let tab_width = normalize_tab_width tab_width in
  if is_empty c then 0
  else if is_simple c then
    let b = Int32.to_int c land 0xFF in
    ascii_display_width ~tab_width b
  else
    let l = left_extent c in
    let r = right_extent c in
    if is_continuation c then l + 1 + r else if l <> 0 then 0 else r + 1

(* --- Refcounting --- *)

let incref pool c =
  if not (is_simple c) then
    let idx, gen = unpack_complex c in
    if
      idx > 0 && idx < pool.next_id
      && Array.unsafe_get pool.generations idx = gen
    then
      Array.unsafe_set pool.refcounts idx
        (Array.unsafe_get pool.refcounts idx + 1)

let decref pool c =
  if not (is_simple c) then
    let idx, gen = unpack_complex c in
    if
      idx > 0 && idx < pool.next_id
      && Array.unsafe_get pool.generations idx = gen
    then
      let rc = Array.unsafe_get pool.refcounts idx in
      if rc > 0 then (
        let rc' = rc - 1 in
        Array.unsafe_set pool.refcounts idx rc';
        if rc' = 0 then (
          Array.unsafe_set pool.borrowed_strings idx empty_string;
          Array.unsafe_set pool.refcounts idx (-1);
          pool.free_list <- idx :: pool.free_list))
      else if rc = 0 then (
        (* Allow single decref of zero-ref glyphs (common in tests or when the
           caller relies on external lifetime). Mark as -1 to avoid duplicates. *)
        Array.unsafe_set pool.refcounts idx (-1);
        Array.unsafe_set pool.borrowed_strings idx empty_string;
        pool.free_list <- idx :: pool.free_list)

let clear pool =
  pool.next_id <- 1;
  pool.storage_cursor <- 0;
  pool.free_list <- [];
  Array.fill pool.offsets ~pos:0 ~len:(Array.length pool.offsets) 0;
  Array.fill pool.lengths ~pos:0 ~len:(Array.length pool.lengths) 0;
  Array.fill pool.borrowed_strings ~pos:0
    ~len:(Array.length pool.borrowed_strings)
    empty_string;
  Array.fill pool.refcounts ~pos:0 ~len:(Array.length pool.refcounts) 0;
  Array.fill pool.generations ~pos:0 ~len:(Array.length pool.generations) 0

(* --- Data Retrieval --- *)

let blit pool c buf off =
  if is_simple c then
    if Bytes.length buf > off then (
      Bytes.unsafe_set buf off (Char.chr (Int32.to_int c land 0xFF));
      1)
    else 0
  else
    let idx, gen = unpack_complex c in
    if
      idx <= 0 || idx >= pool.next_id
      || Array.unsafe_get pool.generations idx <> gen
    then 0
    else
      let len = Array.unsafe_get pool.lengths idx in
      if len > Bytes.length buf - off then 0
      else
        let off_val = Array.unsafe_get pool.offsets idx in
        if off_val >= 0 then (
          Bytes.blit ~src:pool.storage ~src_pos:off_val ~dst:buf ~dst_pos:off
            ~len;
          len)
        else
          let src = Array.unsafe_get pool.borrowed_strings idx in
          let src_off = -off_val - 1 in
          if src_off + len > String.length src then 0
          else (
            Bytes.blit_string ~src ~src_pos:src_off ~dst:buf ~dst_pos:off ~len;
            len)

let copy src_pool c dst_pool =
  if is_simple c then c
  else
    let idx, gen = unpack_complex c in
    if
      idx <= 0 || idx >= src_pool.next_id
      || Array.unsafe_get src_pool.generations idx <> gen
    then 0l
    else
      let len = Array.unsafe_get src_pool.lengths idx in
      let off_val = Array.unsafe_get src_pool.offsets idx in
      let src_is_owned = off_val >= 0 in
      let src_off = if src_is_owned then off_val else -off_val - 1 in
      let src_string =
        if src_is_owned then None
        else Some (Array.unsafe_get src_pool.borrowed_strings idx)
      in
      let src_valid =
        match src_string with
        | None -> src_off + len <= Bytes.length src_pool.storage
        | Some s -> src_off + len <= String.length s
      in

      if not src_valid then 0l
      else (
        ensure_id_capacity dst_pool;
        let dst_id, dst_gen, reused = next_free_id dst_pool in
        let cap =
          if reused && Array.unsafe_get dst_pool.offsets dst_id >= 0 then
            Array.unsafe_get dst_pool.lengths dst_id
          else 0
        in
        let cursor, next_cursor =
          if cap >= len then
            (Array.unsafe_get dst_pool.offsets dst_id, dst_pool.storage_cursor)
          else (
            ensure_storage_capacity dst_pool len;
            let cur = dst_pool.storage_cursor in
            (cur, cur + len))
        in

        (match src_string with
        | None ->
            Bytes.blit ~src:src_pool.storage ~src_pos:src_off
              ~dst:dst_pool.storage ~dst_pos:cursor ~len
        | Some src ->
            Bytes.blit_string ~src ~src_pos:src_off ~dst:dst_pool.storage
              ~dst_pos:cursor ~len);

        Array.unsafe_set dst_pool.offsets dst_id cursor;
        Array.unsafe_set dst_pool.lengths dst_id len;
        Array.unsafe_set dst_pool.borrowed_strings dst_id empty_string;
        Array.unsafe_set dst_pool.refcounts dst_id 0;
        dst_pool.storage_cursor <- next_cursor;

        if is_continuation c then
          pack_continuation ~idx:dst_id ~gen:dst_gen ~left:(left_extent c)
            ~right:(right_extent c)
        else
          let w = width c in
          pack_start dst_id dst_gen w)

let to_string pool c =
  if is_simple c then String.make 1 (Char.chr (Int32.to_int c land 0xFF))
  else
    let idx, gen = unpack_complex c in
    if
      idx <= 0 || idx >= pool.next_id
      || Array.unsafe_get pool.generations idx <> gen
    then ""
    else
      let len = Array.unsafe_get pool.lengths idx in
      let off_val = Array.unsafe_get pool.offsets idx in
      if off_val >= 0 then Bytes.sub_string pool.storage ~pos:off_val ~len
      else
        let src = Array.unsafe_get pool.borrowed_strings idx in
        let src_off = -off_val - 1 in
        if src_off + len > String.length src then ""
        else String.sub src ~pos:src_off ~len

let length pool c =
  if is_simple c then 1
  else
    let idx, gen = unpack_complex c in
    if
      idx <= 0 || idx >= pool.next_id
      || Array.unsafe_get pool.generations idx <> gen
    then 0
    else
      let len = Array.unsafe_get pool.lengths idx in
      let off_val = Array.unsafe_get pool.offsets idx in
      if off_val >= 0 then len
      else
        let src = Array.unsafe_get pool.borrowed_strings idx in
        let src_off = -off_val - 1 in
        if src_off + len > String.length src then 0 else len

(* --- Interning & Encoding --- *)

let intern pool ?(width_method = `Unicode) ?(tab_width = default_tab_width)
    ?(borrow = false) ?width ?(off = 0) ?len str =
  let tab_width = normalize_tab_width tab_width in
  let len = match len with Some l -> l | None -> String.length str - off in
  let alloc_slice = if borrow then alloc_unowned else alloc in

  (* Case 0: Empty *)
  if len = 0 then 0l
    (* Case 1: Single Byte Optimization (Always check this first) *)
  else if len = 1 then
    let b = Char.code (String.unsafe_get str off) in
    let w =
      match width with
      | Some w -> w
      | None ->
          if b < 128 then ascii_display_width ~tab_width b
          else char_width ~method_:width_method ~tab_width b
    in
    if w <= 0 then 0l else Int32.of_int b
  (* Case 2: Multi-byte *)
    else
    match width with
    | Some w ->
        if w <= 0 then 0l
        else
          (* OPTIMIZATION: Caller knows width. Skip scanning. 
             This makes intern_hotset just a memory copy benchmark. *)
          let idx, gen = alloc_slice pool str off len in
          pack_start idx gen w
    | None ->
        (* We must calculate width. Try to stay in ASCII land if possible. *)
        let limit = off + len in

        (* Use a while loop to avoid closure/recursion overhead in the hot path *)
        let ascii_width = ref 0 in
        let is_ascii = ref true in
        let i = ref off in

        while !i < limit && !is_ascii do
          let b = Char.code (String.unsafe_get str !i) in
          if b >= 128 then is_ascii := false
          else
            let w = ascii_display_width ~tab_width b in
            ascii_width := !ascii_width + w;
            incr i
        done;

        if !is_ascii then
          (* Fast Path: Pure ASCII string found *)
          let w = !ascii_width in
          if w <= 0 then 0l
          else
            let idx, gen = alloc_slice pool str off len in
            pack_start idx gen w
        else
          (* Slow Path: Complex Unicode *)
          let w =
            calculate_width ~method_:width_method ~tab_width str off len
          in
          if w <= 0 then 0l
          else
            let idx, gen = alloc_slice pool str off len in
            pack_start idx gen w

let intern_char pool u =
  let tab_width = default_tab_width in
  if u < 128 then
    let w = ascii_display_width ~tab_width u in
    if w <= 0 then 0l else Int32.of_int u
  else
    let len = if u < 0x800 then 2 else if u < 0x10000 then 3 else 4 in
    (* Only width check needed. Simple unicode chars are usually 1 or 2. *)
    let w = char_width ~method_:`Unicode ~tab_width u in
    if w <= 0 then 0l
    else
      let idx, gen = alloc_codepoint pool u len in
      pack_start idx gen w

let encode pool ?(width_method = `Unicode) ?(tab_width = default_tab_width)
    ?(borrow = false) str f =
  let tab_width = normalize_tab_width tab_width in
  let len = String.length str in
  let i = ref 0 in
  let alloc_slice = if borrow then alloc_unowned else alloc in

  while !i < len do
    (* Unrolled ASCII Path *)
    let processed_fast = ref false in
    (if !i + 4 <= len then
       let c0 = String.unsafe_get str !i in
       let c1 = String.unsafe_get str (!i + 1) in
       let c2 = String.unsafe_get str (!i + 2) in
       let c3 = String.unsafe_get str (!i + 3) in
       if Char.code c0 lor Char.code c1 lor Char.code c2 lor Char.code c3 < 128
       then (
         let w0 = ascii_display_width ~tab_width (Char.code c0) in
         let w1 = ascii_display_width ~tab_width (Char.code c1) in
         let w2 = ascii_display_width ~tab_width (Char.code c2) in
         let w3 = ascii_display_width ~tab_width (Char.code c3) in
         if w0 > 0 then f (Int32.of_int (Char.code c0));
         if w1 > 0 then f (Int32.of_int (Char.code c1));
         if w2 > 0 then f (Int32.of_int (Char.code c2));
         if w3 > 0 then f (Int32.of_int (Char.code c3));
         i := !i + 4;
         processed_fast := true));

    if not !processed_fast then
      let c = String.unsafe_get str !i in
      if Char.code c < 128 then (
        let w = ascii_display_width ~tab_width (Char.code c) in
        if w > 0 then f (Int32.of_int (Char.code c));
        incr i)
      else
        let end_pos =
          Grapheme_cluster.next_boundary ~ignore_zwj:(width_method = `No_zwj)
            str !i len
        in
        let clus_len = end_pos - !i in

        let w =
          calculate_width ~method_:width_method ~tab_width str !i clus_len
        in
        if w > 0 then (
          let idx, gen = alloc_slice pool str !i clus_len in

          let start_code = pack_start idx gen w in
          f start_code;

          (if w > 1 then
             let cw = min 4 w in
             for k = 1 to cw - 1 do
               f (pack_continuation ~idx ~gen ~left:k ~right:(cw - 1 - k))
             done);
          i := !i + clus_len)
        else i := !i + clus_len
  done

let iter_graphemes f str =
  let len = String.length str in
  let i = ref 0 in
  while !i < len do
    let start = !i in
    let c = String.unsafe_get str start in
    let code = Char.code c in

    if code < 0x80 then
      (* ASCII Fast Path, with fallback to full segmentation when followed by
         non-ASCII (e.g., combining marks). *)
      if
        (* CR(0x0D) + LF(0x0A) is the only ASCII multi-char grapheme *)
        code = 0x0D
        && start + 1 < len
        && String.unsafe_get str (start + 1) = '\n'
      then (
        f start 2;
        i := start + 2)
      else if
        start + 1 < len && Char.code (String.unsafe_get str (start + 1)) >= 0x80
      then (
        let end_pos = Grapheme_cluster.next_boundary str start len in
        let l = end_pos - start in
        (* Ensure forward progress for safety *)
        let l = if l <= 0 then 1 else l in
        f start l;
        i := start + l)
      else (
        f start 1;
        i := start + 1)
    else
      (* Complex Path *)
      let end_pos = Grapheme_cluster.next_boundary str start len in
      let l = end_pos - start in
      (* Ensure forward progress for safety *)
      let l = if l <= 0 then 1 else l in
      f start l;
      i := start + l
  done

let measure ?(width_method = `Unicode) ?(tab_width = default_tab_width) str =
  let tab_width = normalize_tab_width tab_width in
  let len = String.length str in
  if len = 0 then 0
  else
    (* Phase 1: ASCII Fast Path Check *)
    let rec check_ascii i =
      if i >= len then true
      else if Char.code (String.unsafe_get str i) >= 128 then false
      else check_ascii (i + 1)
    in

    if check_ascii 0 then
      (* --- FAST PATH: Pure ASCII --- *)
      let rec loop i total =
        if i >= len then total
        else
          let b = Char.code (String.unsafe_get str i) in
          let w = ascii_display_width ~tab_width b in
          (* For string measurement, controls (-1) count as 0 *)
          let add = if w > 0 then w else 0 in
          loop (i + 1) (total + add)
      in
      loop 0 0
    else
      (* --- SLOW PATH: Complex / Unicode --- *)
      match width_method with
      | `Wcwidth ->
          (* Per-codepoint loop (No grapheme segmentation) *)
          let rec loop i total =
            if i >= len then total
            else
              let b0 = Char.code (String.unsafe_get str i) in
              let cp, next =
                if b0 < 0x80 then (b0, i + 1)
                else
                  (* Inline decode logic or use helper *)
                  let dec = String.get_utf_8_uchar str i in
                  ( Uchar.to_int (Uchar.utf_decode_uchar dec),
                    i + Uchar.utf_decode_length dec )
              in
              let w = char_width ~method_:`Wcwidth ~tab_width cp in
              loop next (total + w)
          in
          loop 0 0
      | `Unicode | `No_zwj ->
          (* Full UAX #29 Segmentation *)
          let rec loop i total =
            if i >= len then total
            else
              let end_pos =
                Grapheme_cluster.next_boundary
                  ~ignore_zwj:(width_method = `No_zwj) str i len
              in
              let w =
                calculate_width ~method_:width_method ~tab_width str i
                  (end_pos - i)
              in
              let add = if w > 0 then w else 0 in
              loop end_pos (total + add)
          in
          loop 0 0

(* --- Wrap-break predicates --- *)

let[@inline] is_ascii_wrap_break (b : int) =
  match b with
  | 0x20 (* ' ' *) | 0x09 (* '\t' *) -> true
  | 0x2D (* '-' *) -> true
  | 0x2F (* '/' *) | 0x5C (* '\\' *) -> true
  | 0x2E (* '.' *)
  | 0x2C (* ',' *)
  | 0x3B (* ';' *)
  | 0x3A (* ':' *)
  | 0x21 (* '!' *)
  | 0x3F (* '?' *) ->
      true
  | 0x28 (* '(' *)
  | 0x29 (* ')' *)
  | 0x5B (* '[' *)
  | 0x5D (* ']' *)
  | 0x7B (* '{' *)
  | 0x7D (* '}' *) ->
      true
  | _ -> false

let[@inline] is_unicode_wrap_break (cp : int) =
  match cp with
  | 0x00A0 (* NBSP *)
  | 0x1680 (* OGHAM SPACE MARK *)
  | 0x202F (* NARROW NO-BREAK SPACE *)
  | 0x205F (* MEDIUM MATHEMATICAL SPACE *)
  | 0x3000 (* IDEOGRAPHIC SPACE *)
  | 0x200B (* ZERO WIDTH SPACE *)
  | 0x00AD (* SOFT HYPHEN *)
  | 0x2010 (* HYPHEN *) ->
      true
  | cp when cp >= 0x2000 && cp <= 0x200A -> true
  | _ -> false

type wrap_break = { byte_offset : int; grapheme_offset : int }

let iter_wrap_breaks ?(width_method = `Unicode) (f : wrap_break -> unit)
    (s : string) =
  let len = String.length s in
  let ignore_zwj = width_method = `No_zwj in
  let rec loop byte_off g_off =
    if byte_off >= len then ()
    else
      let next = Grapheme_cluster.next_boundary ~ignore_zwj s byte_off len in
      (* Scan codepoints inside this grapheme for break candidates. *)
      let found_break =
        let i = ref byte_off in
        let found = ref false in
        while !i < next do
          let b0 = Char.code (String.unsafe_get s !i) in
          if b0 < 0x80 then (
            if is_ascii_wrap_break b0 then found := true;
            incr i)
          else
            let cp, clen = decode_utf8_bounded s !i next in
            if is_unicode_wrap_break cp then found := true;
            i := !i + clen
        done;
        !found
      in
      if found_break then
        (* The break is "after" this grapheme: byte offset is [next],
           grapheme offset is the index of this grapheme. *)
        f { byte_offset = next; grapheme_offset = g_off };
      loop next (g_off + 1)
  in
  loop 0 0

let wrap_breaks ?width_method s =
  let acc = ref [] in
  iter_wrap_breaks ?width_method (fun br -> acc := br :: !acc) s;
  Array.of_list (List.rev !acc)

type line_break_kind = [ `LF | `CR | `CRLF ]
type line_break = { pos : int; kind : line_break_kind }

let iter_line_breaks (f : line_break -> unit) (s : string) =
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    let b = Char.code (String.unsafe_get s !i) in
    if b = 0x0A (* '\n' *) then (
      (* Check if preceded by CR for CRLF *)
      let kind =
        if !i > 0 && Char.code (String.unsafe_get s (!i - 1)) = 0x0D then `CRLF
        else `LF
      in
      (* For CRLF, we already reported at CR position, skip this LF *)
      if kind = `LF then f { pos = !i; kind };
      incr i)
    else if b = 0x0D (* '\r' *) then
      if !i + 1 < len && Char.code (String.unsafe_get s (!i + 1)) = 0x0A then (
        (* CRLF: report at LF position *)
        f { pos = !i + 1; kind = `CRLF };
        i := !i + 2)
      else (
        f { pos = !i; kind = `CR };
        incr i)
    else incr i
  done
