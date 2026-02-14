(* Unicode property lookups for grapheme segmentation and width calculation.

   All properties are packed into a flat lookup table, lazily initialized on
   first access for O(1) lookup. Uses 2 bytes per codepoint (~2.2MB total).

   Layout per 16-bit entry: - bits 0-4: gcb (grapheme_cluster_break, values
   0-17) - bits 5-6: incb (indic_conjunct_break, values 0-3) - bit 7: extpic
   (extended_pictographic, boolean) - bits 8-9: width (encoded: 0=-1, 1=0, 2=1,
   3=2) *)

(* Surrogate packing: skip 0xD800-0xDFFF range *)
let[@inline] pack_u u = if u > 0xd7ff then u - 0x800 else u

(* Trie lookup for initialization only *)
let trie_get trie default u =
  let u = pack_u u in
  let b0 = u lsr 12 in
  if b0 >= Array.length trie then default
  else
    let l1 = Array.unsafe_get trie b0 in
    if Array.length l1 = 0 then default
    else
      let b1 = (u lsr 6) land 0x3f in
      let l2 = Array.unsafe_get l1 b1 in
      if String.length l2 = 0 then default
      else Char.code (String.unsafe_get l2 (u land 0x3f))

let trie_get_bool trie u =
  let u = pack_u u in
  let b0 = u lsr 12 in
  if b0 >= Array.length trie then false
  else
    let l1 = Array.unsafe_get trie b0 in
    if Array.length l1 = 0 then false
    else
      let b1 = (u lsr 6) land 0x3f in
      let l2 = Array.unsafe_get l1 b1 in
      if String.length l2 = 0 then false
      else
        let b2 = u land 0x3f in
        Char.code (String.unsafe_get l2 (b2 lsr 3)) land (1 lsl (b2 land 7))
        <> 0

(* Binary search for width initialization *)
let interval_search starts ends values u =
  let n = Array.length starts in
  let rec loop lo hi =
    if lo > hi then min_int
    else
      let mid = (lo + hi) / 2 in
      let s = Array.unsafe_get starts mid in
      let e = Array.unsafe_get ends mid in
      if u < s then loop lo (mid - 1)
      else if u > e then loop (mid + 1) hi
      else Array.unsafe_get values mid
  in
  if n = 0 then min_int else loop 0 (n - 1)

(* Flat property table: 16 bits per codepoint *)
let unicode_packed_size = 0x110000 - 0x800

(* Lazy initialization: table is built on first access, not at module load. This
   defers the ~2.2MB initialization cost until actually needed. *)
let prop_table =
  lazy
    (let table =
       Bigarray.Array1.create Bigarray.int16_unsigned Bigarray.c_layout
         unicode_packed_size
     in
     let starts, ends, values = Unicode_data.tty_width_hint in
     for packed = 0 to unicode_packed_size - 1 do
       let cp = if packed < 0xD800 then packed else packed + 0x800 in
       (* GCB *)
       let gcb =
         if cp = 0x0D then 1
         else if cp = 0x0A then 8
         else trie_get Unicode_data.grapheme_cluster_break 16 cp
       in
       (* InCB *)
       let incb =
         if cp < 0x900 then 3
         else trie_get Unicode_data.indic_conjunct_break 3 cp
       in
       (* ExtPic *)
       let extpic =
         if cp < 0x00A9 then false
         else trie_get_bool Unicode_data.extended_pictographic cp
       in
       (* Width: encode as 0=-1, 1=0, 2=1, 3=2 *)
       let width_raw =
         if cp <= 0x001F || (0x007F <= cp && cp <= 0x009F) then -1
         else if cp <= 0x02FF then 1
         else
           let w = interval_search starts ends values cp in
           if w = min_int then 1 else w
       in
       let width_enc = width_raw + 1 in
       (* Pack: [width:2][extpic:1][incb:2][gcb:5] *)
       let v =
         gcb land 0x1F
         lor ((incb land 0x03) lsl 5)
         lor (if extpic then 0x80 else 0)
         lor ((width_enc land 0x03) lsl 8)
       in
       Bigarray.Array1.unsafe_set table packed v
     done;
     table)

(* Force once at module init to avoid Lazy.force overhead on each lookup. *)
let prop_table = Lazy.force prop_table

(* Public API - O(1) lookup *)

let[@inline] grapheme_cluster_break u =
  Bigarray.Array1.unsafe_get prop_table (pack_u (Uchar.to_int u)) land 0x1F

let[@inline] indic_conjunct_break u =
  (Bigarray.Array1.unsafe_get prop_table (pack_u (Uchar.to_int u)) lsr 5)
  land 0x03

let[@inline] is_extended_pictographic u =
  Bigarray.Array1.unsafe_get prop_table (pack_u (Uchar.to_int u)) land 0x80 <> 0

let[@inline] tty_width_hint u =
  (Bigarray.Array1.unsafe_get prop_table (pack_u (Uchar.to_int u)) lsr 8)
  land 0x03
  - 1

(* Combined lookup - returns packed (gcb, incb, extpic) in one Bigarray access.
   Returns: bits 0-4 = gcb, bits 5-6 = incb, bit 7 = extpic *)
let[@inline] grapheme_props u =
  Bigarray.Array1.unsafe_get prop_table (pack_u (Uchar.to_int u)) land 0xFF

(* Full packed lookup - returns all properties including width in one access.
   Returns: bits 0-4 = gcb, bits 5-6 = incb, bit 7 = extpic, bits 8-9 =
   width_enc (0=-1, 1=0, 2=1, 3=2) *)
let[@inline] all_props u =
  Bigarray.Array1.unsafe_get prop_table (pack_u (Uchar.to_int u))
