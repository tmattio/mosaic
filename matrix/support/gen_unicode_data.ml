(* Generate pre-computed Unicode property table for matrix.text.

   Packs all Unicode properties needed for grapheme segmentation and width
   calculation into a two-level page table with block deduplication. Each
   codepoint maps to a 16-bit entry, giving O(1) lookups at runtime with zero
   initialization cost.

   Structure: - prop_index: maps block numbers to deduplicated block IDs (1 byte
   each) - prop_data: concatenated deduplicated blocks (512 bytes each)

   The codepoint space is divided into 256-entry blocks. Blocks with identical
   content share the same data, dramatically reducing size for the large
   unassigned/CJK/etc. ranges that share properties.

   Layout per 16-bit entry: - bits 0-4: grapheme_cluster_break (0-17) - bits
   5-6: indic_conjunct_break (0-3) - bit 7: extended_pictographic (boolean) -
   bits 8-9: width (encoded: 0=-1, 1=0, 2=1, 3=2) - bit 10: zero width inside a grapheme
   - bit 11: emoji (boolean) - bit 12: emoji modifier base (boolean)

   Surrogates (U+D800-U+DFFF) are excluded from the table.

   Adapted from notty's generator (Copyright (c) 2020 David Kaloper Meršinjak).

   Usage: dune exec matrix/support/gen_unicode_data.exe *)

let unicode_packed_size = 0x110000 - 0x800
let block_size = 256
let block_bytes = block_size * 2

(* Matches uucode's wcwidth_zero_in_grapheme property used by OpenTUI. *)
let zero_in_grapheme u gcb =
  let cp = Uchar.to_int u in
  (match Uucp.Gc.general_category u with
    | `Cc | `Cs | `Zl | `Zp | `Mn | `Me -> true
    | _ -> false)
  || (cp <> 0x00AD && Uucp.Gen.is_default_ignorable u)
  || Uucp.Emoji.is_emoji_modifier u
  || gcb = 15 || gcb = 14 || gcb = 11

(* Uucp's tty_width_hint, except that the Hangul conjoining vowels and
   trailing consonants are zero width: a leading consonant before them composes
   them into one syllable (Unicode §3.12), whose width is the leading
   consonant's. Markus Kuhn's wcwidth, glibc and xterm give them 0; Uucp gives
   them their East_Asian_Width, N. *)
let width u =
  let cp = Uchar.to_int u in
  if (cp >= 0x1160 && cp <= 0x11FF) || (cp >= 0xD7B0 && cp <= 0xD7FF) then 0
  else Uucp.Break.tty_width_hint u

let compute_prop_table () =
  let buf = Bytes.create (unicode_packed_size * 2) in
  for packed = 0 to unicode_packed_size - 1 do
    let u = Uchar.of_int (if packed < 0xD800 then packed else packed + 0x800) in
    let gcb = Uucp.Break.Low.grapheme_cluster u in
    let incb = Uucp.Break.Low.indic_conjunct_break u in
    let extpic = Uucp.Emoji.is_extended_pictographic u in
    let emoji = Uucp.Emoji.is_emoji u in
    let modifier_base = Uucp.Emoji.is_emoji_modifier_base u in
    (* Two cells exactly for East_Asian_Width W and F (UAX #11), which
       include every Emoji_Presentation character but the regional
       indicators. An emoji without Emoji_Presentation, such as U+2764, and a
       pictograph that is not an emoji, such as U+1F000 MAHJONG TILE EAST WIND,
       are text
       by default (UTS #51 ED-7) and take one cell; VS16 widens an emoji at
       run time. *)
    let width_enc = width u + 1 in
    let v =
      gcb land 0x1F
      lor ((incb land 0x03) lsl 5)
      lor (if extpic then 0x80 else 0)
      lor ((width_enc land 0x03) lsl 8)
      lor (if zero_in_grapheme u gcb then 0x400 else 0)
      lor (if emoji then 0x800 else 0)
      lor if modifier_base then 0x1000 else 0
    in
    Bytes.set_uint16_le buf (packed * 2) v
  done;
  Bytes.unsafe_to_string buf

let compress_prop_table flat =
  let num_blocks = unicode_packed_size / block_size in
  (* Extract and deduplicate blocks *)
  let block_map = Hashtbl.create 128 in
  let unique_blocks = Buffer.create (64 * block_bytes) in
  let next_id = ref 0 in
  let index = Bytes.create num_blocks in
  for i = 0 to num_blocks - 1 do
    let block = String.sub flat (i * block_bytes) block_bytes in
    let id =
      match Hashtbl.find_opt block_map block with
      | Some id -> id
      | None ->
          let id = !next_id in
          Hashtbl.add block_map block id;
          Buffer.add_string unique_blocks block;
          incr next_id;
          id
    in
    Bytes.set index i (Char.chr id)
  done;
  let num_unique = !next_id in
  assert (num_unique <= 256);
  (Bytes.unsafe_to_string index, Buffer.contents unique_blocks, num_unique)

let header =
  Printf.sprintf
    "(* WARNING: Do not edit. This file was automatically generated.\n\n\
    \   Unicode version %s.\n\
    \   Generated using matrix/support/gen_unicode_data.ml\n\
     *)\n\n\
     [@@@ocamlformat \"disable\"]\n\n"
    Uucp.unicode_version

let write_string_literal oc name data =
  Printf.fprintf oc "let %s = \"\\\n  " name;
  let bytes_per_line = 40 in
  for i = 0 to String.length data - 1 do
    Printf.fprintf oc "\\x%02x" (Char.code (String.get data i));
    if (i + 1) mod bytes_per_line = 0 && i + 1 < String.length data then
      output_string oc "\\\n  "
  done;
  output_string oc "\"\n\n"

let write_mli oc =
  output_string oc header;
  output_string oc
    "(** Block index: maps block numbers (codepoint lsr 8) to deduplicated\n\
    \    block IDs. Each entry is 1 byte. *)\n\
     val prop_index : string\n\n\
     (** Deduplicated block data: concatenated 512-byte blocks, each containing\n\
    \    256 packed 16-bit entries. Lookup: prop_data.[block_id * 512 + (cp \
     land 0xFF) * 2]. *)\n\
     val prop_data : string\n"

let write_ml oc =
  let flat = compute_prop_table () in
  let index, data, num_unique = compress_prop_table flat in
  output_string oc header;
  Printf.fprintf oc
    "(* %d unique blocks out of %d total (%.1f%% dedup, %d bytes data) *)\n\n"
    num_unique
    (unicode_packed_size / block_size)
    (100.
    *. (1.
       -. float_of_int num_unique
          /. float_of_int (unicode_packed_size / block_size)))
    (String.length data);
  write_string_literal oc "prop_index" index;
  write_string_literal oc "prop_data" data

let file = "matrix/lib/text/unicode_data"

let () =
  Format.printf "Dumping Unicode v%s data to %s.@." Uucp.unicode_version file;
  let write name f =
    let oc = open_out_gen [ Open_trunc; Open_creat; Open_wronly ] 0o664 name in
    f oc;
    close_out oc
  in
  write (file ^ ".mli") write_mli;
  write (file ^ ".ml") write_ml
