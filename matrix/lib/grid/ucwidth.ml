(** Unicode character width calculation implementation *)

(** Pre-computed ASCII width table (0-127) for fast lookup *)
let ascii_width_table =
  [|
    (* 0x00-0x1F: Control characters *)
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    (* 0x20-0x3F: Space and printable *)
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    (* 0x40-0x5F: More printable *)
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    (* 0x60-0x7E: More printable *)
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    (* 0x7F: DEL *)
    -1;
  |]

(** Calculate width of a single Unicode code point.

    This uses Uucp's tty_width_hint which implements a predictable width
    algorithm based on Markus Kuhn's portable wcwidth. Returns -1 for control
    characters (C0/C1). *)
let char_width ?(east_asian = false) uchar =
  let code_point = Uchar.to_int uchar in

  (* Fast path for ASCII with direct table lookup *)
  if code_point < 128 then ascii_width_table.(code_point)
  else if code_point >= 0x80 && code_point < 0xA0 then -1
    (* C1 control characters *)
  else
    match Uucp.Break.tty_width_hint uchar with
    | -1 -> 1 (* Shouldn't happen after our control check *)
    | w when w >= 0 && w <= 2 ->
        (* For ambiguous width characters, check East Asian context *)
        if w = 1 && east_asian then
          match Uucp.Break.east_asian_width uchar with
          | `A -> 2 (* Ambiguous characters are width 2 in CJK context *)
          | _ -> w
        else w
    | _ -> 1 (* Fallback *)

(** Check if a code point is a variation selector *)
let is_variation_selector cp =
  (cp >= 0xFE00 && cp <= 0xFE0F)
  ||
  (* Variation Selectors *)
  (cp >= 0xE0100 && cp <= 0xE01EF)
(* Variation Selectors Supplement *)

(** Check if character is part of a keycap sequence base *)
let is_keycap_base uchar =
  let cp = Uchar.to_int uchar in
  cp = 0x23 || cp = 0x2A || (cp >= 0x30 && cp <= 0x39)
(* #, *, 0-9 *)

(** Check if a character has emoji presentation by default *)
let is_emoji_presentation uchar =
  try Uucp.Emoji.is_emoji_presentation uchar
  with _ -> false (* Fallback if function doesn't exist *)

(** Calculate width of a grapheme cluster from a substring *)
let cluster_width_sub ?(east_asian = false) s start len =
  let rec loop dec acc has_vs15 has_vs16 has_zwj ri_count last_non_vs_was_keycap
      has_emoji has_control =
    match Uutf.decode dec with
    | `Uchar u ->
        let cp = Uchar.to_int u in
        (* Check if character is a regional indicator (U+1F1E6 to U+1F1FF) *)
        let is_ri = cp >= 0x1F1E6 && cp <= 0x1F1FF in
        let is_vs = is_variation_selector cp in
        let is_vs15 = cp = 0xFE0E in
        let is_vs16 = cp = 0xFE0F in
        let is_zwj = cp = 0x200D in
        let is_keycap_mark = cp = 0x20E3 in
        let is_emoji = has_emoji || is_emoji_presentation u in

        (* Update tracking for keycap - VS doesn't reset it *)
        let new_last_non_vs_was_keycap =
          if is_vs then last_non_vs_was_keycap else is_keycap_base u
        in

        (* Handle keycap sequences *)
        if is_keycap_mark && last_non_vs_was_keycap then
          loop dec 2 has_vs15 has_vs16 has_zwj ri_count
            new_last_non_vs_was_keycap is_emoji has_control
        else
          let new_has_vs15 = has_vs15 || is_vs15 in
          let new_has_vs16 = has_vs16 || is_vs16 in
          let new_has_zwj = has_zwj || is_zwj in
          let new_ri_count = if is_ri then ri_count + 1 else ri_count in

          let w = char_width ~east_asian u in
          let new_has_control = has_control || w = -1 in
          let new_acc =
            if is_vs || is_zwj || is_ri then acc (* Don't add width for these *)
            else if w = -1 then acc (* Don't add control character width *)
            else acc + w
          in

          loop dec new_acc new_has_vs15 new_has_vs16 new_has_zwj new_ri_count
            new_last_non_vs_was_keycap is_emoji new_has_control
    | `End ->
        (* Final width calculation *)
        if has_control then -1 (* Control character cluster *)
        else if ri_count >= 2 then 2 (* Regional indicator flag *)
        else if ri_count = 1 then
          (* Single RI should be width 1 unless followed by VS-16 *)
          if has_vs16 then 2 else 1
        else if has_zwj && has_emoji then 2 (* Emoji ZWJ sequence *)
        else if has_vs16 then
          if acc > 0 then 2 (* VS-16 forces emoji presentation *)
          else 1 (* Isolated VS-16 has width 1 *)
        else if has_vs15 then 1 (* VS-15 forces text presentation *)
        else acc
    | `Malformed _ ->
        (* Skip malformed sequence *)
        loop dec acc has_vs15 has_vs16 has_zwj ri_count last_non_vs_was_keycap
          has_emoji has_control
    | `Await -> assert false
  in

  let substring = String.sub s start len in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String substring) in
  loop decoder 0 false false false 0 false false false

(** Calculate width of a grapheme cluster *)
let cluster_width ?(east_asian = false) cluster =
  cluster_width_sub ~east_asian cluster 0 (String.length cluster)

(** Simple LRU cache for string width calculations *)
module WidthCache = struct
  type entry = { width : int; mutable last_access : int }

  let cache = Hashtbl.create 1024
  let access_counter = ref 0
  let max_size = 2048

  let evict_oldest () =
    let oldest_key = ref None in
    let oldest_time = ref max_int in
    Hashtbl.iter
      (fun k v ->
        if v.last_access < !oldest_time then (
          oldest_time := v.last_access;
          oldest_key := Some k))
      cache;
    match !oldest_key with Some k -> Hashtbl.remove cache k | None -> ()

  let get key =
    match Hashtbl.find_opt cache key with
    | Some entry ->
        incr access_counter;
        entry.last_access <- !access_counter;
        Some entry.width
    | None -> None

  let put key width =
    if Hashtbl.length cache >= max_size then evict_oldest ();
    incr access_counter;
    Hashtbl.replace cache key { width; last_access = !access_counter }
end

(** Calculate display width of a string using proper grapheme segmentation *)
let string_width ?(east_asian = false) s =
  let len = String.length s in
  if len = 0 then 0
  else
    (* Fast path for pure ASCII strings *)
    let rec check_ascii i acc =
      if i >= len then Some acc
      else
        let byte = Char.code s.[i] in
        if byte >= 128 then None (* Not ASCII *)
        else
          let w = ascii_width_table.(byte) in
          if w = -1 then check_ascii (i + 1) acc (* Skip control chars *)
          else check_ascii (i + 1) (acc + w)
    in
    match check_ascii 0 0 with
    | Some width -> width
    | None -> (
        (* Check cache for non-ASCII strings *)
        let cache_key = (s, east_asian) in
        match WidthCache.get cache_key with
        | Some width -> width
        | None ->
            (* Slow path: full grapheme segmentation *)
            let folder acc g =
              let w = cluster_width ~east_asian g in
              (* Control characters (-1) don't contribute to display width *)
              if w = -1 then acc else acc + w
            in
            let width = Uuseg_string.fold_utf_8 `Grapheme_cluster folder 0 s in
            WidthCache.put cache_key width;
            width)
