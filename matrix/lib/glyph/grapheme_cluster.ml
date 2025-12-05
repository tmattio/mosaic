open StdLabels

(* --- UAX #29 Property Definitions --- *)

module Prop = struct
  let cr = 1
  let lf = 2
  let control = 3
  let extend = 4
  let zwj = 5
  let regional_indicator = 6
  let prepend = 7
  let spacing_mark = 8
  let l = 9
  let v = 10
  let t = 11
  let lv = 12
  let lvt = 13
  let extended_pictographic = 14
end

module Indic = struct
  let consonant = 1
  let linker = 2
  let extend = 3
end

(* --- Combined Property Lookup (Fused) --- *)

(* 
   We store Prop (4 bits) and Indic (2 bits) in a single byte.
   Layout: [00 | Indic (2 bits) | Prop (4 bits)] 
   Note: Indic.extend (3) maps to Prop.extend/zwj usually, so we optimize checks.
*)

module Table = struct
  (* 2-Level Trie: High byte (index) -> Low byte (leaf) *)
  let data = Array.make 4352 Bytes.empty
  let width_shift = 6
  let width_mask = 0xC0
  let default_width_bits = Char.chr (1 lsl width_shift)

  let get_leaf i =
    let leaf = data.(i) in
    if leaf == Bytes.empty then (
      let new_leaf = Bytes.make 256 default_width_bits in
      data.(i) <- new_leaf;
      new_leaf)
    else leaf

  (* Sets the lower 4 bits (Prop) *)
  let set_prop s e v =
    let rec loop cp =
      if cp > e then ()
      else
        let leaf_idx = cp lsr 8 in
        let leaf = get_leaf leaf_idx in
        let local_idx = cp land 0xFF in
        let end_in_leaf = min e (((leaf_idx + 1) * 256) - 1) in
        for i = local_idx to end_in_leaf - cp + local_idx do
          let old = Char.code (Bytes.get leaf i) in
          (* Preserve high bits, set low 4 bits *)
          let new_val = old land 0xF0 lor (v land 0x0F) in
          Bytes.set leaf i (Char.chr new_val)
        done;
        loop (end_in_leaf + 1)
    in
    loop (max 0 s)

  (* Sets bits 4-5 (Indic) *)
  let set_indic s e v =
    let rec loop cp =
      if cp > e then ()
      else
        let leaf_idx = cp lsr 8 in
        let leaf = get_leaf leaf_idx in
        let local_idx = cp land 0xFF in
        let end_in_leaf = min e (((leaf_idx + 1) * 256) - 1) in
        for i = local_idx to end_in_leaf - cp + local_idx do
          let old = Char.code (Bytes.get leaf i) in
          (* Preserve low 4 bits and top 2 bits, set indic bits 4-5 *)
          let new_val = old land 0xCF lor ((v land 0x03) lsl 4) in
          Bytes.set leaf i (Char.chr new_val)
        done;
        loop (end_in_leaf + 1)
    in
    loop (max 0 s)

  let set_width s e v =
    let rec loop cp =
      if cp > e then ()
      else
        let leaf_idx = cp lsr 8 in
        let leaf = get_leaf leaf_idx in
        let local_idx = cp land 0xFF in
        let end_in_leaf = min e (((leaf_idx + 1) * 256) - 1) in
        for i = local_idx to end_in_leaf - cp + local_idx do
          let old = Char.code (Bytes.get leaf i) in
          (* Preserve low 6 bits, set top 2 bits as width *)
          let new_val = old land 0x3F lor ((v land 0x03) lsl width_shift) in
          Bytes.set leaf i (Char.chr new_val)
        done;
        loop (end_in_leaf + 1)
    in
    loop (max 0 s)

  let init () =
    let apply_prop ranges v =
      Array.iter ~f:(fun (s, e) -> set_prop s e v) ranges
    in
    let apply_indic ranges v =
      Array.iter ~f:(fun (s, e) -> set_indic s e v) ranges
    in
    let apply_width ranges v =
      Array.iter ~f:(fun (s, e) -> set_width s e v) ranges
    in

    (* --- Properties --- *)
    apply_prop Data.range_extend Prop.extend;
    apply_prop Data.range_spacing_mark Prop.spacing_mark;
    apply_prop Data.range_prepend Prop.prepend;
    apply_prop Data.range_extended_pictographic Prop.extended_pictographic;
    apply_prop Data.range_hangul_l Prop.l;
    apply_prop Data.range_hangul_v Prop.v;
    apply_prop Data.range_hangul_t Prop.t;

    set_prop 0x1F1E6 0x1F1FF Prop.regional_indicator;
    set_prop 0x200D 0x200D Prop.zwj;
    set_prop 0x00 0x1F Prop.control;
    set_prop 0x7F 0x9F Prop.control;
    set_prop 0x2028 0x2029 Prop.control;
    set_prop 0x0D 0x0D Prop.cr;
    set_prop 0x0A 0x0A Prop.lf;

    (* Algorithmic Hangul *)
    for cp = 0xAC00 to 0xD7A3 do
      let s_index = cp - 0xAC00 in
      let prop = if s_index mod 28 = 0 then Prop.lv else Prop.lvt in
      set_prop cp cp prop
    done;

    (* --- Indic --- *)
    apply_indic Data.range_incb_linker Indic.linker;
    apply_indic Data.range_incb_consonant Indic.consonant;
    apply_indic Data.range_incb_extend Indic.extend;
    set_indic 0x200D 0x200D Indic.extend
    (* --- Widths --- *)
    (* 0: combining/zero-width, 1: default, 2: wide, 3: control *);

    apply_width Data.combining_ranges 0;
    set_width 0x200D 0x200D 0;
    apply_width Data.wide_ranges 2;
    (* Default emoji presentation characters render wide; text-presentation
       pictographs stay narrow unless VS16 is present (handled in utf8). *)
    apply_width Data.emoji_presentation_ranges 2;
    apply_width Data.control_ranges 3

  let _ = init ()

  let[@inline] get_combined cp =
    if cp < 0 || cp > 0x10FFFF then 0
    else
      let leaf = Array.unsafe_get data (cp lsr 8) in
      if leaf == Bytes.empty then 0
      else Char.code (Bytes.unsafe_get leaf (cp land 0xFF))

  let[@inline] width cp =
    if cp < 0 || cp > 0x10FFFF then 1
    else
      let leaf = Array.unsafe_get data (cp lsr 8) in
      if leaf == Bytes.empty then 1
      else
        let v = Char.code (Bytes.unsafe_get leaf (cp land 0xFF)) in
        (v land width_mask) lsr width_shift
end

(* --- Boundary Decision Table (Unchanged) --- *)

module Boundary_table = struct
  let table = Bytes.make 256 '\001'
  let set_no_break prev curr = Bytes.set table ((prev lsl 4) lor curr) '\000'

  let init () =
    let open Prop in
    set_no_break cr lf;
    List.iter ~f:(fun p -> set_no_break l p) [ l; v; lv; lvt ];
    List.iter ~f:(fun p -> set_no_break lv p) [ v; t ];
    List.iter ~f:(fun p -> set_no_break v p) [ v; t ];
    List.iter ~f:(fun p -> set_no_break lvt p) [ t ];
    List.iter ~f:(fun p -> set_no_break t p) [ t ];
    for p = 0 to 14 do
      if p <> control && p <> cr && p <> lf then (
        set_no_break p extend;
        set_no_break p zwj;
        set_no_break p spacing_mark;
        set_no_break prepend p)
    done

  let _ = init ()

  let[@inline] check prev curr =
    Bytes.unsafe_get table ((prev lsl 4) lor curr) = '\001'
end

(* --- Core Segmentation Logic --- *)

type emoji_state = No_Emoji | Saw_ExtPict | Saw_ExtPict_Zwj
type incb_state = No_InCB | InCB_Consonant | InCB_Linker

let[@inline] decode_safe str i limit =
  let b0 = Char.code (String.unsafe_get str i) in
  if b0 < 0x80 then (b0, 1)
  else if b0 < 0xE0 && i + 1 < limit then
    let b1 = Char.code (String.unsafe_get str (i + 1)) in
    (((b0 land 0x1F) lsl 6) lor (b1 land 0x3F), 2)
  else if b0 < 0xF0 && i + 2 < limit then
    let b1 = Char.code (String.unsafe_get str (i + 1)) in
    let b2 = Char.code (String.unsafe_get str (i + 2)) in
    (((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F), 3)
  else if b0 < 0xF8 && i + 3 < limit then
    let b1 = Char.code (String.unsafe_get str (i + 1)) in
    let b2 = Char.code (String.unsafe_get str (i + 2)) in
    let b3 = Char.code (String.unsafe_get str (i + 3)) in
    ( ((b0 land 0x07) lsl 18)
      lor ((b1 land 0x3F) lsl 12)
      lor ((b2 land 0x3F) lsl 6)
      lor (b3 land 0x3F),
      4 )
  else (b0, 1)

let next_boundary ?(ignore_zwj = false) str start limit =
  if start >= limit then limit
  else
    (* 1. Decode first character *)
    let first_cp, first_len = decode_safe str start limit in

    (* Fused Lookup: Get Prop and Indic in one array access *)
    let first_val = Table.get_combined first_cp in
    let first_prop = first_val land 0x0F in
    let first_incb = (first_val lsr 4) land 0x03 in

    (* Initialize State *)
    (* Use mutable refs to avoid closure allocation in loop *)
    let pos = ref (start + first_len) in
    let prev_prop = ref first_prop in
    let ri_chain_len =
      ref (if first_prop = Prop.regional_indicator then 1 else 0)
    in
    let emoji_state =
      ref
        (if first_prop = Prop.extended_pictographic then Saw_ExtPict
         else No_Emoji)
    in
    let incb_state =
      ref (if first_incb = Indic.consonant then InCB_Consonant else No_InCB)
    in
    let done_flag = ref false in

    (* Hot Loop *)
    while not !done_flag do
      if !pos >= limit then done_flag := true
      else
        let cp, len = decode_safe str !pos limit in

        (* Fused Lookup *)
        let val_combined = Table.get_combined cp in
        let curr_prop = val_combined land 0x0F in
        let curr_incb = (val_combined lsr 4) land 0x03 in
        let p_prop = !prev_prop in

        (* no_zwj mode: force a break after ZWJ regardless of GB11 *)
        let forced_break_after_zwj = ignore_zwj && p_prop = Prop.zwj in

        (* Dynamic Rules check (inlined integer comparisons) *)
        (* GB11: Emoji ZWJ Sequence *)
        let is_gb11 =
          (not ignore_zwj)
          && !emoji_state = Saw_ExtPict_Zwj
          && curr_prop = Prop.extended_pictographic
        in

        (* GB9c: Indic Conjuncts *)
        (* Note: Indic.linker=2, Indic.consonant=1. *)
        let is_gb9c =
          !incb_state = InCB_Linker && curr_incb = Indic.consonant
        in

        (* GB12/13: RI Pairs *)
        (* Optimization: Use bitwise check for odd/even *)
        let is_gb12_13 =
          p_prop = Prop.regional_indicator
          && curr_prop = Prop.regional_indicator
          && !ri_chain_len land 1 = 1
        in

        if forced_break_after_zwj then done_flag := true
        else if is_gb11 || is_gb9c || is_gb12_13 then
          (* No Break (Dynamic Rule matched) *)
          ()
        else if Boundary_table.check p_prop curr_prop then
          (* Break found via Static Table *)
          done_flag := true
        else
          (* No Break (Static Table says glue) *)
          ();

        if not !done_flag then (
          (* Advance and Update State *)
          pos := !pos + len;
          prev_prop := curr_prop;

          if curr_prop = Prop.regional_indicator then incr ri_chain_len
          else ri_chain_len := 0;

          (* Emoji State Update *)
          let e_st = !emoji_state in
          if e_st = Saw_ExtPict then
            if curr_prop = Prop.extend then () (* stay Saw_ExtPict *)
            else if curr_prop = Prop.zwj then emoji_state := Saw_ExtPict_Zwj
            else if curr_prop = Prop.extended_pictographic then () (* stay *)
            else emoji_state := No_Emoji
          else if e_st = Saw_ExtPict_Zwj then
            if curr_prop = Prop.extended_pictographic then
              emoji_state := Saw_ExtPict
            else if curr_prop = Prop.extend then ()
            else emoji_state := No_Emoji
          else if curr_prop = Prop.extended_pictographic then
            emoji_state := Saw_ExtPict;

          (* Indic State Update *)
          let i_st = !incb_state in
          if i_st = No_InCB then (
            if curr_incb = Indic.consonant then incb_state := InCB_Consonant)
          else if i_st = InCB_Consonant then
            if curr_incb = Indic.linker then incb_state := InCB_Linker
            else if curr_incb = Indic.extend || curr_incb = Indic.consonant then
              () (* stay *)
            else incb_state := No_InCB
          else if
            (* InCB_Linker *)
            curr_incb = Indic.consonant
          then incb_state := InCB_Consonant
          else if curr_incb = Indic.linker || curr_incb = Indic.extend then ()
            (* stay *)
          else incb_state := No_InCB)
    done;
    !pos

let[@inline] width_of_cp cp = Table.width cp
