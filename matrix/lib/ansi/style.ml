type color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default
  | Bright_black
  | Bright_red
  | Bright_green
  | Bright_yellow
  | Bright_blue
  | Bright_magenta
  | Bright_cyan
  | Bright_white
  | Index of int
  | RGB of int * int * int
  | RGBA of int * int * int * int (* r, g, b, alpha for blending *)

type style =
  [ `Bold
  | `Dim
  | `Italic
  | `Underline
  | `Double_underline
  | `Blink
  | `Reverse
  | `Conceal
  | `Strikethrough
  | `Overline
  | `Framed
  | `Encircled ]

type attr =
  [ `Fg of color
  | `Bg of color
  | `Reset
  | `No_bold
  | `No_dim
  | `No_italic
  | `No_underline
  | `No_blink
  | `No_reverse
  | `No_conceal
  | `No_strikethrough
  | `No_overline
  | `No_framed
  | `No_encircled
  | style ]

type t = int64

(* Extended style for complex cases *)
type unpacked = {
  fg_rgba : int32; (* 0xRRGGBBAA *)
  bg_rgba : int32; (* 0xRRGGBBAA *)
  flags : int; (* Style flags *)
  link : int; (* Link ID *)
  fg_kind : int; (* 0=default, 1=basic, 2=index, 3=RGB, 4=RGBA *)
  bg_kind : int; (* Same encoding *)
}

(* Arena for extended styles *)
module Arena = struct
  type t = {
    mutable data :
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
    mutable size : int; (* Current number of entries *)
    mutable capacity : int; (* Current capacity *)
  }

  let initial_capacity = 1024
  let entry_size = 16 (* Size of unpacked struct in bytes *)
  let max_safe_capacity = max_int / (2 * entry_size)

  (* Prevent overflow in dim calculation *)
  let arena = ref None
  let arena_mutex = Mutex.create ()

  let get_or_create () =
    Mutex.lock arena_mutex;
    let result =
      match !arena with
      | Some a -> a
      | None ->
          let a =
            {
              data =
                Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
                  (initial_capacity * entry_size);
              size = 0;
              capacity = initial_capacity;
            }
          in
          arena := Some a;
          a
    in
    Mutex.unlock arena_mutex;
    result

  let grow arena =
    (* Note: Must be called with mutex held *)
    let old_capacity = arena.capacity in
    let candidate = old_capacity lsl 1 in
    (* *2 *)
    if candidate <= 0 || candidate > max_safe_capacity then
      raise (Failure "Arena capacity overflow");
    let new_capacity = candidate in
    let new_dim = new_capacity * entry_size in
    if new_dim <= 0 || new_dim / entry_size <> new_capacity then
      (* Overflow check *)
      raise (Failure "Arena dimension overflow");
    let new_data =
      Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout new_dim
    in
    let old_dim = old_capacity * entry_size in
    Bigarray.Array1.blit arena.data (Bigarray.Array1.sub new_data 0 old_dim);
    arena.data <- new_data;
    arena.capacity <- new_capacity

  let alloc_unpacked arena unpacked =
    Mutex.lock arena_mutex;
    if arena.size >= arena.capacity then grow arena;
    let offset = arena.size in
    (* Write unpacked data to arena *)
    let write_int32 off v =
      let open Bigarray.Array1 in
      unsafe_set arena.data
        ((offset * entry_size) + off)
        (Int32.to_int (Int32.logand v 0xFFl));
      unsafe_set arena.data
        ((offset * entry_size) + off + 1)
        (Int32.to_int (Int32.shift_right_logical v 8) land 0xFF);
      unsafe_set arena.data
        ((offset * entry_size) + off + 2)
        (Int32.to_int (Int32.shift_right_logical v 16) land 0xFF);
      unsafe_set arena.data
        ((offset * entry_size) + off + 3)
        (Int32.to_int (Int32.shift_right_logical v 24) land 0xFF)
    in
    let write_int16 off v =
      let open Bigarray.Array1 in
      unsafe_set arena.data ((offset * entry_size) + off) (v land 0xFF);
      unsafe_set arena.data
        ((offset * entry_size) + off + 1)
        ((v lsr 8) land 0xFF)
    in
    let write_int8 off v =
      Bigarray.Array1.unsafe_set arena.data ((offset * entry_size) + off) v
    in
    write_int32 0 unpacked.fg_rgba;
    write_int32 4 unpacked.bg_rgba;
    write_int16 8 unpacked.flags;
    write_int8 10 unpacked.link;
    write_int8 11 unpacked.fg_kind;
    write_int8 12 unpacked.bg_kind;
    arena.size <- arena.size + 1;
    Mutex.unlock arena_mutex;
    offset

  let read_unpacked arena offset =
    Mutex.lock arena_mutex;
    (* Bounds check *)
    if offset < 0 || offset >= arena.size then (
      Mutex.unlock arena_mutex;
      (* Return a default unpacked value *)
      {
        fg_rgba = Int32.zero;
        bg_rgba = Int32.zero;
        flags = 0;
        link = 0;
        fg_kind = 0;
        bg_kind = 0;
      })
    else
      let dim64 = Int64.of_int (Bigarray.Array1.dim arena.data) in
      let base64 = Int64.mul (Int64.of_int offset) (Int64.of_int entry_size) in
      if base64 < 0L || base64 >= dim64 then (
        Mutex.unlock arena_mutex;
        {
          fg_rgba = Int32.zero;
          bg_rgba = Int32.zero;
          flags = 0;
          link = 0;
          fg_kind = 0;
          bg_kind = 0;
        })
      else
        let safe_get off =
          let i64 = Int64.add base64 (Int64.of_int off) in
          if i64 < 0L || i64 >= dim64 then 0
          else Bigarray.Array1.unsafe_get arena.data (Int64.to_int i64)
        in
        let read_int32 off =
          let b0 = Int32.of_int (safe_get off) in
          let b1 = Int32.of_int (safe_get (off + 1)) in
          let b2 = Int32.of_int (safe_get (off + 2)) in
          let b3 = Int32.of_int (safe_get (off + 3)) in
          Int32.logor
            (Int32.logor b0 (Int32.shift_left b1 8))
            (Int32.logor (Int32.shift_left b2 16) (Int32.shift_left b3 24))
        in
        let read_int16 off =
          let b0 = safe_get off in
          let b1 = safe_get (off + 1) in
          b0 lor (b1 lsl 8)
        in
        let read_int8 = safe_get in
        let result =
          {
            fg_rgba = read_int32 0;
            bg_rgba = read_int32 4;
            flags = read_int16 8;
            link = read_int8 10;
            fg_kind = read_int8 11;
            bg_kind = read_int8 12;
          }
        in
        Mutex.unlock arena_mutex;
        result
end

(* New bit layout for fast path (no overlaps):
   Bit 63: Extended flag (0 = fast path, 1 = extended)
   Bit 62: Unused (reserved for future)
   Bit 61: fg_none (1 bit) - indicates fg should inherit
   Bit 60: bg_none (1 bit) - indicates bg should inherit
   Bits 58-59: FG color type (2 bits)
   Bits 56-57: BG color type (2 bits)
   Bits 51-55: Link ID (5 bits, 0-31; >31 needs extended)
   Bits 43-50: Style flags (8 bits)
   Bits 35-42: BG color data (8 bits for index/basic only)
   Bits 27-34: FG color data (8 bits for index/basic only)
   Bits 0-26: Reserved for future use
*)

let extended_bit = 63
let fg_none_bit = 61
let bg_none_bit = 60
let fg_type_shift = 58
let bg_type_shift = 56
let link_shift = 51
let flags_shift = 43
let bg_data_shift = 35
let fg_data_shift = 27
let type_mask = 0x3L (* 2 bits *)
let link_mask = 0x1FL (* 5 bits *)
let flags_mask = 0xFFL (* 8 bits *)
let color_data_mask = 0xFFL (* 8 bits for fast path *)

(* Color type encoding *)
let color_type_default = 0L
let color_type_basic = 1L
let color_type_index = 2L
let color_type_rgb = 3L
let color_type_rgba = 4L (* Only used in extended path *)

(* Style flag bits (reduced to 8) *)
let bold_bit = 0
let dim_bit = 1
let italic_bit = 2
let underline_bit = 3
let blink_bit = 4 (* Combines slow/fast blink *)
let reversed_bit = 5
let strikethrough_bit = 6
let overline_bit = 7
(* double_underline, conceal, framed, encircled are moved to extended path *)

(* Helper to check if we need extended encoding *)
let needs_extended ~fg ~bg ~fg_type ~fg_data ~bg_type ~bg_data ~link ~flags =
  (* RGB/RGBA always need extended path *)
  let fg_needs_extended =
    match fg with
    | RGB _ | RGBA _ -> true
    | Index i when i > 255 -> true (* Index >255 needs extended *)
    | _ -> false
  in
  let bg_needs_extended =
    match bg with
    | RGB _ | RGBA _ -> true
    | Index i when i > 255 -> true (* Index >255 needs extended *)
    | _ -> false
  in
  (* Check if data fits in 8 bits for fast path *)
  let data_fits d = d <= 0xFFL in
  let fg_fits =
    match fg_type with
    | 0L | 1L -> true (* Default and basic always fit *)
    | 2L -> data_fits fg_data (* Index must fit in 8 bits *)
    | _ -> false (* RGB (type 3) goes to extended *)
  in
  let bg_fits =
    match bg_type with
    | 0L | 1L -> true
    | 2L -> data_fits bg_data (* Index must fit in 8 bits *)
    | _ -> false (* RGB (type 3) goes to extended *)
  in
  (* Check if we have extended flags *)
  let has_extended_flags = flags > 0xFF in
  (* Need extended if anything doesn't fit *)
  fg_needs_extended || bg_needs_extended
  || (not (fg_fits && bg_fits))
  || link > 31 || has_extended_flags

(* Encode a color value *)
let encode_color_data = function
  | Default -> (color_type_default, 0L)
  | Black -> (color_type_basic, 0L)
  | Red -> (color_type_basic, 1L)
  | Green -> (color_type_basic, 2L)
  | Yellow -> (color_type_basic, 3L)
  | Blue -> (color_type_basic, 4L)
  | Magenta -> (color_type_basic, 5L)
  | Cyan -> (color_type_basic, 6L)
  | White -> (color_type_basic, 7L)
  | Bright_black -> (color_type_basic, 8L)
  | Bright_red -> (color_type_basic, 9L)
  | Bright_green -> (color_type_basic, 10L)
  | Bright_yellow -> (color_type_basic, 11L)
  | Bright_blue -> (color_type_basic, 12L)
  | Bright_magenta -> (color_type_basic, 13L)
  | Bright_cyan -> (color_type_basic, 14L)
  | Bright_white -> (color_type_basic, 15L)
  | Index i -> (color_type_index, Int64.of_int i)
  | RGB (r, g, b) ->
      (* Full 24-bit RGB encoding! *)
      (color_type_rgb, Int64.of_int ((r lsl 16) lor (g lsl 8) lor b))
  | RGBA (r, g, b, a) ->
      (* RGBA needs 32 bits, so it always uses extended path *)
      (* Return color_type_rgba and packed RGBA data *)
      ( color_type_rgba,
        Int64.of_int32
          (Int32.logor
             (Int32.logor
                (Int32.shift_left (Int32.of_int r) 24)
                (Int32.shift_left (Int32.of_int g) 16))
             (Int32.logor
                (Int32.shift_left (Int32.of_int b) 8)
                (Int32.of_int a))) )

(* Decode color from type and data *)
let decode_color_data color_type color_data =
  match color_type with
  | 0L -> Default
  | 1L -> (
      match Int64.to_int color_data with
      | 0 -> Black
      | 1 -> Red
      | 2 -> Green
      | 3 -> Yellow
      | 4 -> Blue
      | 5 -> Magenta
      | 6 -> Cyan
      | 7 -> White
      | 8 -> Bright_black
      | 9 -> Bright_red
      | 10 -> Bright_green
      | 11 -> Bright_yellow
      | 12 -> Bright_blue
      | 13 -> Bright_magenta
      | 14 -> Bright_cyan
      | 15 -> Bright_white
      | _ -> Default)
  | 2L -> Index (Int64.to_int color_data)
  | 3L ->
      let rgb = Int64.to_int color_data in
      RGB ((rgb lsr 16) land 0xFF, (rgb lsr 8) land 0xFF, rgb land 0xFF)
  | 4L ->
      let rgba = Int64.to_int32 color_data in
      RGBA
        ( Int32.to_int (Int32.shift_right_logical rgba 24) land 0xFF,
          Int32.to_int (Int32.shift_right_logical rgba 16) land 0xFF,
          Int32.to_int (Int32.shift_right_logical rgba 8) land 0xFF,
          Int32.to_int (Int32.logand rgba 0xFFl) )
  | _ -> Default

(* Fast path encoding *)
let encode_fast ~fg ~bg ~link ~flags =
  let fg_type, fg_data = encode_color_data fg in
  let bg_type, bg_data = encode_color_data bg in

  if needs_extended ~fg ~bg ~fg_type ~fg_data ~bg_type ~bg_data ~link ~flags
  then None (* Fall back to extended *)
  else
    (* Pack everything into 64 bits with new layout *)
    let result =
      Int64.logor
        (Int64.shift_left fg_type fg_type_shift)
        (Int64.logor
           (Int64.shift_left bg_type bg_type_shift)
           (Int64.logor
              (Int64.shift_left (Int64.of_int link) link_shift)
              (Int64.logor
                 (Int64.shift_left (Int64.of_int flags) flags_shift)
                 (Int64.logor
                    (Int64.shift_left bg_data bg_data_shift)
                    (Int64.shift_left fg_data fg_data_shift)))))
    in
    Some result

(* Extended path encoding *)
let encode_extended ~fg ~bg ~link ~flags =
  let arena = Arena.get_or_create () in
  let fg_type, fg_data = encode_color_data fg in
  let bg_type, bg_data = encode_color_data bg in
  let unpacked =
    {
      fg_rgba = Int32.of_int (Int64.to_int fg_data);
      bg_rgba = Int32.of_int (Int64.to_int bg_data);
      flags;
      link;
      fg_kind = Int64.to_int fg_type;
      bg_kind = Int64.to_int bg_type;
    }
  in
  let offset = Arena.alloc_unpacked arena unpacked in
  (* Set extended bit and store offset *)
  Int64.logor (Int64.shift_left 1L extended_bit) (Int64.of_int offset)

(* Main encoding function *)
let encode ~fg ~bg ~link ~flags =
  match encode_fast ~fg ~bg ~link ~flags with
  | Some v -> v
  | None -> encode_extended ~fg ~bg ~link ~flags

(* Decoding *)
let is_extended style =
  Int64.logand style (Int64.shift_left 1L extended_bit) <> 0L

let decode_fast style =
  let fg_type =
    Int64.logand (Int64.shift_right_logical style fg_type_shift) type_mask
  in
  let bg_type =
    Int64.logand (Int64.shift_right_logical style bg_type_shift) type_mask
  in
  let link =
    Int64.to_int
      (Int64.logand (Int64.shift_right_logical style link_shift) link_mask)
  in
  let flags =
    Int64.to_int
      (Int64.logand (Int64.shift_right_logical style flags_shift) flags_mask)
  in
  let bg_data =
    Int64.logand (Int64.shift_right_logical style bg_data_shift) color_data_mask
  in
  let fg_data =
    Int64.logand (Int64.shift_right_logical style fg_data_shift) color_data_mask
  in

  let fg = decode_color_data fg_type fg_data in
  let bg = decode_color_data bg_type bg_data in

  (* Check for None bits *)
  let fg_is_none = Int64.logand style (Int64.shift_left 1L fg_none_bit) <> 0L in
  let bg_is_none = Int64.logand style (Int64.shift_left 1L bg_none_bit) <> 0L in

  let fg_opt = if fg_is_none then None else Some fg in
  let bg_opt = if bg_is_none then None else Some bg in

  (fg_opt, bg_opt, link, flags)

let decode_extended style =
  let offset = Int64.to_int (Int64.logand style 0x7FFFFFFFFFFFFFFFL) in
  let arena = Arena.get_or_create () in
  (* Bounds check to prevent invalid memory access *)
  if offset < 0 || offset >= arena.size then
    (* Return default style if offset is invalid *)
    (None, None, 0, 0)
  else
    let unpacked = Arena.read_unpacked arena offset in
    let fg =
      decode_color_data
        (Int64.of_int unpacked.fg_kind)
        (Int64.of_int32 unpacked.fg_rgba)
    in
    let bg =
      decode_color_data
        (Int64.of_int unpacked.bg_kind)
        (Int64.of_int32 unpacked.bg_rgba)
    in

    (* Check for None bits *)
    let fg_is_none =
      Int64.logand style (Int64.shift_left 1L fg_none_bit) <> 0L
    in
    let bg_is_none =
      Int64.logand style (Int64.shift_left 1L bg_none_bit) <> 0L
    in

    let fg_opt = if fg_is_none then None else Some fg in
    let bg_opt = if bg_is_none then None else Some bg in

    (fg_opt, bg_opt, unpacked.link, unpacked.flags)

let decode style =
  if is_extended style then decode_extended style else decode_fast style

(* Default style - now with no colors set (None) for inheritance *)
let default =
  (* Set both fg_none and bg_none bits to indicate no colors *)
  let base = encode ~fg:Default ~bg:Default ~link:0 ~flags:0 in
  Int64.logor base
    (Int64.logor
       (Int64.shift_left 1L fg_none_bit)
       (Int64.shift_left 1L bg_none_bit))

(* Flag helpers *)
let flag_of_style = function
  | `Bold -> 1 lsl bold_bit
  | `Dim -> 1 lsl dim_bit
  | `Italic -> 1 lsl italic_bit
  | `Underline -> 1 lsl underline_bit
  | `Blink -> 1 lsl blink_bit
  | `Reverse -> 1 lsl reversed_bit
  | `Strikethrough -> 1 lsl strikethrough_bit
  | `Overline -> 1 lsl overline_bit
  | _ -> 0 (* Extended flags handled separately *)

let _flags_of_attrs attrs =
  List.fold_left
    (fun acc -> function #style as s -> acc lor flag_of_style s | _ -> acc)
    0 attrs

(* Public API *)
let make ?bold ?dim ?italic ?underline ?double_underline ?fg ?bg ?reversed
    ?strikethrough ?overline ?blink () =
  let flags =
    (if bold = Some true then 1 lsl bold_bit else 0)
    lor (if dim = Some true then 1 lsl dim_bit else 0)
    lor (if italic = Some true then 1 lsl italic_bit else 0)
    lor (if underline = Some true then 1 lsl underline_bit else 0)
    lor (if blink = Some true then 1 lsl blink_bit else 0)
    lor (if reversed = Some true then 1 lsl reversed_bit else 0)
    lor (if strikethrough = Some true then 1 lsl strikethrough_bit else 0)
    lor if overline = Some true then 1 lsl overline_bit else 0
  in
  (* Extended flags for double_underline etc. would bump us to extended path *)
  let extended_flags = double_underline = Some true in
  let flags = if extended_flags then flags lor 0x100 else flags in
  let fg = Option.value fg ~default:Default in
  let bg = Option.value bg ~default:Default in
  encode ~fg ~bg ~link:0 ~flags

(* Accessors *)
let bold t =
  let _, _, _, flags = decode t in
  flags land (1 lsl bold_bit) <> 0

let dim t =
  let _, _, _, flags = decode t in
  flags land (1 lsl dim_bit) <> 0

let italic t =
  let _, _, _, flags = decode t in
  flags land (1 lsl italic_bit) <> 0

let underline t =
  let _, _, _, flags = decode t in
  flags land (1 lsl underline_bit) <> 0

let double_underline t =
  let _, _, _, flags = decode t in
  flags land 0x100 <> 0 (* Extended flag *)

let fg t =
  let fg_opt, _, _, _ = decode t in
  match fg_opt with
  | None -> Default (* When no color is set, return Default for compatibility *)
  | Some c -> c

let bg t =
  let _, bg_opt, _, _ = decode t in
  match bg_opt with
  | None -> Default (* When no color is set, return Default for compatibility *)
  | Some c -> c

let reversed t =
  let _, _, _, flags = decode t in
  flags land (1 lsl reversed_bit) <> 0

let strikethrough t =
  let _, _, _, flags = decode t in
  flags land (1 lsl strikethrough_bit) <> 0

let overline t =
  let _, _, _, flags = decode t in
  flags land (1 lsl overline_bit) <> 0

let blink t =
  let _, _, _, flags = decode t in
  flags land (1 lsl blink_bit) <> 0

(* Builders *)
let with_fg color t =
  let _, bg_opt, link, flags = decode t in
  (* For encoding, use Default as placeholder when None *)
  let bg_for_encode = match bg_opt with None -> Default | Some c -> c in
  let result = encode ~fg:color ~bg:bg_for_encode ~link ~flags in
  (* Only manipulate none bits for non-extended styles *)
  if is_extended result then result
    (* Extended styles handle None differently *)
  else
    (* Clear the fg_none bit since we're setting an actual color *)
    let result =
      Int64.logand result (Int64.lognot (Int64.shift_left 1L fg_none_bit))
    in
    (* Preserve bg_none bit if bg was None *)
    if bg_opt = None then Int64.logor result (Int64.shift_left 1L bg_none_bit)
    else result

(* Set fg to None (inherit) *)
let with_no_fg t =
  let _, bg_opt, link, flags = decode t in
  let bg = match bg_opt with None -> Default | Some c -> c in
  let result = encode ~fg:Default ~bg ~link ~flags in
  (* Set the fg_none bit to indicate no fg color *)
  let result = Int64.logor result (Int64.shift_left 1L fg_none_bit) in
  (* Preserve bg_none bit if bg was None *)
  if bg_opt = None then Int64.logor result (Int64.shift_left 1L bg_none_bit)
  else result

let with_bg color t =
  let fg_opt, _, link, flags = decode t in
  (* For encoding, use Default as placeholder when None *)
  let fg_for_encode = match fg_opt with None -> Default | Some c -> c in
  let result = encode ~fg:fg_for_encode ~bg:color ~link ~flags in
  (* Only manipulate none bits for non-extended styles *)
  if is_extended result then result
    (* Extended styles handle None differently *)
  else
    (* Clear the bg_none bit since we're setting an actual color *)
    let result =
      Int64.logand result (Int64.lognot (Int64.shift_left 1L bg_none_bit))
    in
    (* Preserve fg_none bit if fg was None *)
    if fg_opt = None then Int64.logor result (Int64.shift_left 1L fg_none_bit)
    else result

(* Set bg to None (inherit) *)
let with_no_bg t =
  let fg_opt, _, link, flags = decode t in
  let fg = match fg_opt with None -> Default | Some c -> c in
  let result = encode ~fg ~bg:Default ~link ~flags in
  (* Set the bg_none bit to indicate no bg color *)
  let result = Int64.logor result (Int64.shift_left 1L bg_none_bit) in
  (* Preserve fg_none bit if fg was None *)
  if fg_opt = None then Int64.logor result (Int64.shift_left 1L fg_none_bit)
  else result

let with_bold b t =
  let fg_opt, bg_opt, link, flags = decode t in
  let fg = match fg_opt with None -> Default | Some c -> c in
  let bg = match bg_opt with None -> Default | Some c -> c in
  let flags =
    if b then flags lor (1 lsl bold_bit) else flags land lnot (1 lsl bold_bit)
  in
  let result = encode ~fg ~bg ~link ~flags in
  (* Preserve None bits *)
  let result =
    if fg_opt = None then Int64.logor result (Int64.shift_left 1L fg_none_bit)
    else result
  in
  if bg_opt = None then Int64.logor result (Int64.shift_left 1L bg_none_bit)
  else result

let with_italic b t =
  let fg_opt, bg_opt, link, flags = decode t in
  let fg = match fg_opt with None -> Default | Some c -> c in
  let bg = match bg_opt with None -> Default | Some c -> c in
  let flags =
    if b then flags lor (1 lsl italic_bit)
    else flags land lnot (1 lsl italic_bit)
  in
  let result = encode ~fg ~bg ~link ~flags in
  (* Preserve None bits *)
  let result =
    if fg_opt = None then Int64.logor result (Int64.shift_left 1L fg_none_bit)
    else result
  in
  if bg_opt = None then Int64.logor result (Int64.shift_left 1L bg_none_bit)
  else result

let with_underline b t =
  let fg_opt, bg_opt, link, flags = decode t in
  let fg = match fg_opt with None -> Default | Some c -> c in
  let bg = match bg_opt with None -> Default | Some c -> c in
  let flags =
    if b then flags lor (1 lsl underline_bit)
    else flags land lnot (1 lsl underline_bit)
  in
  let result = encode ~fg ~bg ~link ~flags in
  (* Preserve None bits *)
  let result =
    if fg_opt = None then Int64.logor result (Int64.shift_left 1L fg_none_bit)
    else result
  in
  if bg_opt = None then Int64.logor result (Int64.shift_left 1L bg_none_bit)
  else result

let with_double_underline b t =
  let fg_opt, bg_opt, link, flags = decode t in
  let fg = match fg_opt with None -> Default | Some c -> c in
  let bg = match bg_opt with None -> Default | Some c -> c in
  let flags = if b then flags lor 0x100 else flags land lnot 0x100 in
  let result = encode ~fg ~bg ~link ~flags in
  (* Preserve None bits *)
  let result =
    if fg_opt = None then Int64.logor result (Int64.shift_left 1L fg_none_bit)
    else result
  in
  if bg_opt = None then Int64.logor result (Int64.shift_left 1L bg_none_bit)
  else result

let with_strikethrough b t =
  let fg_opt, bg_opt, link, flags = decode t in
  let fg = match fg_opt with None -> Default | Some c -> c in
  let bg = match bg_opt with None -> Default | Some c -> c in
  let flags =
    if b then flags lor (1 lsl strikethrough_bit)
    else flags land lnot (1 lsl strikethrough_bit)
  in
  let result = encode ~fg ~bg ~link ~flags in
  (* Preserve None bits *)
  let result =
    if fg_opt = None then Int64.logor result (Int64.shift_left 1L fg_none_bit)
    else result
  in
  if bg_opt = None then Int64.logor result (Int64.shift_left 1L bg_none_bit)
  else result

let with_reversed b t =
  let fg_opt, bg_opt, link, flags = decode t in
  let fg = match fg_opt with None -> Default | Some c -> c in
  let bg = match bg_opt with None -> Default | Some c -> c in
  let flags =
    if b then flags lor (1 lsl reversed_bit)
    else flags land lnot (1 lsl reversed_bit)
  in
  let result = encode ~fg ~bg ~link ~flags in
  (* Preserve None bits *)
  let result =
    if fg_opt = None then Int64.logor result (Int64.shift_left 1L fg_none_bit)
    else result
  in
  if bg_opt = None then Int64.logor result (Int64.shift_left 1L bg_none_bit)
  else result

let with_blink b t =
  let fg_opt, bg_opt, link, flags = decode t in
  let fg = match fg_opt with None -> Default | Some c -> c in
  let bg = match bg_opt with None -> Default | Some c -> c in
  let flags =
    if b then flags lor (1 lsl blink_bit) else flags land lnot (1 lsl blink_bit)
  in
  let result = encode ~fg ~bg ~link ~flags in
  (* Preserve None bits *)
  let result =
    if fg_opt = None then Int64.logor result (Int64.shift_left 1L fg_none_bit)
    else result
  in
  if bg_opt = None then Int64.logor result (Int64.shift_left 1L bg_none_bit)
  else result

let with_dim b t =
  let fg_opt, bg_opt, link, flags = decode t in
  let fg = match fg_opt with None -> Default | Some c -> c in
  let bg = match bg_opt with None -> Default | Some c -> c in
  let flags =
    if b then flags lor (1 lsl dim_bit) else flags land lnot (1 lsl dim_bit)
  in
  let result = encode ~fg ~bg ~link ~flags in
  (* Preserve None bits *)
  let result =
    if fg_opt = None then Int64.logor result (Int64.shift_left 1L fg_none_bit)
    else result
  in
  if bg_opt = None then Int64.logor result (Int64.shift_left 1L bg_none_bit)
  else result

let with_overline b t =
  let fg_opt, bg_opt, link, flags = decode t in
  let fg = match fg_opt with None -> Default | Some c -> c in
  let bg = match bg_opt with None -> Default | Some c -> c in
  let flags =
    if b then flags lor (1 lsl overline_bit)
    else flags land lnot (1 lsl overline_bit)
  in
  let result = encode ~fg ~bg ~link ~flags in
  (* Preserve None bits *)
  let result =
    if fg_opt = None then Int64.logor result (Int64.shift_left 1L fg_none_bit)
    else result
  in
  if bg_opt = None then Int64.logor result (Int64.shift_left 1L bg_none_bit)
  else result

(* Link ID support *)
let get_link_id t =
  let _, _, link, _ = decode t in
  link

let set_link_id t id =
  let fg_opt, bg_opt, _, flags = decode t in
  let fg = match fg_opt with None -> Default | Some c -> c in
  let bg = match bg_opt with None -> Default | Some c -> c in
  let result = encode ~fg ~bg ~link:id ~flags in
  (* Preserve None bits *)
  let result =
    if fg_opt = None then Int64.logor result (Int64.shift_left 1L fg_none_bit)
    else result
  in
  if bg_opt = None then Int64.logor result (Int64.shift_left 1L bg_none_bit)
  else result

(* Apply SGR attribute *)
let apply_sgr_attr t attr =
  match attr with
  | `Fg color -> with_fg color t
  | `Bg color -> with_bg color t
  | `Bold -> with_bold true t
  | `Dim -> with_dim true t
  | `Italic -> with_italic true t
  | `Underline -> with_underline true t
  | `Double_underline -> with_double_underline true t
  | `Blink -> with_blink true t
  | `Reverse -> with_reversed true t
  | `Strikethrough -> with_strikethrough true t
  | `Overline -> with_overline true t
  | `Reset -> default
  | `No_bold -> with_bold false t
  | `No_dim -> with_dim false t
  | `No_italic -> with_italic false t
  | `No_underline -> with_underline false t
  | `No_blink -> with_blink false t
  | `No_reverse -> with_reversed false t
  | `No_strikethrough -> with_strikethrough false t
  | `No_overline -> with_overline false t
  | _ -> t

(* Equality and hashing *)
let equal = Int64.equal
let hash = Hashtbl.hash

(* Convert color to RGBA for blending *)
let color_to_rgba = function
  | Black -> (0, 0, 0, 255)
  | Red -> (255, 0, 0, 255)
  | Green -> (0, 255, 0, 255)
  | Yellow -> (255, 255, 0, 255)
  | Blue -> (0, 0, 255, 255)
  | Magenta -> (255, 0, 255, 255)
  | Cyan -> (0, 255, 255, 255)
  | White -> (255, 255, 255, 255)
  | Default -> (0, 0, 0, 255) (* Treat default as black *)
  | Bright_black -> (127, 127, 127, 255)
  | Bright_red -> (255, 127, 127, 255)
  | Bright_green -> (127, 255, 127, 255)
  | Bright_yellow -> (255, 255, 127, 255)
  | Bright_blue -> (127, 127, 255, 255)
  | Bright_magenta -> (255, 127, 255, 255)
  | Bright_cyan -> (127, 255, 255, 255)
  | Bright_white -> (255, 255, 255, 255)
  | Index _ -> (0, 0, 0, 255) (* Simplification - would need palette *)
  | RGB (r, g, b) -> (r, g, b, 255)
  | RGBA (r, g, b, a) -> (r, g, b, a)

(* Alpha blend two colors: src over dst *)
let blend_colors ~src ~dst =
  let sr, sg, sb, sa = color_to_rgba src in
  let dr, dg, db, _da = color_to_rgba dst in

  (* Normalize alpha to 0.0-1.0 range *)
  let alpha = float_of_int sa /. 255.0 in
  let inv_alpha = 1.0 -. alpha in

  (* Blend RGB components *)
  let r =
    int_of_float ((float_of_int sr *. alpha) +. (float_of_int dr *. inv_alpha))
  in
  let g =
    int_of_float ((float_of_int sg *. alpha) +. (float_of_int dg *. inv_alpha))
  in
  let b =
    int_of_float ((float_of_int sb *. alpha) +. (float_of_int db *. inv_alpha))
  in

  (* Result is always opaque after blending *)
  RGB (r, g, b)

let equal_color c1 c2 =
  match (c1, c2) with
  | Black, Black
  | Red, Red
  | Green, Green
  | Yellow, Yellow
  | Blue, Blue
  | Magenta, Magenta
  | Cyan, Cyan
  | White, White
  | Default, Default
  | Bright_black, Bright_black
  | Bright_red, Bright_red
  | Bright_green, Bright_green
  | Bright_yellow, Bright_yellow
  | Bright_blue, Bright_blue
  | Bright_magenta, Bright_magenta
  | Bright_cyan, Bright_cyan
  | Bright_white, Bright_white ->
      true
  | Index i1, Index i2 -> i1 = i2
  | RGB (r1, g1, b1), RGB (r2, g2, b2) -> r1 = r2 && g1 = g2 && b1 = b2
  | RGBA (r1, g1, b1, a1), RGBA (r2, g2, b2, a2) ->
      r1 = r2 && g1 = g2 && b1 = b2 && a1 = a2
  | _ -> false

(* Merge styles *)
let merge parent child =
  (* Child overrides colors if set; flags are union *)
  let fgp_opt, bgp_opt, lp, fp = decode parent in
  let fgc_opt, bgc_opt, lc, fc = decode child in

  (* Handle foreground with alpha blending for RGBA *)
  let fg_opt' =
    match fgc_opt with
    | None -> fgp_opt (* Inherit from parent *)
    | Some (RGBA (_, _, _, a) as child_fg) when a < 255 -> (
        (* Child has semi-transparent RGBA foreground *)
        match bgp_opt with
        | Some parent_bg ->
            (* Blend child RGBA foreground over parent background *)
            Some (blend_colors ~src:child_fg ~dst:parent_bg)
        | None ->
            (* No parent background to blend with, keep RGBA as-is *)
            fgc_opt)
    | Some _ -> fgc_opt (* Use child's color as-is *)
  in

  (* Handle background with alpha blending for RGBA *)
  let bg_opt' =
    match bgc_opt with
    | None -> bgp_opt (* Inherit from parent *)
    | Some (RGBA (_, _, _, a) as child_bg) when a < 255 -> (
        (* Child has semi-transparent RGBA background *)
        match bgp_opt with
        | Some parent_bg ->
            (* Blend child RGBA over parent background *)
            Some (blend_colors ~src:child_bg ~dst:parent_bg)
        | None ->
            (* No parent background to blend with, keep RGBA as-is *)
            bgc_opt)
    | Some _ -> bgc_opt (* Use child's color as-is *)
  in

  let link' = if lc <> 0 then lc else lp in
  let flags' = fp lor fc in

  (* Create result preserving None states *)
  let fg = match fg_opt' with None -> Default | Some c -> c in
  let bg = match bg_opt' with None -> Default | Some c -> c in
  let result = encode ~fg ~bg ~link:link' ~flags:flags' in

  (* Set None bits as needed *)
  let result =
    if fg_opt' = None then Int64.logor result (Int64.shift_left 1L fg_none_bit)
    else result
  in
  if bg_opt' = None then Int64.logor result (Int64.shift_left 1L bg_none_bit)
  else result

let ( ++ ) = merge

(* SGR Cache for performance *)
module SGR_cache = struct
  (* Use weak table to allow GC of unused styles *)
  let cache : (t * t option, string) Hashtbl.t = Hashtbl.create 1024
  let max_size = 4096

  let get style prev_style =
    try Some (Hashtbl.find cache (style, prev_style)) with Not_found -> None

  let put style prev_style sgr =
    if Hashtbl.length cache < max_size then
      Hashtbl.add cache (style, prev_style) sgr
    else (
      (* Clear cache when it gets too large *)
      Hashtbl.clear cache;
      Hashtbl.add cache (style, prev_style) sgr)
end

(* SGR generation *)
let color_to_codes ~bg = function
  | Black -> [ (if bg then 40 else 30) ]
  | Red -> [ (if bg then 41 else 31) ]
  | Green -> [ (if bg then 42 else 32) ]
  | Yellow -> [ (if bg then 43 else 33) ]
  | Blue -> [ (if bg then 44 else 34) ]
  | Magenta -> [ (if bg then 45 else 35) ]
  | Cyan -> [ (if bg then 46 else 36) ]
  | White -> [ (if bg then 47 else 37) ]
  | Default -> [ (if bg then 49 else 39) ]
  | Bright_black -> [ (if bg then 100 else 90) ]
  | Bright_red -> [ (if bg then 101 else 91) ]
  | Bright_green -> [ (if bg then 102 else 92) ]
  | Bright_yellow -> [ (if bg then 103 else 93) ]
  | Bright_blue -> [ (if bg then 104 else 94) ]
  | Bright_magenta -> [ (if bg then 105 else 95) ]
  | Bright_cyan -> [ (if bg then 106 else 96) ]
  | Bright_white -> [ (if bg then 107 else 97) ]
  | Index n ->
      let clamped = max 0 (min 255 n) in
      [ (if bg then 48 else 38); 5; clamped ]
  | RGB (r, g, b) ->
      let cr = max 0 (min 255 r) in
      let cg = max 0 (min 255 g) in
      let cb = max 0 (min 255 b) in
      [ (if bg then 48 else 38); 2; cr; cg; cb ]
  | RGBA (r, g, b, _a) ->
      (* ANSI escape sequences don't support alpha, so we render as RGB *)
      let cr = max 0 (min 255 r) in
      let cg = max 0 (min 255 g) in
      let cb = max 0 (min 255 b) in
      [ (if bg then 48 else 38); 2; cr; cg; cb ]

let style_to_code = function
  | `Bold -> 1
  | `Dim -> 2
  | `Italic -> 3
  | `Underline -> 4
  | `Double_underline -> 21
  | `Blink -> 5
  | `Reverse -> 7
  | `Conceal -> 8
  | `Strikethrough -> 9
  | `Overline -> 53
  | `Framed -> 51
  | `Encircled -> 52

let attr_to_codes = function
  | `Fg color -> color_to_codes ~bg:false color
  | `Bg color -> color_to_codes ~bg:true color
  | `Reset -> [ 0 ]
  | `No_bold -> [ 22 ]
  | `No_dim -> [ 22 ]
  | `No_italic -> [ 23 ]
  | `No_underline -> [ 24 ]
  | `No_blink -> [ 25 ]
  | `No_reverse -> [ 27 ]
  | `No_conceal -> [ 28 ]
  | `No_strikethrough -> [ 29 ]
  | `No_overline -> [ 55 ]
  | `No_framed -> [ 54 ]
  | `No_encircled -> [ 54 ]
  | #style as s -> [ style_to_code s ]

let to_sgr ?(prev_style = None) style =
  (* Check cache first *)
  match SGR_cache.get style prev_style with
  | Some sgr -> sgr
  | None ->
      let codes = ref [] in
      let add_code c = codes := c :: !codes in
      let add_codes cs = codes := List.rev_append cs !codes in

      let fg_opt, bg_opt, _, flags = decode style in

      (* Handle resets based on previous style *)
      (match prev_style with
      | Some prev ->
          let prev_fg_opt, prev_bg_opt, _, prev_flags = decode prev in

          (* Reset background if it changed from Some color to None or Some Default *)
          (match (prev_bg_opt, bg_opt) with
          | Some prev_bg, Some Default when prev_bg <> Default -> add_code 49
          | Some prev_bg, None when prev_bg <> Default ->
              (* When transitioning from a set background to None, reset to default *)
              add_code 49
          | _ -> ());

          (* Reset foreground if it changed from Some color to None or Some Default *)
          (match (prev_fg_opt, fg_opt) with
          | Some prev_fg, Some Default when prev_fg <> Default -> add_code 39
          | Some prev_fg, None when prev_fg <> Default ->
              (* When transitioning from a set foreground to None, reset to default *)
              add_code 39
          | _ -> ());

          (* Reset individual style flags that were on but are now off *)
          if
            prev_flags land (1 lsl bold_bit) <> 0
            && flags land (1 lsl bold_bit) = 0
          then add_code 22;
          if
            prev_flags land (1 lsl dim_bit) <> 0
            && flags land (1 lsl dim_bit) = 0
          then add_code 22;
          if
            prev_flags land (1 lsl italic_bit) <> 0
            && flags land (1 lsl italic_bit) = 0
          then add_code 23;
          if
            prev_flags land (1 lsl underline_bit) <> 0
            && flags land (1 lsl underline_bit) = 0
          then add_code 24;
          if
            prev_flags land (1 lsl blink_bit) <> 0
            && flags land (1 lsl blink_bit) = 0
          then add_code 25;
          if
            prev_flags land (1 lsl reversed_bit) <> 0
            && flags land (1 lsl reversed_bit) = 0
          then add_code 27;
          if
            prev_flags land (1 lsl strikethrough_bit) <> 0
            && flags land (1 lsl strikethrough_bit) = 0
          then add_code 29;
          if
            prev_flags land (1 lsl overline_bit) <> 0
            && flags land (1 lsl overline_bit) = 0
          then add_code 55;
          if prev_flags land 0x100 <> 0 && flags land 0x100 = 0 then add_code 24
          (* double underline off *)
      | None -> ());

      (* Now add the new style codes - do this regardless of prev_style *)
      (* Special case: completely default style (both fg and bg are None) with no prev_style should emit nothing *)
      if fg_opt = None && bg_opt = None && flags = 0 && prev_style = None then
        ()
      else (
        (* Colors - only emit codes when color is Some (not None) *)
        (match fg_opt with
        | None -> () (* No color set - inherit from existing *)
        | Some fg -> add_codes (color_to_codes ~bg:false fg));

        (* Emit codes for any set color including Default *)
        (match bg_opt with
        | None -> () (* No color set - inherit from existing *)
        | Some bg -> add_codes (color_to_codes ~bg:true bg));

        (* Emit codes for any set color including Default *)

        (* Style flags *)
        if flags land (1 lsl bold_bit) <> 0 then add_code 1;
        if flags land (1 lsl dim_bit) <> 0 then add_code 2;
        if flags land (1 lsl italic_bit) <> 0 then add_code 3;
        if flags land (1 lsl underline_bit) <> 0 then add_code 4;
        if flags land (1 lsl blink_bit) <> 0 then add_code 5;
        if flags land (1 lsl reversed_bit) <> 0 then add_code 7;
        if flags land (1 lsl strikethrough_bit) <> 0 then add_code 9;
        if flags land (1 lsl overline_bit) <> 0 then add_code 53;
        if flags land 0x100 <> 0 then add_code 21 (* double underline *));

      let result =
        if !codes = [] then ""
        else
          let codes_str =
            String.concat ";" (List.rev_map string_of_int !codes)
          in
          Printf.sprintf "\027[%sm" codes_str
      in
      SGR_cache.put style prev_style result;
      result

(* Conversion for storage *)
let of_int64 i = i

let encode_color color =
  let color_type, color_data = encode_color_data color in
  Int64.logor color_type (Int64.shift_left color_data 2)

let decode_color v =
  let color_type = Int64.logand v 3L in
  let color_data = Int64.shift_right_logical v 2 in
  decode_color_data color_type color_data

(* Pretty printing *)
let pp_color ppf = function
  | Black -> Fmt.string ppf "Black"
  | Red -> Fmt.string ppf "Red"
  | Green -> Fmt.string ppf "Green"
  | Yellow -> Fmt.string ppf "Yellow"
  | Blue -> Fmt.string ppf "Blue"
  | Magenta -> Fmt.string ppf "Magenta"
  | Cyan -> Fmt.string ppf "Cyan"
  | White -> Fmt.string ppf "White"
  | Default -> Fmt.string ppf "Default"
  | Bright_black -> Fmt.string ppf "Bright_black"
  | Bright_red -> Fmt.string ppf "Bright_red"
  | Bright_green -> Fmt.string ppf "Bright_green"
  | Bright_yellow -> Fmt.string ppf "Bright_yellow"
  | Bright_blue -> Fmt.string ppf "Bright_blue"
  | Bright_magenta -> Fmt.string ppf "Bright_magenta"
  | Bright_cyan -> Fmt.string ppf "Bright_cyan"
  | Bright_white -> Fmt.string ppf "Bright_white"
  | Index i -> Fmt.pf ppf "Index(%d)" i
  | RGB (r, g, b) -> Fmt.pf ppf "RGB(%d,%d,%d)" r g b
  | RGBA (r, g, b, a) -> Fmt.pf ppf "RGBA(%d,%d,%d,%d)" r g b a

let pp_color_opt ppf = function
  | None -> Fmt.string ppf "None"
  | Some c -> pp_color ppf c

let pp ppf style =
  let fg, bg, link, flags = decode style in
  let attrs = [] in
  let attrs =
    if flags land (1 lsl bold_bit) <> 0 then "bold" :: attrs else attrs
  in
  let attrs =
    if flags land (1 lsl dim_bit) <> 0 then "dim" :: attrs else attrs
  in
  let attrs =
    if flags land (1 lsl italic_bit) <> 0 then "italic" :: attrs else attrs
  in
  let attrs =
    if flags land (1 lsl underline_bit) <> 0 then "underline" :: attrs
    else attrs
  in
  let attrs =
    if flags land (1 lsl blink_bit) <> 0 then "blink" :: attrs else attrs
  in
  let attrs =
    if flags land (1 lsl reversed_bit) <> 0 then "reversed" :: attrs else attrs
  in
  let attrs =
    if flags land (1 lsl strikethrough_bit) <> 0 then "strikethrough" :: attrs
    else attrs
  in
  let attrs =
    if flags land (1 lsl overline_bit) <> 0 then "overline" :: attrs else attrs
  in
  let attrs =
    if flags land 0x100 <> 0 then "double_underline" :: attrs else attrs
  in

  Fmt.pf ppf "@[<h>Style {fg=%a; bg=%a; link=%d; attrs=[%s]%s}@]" pp_color_opt
    fg pp_color_opt bg link (String.concat "; " attrs)
    (if is_extended style then " [extended]" else "")
