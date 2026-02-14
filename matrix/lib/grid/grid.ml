(* grid.ml *)

module Buf = struct
  type ('a, 'b) t = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t

  let create kind len = Bigarray.Array1.create kind Bigarray.c_layout len
  let make_float len = create Bigarray.float32 len
  let make_int16 len = create Bigarray.int16_unsigned len
  let make_int32 len = create Bigarray.int32 len
  let make_int len = create Bigarray.int len

  (* Unsafe accessors for performance (bounds checked at Grid level) *)
  let[@inline] get arr i = Bigarray.Array1.unsafe_get arr i
  let[@inline] set arr i v = Bigarray.Array1.unsafe_set arr i v
  let fill = Bigarray.Array1.fill
  let blit = Bigarray.Array1.blit
  let sub = Bigarray.Array1.sub
  let dim = Bigarray.Array1.dim
end

module Color_plane = struct
  let channels = 4
  let[@inline] get arr idx off = Buf.get arr ((idx * channels) + off)
  let[@inline] set arr idx off v = Buf.set arr ((idx * channels) + off) v

  let clamp v =
    let v = v *. 255. |> Float.round |> int_of_float in
    max 0 (min 255 v)

  let[@inline] perceptual_alpha a =
    if a >= 0.8 then
      let norm = (a -. 0.8) *. 5. in
      0.8 +. (Float.pow norm 0.2 *. 0.2)
    else Float.pow a 0.9

  let[@inline] mix src dst alpha = (src *. alpha) +. (dst *. (1. -. alpha))

  (* IO operations *)
  let read_rgba arr idx =
    let base = idx * channels in
    ( Buf.get arr base,
      Buf.get arr (base + 1),
      Buf.get arr (base + 2),
      Buf.get arr (base + 3) )

  let equal_eps ?(eps = 0.00001) arr1 idx1 arr2 idx2 =
    let b1 = idx1 * channels in
    let b2 = idx2 * channels in
    Float.abs (Buf.get arr1 b1 -. Buf.get arr2 b2) < eps
    && Float.abs (Buf.get arr1 (b1 + 1) -. Buf.get arr2 (b2 + 1)) < eps
    && Float.abs (Buf.get arr1 (b1 + 2) -. Buf.get arr2 (b2 + 2)) < eps
    && Float.abs (Buf.get arr1 (b1 + 3) -. Buf.get arr2 (b2 + 3)) < eps
end

module Rect = struct
  type t = { x : int; y : int; width : int; height : int }

  let intersection a b =
    let x = max a.x b.x in
    let y = max a.y b.y in
    let w = min (a.x + a.width) (b.x + b.width) - x in
    let h = min (a.y + a.height) (b.y + b.height) - y in
    if w > 0 && h > 0 then Some { x; y; width = w; height = h } else None

  let clip_to_bounds max_w max_h r =
    let x = max 0 r.x in
    let y = max 0 r.y in
    let w = min (r.x + r.width) max_w - x in
    let h = min (r.y + r.height) max_h - y in
    if w > 0 && h > 0 then Some { x; y; width = w; height = h } else None
end

module Scissor_stack = struct
  type t = Rect.t Dynarray.t

  let create_scissor_stack () = Dynarray.create ()
  let current s = if Dynarray.is_empty s then None else Dynarray.find_last s

  let push s r =
    let r =
      match current s with
      | None -> r
      | Some c -> (
          match Rect.intersection c r with
          | Some i -> i
          | None -> Rect.{ x = 0; y = 0; width = 0; height = 0 })
    in
    Dynarray.add_last s r

  let pop s = ignore (Dynarray.pop_last_opt s)
  let clear s = Dynarray.clear s

  let is_contained rect_opt ~x ~y =
    let open Rect in
    match rect_opt with
    | None -> true
    | Some r -> x >= r.x && y >= r.y && x < r.x + r.width && y < r.y + r.height
end

module Cell_code = struct
  (* Bit Layout (aligned with Glyph.t for direct compatibility): bit 30:
     grapheme flag (0 = simple/scalar, 1 = complex grapheme) bit 29:
     continuation flag (0 = start, 1 = continuation) bits 27-28: right_extent
     (width - 1 for start, distance to end for cont) bits 25-26: left_extent
     (distance to start for continuation cells) bits 0-24: payload (Unicode
     scalar for simple, or Glyph pool data for complex)

     This layout matches Glyph.t exactly, allowing direct storage of glyph
     values without transformation. Simple ASCII glyphs are stored as-is (no
     flags set). Complex glyphs from Glyph.encode/intern can be stored
     directly. *)

  let flag_grapheme = 0x40000000 (* bit 30 *)
  let flag_complex_cont = 0x60000000 (* bits 30 + 29: complex continuation *)
  let mask_right_ext = 0x18000000 (* bits 27-28 *)
  let shift_right_ext = 27
  let mask_left_ext = 0x06000000 (* bits 25-26 *)
  let shift_left_ext = 25
  let mask_payload = 0x01FFFFFF (* bits 0-24 *)
  let empty = 0 (* Null/Empty cell *)
  let space = 32 (* ASCII Space *)

  (* Predicates - aligned with Glyph.is_simple, etc. *)
  let[@inline] is_simple c = c land flag_grapheme = 0
  let[@inline] is_complex c = c land flag_grapheme <> 0
  let[@inline] is_continuation c = c land flag_complex_cont = flag_complex_cont
  let[@inline] is_start c = is_complex c && not (is_continuation c)

  (* Extraction *)
  let[@inline] payload c = c land mask_payload

  let[@inline] width c =
    if is_simple c then 1
    else if is_continuation c then 0
    else
      let w_minus_1 = (c land mask_right_ext) lsr shift_right_ext in
      w_minus_1 + 1

  (* Extent extraction for O(1) cleanup *)
  let[@inline] left_extent c = (c land mask_left_ext) lsr shift_left_ext
  let[@inline] right_extent c = (c land mask_right_ext) lsr shift_right_ext

  (* Construction - for continuation cells created during draw_text. Start cells
     can now use the Glyph.t value directly. *)
  let make_cont ~id ~left ~right =
    let l_enc = min 3 left in
    let r_enc = min 3 right in
    flag_complex_cont lor (id land mask_payload) lor (l_enc lsl shift_left_ext)
    lor (r_enc lsl shift_right_ext)
end

module Links = struct
  type t = {
    mutable next_id : int32;
    forward : (string, int32) Hashtbl.t;
    reverse : (int32, string) Hashtbl.t;
  }

  let create () =
    { next_id = 1l; forward = Hashtbl.create 32; reverse = Hashtbl.create 32 }

  let no_link = -1l

  let clear t =
    t.next_id <- 1l;
    Hashtbl.clear t.forward;
    Hashtbl.clear t.reverse

  let copy_state ~src ~dst =
    clear dst;
    dst.next_id <- src.next_id;
    Hashtbl.iter (fun k v -> Hashtbl.add dst.forward k v) src.forward;
    Hashtbl.iter (fun k v -> Hashtbl.add dst.reverse k v) src.reverse

  let intern t url_opt =
    match url_opt with
    | None -> no_link
    | Some url -> (
        match Hashtbl.find_opt t.forward url with
        | Some id -> id
        | None ->
            let id = t.next_id in
            t.next_id <- Int32.add id 1l;
            Hashtbl.add t.forward url id;
            Hashtbl.add t.reverse id url;
            id)

  let resolve t id = Hashtbl.find_opt t.reverse id
  let resolve_direct t id = try Hashtbl.find t.reverse id with Not_found -> ""
end

module Border = Border

type clip_rect = Rect.t = { x : int; y : int; width : int; height : int }

type t = {
  mutable width : int;
  mutable height : int;
  (* Configuration *)
  glyph_pool : Glyph.pool;
  mutable width_method : Glyph.width_method;
  mutable respect_alpha : bool;
  (* Storage *)
  mutable chars : (int, Bigarray.int_elt) Buf.t;
  mutable fg : (float, Bigarray.float32_elt) Buf.t;
  mutable bg : (float, Bigarray.float32_elt) Buf.t;
  mutable attrs : (int, Bigarray.int16_unsigned_elt) Buf.t;
  mutable links : (int32, Bigarray.int32_elt) Buf.t;
  (* Subsystems *)
  link_registry : Links.t;
  grapheme_tracker : Grapheme_tracker.t;
  scissor_stack : Scissor_stack.t;
}

(* ---- Constants ---- *)

let null_cell = Cell_code.empty
let space_cell = Cell_code.space
let no_link = Links.no_link

(* ---- Initialization & Lifecycle ---- *)

let fill_defaults t =
  Grapheme_tracker.clear t.grapheme_tracker;
  Links.clear t.link_registry;
  Scissor_stack.clear t.scissor_stack;

  Buf.fill t.chars space_cell;
  Buf.fill t.attrs 0;
  Buf.fill t.links no_link;
  Buf.fill t.fg 1.0;
  Buf.fill t.bg 0.0

let create ~width ~height ?glyph_pool ?width_method ?(respect_alpha = false) ()
    =
  if width <= 0 || height <= 0 then
    invalid_arg "Grid.create: width and height must be > 0";

  let size = width * height in
  let pool = Option.value glyph_pool ~default:(Glyph.create_pool ()) in
  let w_method = Option.value width_method ~default:`Unicode in

  let t =
    {
      width;
      height;
      glyph_pool = pool;
      width_method = w_method;
      respect_alpha;
      chars = Buf.make_int size;
      fg = Buf.make_float (size * 4);
      bg = Buf.make_float (size * 4);
      attrs = Buf.make_int16 size;
      links = Buf.make_int32 size;
      link_registry = Links.create ();
      grapheme_tracker = Grapheme_tracker.create pool;
      scissor_stack = Scissor_stack.create_scissor_stack ();
    }
  in
  fill_defaults t;
  t

(* ---- Accessors ---- *)

let width t = t.width
let height t = t.height
let glyph_pool t = t.glyph_pool
let width_method t = t.width_method
let set_width_method t m = t.width_method <- m
let respect_alpha t = t.respect_alpha
let set_respect_alpha t b = t.respect_alpha <- b

let hyperlink_url t id =
  if id = no_link then None else Links.resolve t.link_registry id

let hyperlink_url_direct t id =
  if id = no_link then "" else Links.resolve_direct t.link_registry id

(* ---- Cell Accessors ---- *)

(* Cell_code is aligned with Glyph.t, so cell codes are valid Glyph.t values *)
let[@inline] get_code t idx = Buf.get t.chars idx
let[@inline] get_glyph t idx = Buf.get t.chars idx
let[@inline] get_attrs t idx = Buf.get t.attrs idx
let[@inline] get_link t idx = Buf.get t.links idx
let[@inline] get_fg_r t idx = Color_plane.get t.fg idx 0
let[@inline] get_fg_g t idx = Color_plane.get t.fg idx 1
let[@inline] get_fg_b t idx = Color_plane.get t.fg idx 2
let[@inline] get_fg_a t idx = Color_plane.get t.fg idx 3
let[@inline] get_bg_r t idx = Color_plane.get t.bg idx 0
let[@inline] get_bg_g t idx = Color_plane.get t.bg idx 1
let[@inline] get_bg_b t idx = Color_plane.get t.bg idx 2
let[@inline] get_bg_a t idx = Color_plane.get t.bg idx 3

let get_style t idx =
  let attrs = Ansi.Attr.unpack (Buf.get t.attrs idx) in
  let link_id = Buf.get t.links idx in
  let link = hyperlink_url t link_id in

  let to_color plane idx =
    let r = Color_plane.get plane idx 0 in
    let g = Color_plane.get plane idx 1 in
    let b = Color_plane.get plane idx 2 in
    let a = Color_plane.get plane idx 3 in
    (* Use Color_plane.clamp for consistent rounding/clamping with
       get_background *)
    Ansi.Color.of_rgba (Color_plane.clamp r) (Color_plane.clamp g)
      (Color_plane.clamp b) (Color_plane.clamp a)
  in

  Ansi.Style.make ~fg:(to_color t.fg idx) ~bg:(to_color t.bg idx) ?link ()
  |> Ansi.Style.with_attrs attrs

let get_background t idx =
  let r, g, b, a = Color_plane.read_rgba t.bg idx in
  Ansi.Color.of_rgba (Color_plane.clamp r) (Color_plane.clamp g)
    (Color_plane.clamp b) (Color_plane.clamp a)

let get_text t idx =
  (* Cell_code is aligned with Glyph.t, so cell codes can be passed directly to
     Glyph.to_string *)
  let c = Buf.get t.chars idx in
  if Cell_code.is_continuation c then "" else Glyph.to_string t.glyph_pool c

let is_empty t idx = Buf.get t.chars idx = Cell_code.empty
let is_continuation t idx = Cell_code.is_continuation (Buf.get t.chars idx)
let is_simple t idx = Cell_code.is_simple (Buf.get t.chars idx)
let cell_width t idx = Cell_code.width (Buf.get t.chars idx)

let[@inline] cells_equal t1 idx1 t2 idx2 =
  Buf.get t1.chars idx1 = Buf.get t2.chars idx2
  && Buf.get t1.attrs idx1 = Buf.get t2.attrs idx2
  && Buf.get t1.links idx1 = Buf.get t2.links idx2
  && Color_plane.equal_eps t1.fg idx1 t2.fg idx2
  && Color_plane.equal_eps t1.bg idx1 t2.bg idx2

(* ---- Grapheme Cleanup ---- *)

let cleanup_grapheme_at t idx =
  let code = Buf.get t.chars idx in

  if Cell_code.is_simple code then ()
  else
    let left = Cell_code.left_extent code in
    let right = Cell_code.right_extent code in

    if left = 0 && right = 0 then ()
    else
      let start_idx = idx - left in
      let end_idx = idx + right in
      let limit = Buf.dim t.chars in

      if start_idx >= 0 && end_idx < limit then
        for i = start_idx to end_idx do
          if i <> idx then
            let neighbor_code = Buf.get t.chars i in
            if neighbor_code <> space_cell then (
              Grapheme_tracker.remove t.grapheme_tracker neighbor_code;
              Buf.set t.chars i space_cell;
              Buf.set t.attrs i 0;
              Buf.set t.links i no_link)
        done

(* ---- Lifecycle: Clear & Resize ---- *)

let clear ?color t =
  Grapheme_tracker.clear t.grapheme_tracker;
  Links.clear t.link_registry;

  Buf.fill t.chars space_cell;
  Buf.fill t.attrs 0;
  Buf.fill t.links no_link;
  Buf.fill t.fg 1.0;

  let br, bg, bb, ba =
    match color with
    | None -> (0., 0., 0., 0.)
    | Some c -> Ansi.Color.to_rgba_f c
  in
  let len = t.width * t.height in
  for i = 0 to len - 1 do
    Color_plane.set t.bg i 0 br;
    Color_plane.set t.bg i 1 bg;
    Color_plane.set t.bg i 2 bb;
    Color_plane.set t.bg i 3 ba
  done

let resize t ~width ~height =
  if width <= 0 || height <= 0 then
    invalid_arg "Grid.resize: width and height must be > 0";

  if width = t.width && height = t.height then ()
  else
    let old_width = t.width in
    let old_height = t.height in
    let old_chars = t.chars in
    let old_attrs = t.attrs in
    let old_links = t.links in
    let old_fg = t.fg in
    let old_bg = t.bg in

    if width < old_width || height < old_height then
      for y = 0 to old_height - 1 do
        for x = 0 to old_width - 1 do
          if x >= width || y >= height then
            let idx = (y * old_width) + x in
            let code = Buf.get old_chars idx in
            Grapheme_tracker.remove t.grapheme_tracker code
        done
      done;

    (* When shrinking width, drop any grapheme starts whose span crosses the new
       right edge to avoid dangling extents into the next row. *)
    (if width < old_width then
       let max_row = min (old_height - 1) (height - 1) in
       for y = 0 to max_row do
         let row_start = y * old_width in
         for x = 0 to width - 1 do
           let idx = row_start + x in
           let code = Buf.get old_chars idx in
           if Cell_code.is_start code then
             let span = Cell_code.width code in
             if span > width - x then (
               cleanup_grapheme_at t idx;
               Grapheme_tracker.remove t.grapheme_tracker code;
               Buf.set old_chars idx space_cell;
               Buf.set old_attrs idx 0;
               Buf.set old_links idx no_link;
               Color_plane.set old_fg idx 0 1.0;
               Color_plane.set old_fg idx 1 1.0;
               Color_plane.set old_fg idx 2 1.0;
               Color_plane.set old_fg idx 3 1.0;
               Color_plane.set old_bg idx 0 0.0;
               Color_plane.set old_bg idx 1 0.0;
               Color_plane.set old_bg idx 2 0.0;
               Color_plane.set old_bg idx 3 0.0)
         done
       done);

    let new_size = width * height in
    let new_chars = Buf.make_int new_size in
    let new_attrs = Buf.make_int16 new_size in
    let new_links = Buf.make_int32 new_size in
    let new_fg = Buf.make_float (new_size * 4) in
    let new_bg = Buf.make_float (new_size * 4) in

    Buf.fill new_chars space_cell;
    Buf.fill new_attrs 0;
    Buf.fill new_links no_link;
    Buf.fill new_fg 1.0;
    Buf.fill new_bg 0.0;

    let copy_w = min old_width width in
    let copy_h = min old_height height in

    for y = 0 to copy_h - 1 do
      let src_start = y * old_width in
      let dst_start = y * width in
      Buf.blit
        (Buf.sub old_chars src_start copy_w)
        (Buf.sub new_chars dst_start copy_w);
      Buf.blit
        (Buf.sub old_attrs src_start copy_w)
        (Buf.sub new_attrs dst_start copy_w);
      Buf.blit
        (Buf.sub old_links src_start copy_w)
        (Buf.sub new_links dst_start copy_w);
      Buf.blit
        (Buf.sub old_fg (src_start * 4) (copy_w * 4))
        (Buf.sub new_fg (dst_start * 4) (copy_w * 4));
      Buf.blit
        (Buf.sub old_bg (src_start * 4) (copy_w * 4))
        (Buf.sub new_bg (dst_start * 4) (copy_w * 4))
    done;

    t.width <- width;
    t.height <- height;
    t.chars <- new_chars;
    t.attrs <- new_attrs;
    t.links <- new_links;
    t.fg <- new_fg;
    t.bg <- new_bg

(* ---- Scissor Operations ---- *)

let push_scissor t rect = Scissor_stack.push t.scissor_stack rect
let pop_scissor t = Scissor_stack.pop t.scissor_stack
let clear_scissor t = Scissor_stack.clear t.scissor_stack

let with_scissor t rect f =
  push_scissor t rect;
  Fun.protect ~finally:(fun () -> pop_scissor t) f

let[@inline] is_clipped t x y =
  let scissor = Scissor_stack.current t.scissor_stack in
  not (Scissor_stack.is_contained scissor ~x ~y)

(* ---- Core Cell Writing ---- *)

(* Zero-alloc core cell writer. Takes unpacked colors to avoid tuple allocation
   in hot loops. *)
let set_cell_internal t ~idx ~code ~fg_r ~fg_g ~fg_b ~fg_a ~bg_r ~bg_g ~bg_b
    ~bg_a ~attrs ~link_id ~blending =
  if blending && (bg_a < 0.999 || fg_a < 0.999) then (
    (* Blending Path *)
    (* Flattened read of destination colors *)
    let dr_bg = Color_plane.get t.bg idx 0 in
    let dg_bg = Color_plane.get t.bg idx 1 in
    let db_bg = Color_plane.get t.bg idx 2 in
    let da_bg = Color_plane.get t.bg idx 3 in

    let dest_code = Buf.get t.chars idx in
    let overlay_is_space = code = space_cell || code = null_cell in
    let dest_has_content = dest_code <> null_cell && dest_code <> space_cell in
    let preserve =
      overlay_is_space && dest_has_content && Cell_code.width dest_code = 1
    in

    (if preserve then (
       if
         (* Preserve existing glyph/attrs/link; tint foreground over it *)
         bg_a >= 0.999
       then (
         (* Opaque overlay: replace fg color entirely *)
         Color_plane.set t.fg idx 0 bg_r;
         Color_plane.set t.fg idx 1 bg_g;
         Color_plane.set t.fg idx 2 bg_b)
       else if bg_a > 0.001 then (
         let dr_fg = Color_plane.get t.fg idx 0 in
         let dg_fg = Color_plane.get t.fg idx 1 in
         let db_fg = Color_plane.get t.fg idx 2 in
         let pa = Color_plane.perceptual_alpha bg_a in
         Color_plane.set t.fg idx 0 (Color_plane.mix bg_r dr_fg pa);
         Color_plane.set t.fg idx 1 (Color_plane.mix bg_g dg_fg pa);
         Color_plane.set t.fg idx 2 (Color_plane.mix bg_b db_fg pa)
         (* else transparent: keep existing fg *)))
     else
       (* Normal blended overwrite of glyph & style *)
       let old_code = dest_code in
       if old_code <> code then (
         let old_simple = Cell_code.is_simple old_code in
         let new_simple = Cell_code.is_simple code in
         if not old_simple then cleanup_grapheme_at t idx;
         (* Update grapheme tracker only when complex graphemes are involved *)
         (match (old_simple, new_simple) with
         | true, true -> ()
         | true, false -> Grapheme_tracker.add t.grapheme_tracker code
         | false, true -> Grapheme_tracker.remove t.grapheme_tracker old_code
         | false, false ->
             Grapheme_tracker.replace t.grapheme_tracker ~old_id:old_code
               ~new_id:code);
         Buf.set t.chars idx code);
       Buf.set t.attrs idx attrs;
       Buf.set t.links idx link_id;

       (* Blend incoming FG over destination BG *)
       if fg_a < 0.999 then (
         let pa = Color_plane.perceptual_alpha fg_a in
         Color_plane.set t.fg idx 0 (Color_plane.mix fg_r dr_bg pa);
         Color_plane.set t.fg idx 1 (Color_plane.mix fg_g dg_bg pa);
         Color_plane.set t.fg idx 2 (Color_plane.mix fg_b db_bg pa);
         Color_plane.set t.fg idx 3 da_bg)
       else (
         Color_plane.set t.fg idx 0 fg_r;
         Color_plane.set t.fg idx 1 fg_g;
         Color_plane.set t.fg idx 2 fg_b;
         Color_plane.set t.fg idx 3 fg_a));

    (* Always blend BG over dest BG *)
    if bg_a <= 0.001 then
      (* Transparent: preserve dest BG *)
      ()
    else if bg_a >= 0.999 then (
      (* Opaque: direct overwrite, no blending *)
      Color_plane.set t.bg idx 0 bg_r;
      Color_plane.set t.bg idx 1 bg_g;
      Color_plane.set t.bg idx 2 bg_b;
      Color_plane.set t.bg idx 3 bg_a)
    else
      let pa = Color_plane.perceptual_alpha bg_a in
      Color_plane.set t.bg idx 0 (Color_plane.mix bg_r dr_bg pa);
      Color_plane.set t.bg idx 1 (Color_plane.mix bg_g dg_bg pa);
      Color_plane.set t.bg idx 2 (Color_plane.mix bg_b db_bg pa);
      Color_plane.set t.bg idx 3 bg_a)
  else
    (* Fast Path: Opaque Overwrite *)
    let old_code = Buf.get t.chars idx in
    if old_code <> code then (
      let old_simple = Cell_code.is_simple old_code in
      let new_simple = Cell_code.is_simple code in
      if not old_simple then cleanup_grapheme_at t idx;
      (* Only touch grapheme tracker when complex graphemes are involved. Simple
         -> simple (ASCII etc.) is now tracker-free. *)
      (match (old_simple, new_simple) with
      | true, true -> ()
      | true, false -> Grapheme_tracker.add t.grapheme_tracker code
      | false, true -> Grapheme_tracker.remove t.grapheme_tracker old_code
      | false, false ->
          Grapheme_tracker.replace t.grapheme_tracker ~old_id:old_code
            ~new_id:code);
      Buf.set t.chars idx code);
    Buf.set t.attrs idx attrs;
    Buf.set t.links idx link_id;
    Color_plane.set t.fg idx 0 fg_r;
    Color_plane.set t.fg idx 1 fg_g;
    Color_plane.set t.fg idx 2 fg_b;
    Color_plane.set t.fg idx 3 fg_a;
    Color_plane.set t.bg idx 0 bg_r;
    Color_plane.set t.bg idx 1 bg_g;
    Color_plane.set t.bg idx 2 bg_b;
    Color_plane.set t.bg idx 3 bg_a

(* Public Wrappers *)

let set_cell t ~x ~y ~code ~fg ~bg ~attrs ?link () =
  if x >= 0 && y >= 0 && x < t.width && y < t.height && not (is_clipped t x y)
  then
    let idx = (y * t.width) + x in
    let fg_r, fg_g, fg_b, fg_a = Ansi.Color.to_rgba_f fg in
    let bg_r, bg_g, bg_b, bg_a = Ansi.Color.to_rgba_f bg in
    let link_id = Links.intern t.link_registry link in
    let attrs_packed = Ansi.Attr.pack attrs in
    set_cell_internal t ~idx ~code ~fg_r ~fg_g ~fg_b ~fg_a ~bg_r ~bg_g ~bg_b
      ~bg_a ~attrs:attrs_packed ~link_id ~blending:t.respect_alpha

let set_cell_alpha t ~x ~y ~code ~fg ~bg ~attrs ?link () =
  if x >= 0 && y >= 0 && x < t.width && y < t.height && not (is_clipped t x y)
  then
    let idx = (y * t.width) + x in
    let fg_r, fg_g, fg_b, fg_a = Ansi.Color.to_rgba_f fg in
    let bg_r, bg_g, bg_b, bg_a = Ansi.Color.to_rgba_f bg in
    let link_id = Links.intern t.link_registry link in
    let attrs_packed = Ansi.Attr.pack attrs in
    set_cell_internal t ~idx ~code ~fg_r ~fg_g ~fg_b ~fg_a ~bg_r ~bg_g ~bg_b
      ~bg_a ~attrs:attrs_packed ~link_id ~blending:true

(* ---- Bulk Operations ---- *)

let fill_rect t ~x ~y ~width ~height ~color =
  let open Rect in
  let target = { x; y; width; height } in
  let scissor = Scissor_stack.current t.scissor_stack in
  let clipped =
    match scissor with
    | None -> clip_to_bounds t.width t.height target
    | Some s -> (
        match intersection target s with
        | None -> None
        | Some i -> clip_to_bounds t.width t.height i)
  in

  match clipped with
  | None -> ()
  | Some r ->
      let bg_r, bg_g, bg_b, bg_a = Ansi.Color.to_rgba_f color in
      let x_start = r.Rect.x in
      let row_len = r.Rect.width in
      let y_start = r.Rect.y in
      let y_end = y_start + r.Rect.height - 1 in

      if bg_a <= 0.001 then begin
        (* Fully transparent fills: clear characters/attrs but keep
           background. *)
        let fg_r, fg_g, fg_b, fg_a = (1.0, 1.0, 1.0, 1.0) in
        let attrs = 0 in
        let link_id = no_link in
        for row = y_start to y_end do
          let base_idx = (row * t.width) + x_start in
          for dx = 0 to row_len - 1 do
            let idx = base_idx + dx in
            let cur_bg_r = Color_plane.get t.bg idx 0 in
            let cur_bg_g = Color_plane.get t.bg idx 1 in
            let cur_bg_b = Color_plane.get t.bg idx 2 in
            let cur_bg_a = Color_plane.get t.bg idx 3 in
            set_cell_internal t ~idx ~code:space_cell ~fg_r ~fg_g ~fg_b ~fg_a
              ~bg_r:cur_bg_r ~bg_g:cur_bg_g ~bg_b:cur_bg_b ~bg_a:cur_bg_a ~attrs
              ~link_id ~blending:false
          done
        done
      end
      else if bg_a < 0.999 then begin
        (* Semi-transparent fill: per-cell alpha blending without
           allocations. *)
        let fg_r, fg_g, fg_b, fg_a = Ansi.Color.to_rgba_f Ansi.Color.white in
        let attrs = Ansi.Attr.pack Ansi.Attr.empty in
        let link_id = no_link in
        for row = y_start to y_end do
          let base_idx = (row * t.width) + x_start in
          for dx = 0 to row_len - 1 do
            let idx = base_idx + dx in
            set_cell_internal t ~idx ~code:space_cell ~fg_r ~fg_g ~fg_b ~fg_a
              ~bg_r ~bg_g ~bg_b ~bg_a ~attrs ~link_id ~blending:true
          done
        done
      end
      else begin
        (* Fast Path: Bulk fill with Bigarray operations *)
        let code = space_cell in
        let link = no_link in
        let attrs = 0 in
        let has_complex =
          Grapheme_tracker.unique_count t.grapheme_tracker > 0
        in
        for row = y_start to y_end do
          let start_idx = (row * t.width) + x_start in
          let end_idx = start_idx + row_len - 1 in

          (* 1. Grapheme Cleanup: Only scan if we know complex chars exist *)
          if has_complex then
            for i = start_idx to end_idx do
              let old = Buf.get t.chars i in
              if old <> code && not (Cell_code.is_simple old) then (
                cleanup_grapheme_at t i;
                Grapheme_tracker.remove t.grapheme_tracker old)
            done;

          (* 2. Bulk fill primitives *)
          Buf.fill (Buf.sub t.chars start_idx row_len) code;
          Buf.fill (Buf.sub t.attrs start_idx row_len) attrs;
          Buf.fill (Buf.sub t.links start_idx row_len) link;

          (* 3. Set Colors: foreground to white, background to [color] *)
          for i = start_idx to end_idx do
            Color_plane.set t.fg i 0 1.0;
            Color_plane.set t.fg i 1 1.0;
            Color_plane.set t.fg i 2 1.0;
            Color_plane.set t.fg i 3 1.0;
            Color_plane.set t.bg i 0 bg_r;
            Color_plane.set t.bg i 1 bg_g;
            Color_plane.set t.bg i 2 bg_b;
            Color_plane.set t.bg i 3 bg_a
          done
        done
      end

let blit ~src ~dst =
  (* Self-blit is a no-op: blitting a grid to itself would corrupt grapheme
     tracker state since we clear dst.grapheme_tracker while src still
     references those glyphs. *)
  if src == dst then ()
  else (
    resize dst ~width:src.width ~height:src.height;
    dst.width_method <- src.width_method;
    dst.respect_alpha <- src.respect_alpha;

    Buf.blit src.fg dst.fg;
    Buf.blit src.bg dst.bg;
    Buf.blit src.attrs dst.attrs;

    if src.glyph_pool == dst.glyph_pool then (
      Buf.blit src.chars dst.chars;
      Buf.blit src.links dst.links;
      Links.copy_state ~src:src.link_registry ~dst:dst.link_registry;
      Grapheme_tracker.clear dst.grapheme_tracker;
      let len = src.width * src.height in
      for i = 0 to len - 1 do
        let c = Buf.get dst.chars i in
        if Cell_code.is_complex c then
          Grapheme_tracker.add dst.grapheme_tracker c
      done)
    else (
      (* Cross-pool blit: need to copy grapheme data between pools. Cell_code is
         aligned with Glyph.t, so cell codes can be passed directly to
         Glyph.copy and results stored directly. *)
      Grapheme_tracker.clear dst.grapheme_tracker;
      Links.clear dst.link_registry;
      let cache = Hashtbl.create 64 in
      let len = src.width * src.height in

      for i = 0 to len - 1 do
        let src_c = Buf.get src.chars i in
        let dst_c =
          if Cell_code.is_simple src_c then src_c
          else
            (* Cache by payload to avoid re-copying identical graphemes *)
            let src_payload = Cell_code.payload src_c in
            match Hashtbl.find_opt cache src_payload with
            | Some dst_glyph ->
                (* Reconstruct cell code with cached glyph and original
                   extents *)
                if Cell_code.is_start src_c then dst_glyph
                else
                  Cell_code.make_cont
                    ~id:(Cell_code.payload dst_glyph)
                    ~left:(Cell_code.left_extent src_c)
                    ~right:(Cell_code.right_extent src_c)
            | None ->
                (* Copy glyph to destination pool - src_c is a valid Glyph.t *)
                let dst_glyph =
                  Glyph.copy ~src:src.glyph_pool src_c ~dst:dst.glyph_pool
                in
                Hashtbl.add cache src_payload dst_glyph;
                if Cell_code.is_start src_c then dst_glyph
                else
                  Cell_code.make_cont
                    ~id:(Cell_code.payload dst_glyph)
                    ~left:(Cell_code.left_extent src_c)
                    ~right:(Cell_code.right_extent src_c)
        in
        Buf.set dst.chars i dst_c;
        if Cell_code.is_complex dst_c then
          Grapheme_tracker.add dst.grapheme_tracker dst_c;

        let src_l = Buf.get src.links i in
        let dst_l =
          match Links.resolve src.link_registry src_l with
          | None -> no_link
          | Some url -> Links.intern dst.link_registry (Some url)
        in
        Buf.set dst.links i dst_l
      done))

let copy t =
  let dst =
    create ~width:t.width ~height:t.height ~glyph_pool:t.glyph_pool
      ~width_method:t.width_method ~respect_alpha:t.respect_alpha ()
  in
  blit ~src:t ~dst;
  dst

(* Bulk update graphemes: Optimized to skip if tracker is empty *)
let bulk_update_graphemes t ~src_idx ~dst_idx ~len =
  if Grapheme_tracker.unique_count t.grapheme_tracker > 0 then (
    (* 1. Decrement counts for the region being overwritten (dst) *)
    for i = 0 to len - 1 do
      let code = Buf.get t.chars (dst_idx + i) in
      if not (Cell_code.is_simple code) then
        Grapheme_tracker.remove t.grapheme_tracker code
    done;
    (* 2. Increment counts for the region being copied (src) *)
    for i = 0 to len - 1 do
      let code = Buf.get t.chars (src_idx + i) in
      if not (Cell_code.is_simple code) then
        Grapheme_tracker.add t.grapheme_tracker code
    done)

let blit_region ~src ~dst ~src_x ~src_y ~width ~height ~dst_x ~dst_y =
  let width = max 0 width in
  let height = max 0 height in

  let clipped_src_x, clipped_dst_x, width =
    if src_x < 0 then (0, dst_x - src_x, width + src_x)
    else (src_x, dst_x, width)
  in
  let clipped_src_y, clipped_dst_y, height =
    if src_y < 0 then (0, dst_y - src_y, height + src_y)
    else (src_y, dst_y, height)
  in
  let final_src_x, final_dst_x, width =
    if clipped_dst_x < 0 then
      (clipped_src_x - clipped_dst_x, 0, width + clipped_dst_x)
    else (clipped_src_x, clipped_dst_x, width)
  in
  let final_src_y, final_dst_y, height =
    if clipped_dst_y < 0 then
      (clipped_src_y - clipped_dst_y, 0, height + clipped_dst_y)
    else (clipped_src_y, clipped_dst_y, height)
  in
  let width =
    min width (min (src.width - final_src_x) (dst.width - final_dst_x))
  in
  let height =
    min height (min (src.height - final_src_y) (dst.height - final_dst_y))
  in

  if width > 0 && height > 0 then
    let open Rect in
    let target = { x = final_dst_x; y = final_dst_y; width; height } in
    let clipped =
      match Scissor_stack.current dst.scissor_stack with
      | None -> Some target
      | Some scissor -> intersection target scissor
    in
    match clipped with
    | None -> ()
    | Some clipped ->
        let dst_x = clipped.x in
        let dst_y = clipped.y in
        let width = clipped.width in
        let height = clipped.height in
        let src_x = final_src_x + (dst_x - final_dst_x) in
        let src_y = final_src_y + (dst_y - final_dst_y) in
        let same_grid = src == dst in
        let y_start, y_limit, y_step =
          if same_grid && src_y < dst_y then (height - 1, -1, -1)
          else (0, height, 1)
        in

        (* Fast path is only safe for full-width blits (scrolling). Partial
           width blits may copy continuation cells without their start cells,
           creating orphan continuations. The slow path handles this via the
           is_orphan check that converts orphaned continuations to spaces. *)
        let use_fast_path =
          same_grid && src_x = 0 && dst_x = 0 && width = src.width
        in
        if use_fast_path then
          (* Fast Path: Full-width Self-Blit (Scrolling) *)
          let i = ref y_start in
          while !i <> y_limit do
            let sy = src_y + !i in
            let dy = dst_y + !i in
            let src_idx = sy * src.width in
            let dst_idx = dy * dst.width in

            bulk_update_graphemes src ~src_idx ~dst_idx ~len:width;

            Buf.blit
              (Buf.sub src.chars src_idx width)
              (Buf.sub dst.chars dst_idx width);
            Buf.blit
              (Buf.sub src.attrs src_idx width)
              (Buf.sub dst.attrs dst_idx width);
            Buf.blit
              (Buf.sub src.links src_idx width)
              (Buf.sub dst.links dst_idx width);
            Buf.blit
              (Buf.sub src.fg (src_idx * 4) (width * 4))
              (Buf.sub dst.fg (dst_idx * 4) (width * 4));
            Buf.blit
              (Buf.sub src.bg (src_idx * 4) (width * 4))
              (Buf.sub dst.bg (dst_idx * 4) (width * 4));

            i := !i + y_step
          done
        else
          (* Slow Path: Cross-Grid / General Blit Cell_code is aligned with
             Glyph.t, so cell codes can be passed directly to Glyph.copy and
             results stored directly. *)
          let grapheme_map = Hashtbl.create 64 in
          let copy_glyph code =
            (* Cache by payload to avoid re-copying identical graphemes *)
            let payload = Cell_code.payload code in
            match Hashtbl.find_opt grapheme_map payload with
            | Some dst_glyph ->
                if Cell_code.is_start code then dst_glyph
                else
                  Cell_code.make_cont
                    ~id:(Cell_code.payload dst_glyph)
                    ~left:(Cell_code.left_extent code)
                    ~right:(Cell_code.right_extent code)
            | None ->
                (* code is a valid Glyph.t due to aligned formats *)
                let dst_glyph = Glyph.copy ~src:src.glyph_pool code ~dst:dst.glyph_pool in
                Hashtbl.add grapheme_map payload dst_glyph;
                if Cell_code.is_start code then dst_glyph
                else
                  Cell_code.make_cont
                    ~id:(Cell_code.payload dst_glyph)
                    ~left:(Cell_code.left_extent code)
                    ~right:(Cell_code.right_extent code)
          in

          let x_start, x_limit, x_step =
            if same_grid && src_x < dst_x then (width - 1, -1, -1)
            else (0, width, 1)
          in
          let i = ref y_start in
          while !i <> y_limit do
            let sy = src_y + !i in
            let dy = dst_y + !i in
            let k = ref x_start in
            while !k <> x_limit do
              let sx = src_x + !k in
              let dx = dst_x + !k in
              let sidx = (sy * src.width) + sx in

              let code = Buf.get src.chars sidx in
              let fg_r, fg_g, fg_b, fg_a = Color_plane.read_rgba src.fg sidx in
              let bg_r, bg_g, bg_b, bg_a = Color_plane.read_rgba src.bg sidx in
              let attrs = Buf.get src.attrs sidx in
              let src_link = Buf.get src.links sidx in

              let link_id =
                match Links.resolve src.link_registry src_link with
                | Some url -> Links.intern dst.link_registry (Some url)
                | None -> no_link
              in

              let is_orphan =
                if Cell_code.is_continuation code then
                  let left = Cell_code.left_extent code in
                  sx - left < src_x
                else false
              in

              let final_code_mapped =
                if is_orphan then space_cell
                else if
                  src.glyph_pool == dst.glyph_pool || Cell_code.is_simple code
                then code
                else copy_glyph code
              in

              (* If we mapped to space due to orphan/missing logic, reset
                 style *)
              let is_reset =
                final_code_mapped = space_cell && code <> space_cell
              in
              let attrs = if is_reset then 0 else attrs in
              let link_id = if is_reset then no_link else link_id in
              let fg_r, fg_g, fg_b, fg_a =
                if is_reset then (1., 1., 1., 1.) else (fg_r, fg_g, fg_b, fg_a)
              in

              (* NOTE: We calculate index here again, but it's the slow path anyway *)
              (* For same-grid blits, don't infer blending from alpha - just use
                 respect_alpha. This prevents the "preserve existing content" logic
                 from incorrectly keeping destination content when copying cells
                 with transparent backgrounds (e.g., during DCH operations). *)
              let blending =
                if same_grid then dst.respect_alpha
                else dst.respect_alpha || fg_a < 0.999 || bg_a < 0.999
              in
              set_cell_internal dst
                ~idx:((dy * dst.width) + dx)
                ~code:final_code_mapped ~fg_r ~fg_g ~fg_b ~fg_a ~bg_r ~bg_g
                ~bg_b ~bg_a ~attrs ~link_id ~blending;

              k := !k + x_step
            done;
            i := !i + y_step
          done

(* ---- Scrolling ---- *)

let scroll t ~top ~bottom ~delta =
  if delta = 0 || top >= bottom then ()
  else
    let region_h = bottom - top + 1 in
    let abs_delta = abs delta in

    if abs_delta >= region_h then
      let clear_c = Ansi.Color.of_rgba 0 0 0 0 in
      fill_rect t ~x:0 ~y:top ~width:t.width ~height:region_h ~color:clear_c
    else
      let copy_h = region_h - abs_delta in
      let clear_c = Ansi.Color.of_rgba 0 0 0 0 in

      if delta > 0 then (
        blit_region ~src:t ~dst:t ~src_x:0 ~src_y:top ~width:t.width
          ~height:copy_h ~dst_x:0 ~dst_y:(top + delta);
        fill_rect t ~x:0 ~y:top ~width:t.width ~height:abs_delta ~color:clear_c)
      else (
        blit_region ~src:t ~dst:t ~src_x:0 ~src_y:(top + abs_delta)
          ~width:t.width ~height:copy_h ~dst_x:0 ~dst_y:top;
        fill_rect t ~x:0
          ~y:(bottom - abs_delta + 1)
          ~width:t.width ~height:abs_delta ~color:clear_c)

let scroll_up t ~top ~bottom ~n = scroll t ~top ~bottom ~delta:(-n)
let scroll_down t ~top ~bottom ~n = scroll t ~top ~bottom ~delta:n

(* ---- Text Rendering ---- *)

let draw_text ?style ?(tab_width = 2) t ~x ~y ~text =
  if text = "" || y < 0 || y >= t.height then ()
  else
    let s = Option.value style ~default:Ansi.Style.default in
    let fg_r, fg_g, fg_b, fg_a =
      match s.fg with
      | Some c -> Ansi.Color.to_rgba_f c
      | None -> (1., 1., 1., 1.)
    in
    let explicit_bg =
      match s.bg with Some c -> Some (Ansi.Color.to_rgba_f c) | None -> None
    in
    let attrs = Ansi.Attr.pack s.attrs in
    let link_id = Links.intern t.link_registry s.link in

    let cur_x = ref x in
    let tabw = if tab_width <= 0 then 2 else tab_width in

    let scissor = Scissor_stack.current t.scissor_stack in
    let row_visible =
      match scissor with
      | None -> true
      | Some rect -> y >= rect.y && y < rect.y + rect.height
    in

    if not row_visible then ()
    else
      let cell_visible =
        match scissor with
        | None -> fun _ -> true
        | Some rect ->
            let x_min = rect.x in
            let x_max = rect.x + rect.width in
            fun x -> x >= x_min && x < x_max
      in
      let scissor_bounds =
        match scissor with
        | None -> None
        | Some rect -> Some (rect.x, rect.x + rect.width)
      in

      let writer code =
        if Cell_code.is_simple code && Cell_code.payload code = 9 then
          let spaces = tabw in
          let start_visible = cell_visible !cur_x in
          for _ = 1 to spaces do
            if !cur_x >= 0 && !cur_x < t.width then (
              (if start_visible && cell_visible !cur_x then
                 let idx = (y * t.width) + !cur_x in
                 let br, bg, bb, ba =
                   match explicit_bg with
                   | Some (r, g, b, a) -> (r, g, b, a)
                   | None -> Color_plane.read_rgba t.bg idx
                 in
                 let blending = fg_a < 0.999 || ba < 0.999 || t.respect_alpha in
                 set_cell_internal t ~idx ~code:space_cell ~fg_r ~fg_g ~fg_b
                   ~fg_a ~bg_r:br ~bg_g:bg ~bg_b:bb ~bg_a:ba ~attrs ~link_id
                   ~blending);
              incr cur_x)
          done
        else if !cur_x < t.width then
          let w = Cell_code.width code in
          if w > 0 then (
            let bounds_ok = !cur_x + w <= t.width && !cur_x >= 0 in
            let start_visible = cell_visible !cur_x in

            (if bounds_ok && start_visible then (
               let idx = (y * t.width) + !cur_x in
               let br, bg, bb, ba =
                 match explicit_bg with
                 | Some (r, g, b, a) -> (r, g, b, a)
                 | None -> Color_plane.read_rgba t.bg idx
               in
               let blending = fg_a < 0.999 || ba < 0.999 || t.respect_alpha in

               set_cell_internal t ~idx ~code ~fg_r ~fg_g ~fg_b ~fg_a ~bg_r:br
                 ~bg_g:bg ~bg_b:bb ~bg_a:ba ~attrs ~link_id ~blending;
               if w > 1 then
                 for i = 1 to w - 1 do
                   let c_x = !cur_x + i in
                   if cell_visible c_x then
                     let c_idx = (y * t.width) + c_x in
                     let br_c, bg_c, bb_c, ba_c =
                       match explicit_bg with
                       | Some (r, g, b, a) -> (r, g, b, a)
                       | None -> Color_plane.read_rgba t.bg c_idx
                     in
                     let cont =
                       Cell_code.make_cont ~id:(Cell_code.payload code) ~left:i
                         ~right:(w - 1 - i)
                     in
                     let blending_c =
                       fg_a < 0.999 || ba_c < 0.999 || t.respect_alpha
                     in
                     set_cell_internal t ~idx:c_idx ~code:cont ~fg_r ~fg_g ~fg_b
                       ~fg_a ~bg_r:br_c ~bg_g:bg_c ~bg_b:bb_c ~bg_a:ba_c ~attrs
                       ~link_id ~blending:blending_c
                 done)
             else if (not bounds_ok) && !cur_x >= 0 && !cur_x < t.width then
               (* Clear the remainder of the row with styled spaces when a glyph
                  would overflow. Only do this for actual overflow, not for
                  scissor-clipped glyphs - those should just be skipped. *)
               let start_x = !cur_x in
               for x_fill = start_x to t.width - 1 do
                 if cell_visible x_fill then
                   let idx = (y * t.width) + x_fill in
                   let br, bg, bb, ba =
                     match explicit_bg with
                     | Some (r, g, b, a) -> (r, g, b, a)
                     | None -> Color_plane.read_rgba t.bg idx
                   in
                   set_cell_internal t ~idx ~code:space_cell ~fg_r ~fg_g ~fg_b
                     ~fg_a ~bg_r:br ~bg_g:bg ~bg_b:bb ~bg_a:ba ~attrs ~link_id
                     ~blending:false
               done);

            cur_x := !cur_x + w)
      in
      let stop = ref false in
      try
        Glyph.iter_grapheme_info ~width_method:t.width_method ~tab_width:tabw
          (fun ~offset ~len ~width:w ->
            if !stop || w <= 0 then ()
            else
              let start_x = !cur_x in
              let end_x = start_x + w in
              if end_x <= 0 then cur_x := end_x
              else if start_x >= t.width then (
                stop := true;
                raise Exit)
              else if
                match scissor_bounds with
                | None -> false
                | Some (x_min, x_max) -> end_x <= x_min || start_x >= x_max
              then cur_x := end_x
              else
                let g =
                  Glyph.intern t.glyph_pool ~width_method:t.width_method
                    ~tab_width:tabw ~width:w ~pos:offset ~len text
                in
                if Glyph.is_continuation g then () else writer g)
          text
      with Exit -> ()

(* ---- Box Drawing ---- *)

let draw_box t ~x ~y ~width ~height ~border_chars ~border_sides ~border_style
    ~bg_color ~should_fill ?title ?title_alignment ?title_style () =
  if width <= 0 || height <= 0 then ()
  else
    let target = Rect.{ x; y; width; height } in
    let scissor_ok =
      match Scissor_stack.current t.scissor_stack with
      | None -> true
      | Some scissor -> Option.is_some (Rect.intersection target scissor)
    in
    if not scissor_ok then ()
    else
      let open Border in
      let sx = max 0 x in
      let sy = max 0 y in
      let ex = min (t.width - 1) (x + width - 1) in
      let ey = min (t.height - 1) (y + height - 1) in

      (if should_fill then
         let has_l, has_r =
           (List.mem `Left border_sides, List.mem `Right border_sides)
         in
         let has_t, has_b =
           (List.mem `Top border_sides, List.mem `Bottom border_sides)
         in
         let ix = if has_l then x + 1 else x in
         let iy = if has_t then y + 1 else y in
         let iw = width - (if has_l then 1 else 0) - if has_r then 1 else 0 in
         let ih = height - (if has_t then 1 else 0) - if has_b then 1 else 0 in
         if iw > 0 && ih > 0 then
           fill_rect t ~x:ix ~y:iy ~width:iw ~height:ih ~color:bg_color);

      let b_fg_r, b_fg_g, b_fg_b, b_fg_a =
        match border_style.Ansi.Style.fg with
        | Some c -> Ansi.Color.to_rgba_f c
        | None -> (1., 1., 1., 1.)
      in
      let b_bg_r, b_bg_g, b_bg_b, b_bg_a = Ansi.Color.to_rgba_f bg_color in
      let b_attrs = Ansi.Attr.pack border_style.attrs in
      let blending = true in

      let draw_b x y code =
        if
          x >= 0 && y >= 0 && x < t.width && y < t.height
          && not (is_clipped t x y)
        then
          (* ASCII can be stored directly; non-ASCII must be interned *)
          let cell =
            if code < 128 then code
            else Glyph.intern_uchar t.glyph_pool (Uchar.of_int code)
          in
          set_cell_internal t
            ~idx:((y * t.width) + x)
            ~code:cell ~fg_r:b_fg_r ~fg_g:b_fg_g ~fg_b:b_fg_b ~fg_a:b_fg_a
            ~bg_r:b_bg_r ~bg_g:b_bg_g ~bg_b:b_bg_b ~bg_a:b_bg_a ~attrs:b_attrs
            ~link_id:no_link ~blending
      in

      let has s = List.mem s border_sides in
      let is_left x' = x' = x in
      let is_right x' = x' = x + width - 1 in

      let left_border_only =
        has `Left && (not (has `Top)) && not (has `Bottom)
      in
      let right_border_only =
        has `Right && (not (has `Top)) && not (has `Bottom)
      in
      let bottom_only_with_verts =
        has `Bottom && (not (has `Top)) && (has `Left || has `Right)
      in
      let top_only_with_verts =
        has `Top && (not (has `Bottom)) && (has `Left || has `Right)
      in

      let extend_verts_to_top =
        left_border_only || right_border_only || bottom_only_with_verts
      in
      let extend_verts_to_bottom =
        left_border_only || right_border_only || top_only_with_verts
      in

      if has `Top || has `Bottom then (
        let row y_pos top =
          if y_pos >= 0 && y_pos < t.height then
            for c = sx to ex do
              if c >= x && c < x + width then
                let char =
                  if is_left c then
                    if has `Left then
                      if top then border_chars.top_left
                      else border_chars.bottom_left
                    else border_chars.horizontal
                  else if is_right c then
                    if has `Right then
                      if top then border_chars.top_right
                      else border_chars.bottom_right
                    else border_chars.horizontal
                  else border_chars.horizontal
                in
                draw_b c y_pos char
            done
        in
        if has `Top then row y true;
        if has `Bottom then row (y + height - 1) false);

      let vy_start = if extend_verts_to_top then y else y + 1 in
      let vy_end =
        if extend_verts_to_bottom then y + height - 1 else y + height - 2
      in
      let vy_start = max sy vy_start in
      let vy_end = min ey vy_end in

      if vy_start <= vy_end then (
        if has `Left && x >= 0 && x < t.width then
          for r = vy_start to vy_end do
            draw_b x r border_chars.vertical
          done;
        if has `Right && x + width - 1 >= 0 && x + width - 1 < t.width then
          for r = vy_start to vy_end do
            draw_b (x + width - 1) r border_chars.vertical
          done);

      match title with
      | Some txt when has `Top && width >= 4 ->
          let w = Glyph.measure ~width_method:t.width_method ~tab_width:2 txt in
          if w <= width - 4 then
            let pad =
              match title_alignment with
              | Some `Right -> width - 2 - w
              | Some `Center -> (width - w) / 2
              | _ -> 2
            in
            let style =
              Option.value title_style ~default:border_style
              |> Ansi.Style.bg bg_color
            in
            draw_text t ~x:(x + pad) ~y ~text:txt ~style
      | _ -> ()

(* ---- Line Drawing ---- *)

type line_glyphs = {
  h : string;
  v : string;
  diag_up : string;
  diag_down : string;
}

let default_line_glyphs = { h = "─"; v = "│"; diag_up = "╱"; diag_down = "╲" }

let ascii_line_glyphs = { h = "-"; v = "|"; diag_up = "/"; diag_down = "\\" }

(* Braille lookup table: precompute all 256 braille patterns once. This avoids
   per-cell Buffer allocation during line drawing. *)
let braille_lut : string array =
  Array.init 256 (fun bits ->
      let u = Uchar.of_int (0x2800 + bits) in
      let b = Buffer.create 4 in
      Buffer.add_utf_8_uchar b u;
      Buffer.contents b)

(* Braille base codepoint for detecting existing braille cells *)
let braille_base = 0x2800
let braille_max = 0x28FF

(* Decode an existing braille cell to get its bits, returns 0 for non-braille
   cells *)
let decode_braille_bits t ~x ~y =
  if x < 0 || y < 0 || x >= t.width || y >= t.height then 0
  else
    let idx = (y * t.width) + x in
    let code = Buf.get t.chars idx in
    if Cell_code.is_simple code then
      (* Simple cell: direct codepoint check *)
      if code >= braille_base && code <= braille_max then code - braille_base
      else 0
    else if Cell_code.is_start code then
      (* Complex cell: decode from glyph pool. Braille is 3-byte UTF-8. *)
      let s = Glyph.to_string t.glyph_pool code in
      if String.length s = 3 then
        (* Decode 3-byte UTF-8: 1110xxxx 10xxxxxx 10xxxxxx *)
        let b0 = Char.code (String.unsafe_get s 0) in
        let b1 = Char.code (String.unsafe_get s 1) in
        let b2 = Char.code (String.unsafe_get s 2) in
        if b0 land 0xF0 = 0xE0 && b1 land 0xC0 = 0x80 && b2 land 0xC0 = 0x80
        then
          let cp =
            ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F)
          in
          if cp >= braille_base && cp <= braille_max then cp - braille_base
          else 0
        else 0
      else 0
    else 0

(* Compute braille bit position from sub-cell coordinates *)
let[@inline] braille_bit_pos bit_x bit_y =
  match (bit_x, bit_y) with
  | 0, 0 -> 0
  | 0, 1 -> 1
  | 0, 2 -> 2
  | 0, 3 -> 6
  | 1, 0 -> 3
  | 1, 1 -> 4
  | 1, 2 -> 5
  | 1, 3 -> 7
  | _ -> 0

let draw_line t ~x1 ~y1 ~x2 ~y2 ?(style = Ansi.Style.default)
    ?(glyphs = default_line_glyphs) ?(kind = `Line) () =
  let dx = abs (x2 - x1) in
  let dy = abs (y2 - y1) in
  let sx = if x1 < x2 then 1 else -1 in
  let sy = if y1 < y2 then 1 else -1 in

  let plot_basic () =
    (* Determine diagonal direction for this line *)
    let diag_glyph =
      if (x2 - x1) * (y2 - y1) > 0 then glyphs.diag_down else glyphs.diag_up
    in
    let x = ref x1 and y = ref y1 and err = ref (dx - dy) in
    while not (!x = x2 && !y = y2) do
      (* Compute next step *)
      let e2 = 2 * !err in
      let move_x = e2 > -dy in
      let move_y = e2 < dx in
      (* Choose glyph based on step direction *)
      let glyph =
        if move_x && move_y then diag_glyph
        else if move_x then glyphs.h
        else glyphs.v
      in
      draw_text t ~x:!x ~y:!y ~text:glyph ~style;
      if move_x then (
        x := !x + sx;
        err := !err - dy);
      if move_y then (
        y := !y + sy;
        err := !err + dx)
    done;
    (* Draw final point *)
    let final_glyph =
      if dx = 0 && dy = 0 then glyphs.h (* single point *)
      else if dx = 0 then glyphs.v
      else if dy = 0 then glyphs.h
      else diag_glyph
    in
    draw_text t ~x:!x ~y:!y ~text:final_glyph ~style
  in

  let plot_braille () =
    (* Use array-based buffer for efficiency *)
    let buffer = Hashtbl.create 32 in
    let set_dot x y =
      (* Floor division for negative coordinates *)
      let cell_x = if x >= 0 then x / 2 else ((x + 1) / 2) - 1 in
      let cell_y = if y >= 0 then y / 4 else ((y + 1) / 4) - 1 in
      (* Clip to grid bounds *)
      if cell_x >= 0 && cell_x < t.width && cell_y >= 0 && cell_y < t.height
      then
        let bit_x = ((x mod 2) + 2) mod 2 in
        let bit_y = ((y mod 4) + 4) mod 4 in
        let bit_pos = braille_bit_pos bit_x bit_y in
        let key = (cell_x, cell_y) in
        let current = Option.value (Hashtbl.find_opt buffer key) ~default:0 in
        Hashtbl.replace buffer key (current lor (1 lsl bit_pos))
    in
    let x = ref x1 and y = ref y1 and err = ref (dx - dy) in
    while not (!x = x2 && !y = y2) do
      set_dot !x !y;
      let e2 = 2 * !err in
      if e2 > -dy then (
        x := !x + sx;
        err := !err - dy);
      if e2 < dx then (
        y := !y + sy;
        err := !err + dx)
    done;
    set_dot x2 y2;
    (* Commit buffer to grid, merging with existing braille cells *)
    Hashtbl.iter
      (fun (cell_x, cell_y) bits ->
        (* Read existing braille bits from the grid *)
        let existing_bits = decode_braille_bits t ~x:cell_x ~y:cell_y in
        let merged_bits = existing_bits lor bits in
        let glyph = braille_lut.(merged_bits) in
        draw_text t ~x:cell_x ~y:cell_y ~text:glyph ~style)
      buffer
  in
  match kind with `Line -> plot_basic () | `Braille -> plot_braille ()

(* ---- Inspection & Utilities ---- *)

let active_height t =
  let rec find_row y =
    if y < 0 then 0
    else
      let row_start = y * t.width in
      let rec scan_col x =
        if x >= t.width then false
        else
          let idx = row_start + x in
          let code = Buf.get t.chars idx in
          if code <> space_cell && code <> null_cell then true
          else scan_col (x + 1)
      in
      if scan_col 0 then y + 1 else find_row (y - 1)
  in
  find_row (t.height - 1)

let iter_diffs prev curr f =
  let max_w = max prev.width curr.width in
  let max_h = max prev.height curr.height in

  let colors_different plane1 idx1 plane2 idx2 =
    not (Color_plane.equal_eps plane1 idx1 plane2 idx2)
  in

  for y = 0 to max_h - 1 do
    for x = 0 to max_w - 1 do
      let in_prev = x < prev.width && y < prev.height in
      let in_curr = x < curr.width && y < curr.height in

      match (in_prev, in_curr) with
      | false, false -> ()
      | true, false | false, true -> f y x
      | true, true ->
          let p_idx = (y * prev.width) + x in
          let c_idx = (y * curr.width) + x in

          if
            Buf.get prev.chars p_idx <> Buf.get curr.chars c_idx
            || Buf.get prev.attrs p_idx <> Buf.get curr.attrs c_idx
            || colors_different prev.fg p_idx curr.fg c_idx
            || colors_different prev.bg p_idx curr.bg c_idx
            || Buf.get prev.links p_idx <> Buf.get curr.links c_idx
          then f y x
    done
  done

let diff_cells prev curr =
  let diffs = Dynarray.create () in
  iter_diffs prev curr (fun y x -> Dynarray.add_last diffs (y, x));
  Dynarray.to_array diffs

(* Snapshotting *)

let snapshot ?(reset = true) t : string =
  let width = width t in
  let height = height t in
  if width <= 0 || height <= 0 then ""
  else
    let pool = glyph_pool t in
    let scratch = ref (Bytes.create 256) in

    Ansi.to_string (fun w ->
        let style = Ansi.Sgr_state.create () in

        for y = 0 to height - 1 do
          if y > 0 then Ansi.emit (Ansi.char '\n') w;

          let x = ref 0 in
          while !x < width do
            let idx = (y * width) + !x in
            let code = get_code t idx in
            let cell_w = cell_width t idx in

            if cell_w > 0 then (
              (* Style/colors/attrs/hyperlinks *)
              let attrs = get_attrs t idx in
              let link = hyperlink_url_direct t (get_link t idx) in
              let fg_r = get_fg_r t idx in
              let fg_g = get_fg_g t idx in
              let fg_b = get_fg_b t idx in
              let fg_a = get_fg_a t idx in
              let bg_r = get_bg_r t idx in
              let bg_g = get_bg_g t idx in
              let bg_b = get_bg_b t idx in
              let bg_a = get_bg_a t idx in
              Ansi.Sgr_state.update style w ~fg_r ~fg_g ~fg_b ~fg_a ~bg_r ~bg_g
                ~bg_b ~bg_a ~attrs ~link;

              (* Emit grapheme - Cell_code is aligned with Glyph.t *)
              if
                Cell_code.is_continuation code || Int.equal code Cell_code.empty
              then Ansi.emit (Ansi.char ' ') w
              else
                (* code is a valid Glyph.t due to aligned formats *)
                let len = Glyph.length pool code in
                if len <= 0 then Ansi.emit (Ansi.char ' ') w
                else (
                  if len > Bytes.length !scratch then
                    scratch :=
                      Bytes.create (max (Bytes.length !scratch * 2) len);
                  let written = Glyph.blit pool code !scratch ~pos:0 in
                  Ansi.emit
                    (Ansi.bytes !scratch ~off:0 ~len:written)
                    w));

            x := !x + cell_w
          done;

          Ansi.Sgr_state.close_link style w;
          Ansi.emit Ansi.reset w;
          Ansi.Sgr_state.reset style
        done;

        if reset then Ansi.emit Ansi.reset w)
