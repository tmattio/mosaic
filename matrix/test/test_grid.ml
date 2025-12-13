open Alcotest

(* Helper to calculate linear index *)
let idx grid x y = (y * Grid.width grid) + x

(* Updated: Use Grid.get_text and predicates instead of Cell matching *)
let read_char grid x y =
  let i = idx grid x y in
  if Grid.is_empty grid i then 32 (* Space *)
  else if Grid.is_continuation grid i then 32 (* Treat continuation as space *)
  else
    let text = Grid.get_text grid i in
    if String.length text = 0 then 32
    else
      let decoder = String.get_utf_8_uchar text 0 in
      Uchar.to_int (Uchar.utf_decode_uchar decoder)

(* Updated: Use Grid.cell_width *)
let read_width grid x y =
  let i = idx grid x y in
  if Grid.is_empty grid i then 1 else Grid.cell_width grid i

(* Updated: Use Grid.attrs *)
let read_attr grid x y = Grid.get_attrs grid (idx grid x y)

(* Updated: Use Grid.Fg/Bg submodules and convert float back to int *)
let to_byte f = Float.round (f *. 255.) |> int_of_float

let read_fg grid x y =
  let i = idx grid x y in
  ( to_byte (Grid.get_fg_r grid i),
    to_byte (Grid.get_fg_g grid i),
    to_byte (Grid.get_fg_b grid i),
    to_byte (Grid.get_fg_a grid i) )

let read_bg grid x y =
  let i = idx grid x y in
  ( to_byte (Grid.get_bg_r grid i),
    to_byte (Grid.get_bg_g grid i),
    to_byte (Grid.get_bg_b grid i),
    to_byte (Grid.get_bg_a grid i) )

let rgba = Alcotest.(pair int (pair int (pair int int)))

(* Updated: Iterate width and use predicates/get_text *)
let row_to_string grid y =
  let width = Grid.width grid in
  let buf = Buffer.create width in
  for x = 0 to width - 1 do
    let i = idx grid x y in
    if Grid.is_empty grid i then Buffer.add_char buf ' '
    else if Grid.is_continuation grid i then ()
    else Buffer.add_string buf (Grid.get_text grid i)
  done;
  Buffer.contents buf

let trim_right s =
  let len = String.length s in
  let rec loop i =
    if i < 0 then ""
    else if s.[i] = ' ' then loop (i - 1)
    else String.sub s 0 (i + 1)
  in
  if len = 0 then s else loop (len - 1)

let row_trimmed grid y = trim_right (row_to_string grid y)

(* --- Tests --- *)

let inherit_bg_on_unwritten_ascii () =
  let grid = Grid.create ~width:2 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"A";
  let r, g, b, a = read_bg grid 0 0 in
  check int "bg r" 0 r;
  check int "bg g" 0 g;
  check int "bg b" 0 b;
  check int "bg a" 0 a;
  ()

let unicode_inherit_bg_on_unwritten_cell () =
  let grid = Grid.create ~width:4 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"中";
  let r0, g0, b0, a0 = read_bg grid 0 0 in
  check int "bg r0" 0 r0;
  check int "bg g0" 0 g0;
  check int "bg b0" 0 b0;
  check int "bg a0" 0 a0;
  let r1, g1, b1, a1 = read_bg grid 1 0 in
  check int "bg r1" 0 r1;
  check int "bg g1" 0 g1;
  check int "bg b1" 0 b1;
  check int "bg a1" 0 a1;
  ()

let overflow_respects_scissor_for_wide_grapheme () =
  let grid = Grid.create ~width:4 ~height:1 () in
  Grid.push_scissor grid { x = 0; y = 0; width = 3; height = 1 };
  Grid.draw_text grid ~x:2 ~y:0 ~text:"中";
  Grid.pop_scissor grid;
  check int "x=3 untouched" 32 (read_char grid 3 0)

let alpha_blit_orphan_continuation_draws_space () =
  let src = Grid.create ~width:3 ~height:1 ~respect_alpha:true () in
  let style = Ansi.Style.make ~bg:(Ansi.Color.of_rgba 0 255 0 128) () in
  Grid.draw_text src ~x:0 ~y:0 ~text:"中" ~style;
  let dst = Grid.create ~width:1 ~height:1 () in
  Grid.blit_region ~src ~dst ~src_x:1 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
    ~dst_y:0;
  check int "dst(0,0) is space" (Char.code ' ') (read_char dst 0 0)

let cross_pool_blit_remaps_graphemes () =
  let src = Grid.create ~width:2 ~height:1 () in
  Grid.draw_text src ~x:0 ~y:0 ~text:"😊";
  let other_pool = Glyph.create_pool () in
  let dst = Grid.create ~width:2 ~height:1 ~glyph_pool:other_pool () in
  Grid.blit_region ~src ~dst ~src_x:0 ~src_y:0 ~width:2 ~height:1 ~dst_x:0
    ~dst_y:0;
  check string "text copied" "😊" (row_trimmed dst 0);
  let start_idx = idx dst 0 0 in
  let cont_idx = idx dst 1 0 in
  check int "width preserved" 2 (Grid.cell_width dst start_idx);
  check bool "continuation copied" true (Grid.is_continuation dst cont_idx)

let blit_preserves_respect_alpha () =
  let src = Grid.create ~width:2 ~height:2 ~respect_alpha:true () in
  let dst = Grid.create ~width:2 ~height:2 () in
  Grid.blit ~src ~dst;
  check bool "respect alpha copied" true (Grid.respect_alpha dst)

let blit_bulk_tracks_graphemes () =
  let pool = Glyph.create_pool () in
  let src = Grid.create ~width:4 ~height:1 ~glyph_pool:pool () in
  let dst = Grid.create ~width:4 ~height:1 ~glyph_pool:pool () in
  Grid.draw_text src ~x:0 ~y:0 ~text:"a😊a";
  Grid.blit_region ~src ~dst ~src_x:0 ~src_y:0 ~width:4 ~height:1 ~dst_x:0
    ~dst_y:0;
  Grid.clear src;
  (* Updated: use Grid.get_text directly *)
  let text = Grid.get_text dst (idx dst 1 0) in
  check string "emoji text" "😊" text

let overlap_blit_direction_correctness () =
  let grid = Grid.create ~width:5 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"ABCDE";
  Grid.blit_region ~src:grid ~dst:grid ~src_x:0 ~src_y:0 ~width:4 ~height:1
    ~dst_x:1 ~dst_y:0;
  check string "overlap result" "AABCD" (row_trimmed grid 0)

let box_title_left_aligned () =
  let grid = Grid.create ~width:12 ~height:3 () in
  let border_chars : Grid.Border.t =
    {
      top_left = 0x250C;
      (* ┌ *)
      top_right = 0x2510;
      (* ┐ *)
      bottom_left = 0x2514;
      (* └ *)
      bottom_right = 0x2518;
      (* ┘ *)
      horizontal = 0x2500;
      (* ─ *)
      vertical = 0x2502;
      (* │ *)
      top_t = 0;
      bottom_t = 0;
      left_t = 0;
      right_t = 0;
      cross = 0;
    }
  in
  let style = Ansi.Style.default in
  Grid.draw_box grid ~x:0 ~y:0 ~width:12 ~height:3 ~border_chars
    ~border_sides:[ `Top; `Left; `Right ] ~border_style:style
    ~bg_color:(Ansi.Color.of_rgba 0 0 0 0)
    ~should_fill:false ~title:"Title" ();
  check int "T at x=2" (Char.code 'T') (read_char grid 2 0)

let diff_detects_single_rgb_step () =
  let a = Grid.create ~width:1 ~height:1 () in
  let b = Grid.copy a in
  (* Instead of hacking raw arrays, we set the cell with a minimal color difference.
     1/255 is approx 0.0039, which is > the internal epsilon (0.00001). *)
  let minimal_diff_color = Ansi.Color.of_rgba 1 1 1 1 in
  Grid.set_cell b ~x:0 ~y:0 ~code:(Grid.get_code a 0) ~fg:Ansi.Color.white
    ~bg:minimal_diff_color ~attrs:Ansi.Attr.empty ();
  let diffs = Grid.diff_cells a b in
  check
    (list (pair int int))
    "diffs include cell when RGB changes by 1 step"
    [ (0, 0) ]
    (Array.to_list diffs)

let alpha_blit_blends_fg_bg () =
  let src = Grid.create ~width:1 ~height:1 ~respect_alpha:true () in
  let dst = Grid.create ~width:1 ~height:1 () in
  let blue = Ansi.Color.of_rgba 0 0 255 255 in
  Grid.fill_rect dst ~x:0 ~y:0 ~width:1 ~height:1 ~color:blue;
  let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
  let semi_green = Ansi.Color.of_rgba 0 255 0 128 in
  let style = Ansi.Style.make ~fg:semi_red ~bg:semi_green () in
  Grid.draw_text src ~x:0 ~y:0 ~text:"C" ~style;
  Grid.blit_region ~src ~dst ~src_x:0 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
    ~dst_y:0;
  let r_bg, g_bg, b_bg, a_bg = read_bg dst 0 0 in
  (* Validate qualitative blend: green increases from 0, blue decreases from 255.
     Background alpha should match the overlay value (128) rather than
     compositing with the destination alpha. *)
  check bool "bg green increased" true (g_bg > 0);
  check bool "bg blue decreased" true (b_bg < 255);
  check int "bg red stays 0" 0 r_bg;
  check int "bg alpha is 128" 128 a_bg;
  let r_fg, _g_fg, _b_fg, a_fg = read_fg dst 0 0 in
  (* With src FG alpha resolved to 0 in the source buffer, FG tint contribution
     is zero; alpha uses destination background (255). *)
  check int "fg red stays 0" 0 r_fg;
  check int "fg alpha 255" 255 a_fg

let resize_shrink_clips_continuation () =
  let grid = Grid.create ~width:2 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"中";
  Grid.resize grid ~width:1 ~height:1;
  check string "row after shrink clears truncated grapheme" ""
    (row_trimmed grid 0);
  ignore (read_char grid 0 0)

let resize_truncated_grapheme_does_not_bleed () =
  let grid = Grid.create ~width:5 ~height:2 () in
  Grid.draw_text grid ~x:3 ~y:0 ~text:"中";
  Grid.draw_text grid ~x:0 ~y:1 ~text:"B";
  Grid.resize grid ~width:4 ~height:2;
  Grid.draw_text grid ~x:3 ~y:0 ~text:"X";
  check string "bottom row preserved" "B" (row_trimmed grid 1)

let create_defaults () =
  let grid = Grid.create ~width:2 ~height:3 () in
  check int "width" 2 (Grid.width grid);
  check int "height" 3 (Grid.height grid);
  check bool "respect alpha" false (Grid.respect_alpha grid);
  check bool "width method" true (Grid.width_method grid = `Unicode)

let create_with_configuration () =
  let pool = Glyph.create_pool () in
  let grid =
    Grid.create ~width:1 ~height:1 ~glyph_pool:pool ~width_method:`Wcwidth
      ~respect_alpha:true ()
  in
  check bool "glyph pool reused" true (Grid.glyph_pool grid == pool);
  check bool "width method" true (Grid.width_method grid = `Wcwidth);
  check bool "respect alpha" true (Grid.respect_alpha grid)

let set_width_method_updates () =
  let grid = Grid.create ~width:1 ~height:1 () in
  Grid.set_width_method grid `Wcwidth;
  check bool "updated" true (Grid.width_method grid = `Wcwidth)

let set_respect_alpha_updates () =
  let grid = Grid.create ~width:1 ~height:1 () in
  Grid.set_respect_alpha grid true;
  check bool "updated" true (Grid.respect_alpha grid)

let set_cell_writes_all_planes () =
  let grid = Grid.create ~width:2 ~height:2 () in
  let attrs = Ansi.Attr.bold in
  Grid.set_cell_alpha grid ~x:1 ~y:0 ~code:(Char.code 'A') ~fg:Ansi.Color.red
    ~bg:Ansi.Color.blue ~attrs ();
  check int "char" (Char.code 'A') (read_char grid 1 0);
  check int "width" 1 (read_width grid 1 0);
  check int "attrs" (Ansi.Attr.pack attrs) (read_attr grid 1 0);
  let r_fg, g_fg, b_fg, a_fg = read_fg grid 1 0 in
  let er, eg, eb, ea = Ansi.Color.to_rgba Ansi.Color.red in
  check rgba "fg color" (er, (eg, (eb, ea))) (r_fg, (g_fg, (b_fg, a_fg)));
  let r_bg, g_bg, b_bg, a_bg = read_bg grid 1 0 in
  let br, bg, bb, ba = Ansi.Color.to_rgba Ansi.Color.blue in
  check rgba "bg color" (br, (bg, (bb, ba))) (r_bg, (g_bg, (b_bg, a_bg)))

let set_cell_outside_scissor_ignored () =
  let grid = Grid.create ~width:2 ~height:2 () in
  Grid.push_scissor grid { x = 1; y = 1; width = 1; height = 1 };
  Grid.set_cell_alpha grid ~x:0 ~y:0 ~code:(Char.code 'X') ~fg:Ansi.Color.white
    ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  Grid.pop_scissor grid;
  check int "char remains empty" 32 (read_char grid 0 0)

let with_scissor_restores_stack () =
  let grid = Grid.create ~width:2 ~height:2 () in
  let result =
    Grid.with_scissor grid { x = 0; y = 0; width = 1; height = 1 } (fun () ->
        Grid.set_cell_alpha grid ~x:0 ~y:0 ~code:(Char.code 'Y')
          ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
        42)
  in
  check int "scoped result" 42 result;
  check int "inside write" (Char.code 'Y') (read_char grid 0 0);
  (* After scope, writing outside should succeed. *)
  Grid.set_cell_alpha grid ~x:1 ~y:1 ~code:(Char.code 'Z') ~fg:Ansi.Color.white
    ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  check int "char set" (Char.code 'Z') (read_char grid 1 1)

let set_cell_records_hyperlink () =
  let grid = Grid.create ~width:1 ~height:1 () in
  Grid.set_cell_alpha grid ~x:0 ~y:0 ~code:(Char.code 'L') ~fg:Ansi.Color.white
    ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ~link:"https://example.com" ();
  (* Use the new zero-alloc accessor to get the ID *)
  let id = Grid.get_link grid 0 in
  match Grid.hyperlink_url grid id with
  | Some url -> check string "link stored" "https://example.com" url
  | None -> failwith "expected hyperlink"

let draw_text_applies_style () =
  let grid = Grid.create ~width:4 ~height:1 () in
  let fg_color = Ansi.Color.of_rgb 50 100 150 in
  let bg_color = Ansi.Color.of_rgba 10 20 30 200 in
  let style = Ansi.Style.make ~fg:fg_color ~bg:bg_color ~bold:true () in
  Grid.draw_text grid ~x:1 ~y:0 ~text:"Hi" ~style;
  check int "first char" (Char.code 'H') (read_char grid 1 0);
  check int "second char" (Char.code 'i') (read_char grid 2 0);
  let attrs = style.Ansi.Style.attrs in
  check int "attr first" (Ansi.Attr.pack attrs) (read_attr grid 1 0);
  check int "attr second" (Ansi.Attr.pack attrs) (read_attr grid 2 0);
  let expected_fg =
    let r, g, b, a = Ansi.Color.to_rgba fg_color in
    (r, (g, (b, a)))
  in
  let expected_bg_color =
    Ansi.Color.blend ~src:bg_color ~dst:Ansi.Color.default ()
  in
  let expected_bg =
    let r, g, b, a = Ansi.Color.to_rgba expected_bg_color in
    (r, (g, (b, a)))
  in
  let r, g, b, a = read_fg grid 1 0 in
  check rgba "fg first" expected_fg (r, (g, (b, a)));
  let r, g, b, a = read_fg grid 2 0 in
  check rgba "fg second" expected_fg (r, (g, (b, a)));
  let r, g, b, a = read_bg grid 1 0 in
  check rgba "bg first" expected_bg (r, (g, (b, a)));
  let r, g, b, a = read_bg grid 2 0 in
  check rgba "bg second" expected_bg (r, (g, (b, a)))

let draw_text_inherits_existing_background () =
  let grid = Grid.create ~width:4 ~height:1 () in
  let bg_color = Ansi.Color.of_rgb 40 80 120 in
  Grid.fill_rect grid ~x:0 ~y:0 ~width:4 ~height:1 ~color:bg_color;
  Grid.draw_text grid ~x:0 ~y:0 ~text:"Hi";
  check int "char H" (Char.code 'H') (read_char grid 0 0);
  check int "char i" (Char.code 'i') (read_char grid 1 0);
  let expected =
    let r, g, b, a = Ansi.Color.to_rgba bg_color in
    (r, (g, (b, a)))
  in
  let assert_bg x =
    let r, g, b, a = read_bg grid x 0 in
    check rgba (Printf.sprintf "bg cell %d" x) expected (r, (g, (b, a)))
  in
  assert_bg 0;
  assert_bg 1;
  assert_bg 2;
  assert_bg 3

let draw_text_skips_newline () =
  let grid = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"A\nB";
  check int "A at x=0" (Char.code 'A') (read_char grid 0 0);
  check int "B at x=1" (Char.code 'B') (read_char grid 1 0);
  check int "x=2 space" (Char.code ' ') (read_char grid 2 0)

let draw_box_left_border_spans_full_height () =
  let grid = Grid.create ~width:3 ~height:4 () in
  let border_chars : Grid.Border.t =
    {
      top_left = 0;
      top_right = 0;
      bottom_left = 0;
      bottom_right = 0;
      horizontal = 0;
      vertical = 0x2502;
      top_t = 0;
      bottom_t = 0;
      left_t = 0;
      right_t = 0;
      cross = 0;
    }
  in
  Grid.draw_box grid ~x:0 ~y:0 ~width:3 ~height:4 ~border_chars
    ~border_sides:[ `Left ] ~border_style:Ansi.Style.default
    ~bg_color:(Ansi.Color.of_rgba 0 0 0 0)
    ~should_fill:true ();
  let pipe = 0x2502 in
  check int "top cell" pipe (read_char grid 0 0);
  check int "middle cell" pipe (read_char grid 0 1);
  check int "bottom cell" pipe (read_char grid 0 3)

let set_cell_alpha_honours_blending () =
  let grid = Grid.create ~width:1 ~height:1 ~respect_alpha:true () in
  Grid.set_cell_alpha grid ~x:0 ~y:0 ~code:(Char.code 'B') ~fg:Ansi.Color.blue
    ~bg:Ansi.Color.blue ~attrs:Ansi.Attr.empty ();
  let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
  let semi_green = Ansi.Color.of_rgba 0 255 0 128 in
  Grid.set_cell_alpha grid ~x:0 ~y:0 ~code:(Char.code 'C') ~fg:semi_red
    ~bg:semi_green ~attrs:Ansi.Attr.empty ();
  let expected_fg = Ansi.Color.blend ~src:semi_red ~dst:Ansi.Color.blue () in
  (* Background alpha is replaced by the overlay alpha instead of being blended,
     matching the renderer's contract for translucent backgrounds. *)
  let expected_bg =
    let blended = Ansi.Color.blend ~src:semi_green ~dst:Ansi.Color.blue () in
    let r, g, b, _ = Ansi.Color.to_rgba blended in
    (* overlay alpha = 128 *)
    Ansi.Color.of_rgba r g b 128
  in
  check int "char" (Char.code 'C') (read_char grid 0 0);
  check int "width" 1 (read_width grid 0 0);
  let expected_fg =
    let r, g, b, a = Ansi.Color.to_rgba expected_fg in
    (r, (g, (b, a)))
  in
  let expected_bg =
    let r, g, b, a = Ansi.Color.to_rgba expected_bg in
    (r, (g, (b, a)))
  in
  let actual_fg =
    let r, g, b, a = read_fg grid 0 0 in
    (r, (g, (b, a)))
  in
  let actual_bg =
    let r, g, b, a = read_bg grid 0 0 in
    (r, (g, (b, a)))
  in
  check rgba "fg blended" expected_fg actual_fg;
  check rgba "bg blended" expected_bg actual_bg

let set_cell_alpha_without_respect_skips_blend () =
  let grid = Grid.create ~width:1 ~height:1 () in
  let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
  Grid.set_cell_alpha grid ~x:0 ~y:0 ~code:(Char.code 'C') ~fg:semi_red
    ~bg:semi_red ~attrs:Ansi.Attr.empty ();
  check int "char" (Char.code 'C') (read_char grid 0 0);
  check int "width" 1 (read_width grid 0 0);
  (* {!Grid.set_cell_alpha} always blends and keeps the destination alpha for
     the foreground channel, so the default background alpha (0) persists. *)
  let expected =
    let blended = Ansi.Color.blend ~src:semi_red ~dst:Ansi.Color.default () in
    let r, g, b, _ = Ansi.Color.to_rgba blended in
    (r, (g, (b, 0)))
  in
  let actual =
    let r, g, b, a = read_fg grid 0 0 in
    (r, (g, (b, a)))
  in
  check rgba "fg blended" expected actual

let draw_text_blends_fg_alpha_over_opaque_bg () =
  let grid = Grid.create ~width:1 ~height:1 () in
  let opaque_blue = Ansi.Color.of_rgba 0 0 255 255 in
  Grid.fill_rect grid ~x:0 ~y:0 ~width:1 ~height:1 ~color:opaque_blue;
  let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
  let style = Ansi.Style.make ~fg:semi_red () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"X" ~style;
  let r, _g, b, a = read_fg grid 0 0 in
  (* Blended path sets fg alpha to destination bg alpha and retains some blue *)
  check int "fg alpha promoted" 255 a;
  check bool "blue component preserved" true (b > 0);
  check bool "red component applied" true (r > 0)

let blit_region_blends_without_respect_alpha () =
  let src = Grid.create ~width:1 ~height:1 () in
  let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
  Grid.set_cell_alpha src ~x:0 ~y:0 ~code:(Char.code 'R') ~fg:Ansi.Color.white
    ~bg:semi_red ~attrs:Ansi.Attr.empty ();
  let dst = Grid.create ~width:1 ~height:1 () in
  let opaque_blue = Ansi.Color.of_rgba 0 0 255 255 in
  Grid.fill_rect dst ~x:0 ~y:0 ~width:1 ~height:1 ~color:opaque_blue;
  Grid.blit_region ~src ~dst ~src_x:0 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
    ~dst_y:0;
  let r, _g, b, a = read_bg dst 0 0 in
  check bool "background blended keeps blue" true (b > 0 && b < 255);
  check bool "background blended adds red" true (r > 0 && r < 255);
  check int "alpha preserved" 128 a

let scissor_push_intersects_parent () =
  let grid = Grid.create ~width:4 ~height:1 () in
  (* Parent scissor clips to first cell *)
  Grid.push_scissor grid { x = 0; y = 0; width = 1; height = 1 };
  (* Child scissor outside parent should intersect to empty, so writes are clipped *)
  Grid.push_scissor grid { x = 2; y = 0; width = 1; height = 1 };
  Grid.set_cell_alpha grid ~x:2 ~y:0 ~code:(Char.code 'B') ~fg:Ansi.Color.white
    ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  (* Pop child, write inside parent *)
  Grid.pop_scissor grid;
  Grid.set_cell_alpha grid ~x:0 ~y:0 ~code:(Char.code 'A') ~fg:Ansi.Color.white
    ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  Grid.pop_scissor grid;
  check int "child write clipped by parent" (Char.code ' ') (read_char grid 2 0);
  check int "parent restored after pop" (Char.code 'A') (read_char grid 0 0)

let clear_scissor_allows_future_writes () =
  let grid = Grid.create ~width:2 ~height:2 () in
  Grid.push_scissor grid { x = 0; y = 0; width = 1; height = 1 };
  Grid.clear_scissor grid;
  Grid.set_cell_alpha grid ~x:1 ~y:1 ~code:(Char.code 'W') ~fg:Ansi.Color.white
    ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  check int "write succeeded" (Char.code 'W') (read_char grid 1 1)

let fill_rect_fills_region () =
  let grid = Grid.create ~width:3 ~height:3 () in
  Grid.fill_rect grid ~x:1 ~y:1 ~width:2 ~height:2 ~color:Ansi.Color.green;
  for y = 0 to 2 do
    for x = 0 to 2 do
      let inside = x >= 1 && x <= 2 && y >= 1 && y <= 2 in
      if inside then (
        check int "char" (Char.code ' ') (read_char grid x y);
        check int "width" 1 (read_width grid x y);
        let r, g, b, _ = read_bg grid x y in
        let er, eg, eb, _ = Ansi.Color.to_rgba Ansi.Color.green in
        check (triple int int int) "color" (er, eg, eb) (r, g, b))
      else check int "outside char" 32 (read_char grid x y)
    done
  done

let replace_wide_grapheme_clears_continuations () =
  let grid = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"😊";
  Grid.set_cell_alpha grid ~x:0 ~y:0 ~code:(Char.code 'A') ~fg:Ansi.Color.white
    ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  check int "continuation cleared to space" 32 (read_char grid 1 0);
  check int "continuation width reset" 1 (read_width grid 1 0)

let fill_rect_alpha_preserves_glyph () =
  let grid = Grid.create ~width:1 ~height:1 ~respect_alpha:true () in
  Grid.set_cell_alpha grid ~x:0 ~y:0 ~code:(Char.code 'X') ~fg:Ansi.Color.white
    ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  let overlay = Ansi.Color.of_rgba 0 255 0 128 in
  Grid.fill_rect grid ~x:0 ~y:0 ~width:1 ~height:1 ~color:overlay;
  check int "char preserved" (Char.code 'X') (read_char grid 0 0);
  (* BG alpha becomes overlay alpha with RGB blended against dest BG. *)
  let blended_bg = Ansi.Color.blend ~src:overlay ~dst:Ansi.Color.black () in
  let er, eg, eb, _ = Ansi.Color.to_rgba blended_bg in
  let ea = 128 in
  let r, g, b, a = read_bg grid 0 0 in
  check rgba "background blended" (er, (eg, (eb, ea))) (r, (g, (b, a)));
  let expected_fg = Ansi.Color.blend ~src:overlay ~dst:Ansi.Color.white () in
  let fr, fg, fb, fa = Ansi.Color.to_rgba expected_fg in
  let r, g, b, a = read_fg grid 0 0 in
  check rgba "foreground tinted" (fr, (fg, (fb, fa))) (r, (g, (b, a)))

let fill_rect_transparent_preserves_background () =
  let grid = Grid.create ~width:3 ~height:1 () in
  let bg_color = Ansi.Color.of_rgb 10 20 30 in
  Grid.fill_rect grid ~x:0 ~y:0 ~width:3 ~height:1 ~color:bg_color;
  let transparent = Ansi.Color.of_rgba 0 0 0 0 in
  Grid.fill_rect grid ~x:0 ~y:0 ~width:3 ~height:1 ~color:transparent;
  let expected =
    let r, g, b, a = Ansi.Color.to_rgba bg_color in
    (r, (g, (b, a)))
  in
  for x = 0 to 2 do
    check int
      (Printf.sprintf "char cleared %d" x)
      (Char.code ' ') (read_char grid x 0);
    let r, g, b, a = read_bg grid x 0 in
    check rgba (Printf.sprintf "bg preserved %d" x) expected (r, (g, (b, a)))
  done

let scroll_uses_transparent_background () =
  let grid = Grid.create ~width:2 ~height:2 () in
  (* Write something on the first row to force a scroll source. *)
  Grid.draw_text grid ~x:0 ~y:0 ~text:"AA";
  Grid.scroll_up grid ~top:0 ~bottom:1 ~n:1;
  (* Bottom row is newly cleared; its background should stay transparent. *)
  let r, g, b, a = read_bg grid 0 1 in
  check rgba "transparent bg" (0, (0, (0, 0))) (r, (g, (b, a)));
  check int "space char" (Char.code ' ') (read_char grid 0 1)

let draw_text_overwrite_clears_span () =
  let grid = Grid.create ~width:4 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"中";
  Grid.draw_text grid ~x:0 ~y:0 ~text:"a";
  check int "start replaced" (Char.code 'a') (read_char grid 0 0);
  check int "continuation cleared" 32 (read_char grid 1 0);
  check int "start width" 1 (read_width grid 0 0);
  check int "continuation width cleared" 1 (read_width grid 1 0)

let blit_region_skips_partial_span () =
  let src = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text src ~x:0 ~y:0 ~text:"中a";
  let dst = Grid.create ~width:2 ~height:1 () in
  Grid.blit_region ~src ~dst ~src_x:1 ~src_y:0 ~width:2 ~height:1 ~dst_x:0
    ~dst_y:0;
  check int "orphan cleared" 32 (read_char dst 0 0);
  check int "orphan width" 1 (read_width dst 0 0);
  check int "trailing char copied" (Char.code 'a') (read_char dst 1 0);
  check int "trailing width" 1 (read_width dst 1 0)

let draw_text_overflow_does_nothing () =
  let grid = Grid.create ~width:2 ~height:1 () in
  Grid.draw_text grid ~x:1 ~y:0 ~text:"中";
  (* Wide graphemes that overflow are discarded by clearing to end-of-line with
     styled spaces, preventing partially drawn clusters. *)
  check int "overflow clears to space" (Char.code ' ') (read_char grid 1 0);
  check int "width set to 1" 1 (read_width grid 1 0)

let ambiguous_width_defaults_to_one () =
  let check_width label s =
    let width = Glyph.measure ~width_method:`Unicode ~tab_width:2 s in
    check int label 1 width
  in
  check_width "┌ width" "┌";
  check_width "┐ width" "┐";
  check_width "─ width" "─";
  check_width "│ width" "│"

let canvas_like_primitives_render () =
  let grid = Grid.create ~width:8 ~height:4 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"────────";
  for y = 0 to 3 do
    Grid.draw_text grid ~x:0 ~y ~text:"│"
  done;
  Grid.draw_text grid ~x:3 ~y:2 ~text:"x";
  check string "row 0" "│───────" (row_trimmed grid 0);
  check string "row 1" "│" (row_trimmed grid 1);
  check string "row 2" "│  x" (row_trimmed grid 2);
  check string "row 3" "│" (row_trimmed grid 3)

let canvas_like_resizing () =
  let grid = Grid.create ~width:1 ~height:1 () in
  let write_text text ~x ~y =
    let width =
      Glyph.measure ~width_method:(Grid.width_method grid) ~tab_width:2 text
    in
    let width = if width <= 0 then 1 else width in
    if x + width > Grid.width grid then
      Grid.resize grid ~width:(x + width) ~height:(Grid.height grid);
    if y + 1 > Grid.height grid then
      Grid.resize grid ~width:(Grid.width grid) ~height:(y + 1);
    Grid.draw_text grid ~x ~y ~text
  in
  for x = 0 to 7 do
    write_text "─" ~x ~y:0
  done;
  for y = 0 to 3 do
    write_text "│" ~x:0 ~y
  done;
  Grid.draw_text grid ~x:3 ~y:2 ~text:"x";
  check int "width resized" 8 (Grid.width grid);
  check string "row 0 after resize" "│───────" (row_trimmed grid 0);
  check string "row 2 after resize" "│  x    " (row_to_string grid 2)

let canvas_blit_into_box () =
  let canvas = Grid.create ~width:8 ~height:4 () in
  Grid.draw_text canvas ~x:0 ~y:0 ~text:"────────";
  for y = 0 to 3 do
    Grid.draw_text canvas ~x:0 ~y ~text:"│"
  done;
  Grid.draw_text canvas ~x:3 ~y:2 ~text:"x";
  let dest = Grid.create ~width:10 ~height:6 () in
  (* Draw outer border manually *)
  Grid.draw_text dest ~x:0 ~y:0 ~text:"┌";
  Grid.draw_text dest ~x:9 ~y:0 ~text:"┐";
  Grid.draw_text dest ~x:0 ~y:5 ~text:"└";
  Grid.draw_text dest ~x:9 ~y:5 ~text:"┘";
  Grid.draw_text dest ~x:1 ~y:0 ~text:"────────";
  Grid.draw_text dest ~x:1 ~y:5 ~text:"────────";
  for y = 1 to 4 do
    Grid.draw_text dest ~x:0 ~y ~text:"│";
    Grid.draw_text dest ~x:9 ~y ~text:"│"
  done;
  Grid.push_scissor dest { x = 1; y = 1; width = 8; height = 4 };
  Grid.blit_region ~src:canvas ~dst:dest ~src_x:0 ~src_y:0 ~width:8 ~height:4
    ~dst_x:1 ~dst_y:1;
  Grid.pop_scissor dest;
  check string "blit row 0" "┌────────┐" (row_trimmed dest 0);
  check string "blit row 1" "││───────│" (row_trimmed dest 1);
  check string "blit row 2" "││       │" (row_trimmed dest 2);
  check string "blit row 3" "││  x    │" (row_trimmed dest 3);
  check string "blit row 4" "││       │" (row_trimmed dest 4);
  check string "blit row 5" "└────────┘" (row_trimmed dest 5)

let clear_resets_grid () =
  let grid = Grid.create ~width:2 ~height:2 () in
  Grid.set_cell_alpha grid ~x:0 ~y:0 ~code:(Char.code 'Q') ~fg:Ansi.Color.white
    ~bg:Ansi.Color.white ~attrs:Ansi.Attr.bold ();
  let color = Ansi.Color.of_rgb 10 20 30 in
  Grid.clear ~color grid;
  for y = 0 to 1 do
    for x = 0 to 1 do
      (* After clear, chars are spaces so blank cells mirror terminal defaults. *)
      check int "char" (Char.code ' ') (read_char grid x y);
      check int "width" 1 (read_width grid x y);
      let r, g, b, _ = read_bg grid x y in
      let er, eg, eb, _ = Ansi.Color.to_rgba color in
      check (triple int int int) "color" (er, eg, eb) (r, g, b)
    done
  done

let resize_updates_dimensions () =
  let grid = Grid.create ~width:2 ~height:2 () in
  Grid.resize grid ~width:4 ~height:1;
  check int "width" 4 (Grid.width grid);
  check int "height" 1 (Grid.height grid)

let blit_copies_full_buffer () =
  let src = Grid.create ~width:2 ~height:2 () in
  Grid.set_cell_alpha src ~x:0 ~y:0 ~code:(Char.code 'A') ~fg:Ansi.Color.cyan
    ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  let dst = Grid.create ~width:1 ~height:1 () in
  Grid.blit ~src ~dst;
  check int "width" 2 (Grid.width dst);
  check int "height" 2 (Grid.height dst);
  check int "copied char" (Char.code 'A') (read_char dst 0 0)

let blit_region_copies_subrect () =
  let src = Grid.create ~width:3 ~height:3 () in
  for y = 0 to 2 do
    for x = 0 to 2 do
      let char = Char.code 'a' + idx src x y in
      Grid.set_cell_alpha src ~x ~y ~code:char ~fg:Ansi.Color.white
        ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ()
    done
  done;
  let dst = Grid.create ~width:3 ~height:3 () in
  Grid.blit_region ~src ~dst ~src_x:1 ~src_y:1 ~width:2 ~height:2 ~dst_x:0
    ~dst_y:0;
  check int "copy (0,0)" (read_char src 1 1) (read_char dst 0 0);
  check int "copy (1,1)" (read_char src 2 2) (read_char dst 1 1);
  check int "outside untouched" 32 (read_char dst 2 2)

(* Regression test: ASCII fast path produces correct output *)
let ascii_fast_path_correctness () =
  let grid = Grid.create ~width:10 ~height:1 () in
  let style =
    Ansi.Style.make
      ~fg:(Ansi.Color.of_rgb 100 150 200)
      ~bg:(Ansi.Color.of_rgb 20 30 40)
      ~bold:true ()
  in
  Grid.draw_text ~style grid ~x:0 ~y:0 ~text:"Hello";
  check int "char H" (Char.code 'H') (read_char grid 0 0);
  check int "char o" (Char.code 'o') (read_char grid 4 0);
  let r, g, _b, _a = read_fg grid 0 0 in
  check int "fg red" 100 r;
  check int "fg green" 150 g;
  let attrs = read_attr grid 0 0 |> Ansi.Attr.unpack in
  check bool "bold set" true (Ansi.Attr.mem Ansi.Attr.Bold attrs)

(* Regression test: ASCII overwrites middle of wide grapheme *)
let ascii_overwrites_wide_grapheme_middle () =
  let grid = Grid.create ~width:5 ~height:1 () in
  Grid.draw_text grid ~x:1 ~y:0 ~text:"😊";
  check int "continuation width" 0 (read_width grid 2 0);
  Grid.draw_text grid ~x:2 ~y:0 ~text:"X";
  check int "emoji cleared" 32 (read_char grid 1 0);
  check int "X written" (Char.code 'X') (read_char grid 2 0);
  check int "width reset" 1 (read_width grid 1 0)

(* Regression test: Mixed ASCII and emoji both render (control flow bug) *)
let mixed_ascii_emoji_render () =
  let grid = Grid.create ~width:10 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"Hi";
  Grid.draw_text grid ~x:2 ~y:0 ~text:"😊";
  Grid.draw_text grid ~x:4 ~y:0 ~text:"Ok";
  check int "H" (Char.code 'H') (read_char grid 0 0);
  check bool "emoji rendered" true (read_char grid 2 0 <> 0);
  check int "O" (Char.code 'O') (read_char grid 4 0)

(* Regression test: Clear resets all cells including after graphemes *)
let clear_after_graphemes () =
  let grid = Grid.create ~width:5 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"😊🚀";
  check bool "grapheme written" true (read_char grid 0 0 <> 0);
  Grid.clear grid ~color:(Ansi.Color.of_rgb 10 20 30);
  (* All cells should be cleared *)
  check int "char cleared" (Char.code ' ') (read_char grid 0 0);
  check int "width reset" 1 (read_width grid 0 0);
  let r, g, b, _a = read_bg grid 0 0 in
  check int "bg red" 10 r;
  check int "bg green" 20 g;
  check int "bg blue" 30 b

(* Edge case: Empty string is safe no-op *)
let empty_string_is_noop () =
  let grid = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"ABC";
  Grid.draw_text grid ~x:0 ~y:0 ~text:"";
  check int "A preserved" (Char.code 'A') (read_char grid 0 0)

(* Edge case: Negative x clips correctly *)
let negative_x_clips_text () =
  let grid = Grid.create ~width:5 ~height:1 () in
  Grid.draw_text grid ~x:(-2) ~y:0 ~text:"ABCDE";
  check int "C at x=0" (Char.code 'C') (read_char grid 0 0);
  check int "x=3 empty" 32 (read_char grid 3 0)

let box_drawing_characters_render () =
  let grid = Grid.create ~width:4 ~height:4 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"┌──┐";
  Grid.draw_text grid ~x:0 ~y:3 ~text:"└──┘";
  Grid.draw_text grid ~x:0 ~y:1 ~text:"│";
  Grid.draw_text grid ~x:0 ~y:2 ~text:"│";
  Grid.draw_text grid ~x:3 ~y:1 ~text:"│";
  Grid.draw_text grid ~x:3 ~y:2 ~text:"│";
  check string "top row" "┌──┐" (row_to_string grid 0);
  check string "inner row 1" "│  │" (row_to_string grid 1);
  check string "inner row 2" "│  │" (row_to_string grid 2);
  check string "bottom row" "└──┘" (row_to_string grid 3)

let draw_text_ascii_respects_scissor () =
  let grid = Grid.create ~width:5 ~height:1 () in
  (* Only columns 2..4 are writable *)
  Grid.push_scissor grid { x = 2; y = 0; width = 3; height = 1 };
  Grid.draw_text grid ~x:0 ~y:0 ~text:"Hello";
  Grid.pop_scissor grid;
  (* Outside scissor should be untouched *)
  check int "x=0 untouched" 32 (read_char grid 0 0);
  check int "x=1 untouched" 32 (read_char grid 1 0);
  (* Inside scissor has clipped text: 'llo' at 2..4 *)
  check int "x=2 l" (Char.code 'l') (read_char grid 2 0);
  check int "x=3 l" (Char.code 'l') (read_char grid 3 0);
  check int "x=4 o" (Char.code 'o') (read_char grid 4 0)

let fill_rect_respects_scissor () =
  let grid = Grid.create ~width:4 ~height:2 () in
  Grid.push_scissor grid { x = 1; y = 0; width = 2; height = 2 };
  let color = Ansi.Color.green in
  Grid.fill_rect grid ~x:0 ~y:0 ~width:4 ~height:2 ~color;
  Grid.pop_scissor grid;
  (* Only x=1 and x=2 columns should be filled with spaces and color *)
  let er, eg, eb, _ = Ansi.Color.to_rgba color in
  for y = 0 to 1 do
    (* outside left *)
    check int (Printf.sprintf "(%d, %d) untouched" 0 y) 32 (read_char grid 0 y);
    (* inside *)
    check int
      (Printf.sprintf "(%d, %d) space" 1 y)
      (Char.code ' ') (read_char grid 1 y);
    let r, g, b, _ = read_bg grid 1 y in
    check (triple int int int) "bg left in scissor" (er, eg, eb) (r, g, b);
    check int
      (Printf.sprintf "(%d, %d) space" 2 y)
      (Char.code ' ') (read_char grid 2 y);
    let r2, g2, b2, _ = read_bg grid 2 y in
    check (triple int int int) "bg right in scissor" (er, eg, eb) (r2, g2, b2);
    (* outside right *)
    check int (Printf.sprintf "(%d, %d) untouched" 3 y) 32 (read_char grid 3 y)
  done

let blit_region_respects_scissor () =
  let src = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text src ~x:0 ~y:0 ~text:"ABC";
  let dst = Grid.create ~width:3 ~height:1 () in
  Grid.push_scissor dst { x = 1; y = 0; width = 2; height = 1 };
  Grid.blit_region ~src ~dst ~src_x:0 ~src_y:0 ~width:3 ~height:1 ~dst_x:0
    ~dst_y:0;
  Grid.pop_scissor dst;
  check int "dst(0) untouched" 32 (read_char dst 0 0);
  check int "dst(1)=B" (Char.code 'B') (read_char dst 1 0);
  check int "dst(2)=C" (Char.code 'C') (read_char dst 2 0)

let clear_preserves_scissor_state () =
  let grid = Grid.create ~width:3 ~height:1 () in
  Grid.push_scissor grid { x = 1; y = 0; width = 1; height = 1 };
  Grid.clear grid;
  Grid.draw_text grid ~x:0 ~y:0 ~text:"A";
  Grid.pop_scissor grid;
  check int "write outside preserved scissor ignored" 32 (read_char grid 0 0)

let draw_text_overflow_clears_row_tail () =
  let grid = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"XYZ";
  Grid.draw_text grid ~x:2 ~y:0 ~text:"中";
  check int "overflow start cleared" (Char.code ' ') (read_char grid 2 0);
  check string "prefix preserved" "XY" (String.sub (row_to_string grid 0) 0 2)

let tests =
  [
    test_case "create defaults" `Quick create_defaults;
    test_case "create with configuration" `Quick create_with_configuration;
    test_case "set width method" `Quick set_width_method_updates;
    test_case "set respect alpha" `Quick set_respect_alpha_updates;
    test_case "set cell writes all planes" `Quick set_cell_writes_all_planes;
    test_case "set cell stores hyperlink" `Quick set_cell_records_hyperlink;
    test_case "set cell outside scissor" `Quick set_cell_outside_scissor_ignored;
    test_case "with scissor restores" `Quick with_scissor_restores_stack;
    test_case "clear preserves scissor" `Quick clear_preserves_scissor_state;
    test_case "draw text applies style" `Quick draw_text_applies_style;
    test_case "draw text inherits background" `Quick
      draw_text_inherits_existing_background;
    test_case "draw text skips newline" `Quick draw_text_skips_newline;
    test_case "clear scissor" `Quick clear_scissor_allows_future_writes;
    test_case "set cell alpha blends" `Quick set_cell_alpha_honours_blending;
    test_case "set cell alpha without respect" `Quick
      set_cell_alpha_without_respect_skips_blend;
    test_case "fill rect" `Quick fill_rect_fills_region;
    test_case "replace wide grapheme clears continuations" `Quick
      replace_wide_grapheme_clears_continuations;
    test_case "fill rect alpha preserves glyph" `Quick
      fill_rect_alpha_preserves_glyph;
    test_case "fill rect transparent preserves background" `Quick
      fill_rect_transparent_preserves_background;
    test_case "scroll uses transparent background" `Quick
      scroll_uses_transparent_background;
    test_case "draw text overwrites grapheme span" `Quick
      draw_text_overwrite_clears_span;
    test_case "draw text overflow clears row tail" `Quick
      draw_text_overflow_clears_row_tail;
    test_case "blit region skips partial spans" `Quick
      blit_region_skips_partial_span;
    test_case "draw text overflow does nothing" `Quick
      draw_text_overflow_does_nothing;
    test_case "box left border spans full height" `Quick
      draw_box_left_border_spans_full_height;
    test_case "canvas-like resizing" `Quick canvas_like_resizing;
    test_case "clear" `Quick clear_resets_grid;
    test_case "resize" `Quick resize_updates_dimensions;
    test_case "resize truncated grapheme does not bleed" `Quick
      resize_truncated_grapheme_does_not_bleed;
    test_case "blit" `Quick blit_copies_full_buffer;
    test_case "blit region" `Quick blit_region_copies_subrect;
    (* Regression tests for optimization bugs *)
    test_case "ascii fast path correctness" `Quick ascii_fast_path_correctness;
    test_case "ascii overwrites wide grapheme middle" `Quick
      ascii_overwrites_wide_grapheme_middle;
    test_case "mixed ascii emoji render" `Quick mixed_ascii_emoji_render;
    test_case "clear after graphemes" `Quick clear_after_graphemes;
    test_case "empty string is noop" `Quick empty_string_is_noop;
    test_case "negative x clips text" `Quick negative_x_clips_text;
    test_case "ambiguous width defaults to one" `Quick
      ambiguous_width_defaults_to_one;
    test_case "canvas-like primitives render" `Quick
      canvas_like_primitives_render;
    test_case "canvas blit into box" `Quick canvas_blit_into_box;
    test_case "box drawing characters render" `Quick
      box_drawing_characters_render;
    (* Scissor for fast paths *)
    test_case "draw_text ASCII respects scissor" `Quick
      draw_text_ascii_respects_scissor;
    test_case "fill_rect respects scissor" `Quick fill_rect_respects_scissor;
    test_case "blit_region respects scissor" `Quick blit_region_respects_scissor;
    test_case "inherit bg on unwritten ascii" `Quick
      inherit_bg_on_unwritten_ascii;
    test_case "unicode inherit bg on unwritten cell" `Quick
      unicode_inherit_bg_on_unwritten_cell;
    test_case "overflow respects scissor for wide grapheme" `Quick
      overflow_respects_scissor_for_wide_grapheme;
    test_case "alpha blit orphan continuation draws space" `Quick
      alpha_blit_orphan_continuation_draws_space;
    test_case "cross-pool blit remaps graphemes" `Quick
      cross_pool_blit_remaps_graphemes;
    test_case "blit preserves respect alpha" `Quick blit_preserves_respect_alpha;
    test_case "same-pool bulk blit tracks graphemes" `Quick
      blit_bulk_tracks_graphemes;
    test_case "overlap blit direction correctness" `Quick
      overlap_blit_direction_correctness;
    test_case "box title left aligned" `Quick box_title_left_aligned;
    test_case "diff detects single RGB step" `Quick diff_detects_single_rgb_step;
    test_case "alpha blit blends fg and bg" `Quick alpha_blit_blends_fg_bg;
    test_case "resize shrink clips continuation" `Quick
      resize_shrink_clips_continuation;
    test_case "draw_text blends FG alpha over opaque BG" `Quick
      draw_text_blends_fg_alpha_over_opaque_bg;
    test_case "blit_region blends semi-transparent src without respect_alpha"
      `Quick blit_region_blends_without_respect_alpha;
    test_case "scissor push intersects parent" `Quick
      scissor_push_intersects_parent;
    (* Alpha overlay semantics *)
    test_case "semi-transparent overlay preserves text and tints fg" `Quick
      (fun () ->
        let grid = Grid.create ~width:1 ~height:1 ~respect_alpha:true () in
        (* Draw text with white fg on black bg *)
        let white = Ansi.Color.of_rgba 255 255 255 255 in
        let black = Ansi.Color.of_rgba 0 0 0 255 in
        let style = Ansi.Style.make ~fg:white ~bg:black () in
        Grid.draw_text grid ~x:0 ~y:0 ~text:"X" ~style;
        (* Overlay with semi-transparent red background *)
        let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
        Grid.fill_rect grid ~x:0 ~y:0 ~width:1 ~height:1 ~color:semi_red;
        (* Text should be preserved *)
        check int "char preserved" (Char.code 'X') (read_char grid 0 0);
        (* FG should be tinted (white + red overlay) *)
        let r_fg, g_fg, b_fg, a_fg = read_fg grid 0 0 in
        check bool "fg red increased" true (r_fg > 128);
        (* Should be blended *)
        check bool "fg green decreased" true (g_fg < 255);
        check bool "fg blue decreased" true (b_fg < 255);
        check int "fg alpha preserved" 255 a_fg;
        (* BG should be blended (red over black) *)
        let r_bg, g_bg, b_bg, a_bg = read_bg grid 0 0 in
        check bool "bg red blended" true (r_bg > 128 && r_bg < 255);
        check int "bg green blended" 0 g_bg;
        check int "bg blue blended" 0 b_bg;
        check int "bg alpha is overlay" 128 a_bg);
    test_case "semi-transparent overlay on space doesn't preserve" `Quick
      (fun () ->
        let grid = Grid.create ~width:1 ~height:1 ~respect_alpha:true () in
        (* Fill with white background (space) *)
        let white = Ansi.Color.of_rgba 255 255 255 255 in
        Grid.fill_rect grid ~x:0 ~y:0 ~width:1 ~height:1 ~color:white;
        (* Overlay with semi-transparent red *)
        let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
        Grid.fill_rect grid ~x:0 ~y:0 ~width:1 ~height:1 ~color:semi_red;
        (* Should remain a space, no text preservation *)
        check int "remains space" (Char.code ' ') (read_char grid 0 0);
        (* BG should be blended (red over white = pink) *)
        let r_bg, g_bg, b_bg, a_bg = read_bg grid 0 0 in
        check int "bg red full" 255 r_bg;
        check bool "bg green blended" true (g_bg > 0 && g_bg < 255);
        check bool "bg blue blended" true (b_bg > 0 && b_bg < 255);
        check int "bg alpha is overlay" 128 a_bg);
    (* Box clipping tests *)
    test_case "box partially off left edge uses correct corners" `Quick
      (fun () ->
        let grid = Grid.create ~width:3 ~height:3 () in
        let border_chars = Grid.Border.single in
        Grid.draw_box grid ~x:(-1) ~y:0 ~width:4 ~height:3 ~border_chars
          ~border_sides:[ `Top; `Right; `Bottom; `Left ]
          ~border_style:Ansi.Style.default ~bg_color:Ansi.Color.black
          ~should_fill:false ();
        (* Box is clipped on left, so no left corners are drawn *)
        check int "horizontal at (0,0)" border_chars.horizontal
          (read_char grid 0 0);
        check int "top-right corner at (2,0)" border_chars.top_right
          (read_char grid 2 0);
        check int "horizontal at (0,2)" border_chars.horizontal
          (read_char grid 0 2);
        check int "bottom-right corner at (2,2)" border_chars.bottom_right
          (read_char grid 2 2));
    test_case "box partially off top edge extends verticals down" `Quick
      (fun () ->
        let grid = Grid.create ~width:3 ~height:3 () in
        let border_chars = Grid.Border.single in
        Grid.draw_box grid ~x:0 ~y:(-1) ~width:3 ~height:4 ~border_chars
          ~border_sides:[ `Top; `Left ] ~border_style:Ansi.Style.default
          ~bg_color:Ansi.Color.black ~should_fill:false ();
        (* Top not drawn, so verticals should extend to top of screen *)
        check int "left border at (0,0)" border_chars.vertical
          (read_char grid 0 0);
        check int "left border at (0,1)" border_chars.vertical
          (read_char grid 0 1);
        check int "left border at (0,2)" border_chars.vertical
          (read_char grid 0 2));
    test_case "box partially off right edge uses correct right corners" `Quick
      (fun () ->
        let grid = Grid.create ~width:3 ~height:3 () in
        let border_chars = Grid.Border.single in
        Grid.draw_box grid ~x:1 ~y:0 ~width:3 ~height:3 ~border_chars
          ~border_sides:[ `Top; `Right; `Bottom; `Left ]
          ~border_style:Ansi.Style.default ~bg_color:Ansi.Color.black
          ~should_fill:false ();
        (* Box extends beyond right edge, so no right corners are drawn *)
        check int "horizontal at (2,0)" border_chars.horizontal
          (read_char grid 2 0);
        check int "horizontal at (2,2)" border_chars.horizontal
          (read_char grid 2 2));
    test_case "box fully inside grid works normally" `Quick (fun () ->
        let grid = Grid.create ~width:5 ~height:5 () in
        let border_chars = Grid.Border.single in
        Grid.draw_box grid ~x:1 ~y:1 ~width:3 ~height:3 ~border_chars
          ~border_sides:[ `Top; `Right; `Bottom; `Left ]
          ~border_style:Ansi.Style.default ~bg_color:Ansi.Color.black
          ~should_fill:false ();
        check int "top-left corner" border_chars.top_left (read_char grid 1 1);
        check int "top-right corner" border_chars.top_right (read_char grid 3 1);
        check int "bottom-left corner" border_chars.bottom_left
          (read_char grid 1 3);
        check int "bottom-right corner" border_chars.bottom_right
          (read_char grid 3 3));
    (* Diff tests *)
    test_case "diff identical grids produces no diffs" `Quick (fun () ->
        let a = Grid.create ~width:2 ~height:2 () in
        let b = Grid.copy a in
        let diffs = Grid.diff_cells a b in
        check int "no diffs" 0 (Array.length diffs));
    test_case "diff detects single char change" `Quick (fun () ->
        let a = Grid.create ~width:2 ~height:2 () in
        let b = Grid.copy a in
        Grid.set_cell_alpha b ~x:1 ~y:1 ~code:(Char.code 'X')
          ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
        let diffs = Grid.diff_cells a b in
        check
          (list (pair int int))
          "single diff at changed cell"
          [ (1, 1) ]
          (Array.to_list diffs));
    test_case "diff detects single color change" `Quick (fun () ->
        let a = Grid.create ~width:2 ~height:2 () in
        let b = Grid.copy a in
        Grid.fill_rect b ~x:0 ~y:0 ~width:1 ~height:1 ~color:Ansi.Color.red;
        let diffs = Grid.diff_cells a b in
        check
          (list (pair int int))
          "single diff at colored cell"
          [ (0, 0) ]
          (Array.to_list diffs));
    test_case "diff detects hyperlink change" `Quick (fun () ->
        let a = Grid.create ~width:2 ~height:2 () in
        let b = Grid.copy a in
        Grid.set_cell_alpha b ~x:0 ~y:0 ~code:(Char.code 'A')
          ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty
          ~link:"http://example.com" ();
        let diffs = Grid.diff_cells a b in
        check
          (list (pair int int))
          "single diff at linked cell"
          [ (0, 0) ]
          (Array.to_list diffs));
    (* Resize tests *)
    test_case "resize preserves overlapping content" `Quick (fun () ->
        let grid = Grid.create ~width:4 ~height:2 () in
        Grid.draw_text grid ~x:0 ~y:0 ~text:"ABCD";
        Grid.draw_text grid ~x:0 ~y:1 ~text:"EFGH";
        (* Resize to smaller - should preserve top-left content *)
        Grid.resize grid ~width:2 ~height:1;
        check int "width after resize" 2 (Grid.width grid);
        check int "height after resize" 1 (Grid.height grid);
        check string "preserved content" "AB" (row_trimmed grid 0));
    test_case "resize up fills new areas with spaces" `Quick (fun () ->
        let grid = Grid.create ~width:2 ~height:1 () in
        Grid.draw_text grid ~x:0 ~y:0 ~text:"AB";
        Grid.resize grid ~width:4 ~height:2;
        check int "width after resize" 4 (Grid.width grid);
        check int "height after resize" 2 (Grid.height grid);
        check string "original content preserved" "AB  " (row_to_string grid 0);
        check string "new row is spaces" "    " (row_to_string grid 1));
  ]

let () = run "matrix.grid" [ ("grid", tests) ]
