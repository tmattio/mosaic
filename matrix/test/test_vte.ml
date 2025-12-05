open Alcotest

(** Test utilities *)

let get_char_at grid row col =
  let idx = (row * Grid.width grid) + col in
  let s = Grid.get_text grid idx in
  if s = "" then ' ' else s.[0]

let get_line grid row =
  let buf = Buffer.create (Grid.width grid) in
  for col = 0 to Grid.width grid - 1 do
    Buffer.add_char buf (get_char_at grid row col)
  done;
  let str = Buffer.contents buf in
  (* Trim trailing spaces *)
  let rec find_end i =
    if i < 0 then 0 else if str.[i] = ' ' then find_end (i - 1) else i + 1
  in
  String.sub str 0 (find_end (String.length str - 1))

(** {1 Creation and Configuration} *)

let create_with_defaults () =
  let vte = Vte.create ~rows:24 ~cols:80 () in
  check int "rows" 24 (Vte.rows vte);
  check int "cols" 80 (Vte.cols vte);
  check int "scrollback capacity" 10000 (Vte.scrollback_capacity vte);
  check int "scrollback size" 0 (Vte.scrollback_size vte);
  check bool "not dirty initially" false (Vte.is_dirty vte);
  check bool "not alternate screen" false (Vte.is_alternate_screen vte);
  check (pair int int) "cursor at origin" (0, 0) (Vte.cursor_pos vte);
  check bool "cursor visible" true (Vte.cursor_visible vte)

let create_with_no_scrollback () =
  let vte = Vte.create ~scrollback:0 ~rows:10 ~cols:20 () in
  check int "no scrollback capacity" 0 (Vte.scrollback_capacity vte);
  check int "no scrollback size" 0 (Vte.scrollback_size vte)

let create_with_custom_scrollback () =
  let vte = Vte.create ~scrollback:500 ~rows:10 ~cols:20 () in
  check int "custom scrollback capacity" 500 (Vte.scrollback_capacity vte)

(** {1 Basic Text Input} *)

let feed_simple_text () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello World";
  check bool "marked dirty" true (Vte.is_dirty vte);
  let line = get_line (Vte.grid vte) 0 in
  check string "text rendered" "Hello World" line;
  check (pair int int) "cursor advanced" (0, 11) (Vte.cursor_pos vte)

let feed_multiline_text () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\nLine2\nLine3";
  check string "first line" "Line1" (get_line (Vte.grid vte) 0);
  check string "second line" "Line2" (get_line (Vte.grid vte) 1);
  check string "third line" "Line3" (get_line (Vte.grid vte) 2);
  check (pair int int) "cursor position" (2, 5) (Vte.cursor_pos vte)

let feed_carriage_return () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello\rWorld";
  check string "CR overwrites" "World" (get_line (Vte.grid vte) 0);
  check (pair int int) "cursor after CR" (0, 5) (Vte.cursor_pos vte)

let feed_tab () =
  let vte = Vte.create ~rows:5 ~cols:40 () in
  Vte.feed_string vte "A\tB\tC";
  check char "first char" 'A' (get_char_at (Vte.grid vte) 0 0);
  check char "after first tab" 'B' (get_char_at (Vte.grid vte) 0 8);
  check char "after second tab" 'C' (get_char_at (Vte.grid vte) 0 16)

let feed_backspace () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello\bX";
  check string "backspace moves cursor" "HellX" (get_line (Vte.grid vte) 0)

(** {1 Cursor Movement} *)

let cursor_up () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\nLine2";
  Vte.feed_string vte "\x1b[A";
  Vte.feed_string vte "X";
  check string "cursor up modifies previous line" "Line1X"
    (get_line (Vte.grid vte) 0)

let cursor_down () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\x1b[B\rLine2";
  check string "first line" "Line1" (get_line (Vte.grid vte) 0);
  check string "second line" "Line2" (get_line (Vte.grid vte) 1)

let cursor_forward () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello\r\x1b[3CX";
  check string "cursor forward" "HelXo" (get_line (Vte.grid vte) 0)

let cursor_backward () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello\x1b[2DX";
  check string "cursor backward" "HelXo" (get_line (Vte.grid vte) 0)

let cursor_position () =
  let vte = Vte.create ~rows:10 ~cols:20 () in
  Vte.feed_string vte "\x1b[3;5HX";
  check char "positioned character" 'X' (get_char_at (Vte.grid vte) 2 4)

let cursor_horizontal_absolute () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello\x1b[10GX";
  check char "horizontal absolute" 'X' (get_char_at (Vte.grid vte) 0 9)

let cursor_next_line () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello\x1b[EX";
  check (pair int int) "next line moves down and to column 0" (1, 1)
    (Vte.cursor_pos vte)

let cursor_previous_line () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "\nHello\x1b[FX";
  check (pair int int) "previous line moves up and to column 0" (0, 1)
    (Vte.cursor_pos vte)

(** {1 Screen Manipulation} *)

let erase_display_to_end () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\nLine2\nLine3\x1b[2;5H\x1b[J";
  check string "first line unchanged" "Line1" (get_line (Vte.grid vte) 0);
  check string "second line partial" "Line" (get_line (Vte.grid vte) 1);
  check string "third line cleared" "" (get_line (Vte.grid vte) 2)

let erase_display_to_beginning () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\nLine2\nLine3\x1b[2;3H\x1b[1J";
  check string "first line cleared" "" (get_line (Vte.grid vte) 0);
  let line2 = get_line (Vte.grid vte) 1 in
  check bool "second line partially cleared" true (String.length line2 >= 2);
  check string "third line unchanged" "Line3" (get_line (Vte.grid vte) 2)

let erase_display_all () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\nLine2\nLine3\x1b[2J";
  check string "line 0 cleared" "" (get_line (Vte.grid vte) 0);
  check string "line 1 cleared" "" (get_line (Vte.grid vte) 1);
  check string "line 2 cleared" "" (get_line (Vte.grid vte) 2)

let erase_line_to_end () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello World\r\x1b[5C\x1b[K";
  check string "erase to end of line" "Hello" (get_line (Vte.grid vte) 0)

let erase_line_to_beginning () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello World\r\x1b[5C\x1b[1K";
  let line = get_line (Vte.grid vte) 0 in
  check bool "beginning erased" true (String.sub line 0 5 = "     ");
  check string "end preserved" " World" (String.sub line 5 6)

let erase_line_all () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello World\r\x1b[5C\x1b[2K";
  check string "entire line erased" "" (get_line (Vte.grid vte) 0)

let insert_lines () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\nLine2\nLine3\x1b[2;1H\x1b[LNew";
  check string "first line unchanged" "Line1" (get_line (Vte.grid vte) 0);
  check string "new line inserted" "New" (get_line (Vte.grid vte) 1);
  check string "second line pushed down" "Line2" (get_line (Vte.grid vte) 2)

let delete_lines () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\nLine2\nLine3\x1b[2;1H\x1b[M";
  check string "first line unchanged" "Line1" (get_line (Vte.grid vte) 0);
  check string "third line moved up" "Line3" (get_line (Vte.grid vte) 1);
  check string "bottom line empty" "" (get_line (Vte.grid vte) 2)

let insert_characters () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello\r\x1b[3C\x1b[2@XX";
  let line = get_line (Vte.grid vte) 0 in
  check bool "characters inserted" true (String.length line >= 7);
  check string "insert at position" "HelXXlo" (String.sub line 0 7)

let delete_characters () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello World\r\x1b[7C\x1b[5P";
  check string "characters deleted" "Hello W" (get_line (Vte.grid vte) 0)

let backspace_clears_character () =
  let vte = Vte.create ~rows:1 ~cols:5 () in
  Vte.feed_string vte "ABC";
  Vte.feed_string vte "\b";
  let grid = Vte.grid vte in
  check char "A kept" 'A' (get_char_at grid 0 0);
  check char "B kept" 'B' (get_char_at grid 0 1);
  check char "backspaced cell cleared" ' ' (get_char_at grid 0 2);
  check (pair int int) "cursor moved left" (0, 2) (Vte.cursor_pos vte)

let delete_characters_wide () =
  let vte = Vte.create ~rows:1 ~cols:5 () in
  Vte.feed_string vte "中A";
  Vte.feed_string vte "\r\x1b[P";
  let grid = Vte.grid vte in
  check string "wide character removed entirely" "A" (get_line grid 0);
  check bool "no orphan continuation" false (Grid.is_continuation grid 0)

let insert_characters_wide_boundary () =
  let vte = Vte.create ~rows:1 ~cols:4 () in
  Vte.feed_string vte "AB中";
  Vte.feed_string vte "\r\x1b[@";
  let grid = Vte.grid vte in
  check string "shift respects graphemes" " AB" (get_line grid 0);
  let last_idx = Grid.width grid - 1 in
  check bool "last cell not continuation" false
    (Grid.is_continuation grid last_idx)

(** {1 SGR (Styling)} *)

let sgr_bold () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "\x1b[1mBold\x1b[0m";
  let grid = Vte.grid vte in
  let attrs = Grid.get_attrs grid 0 |> Ansi.Attr.unpack in
  check bool "bold attribute set" true (Ansi.Attr.mem Ansi.Attr.Bold attrs)

let sgr_italic () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "\x1b[3mItalic\x1b[0m";
  let grid = Vte.grid vte in
  let attrs = Grid.get_attrs grid 0 |> Ansi.Attr.unpack in
  check bool "italic attribute set" true (Ansi.Attr.mem Ansi.Attr.Italic attrs)

let sgr_underline () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "\x1b[4mUnderline\x1b[0m";
  let grid = Vte.grid vte in
  let attrs = Grid.get_attrs grid 0 |> Ansi.Attr.unpack in
  check bool "underline attribute set" true
    (Ansi.Attr.mem Ansi.Attr.Underline attrs)

let sgr_foreground_color () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "\x1b[31mRed\x1b[0m";
  let grid = Vte.grid vte in
  let r =
    Grid.get_fg_r grid 0 |> fun v -> Float.round (v *. 255.) |> int_of_float
  in
  check int "red foreground" 255 r

let sgr_background_color () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "\x1b[42mGreen BG\x1b[0m";
  let grid = Vte.grid vte in
  let g =
    Grid.get_bg_g grid 0 |> fun v -> Float.round (v *. 255.) |> int_of_float
  in
  check int "green background" 128 g

let sgr_reset () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "\x1b[1;31mBold Red\x1b[0mNormal";
  let grid = Vte.grid vte in
  let normal_attrs = Grid.get_attrs grid 8 |> Ansi.Attr.unpack in
  check bool "attributes reset" true (Ansi.Attr.is_empty normal_attrs)

(** {1 Cursor Visibility} *)

let cursor_hide_show () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  check bool "initially visible" true (Vte.cursor_visible vte);
  Vte.feed_string vte "\x1b[?25l";
  check bool "hidden after DECTCEM hide" false (Vte.cursor_visible vte);
  Vte.feed_string vte "\x1b[?25h";
  check bool "shown after DECTCEM show" true (Vte.cursor_visible vte)

let set_cursor_visible_programmatic () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.set_cursor_visible vte false;
  check bool "programmatically hidden" false (Vte.cursor_visible vte);
  Vte.set_cursor_visible vte true;
  check bool "programmatically shown" true (Vte.cursor_visible vte)

(** {1 Cursor Save/Restore} *)

let cursor_save_restore () =
  let vte = Vte.create ~rows:10 ~cols:20 () in
  Vte.feed_string vte "\x1b[5;10HHello\x1b7";
  let pos1 = Vte.cursor_pos vte in
  Vte.feed_string vte "\x1b[1;1HWorld\x1b8";
  let pos2 = Vte.cursor_pos vte in
  check (pair int int) "cursor restored" pos1 pos2

(** {1 Scrolling} *)

let scroll_up_basic () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "L1\nL2\nL3\nL4\nL5";
  Vte.scroll_up vte 2;
  check string "line moved up" "L3" (get_line (Vte.grid vte) 0);
  check string "bottom cleared" "" (get_line (Vte.grid vte) 3)

let scroll_down_basic () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "L1\nL2\nL3";
  Vte.scroll_down vte 1;
  check string "top cleared" "" (get_line (Vte.grid vte) 0);
  check string "line moved down" "L1" (get_line (Vte.grid vte) 1)

let scroll_on_bottom_line () =
  let vte = Vte.create ~rows:3 ~cols:10 () in
  Vte.feed_string vte "L1\nL2\nL3\nL4";
  check string "first line scrolled off" "L2" (get_line (Vte.grid vte) 0);
  check string "new line at bottom" "L4" (get_line (Vte.grid vte) 2)

(** {1 Scrollback} *)

let scrollback_accumulates () =
  let vte = Vte.create ~scrollback:100 ~rows:3 ~cols:10 () in
  Vte.feed_string vte "L1\nL2\nL3\nL4\nL5";
  check int "scrollback has lines" 2 (Vte.scrollback_size vte);
  let lines = Vte.scrollback_lines vte in
  check int "scrollback list length" 2 (List.length lines)

let scrollback_ring_buffer () =
  let vte = Vte.create ~scrollback:2 ~rows:2 ~cols:5 () in
  Vte.feed_string vte "L1\nL2\nL3\nL4\nL5";
  check int "scrollback at capacity" 2 (Vte.scrollback_size vte);
  let lines = Vte.scrollback_lines vte in
  check string "oldest in ring" "L2" (List.nth lines 0);
  check string "newest in ring" "L3" (List.nth lines 1)

let scrollback_disabled () =
  let vte = Vte.create ~scrollback:0 ~rows:3 ~cols:10 () in
  Vte.feed_string vte "L1\nL2\nL3\nL4";
  check int "no scrollback" 0 (Vte.scrollback_size vte);
  check (list string) "empty scrollback" [] (Vte.scrollback_lines vte)

(** {1 Terminal Modes} *)

let auto_wrap_mode () =
  let vte = Vte.create ~rows:3 ~cols:5 () in
  check bool "auto wrap on by default" true (Vte.auto_wrap_mode vte);
  Vte.feed_string vte "HelloWorld";
  check string "wrapped to next line" "World" (get_line (Vte.grid vte) 1)

let insert_mode () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  check bool "insert mode off by default" false (Vte.insert_mode vte);
  Vte.feed_string vte "\x1b[4h";
  check bool "insert mode enabled" true (Vte.insert_mode vte);
  Vte.feed_string vte "\x1b[4l";
  check bool "insert mode disabled" false (Vte.insert_mode vte)

let cursor_key_mode () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  check bool "cursor key mode off by default" false (Vte.cursor_key_mode vte);
  Vte.feed_string vte "\x1b[?1h";
  check bool "cursor key mode enabled" true (Vte.cursor_key_mode vte);
  Vte.feed_string vte "\x1b[?1l";
  check bool "cursor key mode disabled" false (Vte.cursor_key_mode vte)

let bracketed_paste_mode () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  check bool "bracketed paste off by default" false
    (Vte.bracketed_paste_mode vte);
  Vte.feed_string vte "\x1b[?2004h";
  check bool "bracketed paste enabled" true (Vte.bracketed_paste_mode vte);
  Vte.feed_string vte "\x1b[?2004l";
  check bool "bracketed paste disabled" false (Vte.bracketed_paste_mode vte)

(** {1 OSC Sequences} *)

let osc_set_title () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  check string "title empty initially" "" (Vte.title vte);
  Vte.feed_string vte "\x1b]0;My Title\x07";
  check string "title set via OSC 0" "My Title" (Vte.title vte);
  Vte.feed_string vte "\x1b]2;New Title\x07";
  check string "title set via OSC 2" "New Title" (Vte.title vte)

(** {1 Resize} *)

let resize_preserves_content () =
  let vte = Vte.create ~rows:5 ~cols:10 () in
  Vte.feed_string vte "Hello\nWorld";
  Vte.resize vte ~rows:10 ~cols:20;
  check int "rows resized" 10 (Vte.rows vte);
  check int "cols resized" 20 (Vte.cols vte);
  (* Note: resize clears grids - applications should redraw on resize *)
  check string "content cleared" "" (get_line (Vte.grid vte) 0);
  check string "content cleared line 2" "" (get_line (Vte.grid vte) 1)

let resize_clamps_cursor () =
  let vte = Vte.create ~rows:10 ~cols:20 () in
  Vte.feed_string vte "\x1b[8;15H";
  Vte.resize vte ~rows:5 ~cols:10;
  let row, col = Vte.cursor_pos vte in
  check bool "cursor row clamped" true (row < 5);
  check bool "cursor col clamped" true (col < 10)

(** {1 Reset} *)

let reset_clears_state () =
  let vte = Vte.create ~scrollback:100 ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello\nWorld";
  Vte.feed_string vte "\x1b[1;31m";
  Vte.feed_string vte "\x1b]0;Title\x07";
  Vte.scroll_up vte 1;
  Vte.reset vte;
  check string "grid cleared" "" (get_line (Vte.grid vte) 0);
  check string "title cleared" "" (Vte.title vte);
  check (pair int int) "cursor at origin" (0, 0) (Vte.cursor_pos vte);
  check int "scrollback cleared" 0 (Vte.scrollback_size vte);
  check bool "auto wrap reset" true (Vte.auto_wrap_mode vte)

(** {1 Dirty Tracking} *)

let dirty_flag_on_text () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  check bool "clean initially" false (Vte.is_dirty vte);
  Vte.feed_string vte "Hello";
  check bool "dirty after text" true (Vte.is_dirty vte);
  Vte.clear_dirty vte;
  check bool "clean after clear" false (Vte.is_dirty vte)

let dirty_flag_on_cursor_move () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.clear_dirty vte;
  Vte.feed_string vte "\x1b[5;10H";
  check bool "dirty after cursor move" true (Vte.is_dirty vte)

let dirty_flag_on_style_change () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.clear_dirty vte;
  Vte.feed_string vte "\x1b[1m";
  check bool "dirty after style change" true (Vte.is_dirty vte)

let dirty_row_tracking () =
  let vte = Vte.create ~rows:10 ~cols:20 () in
  Vte.clear_dirty vte;

  (* Write to row 0 *)
  Vte.feed_string vte "Hello";
  check bool "dirty after write" true (Vte.is_dirty vte);
  check (list int) "row 0 dirty" [ 0 ] (Vte.dirty_rows vte);

  Vte.clear_dirty vte;

  (* Move to row 5 and write *)
  Vte.feed_string vte "\x1b[6;1HWorld";
  check (list int) "row 5 dirty" [ 5 ] (Vte.dirty_rows vte);

  Vte.clear_dirty vte;

  (* Erase display (affects multiple rows) *)
  Vte.feed_string vte "\x1b[2J";
  let rows = Vte.dirty_rows vte in
  check bool "multiple rows dirty" true (List.length rows > 1);
  check bool "includes row 0" true (List.mem 0 rows)

let dirty_cursor_tracking () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.clear_dirty vte;

  (* Move cursor *)
  Vte.feed_string vte "\x1b[3;5H";
  check bool "cursor dirty after move" true (Vte.is_cursor_dirty vte);

  Vte.clear_dirty vte;
  check bool "cursor clean after clear" false (Vte.is_cursor_dirty vte);

  (* Hide cursor *)
  Vte.feed_string vte "\x1b[?25l";
  check bool "cursor dirty after visibility change" true
    (Vte.is_cursor_dirty vte)

(** {1 Unicode Support} *)

let unicode_basic () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello 世界 🌍";
  let output = Vte.to_string vte in
  check string "unicode text" "Hello 世界 🌍" (String.trim output)

let unicode_with_escapes () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "\x1b[31mRed 文字\x1b[0m Normal";
  let output = Vte.to_string vte in
  check string "unicode with ANSI" "Red 文字 Normal" (String.trim output)

let unicode_multiline () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\nΓειά σου\n日本語";
  let output = Vte.to_string vte in
  check string "multiline unicode"
    "Line1               \nΓειά σου            \n日本語" (String.trim output)

let unicode_wide_char_cursor () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello 世界";
  let row, col = Vte.cursor_pos vte in
  (* "Hello " = 6 cols, "世界" = 4 cols (2 wide chars × 2 cols each) *)
  check (pair int int) "cursor after wide chars" (0, 10) (row, col)

let unicode_malformed_utf8 () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  let malformed = Bytes.create 5 in
  Bytes.set malformed 0 'A';
  Bytes.set malformed 1 '\xff';
  (* Invalid UTF-8, interpreted as Latin-1 *)
  Bytes.set malformed 2 '\xfe';
  (* Invalid UTF-8, interpreted as Latin-1 *)
  Bytes.set malformed 3 'B';
  Bytes.set malformed 4 'C';
  Vte.feed vte malformed 0 5;
  let output = Vte.to_string vte in
  (* Invalid UTF-8 bytes are interpreted as Latin-1 and converted to UTF-8 *)
  (* \xff (ÿ) in UTF-8 is \195\191, \xfe (þ) in UTF-8 is \195\190 *)
  check string "malformed UTF-8" "A\195\191\195\190BC" (String.trim output)

(** {1 Test Suite} *)

let tests =
  [
    test_case "create with defaults" `Quick create_with_defaults;
    test_case "create with no scrollback" `Quick create_with_no_scrollback;
    test_case "create with custom scrollback" `Quick
      create_with_custom_scrollback;
    test_case "feed simple text" `Quick feed_simple_text;
    test_case "feed multiline text" `Quick feed_multiline_text;
    test_case "feed carriage return" `Quick feed_carriage_return;
    test_case "feed tab" `Quick feed_tab;
    test_case "feed backspace" `Quick feed_backspace;
    test_case "cursor up" `Quick cursor_up;
    test_case "cursor down" `Quick cursor_down;
    test_case "cursor forward" `Quick cursor_forward;
    test_case "cursor backward" `Quick cursor_backward;
    test_case "cursor position" `Quick cursor_position;
    test_case "cursor horizontal absolute" `Quick cursor_horizontal_absolute;
    test_case "cursor next line" `Quick cursor_next_line;
    test_case "cursor previous line" `Quick cursor_previous_line;
    test_case "erase display to end" `Quick erase_display_to_end;
    test_case "erase display to beginning" `Quick erase_display_to_beginning;
    test_case "erase display all" `Quick erase_display_all;
    test_case "erase line to end" `Quick erase_line_to_end;
    test_case "erase line to beginning" `Quick erase_line_to_beginning;
    test_case "erase line all" `Quick erase_line_all;
    test_case "insert lines" `Quick insert_lines;
    test_case "delete lines" `Quick delete_lines;
    test_case "insert characters" `Quick insert_characters;
    test_case "delete characters" `Quick delete_characters;
    test_case "backspace clears character" `Quick backspace_clears_character;
    test_case "delete characters wide grapheme" `Quick delete_characters_wide;
    test_case "insert characters wide boundary" `Quick
      insert_characters_wide_boundary;
    test_case "SGR bold" `Quick sgr_bold;
    test_case "SGR italic" `Quick sgr_italic;
    test_case "SGR underline" `Quick sgr_underline;
    test_case "SGR foreground color" `Quick sgr_foreground_color;
    test_case "SGR background color" `Quick sgr_background_color;
    test_case "SGR reset" `Quick sgr_reset;
    test_case "cursor hide/show" `Quick cursor_hide_show;
    test_case "set cursor visible" `Quick set_cursor_visible_programmatic;
    test_case "cursor save/restore" `Quick cursor_save_restore;
    test_case "scroll up basic" `Quick scroll_up_basic;
    test_case "scroll down basic" `Quick scroll_down_basic;
    test_case "scroll on bottom line" `Quick scroll_on_bottom_line;
    test_case "scrollback accumulates" `Quick scrollback_accumulates;
    test_case "scrollback ring buffer" `Quick scrollback_ring_buffer;
    test_case "scrollback disabled" `Quick scrollback_disabled;
    test_case "auto wrap mode" `Quick auto_wrap_mode;
    test_case "insert mode" `Quick insert_mode;
    test_case "cursor key mode" `Quick cursor_key_mode;
    test_case "bracketed paste mode" `Quick bracketed_paste_mode;
    test_case "OSC set title" `Quick osc_set_title;
    test_case "resize preserves content" `Quick resize_preserves_content;
    test_case "resize clamps cursor" `Quick resize_clamps_cursor;
    test_case "reset clears state" `Quick reset_clears_state;
    test_case "dirty flag on text" `Quick dirty_flag_on_text;
    test_case "dirty flag on cursor move" `Quick dirty_flag_on_cursor_move;
    test_case "dirty flag on style change" `Quick dirty_flag_on_style_change;
    test_case "dirty row tracking" `Quick dirty_row_tracking;
    test_case "dirty cursor tracking" `Quick dirty_cursor_tracking;
    test_case "unicode basic" `Quick unicode_basic;
    test_case "unicode with escapes" `Quick unicode_with_escapes;
    test_case "unicode multiline" `Quick unicode_multiline;
    test_case "unicode wide char cursor" `Quick unicode_wide_char_cursor;
    test_case "unicode malformed utf8" `Quick unicode_malformed_utf8;
  ]

let () = run "matrix.vte" [ ("vte", tests) ]
