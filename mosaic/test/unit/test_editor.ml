open Windtrap
open Mosaic_ui

(* ── Helpers ── *)

let sel = option (pair int int)
let at pos editor = Editor.set_cursor pos editor
let text_changed a b = not (String.equal (Editor.text a) (Editor.text b))

(* A value whose caret and selection did not move is returned unchanged. *)
let unchanged ~msg before after = is_true ~msg (after == before)
let no_op ~msg f editor = unchanged ~msg editor (f editor)

let persistent_edits () =
  let original = Editor.of_string "a\xcc\x81b" in
  let selected = Editor.select ~anchor:0 ~cursor:1 original in
  let edited = Editor.insert "x" selected in
  equal string "a\xcc\x81b" (Editor.text original);
  equal int 2 (Editor.cursor original);
  is_none (Editor.selection original);
  equal string "a\xcc\x81" (Editor.selected_text selected);
  equal string "xb" (Editor.text edited);
  let restored = Editor.undo edited in
  equal string (Editor.text selected) (Editor.text restored);
  equal
    (option (pair int int))
    (Editor.selection selected)
    (Editor.selection restored)

let independent_history_branches () =
  let base = Editor.of_string "a" |> Editor.insert "b" in
  let left = Editor.insert "x" base and right = Editor.insert "y" base in
  equal string "abx" (Editor.text left);
  equal string "aby" (Editor.text right);
  equal string "ab" (Editor.text (Editor.undo left));
  equal string "a" (Editor.text (Editor.undo base));
  equal string "aby" (Editor.text (Editor.redo (Editor.undo right)))

let clear_restores_original_caret () =
  let original = Editor.of_string "abcd" |> Editor.set_cursor 1 in
  let cleared = Editor.clear original in
  equal string "" (Editor.text cleared);
  let restored = Editor.undo cleared in
  equal string "abcd" (Editor.text restored);
  equal int 1 (Editor.cursor restored);
  is_none (Editor.selection restored)

let cursor_shares_large_source () =
  let original = Editor.of_string (String.make 1_000_000 'x') in
  let source = Editor.source original in
  let before = Gc.allocated_bytes () in
  let moved = ref original in
  for i = 0 to 999 do
    moved := Editor.set_cursor i !moved
  done;
  let allocated = Gc.allocated_bytes () -. before in
  is_true ~msg:"cursor transitions share indexed source"
    (Editor.source !moved == source);
  is_true ~msg:"cursor transitions share text"
    (Editor.text !moved == Editor.text original);
  is_true ~msg:"cursor movement allocation is independent of document length"
    (allocated < 1_000_000.);
  equal int 1_000_000 (Editor.cursor original)

let restored_source_is_shared_by_cursor_branches () =
  let restored = Editor.of_string "abc" |> Editor.insert "d" |> Editor.undo in
  let left = Editor.move_left restored and home = Editor.move_home restored in
  is_true (Editor.source left == Editor.source home);
  is_true (Editor.source restored == Editor.source left)

let stale_same_source_proposal () =
  let original = Editor.of_string "abcd" in
  let proposal = Editor.insert "x" original in
  let moved = Editor.move_left original in
  is_true (Editor.source moved == Editor.source original);
  is_true (Editor.accept ~current:moved proposal == moved);
  let selected = Editor.select ~anchor:0 ~cursor:4 original in
  is_true (Editor.accept ~current:selected proposal == selected);
  is_true (Editor.accept ~current:original proposal == proposal)

let stale_affinity_proposal () =
  let original = Editor.of_string "abcd" in
  let proposal = Editor.insert "x" original in
  let moved =
    Editor.set_caret
      Matrix.Text.Layout.{ grapheme = 4; affinity = Upstream }
      original
  in
  equal int (Editor.cursor original) (Editor.cursor moved);
  is_true (Editor.accept ~current:moved proposal == moved)

let stale_text_proposal () =
  let original = Editor.of_string "abcd" in
  let proposal = Editor.insert "x" original in
  let caret_proposal = Editor.move_left original in
  let edited = Editor.insert "y" original in
  is_true ~msg:"edit proposal after a text change"
    (Editor.accept ~current:edited proposal == edited);
  is_true ~msg:"caret proposal does not restore older text"
    (Editor.accept ~current:edited caret_proposal == edited);
  let same_text = Editor.insert "x" original in
  equal string (Editor.text proposal) (Editor.text same_text);
  is_true ~msg:"equal text from another edit is still newer"
    (Editor.accept ~current:same_text proposal == same_text);
  let restored = Editor.undo edited in
  equal string (Editor.text original) (Editor.text restored);
  is_true ~msg:"undo back to the base text is a newer state"
    (Editor.accept ~current:restored proposal == restored);
  let replaced = Editor.of_string "zz" in
  is_true ~msg:"replacement"
    (Editor.accept ~current:replaced proposal == replaced)

let combining_insertion_and_limit () =
  let original = Editor.of_string ~max_length:1 "e" in
  let edited = Editor.insert "\xcc\x81" original in
  equal string "e\xcc\x81" (Editor.text edited);
  equal int 1 (Editor.length edited);
  equal int 1 (Editor.cursor edited);
  equal string "e" (Editor.text original)

let no_op_identity () =
  let original = Editor.of_string "ab" in
  is_true (Editor.move_end original == original);
  is_true (Editor.clear_selection original == original);
  is_true (Editor.insert "" original == original);
  is_true (Editor.undo original == original);
  is_true (Editor.delete_forward original == original)

(* ── Construction ── *)

let create_empty () =
  let e = Editor.empty in
  equal ~msg:"text" string "" (Editor.text e);
  equal ~msg:"cursor" int 0 (Editor.cursor e);
  equal ~msg:"length" int 0 (Editor.length e);
  equal ~msg:"selection" sel None (Editor.selection e)

let create_hello () =
  let e = Editor.of_string "hello" in
  equal ~msg:"text" string "hello" (Editor.text e);
  equal ~msg:"cursor at end" int 5 (Editor.cursor e);
  equal ~msg:"length" int 5 (Editor.length e)

let create_preserves_newlines () =
  let e = Editor.of_string "a\nb\nc" in
  equal ~msg:"text" string "a\nb\nc" (Editor.text e);
  equal ~msg:"cursor" int 5 (Editor.cursor e)

let create_max_length_truncates_initial () =
  let e = Editor.of_string ~max_length:3 "abcdef" in
  equal ~msg:"length" int 3 (Editor.length e);
  equal ~msg:"text" string "abc" (Editor.text e)

(* A negative cap clamps to zero: nothing fits, and the value stays
   navigable. *)
let create_negative_max_length_clamped () =
  let e = Editor.of_string ~max_length:(-3) "abc" in
  equal ~msg:"content truncated" string "" (Editor.text e);
  equal ~msg:"cursor at end" int 0 (Editor.cursor e);
  equal ~msg:"nothing can be typed" string ""
    (Editor.text (Editor.insert "x" e))

let create_zero_max_length () =
  equal ~msg:"nothing fits" string ""
    (Editor.text (Editor.of_string ~max_length:0 "abc"));
  equal ~msg:"one grapheme fits" string "a"
    (Editor.text (Editor.of_string ~max_length:1 "abc"))

let create_multibyte_unicode () =
  let e = Editor.of_string "caf\xc3\xa9" in
  equal ~msg:"length" int 4 (Editor.length e);
  equal ~msg:"text" string "caf\xc3\xa9" (Editor.text e)

let default_length_is_unlimited () =
  let text = String.make 1500 'x' in
  equal ~msg:"no implicit cap" int 1500 (Editor.length (Editor.of_string text))

(* ── Cursor ── *)

let set_cursor_moves () =
  equal int 2 (Editor.cursor (at 2 (Editor.of_string "hello")))

let set_cursor_clamps () =
  let e = Editor.of_string "hello" in
  equal ~msg:"negative" int 0 (Editor.cursor (at (-5) e));
  equal ~msg:"past the end" int 5 (Editor.cursor (at 100 e))

let set_cursor_clears_selection () =
  let e = Editor.select_all (Editor.of_string "hello") in
  is_some ~msg:"selection before" (Editor.selection e);
  is_none ~msg:"no selection after" (Editor.selection (at 2 e))

let set_cursor_with_select () =
  let e = Editor.set_cursor ~select:true 2 (Editor.of_string "hello") in
  equal ~msg:"cursor" int 2 (Editor.cursor e);
  equal ~msg:"selected" string "llo" (Editor.selected_text e)

(* ── Selection ── *)

let no_selection_initially () =
  equal sel None (Editor.selection (Editor.of_string "hello"))

let select_all_sets_selection () =
  let e = Editor.select_all (Editor.of_string "hello world") in
  equal ~msg:"selection" sel (Some (0, 11)) (Editor.selection e);
  equal ~msg:"cursor at end" int 11 (Editor.cursor e);
  equal ~msg:"selected text" string "hello world" (Editor.selected_text e)

let selected_text_no_selection () =
  equal string "" (Editor.selected_text (Editor.of_string "hello"))

let clear_selection_keeps_cursor () =
  let e = Editor.select_all (Editor.of_string "hello") in
  let cleared = Editor.clear_selection e in
  is_none ~msg:"no selection" (Editor.selection cleared);
  equal ~msg:"cursor unchanged" int (Editor.cursor e) (Editor.cursor cleared)

let selection_by_movement () =
  let e = Editor.of_string "hello" in
  let forward =
    e |> at 0
    |> Editor.move_right ~select:true
    |> Editor.move_right ~select:true
  in
  equal ~msg:"forward" sel (Some (0, 2)) (Editor.selection forward);
  let backward =
    e |> Editor.move_left ~select:true |> Editor.move_left ~select:true
  in
  equal ~msg:"backward, normalized" sel
    (Some (3, 5))
    (Editor.selection backward);
  is_none ~msg:"plain movement clears it"
    (Editor.selection (Editor.move_right forward))

let select_all_empty () =
  is_none (Editor.selection (Editor.select_all Editor.empty))

(* Found by the model-based suite: select_all on an empty value leaves an
   anchor at 0 that the next insertion must not turn into a selection over the
   typed text. *)
let select_all_on_empty_leaves_no_phantom_selection () =
  let e = Editor.select_all Editor.empty |> Editor.insert "a" in
  equal ~msg:"nothing is selected after typing" sel None (Editor.selection e);
  equal ~msg:"the second character appends, it does not replace" string "ab"
    (Editor.text (Editor.insert "b" e))

(* ── Insertion ── *)

let insert_at_end_appends () =
  equal string "hello world"
    (Editor.text (Editor.insert " world" (Editor.of_string "hello")))

let insert_at_middle () =
  let e = Editor.of_string "helo" |> at 2 |> Editor.insert "l" in
  equal ~msg:"text" string "hello" (Editor.text e);
  equal ~msg:"cursor" int 3 (Editor.cursor e)

let insert_empty_is_noop () =
  let e = Editor.of_string "hello" in
  unchanged ~msg:"no change" e (Editor.insert "" e)

(* An insertion that leaves the text unchanged keeps the value, its caret,
   selection and history: no undo point is added. *)
let insert_same_selected_text_is_noop () =
  let e = Editor.of_string "a" |> Editor.insert "b" |> Editor.undo in
  let selected = Editor.select_all e in
  let replaced = Editor.insert "a" selected in
  unchanged ~msg:"same value" selected replaced;
  equal ~msg:"selection kept" sel (Some (0, 1)) (Editor.selection replaced);
  unchanged ~msg:"no undo point added" replaced (Editor.undo replaced);
  equal ~msg:"redo history preserved" string "ab"
    (Editor.text (Editor.redo replaced))

let insert_replaces_selection () =
  equal string "bye"
    (Editor.text
       (Editor.insert "bye"
          (Editor.select_all (Editor.of_string "hello world"))))

let insert_preserves_newlines () =
  equal string "a\nb\nc" (Editor.text (Editor.insert "a\nb\nc" Editor.empty))

let insert_enforces_max_length () =
  let e = Editor.insert "abcdefgh" (Editor.of_string ~max_length:5 "") in
  equal ~msg:"text" string "abcde" (Editor.text e);
  let full = Editor.insert "abcde" (Editor.of_string ~max_length:5 "") in
  unchanged ~msg:"no room" full (Editor.insert "x" full);
  equal ~msg:"partial admission" string "abcde"
    (Editor.text
       (Editor.of_string ~max_length:5 ""
       |> Editor.insert "abc" |> Editor.insert "defgh"))

let insert_at_limit_preserves_suffix () =
  let e = Editor.of_string ~max_length:5 "abcde" |> at 2 in
  unchanged ~msg:"full" e (Editor.insert "x" e);
  equal ~msg:"fitting prefix keeps the suffix" string "abXde"
    (Editor.text
       (Editor.of_string ~max_length:5 "abde" |> at 2 |> Editor.insert "XYZ"))

let insert_moves_cursor () =
  equal int 3 (Editor.cursor (Editor.insert "abc" Editor.empty))

let insert_wide_chars () =
  let e = Editor.insert "\xe4\xbd\xa0" Editor.empty in
  equal ~msg:"one grapheme" int 1 (Editor.length e);
  let e = Editor.of_string "\xe4\xbd\xa0\xe5\xa5\xbd" in
  equal ~msg:"cursor over wide graphemes" int 0
    (Editor.cursor (e |> Editor.move_left |> Editor.move_left));
  equal ~msg:"delete one wide grapheme" string "\xe4\xbd\xa0"
    (Editor.text (Editor.delete_backward e));
  equal ~msg:"emoji is one grapheme" int 1
    (Editor.length (Editor.of_string "\xf0\x9f\x91\x8b"))

(* ── Deletion ── *)

let delete_backward () =
  let e = Editor.delete_backward (Editor.of_string "hello") in
  equal ~msg:"text" string "hell" (Editor.text e);
  equal ~msg:"cursor" int 4 (Editor.cursor e);
  let start = at 0 (Editor.of_string "hello") in
  unchanged ~msg:"at zero" start (Editor.delete_backward start);
  equal ~msg:"deletes the selection" string ""
    (Editor.text
       (Editor.delete_backward (Editor.select_all (Editor.of_string "hello"))))

let delete_forward () =
  let e = Editor.of_string "hello" |> at 0 |> Editor.delete_forward in
  equal ~msg:"text" string "ello" (Editor.text e);
  equal ~msg:"cursor" int 0 (Editor.cursor e);
  let finish = Editor.of_string "hello" in
  unchanged ~msg:"at end" finish (Editor.delete_forward finish);
  equal ~msg:"deletes the selection" string ""
    (Editor.text
       (Editor.delete_forward (Editor.select_all (Editor.of_string "hello"))))

let delete_word_backward () =
  let e = Editor.of_string "hello world test" in
  let once = Editor.delete_word_backward e in
  equal ~msg:"first" string "hello world " (Editor.text once);
  equal ~msg:"second" string "hello "
    (Editor.text (Editor.delete_word_backward once));
  (* After spaces: skip the breaks, then the word (readline-style). *)
  let spaced = Editor.delete_word_backward (Editor.of_string "hello   world") in
  equal ~msg:"spaces kept" string "hello   " (Editor.text spaced);
  equal ~msg:"spaces and word" string ""
    (Editor.text (Editor.delete_word_backward spaced));
  let start = at 0 (Editor.of_string "hello") in
  unchanged ~msg:"at start" start (Editor.delete_word_backward start)

let delete_word_backward_and_newlines () =
  let e = Editor.delete_word_backward (Editor.of_string "hello\nworld") in
  equal ~msg:"stops at newline" string "hello\n" (Editor.text e);
  equal ~msg:"cursor" int 6 (Editor.cursor e);
  let after =
    Editor.of_string "hello\nworld" |> at 6 |> Editor.delete_word_backward
  in
  equal ~msg:"after a newline deletes the previous word" string "world"
    (Editor.text after);
  equal ~msg:"cursor at start" int 0 (Editor.cursor after)

let delete_word_forward () =
  equal ~msg:"basic" string "world"
    (Editor.text
       (Editor.of_string "hello world" |> at 0 |> Editor.delete_word_forward));
  let e =
    Editor.of_string "hello\nworld" |> at 0 |> Editor.delete_word_forward
  in
  equal ~msg:"stops at newline" string "world" (Editor.text e);
  equal ~msg:"cursor" int 0 (Editor.cursor e);
  let finish = Editor.of_string "hello" in
  unchanged ~msg:"at end" finish (Editor.delete_word_forward finish)

let cjk_words_stay_grouped () =
  List.iter
    (fun text ->
      let e = Editor.of_string text in
      let length = Editor.length e in
      let selected = e |> at 0 |> Editor.move_word_forward ~select:true in
      equal ~msg:"forward through the run" int length (Editor.cursor selected);
      equal ~msg:"select the entire word" sel
        (Some (0, length))
        (Editor.selection selected);
      equal ~msg:"backward through the run" int 0
        (Editor.cursor (e |> at length |> Editor.move_word_backward));
      let deleted = e |> at 0 |> Editor.delete_word_forward in
      equal ~msg:"delete the whole run" string "" (Editor.text deleted);
      equal ~msg:"backspace the whole run" string ""
        (Editor.text
           (deleted |> Editor.undo |> at length |> Editor.delete_word_backward)))
    [ "日本語文字"; "테스트"; "カ・ナ"; "か\u{3099}く" ]

let delete_line () =
  let all = Editor.delete_line (Editor.of_string "hello world") in
  equal ~msg:"single line" string "" (Editor.text all);
  equal ~msg:"cursor" int 0 (Editor.cursor all);
  unchanged ~msg:"empty" Editor.empty (Editor.delete_line Editor.empty);
  let lines = Editor.of_string "abc\ndef\nghi" in
  equal ~msg:"first line" string "def\nghi"
    (Editor.text (Editor.delete_line (at 1 lines)));
  equal ~msg:"middle line" string "abc\nghi"
    (Editor.text (Editor.delete_line (at 5 lines)));
  equal ~msg:"last line" string "abc\ndef"
    (Editor.text (Editor.delete_line (at 9 lines)))

let delete_to_line_edges () =
  let e = Editor.of_string "abc\ndef" in
  let start = Editor.delete_to_line_start (at 6 e) in
  equal ~msg:"to line start" string "abc\nf" (Editor.text start);
  equal ~msg:"cursor" int 4 (Editor.cursor start);
  no_op ~msg:"at line start" Editor.delete_to_line_start (at 4 e);
  equal ~msg:"to line end" string "abc\n"
    (Editor.text (Editor.delete_to_line_end (at 4 e)));
  no_op ~msg:"at line end" Editor.delete_to_line_end (at 3 e);
  unchanged ~msg:"at text end" e (Editor.delete_to_line_end e)

let newline_editing () =
  equal ~msg:"insert newline" string "ab\ncd"
    (Editor.text (Editor.of_string "abcd" |> at 2 |> Editor.insert "\n"));
  equal ~msg:"backspace joins" string "abcdef"
    (Editor.text
       (Editor.of_string "abc\ndef" |> at 4 |> Editor.delete_backward));
  equal ~msg:"delete joins" string "abcdef"
    (Editor.text (Editor.of_string "abc\ndef" |> at 3 |> Editor.delete_forward))

(* ── Movement ── *)

let move_left_and_right () =
  let e = Editor.of_string "hello" in
  equal ~msg:"left" int 4 (Editor.cursor (Editor.move_left e));
  no_op ~msg:"left at zero" Editor.move_left (at 0 e);
  equal ~msg:"right" int 1 (Editor.cursor (Editor.move_right (at 0 e)));
  unchanged ~msg:"right at end" e (Editor.move_right e);
  let selected =
    e |> at 1
    |> Editor.move_right ~select:true
    |> Editor.move_right ~select:true
  in
  let left = Editor.move_left selected and right = Editor.move_right selected in
  equal ~msg:"left collapses to the start" int 1 (Editor.cursor left);
  is_none ~msg:"left clears" (Editor.selection left);
  equal ~msg:"right collapses to the end" int 3 (Editor.cursor right);
  is_none ~msg:"right clears" (Editor.selection right)

let move_select_creates_selection () =
  let e = Editor.of_string "hello" in
  equal ~msg:"left" sel
    (Some (4, 5))
    (Editor.selection (Editor.move_left ~select:true e));
  equal ~msg:"right" sel
    (Some (0, 1))
    (Editor.selection (Editor.move_right ~select:true (at 0 e)));
  equal ~msg:"home" sel
    (Some (0, 5))
    (Editor.selection (Editor.move_home ~select:true e));
  equal ~msg:"end" sel
    (Some (0, 5))
    (Editor.selection (Editor.move_end ~select:true (at 0 e)))

let move_home_and_end () =
  let e = Editor.of_string "hello" in
  equal ~msg:"home" int 0 (Editor.cursor (Editor.move_home e));
  no_op ~msg:"home at zero" Editor.move_home (at 0 e);
  equal ~msg:"end" int 5 (Editor.cursor (Editor.move_end (at 0 e)));
  unchanged ~msg:"end at end" e (Editor.move_end e)

let move_by_word () =
  satisfies ~claim:"forward advances" int
    (fun c -> c > 0)
    (Editor.cursor
       (Editor.of_string "hello world" |> at 0 |> Editor.move_word_forward));
  satisfies ~claim:"backward retreats" int
    (fun c -> c < 11)
    (Editor.cursor (Editor.move_word_backward (Editor.of_string "hello world")));
  let lines = Editor.of_string "hello\nworld" in
  equal ~msg:"forward crosses a newline" int 6
    (Editor.cursor (Editor.move_word_forward (at 0 lines)));
  equal ~msg:"backward crosses a newline" int 6
    (Editor.cursor (Editor.move_word_backward lines))

let move_by_line () =
  let e = Editor.of_string "abc\ndef" in
  equal ~msg:"line start" int 4
    (Editor.cursor (Editor.move_line_start (at 5 e)));
  no_op ~msg:"already at line start" Editor.move_line_start (at 4 e);
  equal ~msg:"first line start" int 0
    (Editor.cursor (Editor.move_line_start (at 2 e)));
  equal ~msg:"select to line start" string "de"
    (Editor.selected_text (Editor.move_line_start ~select:true (at 6 e)));
  equal ~msg:"line end" int 7 (Editor.cursor (Editor.move_line_end (at 4 e)));
  equal ~msg:"first line end is before the newline" int 3
    (Editor.cursor (Editor.move_line_end (at 0 e)));
  unchanged ~msg:"already at line end" e (Editor.move_line_end e);
  equal ~msg:"select to line end" string "def"
    (Editor.selected_text (Editor.move_line_end ~select:true (at 4 e)))

(* ── Undo and redo ── *)

let undo_no_history () =
  let e = Editor.of_string "hello" in
  unchanged ~msg:"nothing to undo" e (Editor.undo e);
  unchanged ~msg:"nothing to redo" e (Editor.redo e)

let undo_history_keeps_every_edit () =
  (* Entries hold the inverse of their change, so no edit is dropped to bound
     memory. *)
  let e = ref (Editor.of_string "") in
  for _ = 1 to 250 do
    e := Editor.insert "a" !e
  done;
  let undos = ref 0 in
  let rec undo_all e =
    let previous = Editor.undo e in
    if previous == e then e
    else begin
      incr undos;
      undo_all previous
    end
  in
  let oldest = undo_all !e in
  equal ~msg:"every edit can be undone" int 250 !undos;
  equal ~msg:"back to the initial text" string "" (Editor.text oldest)

let undo_restores_text_and_cursor () =
  let e = Editor.of_string "hello" |> at 2 |> Editor.insert "X" in
  equal ~msg:"cursor after insert" int 3 (Editor.cursor e);
  let undone = Editor.undo e in
  equal ~msg:"text" string "hello" (Editor.text undone);
  equal ~msg:"cursor" int 2 (Editor.cursor undone)

let undo_restores_no_selection () =
  let e =
    Editor.of_string "hello" |> Editor.insert " world" |> Editor.select_all
  in
  is_none ~msg:"no selection after undo" (Editor.selection (Editor.undo e))

let undo_restores_selection () =
  let e =
    Editor.of_string "hello world" |> at 6 |> Editor.set_cursor ~select:true 11
  in
  let replaced = Editor.insert "earth" e in
  is_none ~msg:"no selection after insert" (Editor.selection replaced);
  equal ~msg:"selection restored" sel
    (Some (6, 11))
    (Editor.selection (Editor.undo replaced))

let redo_after_undo () =
  let undone =
    Editor.of_string "hello" |> Editor.insert " world" |> Editor.undo
  in
  equal ~msg:"after undo" string "hello" (Editor.text undone);
  equal ~msg:"after redo" string "hello world"
    (Editor.text (Editor.redo undone))

let edit_after_undo_clears_redo () =
  let e =
    Editor.of_string "a" |> Editor.insert "b" |> Editor.undo
    |> Editor.insert "c"
  in
  unchanged ~msg:"redo cleared" e (Editor.redo e)

let multiple_undo_steps () =
  let e =
    Editor.empty |> Editor.insert "a" |> Editor.insert "b" |> Editor.insert "c"
  in
  let one = Editor.undo e in
  let two = Editor.undo one in
  equal ~msg:"undo 1" string "ab" (Editor.text one);
  equal ~msg:"undo 2" string "a" (Editor.text two);
  equal ~msg:"undo 3" string "" (Editor.text (Editor.undo two))

let deletions_undo () =
  equal ~msg:"delete_backward" string "hello"
    (Editor.text
       (Editor.of_string "hello" |> Editor.delete_backward |> Editor.undo));
  equal ~msg:"delete_word_backward" string "hello world"
    (Editor.text
       (Editor.of_string "hello world"
       |> Editor.delete_word_backward |> Editor.undo))

(* ── Shared source index ── *)

let resegment_inserted_combining_mark () =
  let e = Editor.insert "\204\129" (Editor.of_string ~max_length:1 "e") in
  equal ~msg:"text" string "e\204\129" (Editor.text e);
  equal ~msg:"cursor" int 1 (Editor.cursor e);
  equal ~msg:"length" int 1 (Editor.length e);
  equal ~msg:"undo" string "e" (Editor.text (Editor.undo e))

let resegment_deleted_separator () =
  let e = Editor.of_string "🇦 🇧" |> at 1 |> Editor.delete_forward in
  equal string "🇦🇧" (Editor.text e);
  equal int 1 (Editor.length e);
  equal int 1 (Editor.cursor e)

let invisible_clusters_count_towards_limit () =
  let e = Editor.of_string ~max_length:2 "a\226\128\139b" in
  equal string "a\226\128\139" (Editor.text e);
  equal int 2 (Editor.length e)

(* ── Model-based suite ── *)

(* Everything above pins single operations. What no example test reaches is
   the interaction between them: a selection created by one call, collapsed by
   the next, deleted by a third, and restored by an undo four steps later.
   [stateful] checks a model over generated sequences instead.

   The model is a plain string with a cursor and a selection anchor. That is a
   specification rather than a second implementation only because the generated
   content is ASCII: there one byte is one grapheme cluster, so the model never
   segments anything and the editor's real work is not duplicated. Grapheme
   clustering and word boundaries stay the job of the example tests above,
   which feed real multi-codepoint input.

   Only operations whose contract the .mli and the reviewed examples pin
   exactly are modelled. move_home/move_end and the word-wise moves are left
   out: the model would author their contract rather than read it. *)

type snapshot = { s_text : string; s_cursor : int; s_anchor : int option }

type model = {
  text : string;
  cursor : int;
  anchor : int option; (* where a live selection is anchored *)
  undo : snapshot list; (* newest first *)
  redo : snapshot list;
}

(* [selection] is [Some (lo, hi)] only when [lo < hi], so an anchor sitting on
   the cursor is no selection at all; the model normalises it away. *)
let anchor_of ~cursor = function Some a when a <> cursor -> Some a | _ -> None

let model_selection m =
  match m.anchor with
  | Some a -> Some (min a m.cursor, max a m.cursor)
  | None -> None

let pp_model ppf m =
  Format.fprintf ppf "%S cursor=%d sel=%s undo=%d redo=%d" m.text m.cursor
    (match model_selection m with
    | None -> "-"
    | Some (lo, hi) -> Printf.sprintf "%d..%d" lo hi)
    (List.length m.undo) (List.length m.redo)

let snapshot_of m =
  { s_text = m.text; s_cursor = m.cursor; s_anchor = m.anchor }

(* Saving an undo point drops the redo history: the branch it would replay no
   longer exists. *)
let push_undo m = { m with undo = snapshot_of m :: m.undo; redo = [] }

let drop_range text lo hi =
  String.sub text 0 lo ^ String.sub text hi (String.length text - hi)

(* Deleting the selection is the shared prefix of insert, delete_backward and
   delete_forward: save an undo point, drop the range, land the cursor on its
   low edge. *)
let delete_selection m =
  match model_selection m with
  | None -> None
  | Some (lo, hi) ->
      let m = push_undo m in
      Some { m with text = drop_range m.text lo hi; cursor = lo; anchor = None }

let model_length m = String.length m.text
let clamp m pos = max 0 (min pos (model_length m))

(* Movement anchors at the cursor it is leaving when selecting, and drops the
   anchor when not. *)
let move_to m ~select pos =
  let anchor =
    if select then Some (Option.value m.anchor ~default:m.cursor) else None
  in
  { m with cursor = pos; anchor = anchor_of ~cursor:pos anchor }

let restore m snap =
  {
    m with
    text = snap.s_text;
    cursor = snap.s_cursor;
    anchor = anchor_of ~cursor:snap.s_cursor snap.s_anchor;
  }

(* Generated text is ASCII and newline-free: multi-line editing has its own
   examples, and no newlines means one byte is one grapheme in the model. *)
let gen_insert =
  Gen.string_of ~size:(Gen.int_range 1 3) (Gen.char_range 'a' 'z')

(* Offsets are drawn beyond the text so the clamping contract is exercised. *)
let gen_offset = Gen.int_range (-2) 12

let inserted_model m s =
  let first, last =
    match model_selection m with
    | Some range -> range
    | None -> (m.cursor, m.cursor)
  in
  let text =
    String.sub m.text 0 first ^ s
    ^ String.sub m.text last (String.length m.text - last)
  in
  if text = m.text then m
  else
    { (push_undo m) with text; cursor = first + String.length s; anchor = None }

(* Applies [f] to the editor under test, returning the previous value. *)
let step f editor =
  let before = !editor in
  editor := f before;
  before

let caret_moved before after =
  (Editor.cursor before, Editor.selection before)
  <> (Editor.cursor after, Editor.selection after)

let commands =
  [
    command "insert" gen_insert ~next:inserted_model (fun m s editor ->
        let before = step (Editor.insert s) editor in
        equal ~msg:"insert changes the text iff the model does" bool
          ((inserted_model m s).text <> m.text)
          (text_changed before !editor));
    call "delete_backward"
      ~next:(fun m ->
        match delete_selection m with
        | Some m -> m
        | None ->
            if m.cursor = 0 then m
            else
              let m = push_undo m in
              {
                m with
                text = drop_range m.text (m.cursor - 1) m.cursor;
                cursor = m.cursor - 1;
                anchor = None;
              })
      (fun m editor ->
        let before = step Editor.delete_backward editor in
        equal ~msg:"delete_backward changes the text" bool
          (model_selection m <> None || m.cursor > 0)
          (text_changed before !editor));
    call "delete_forward"
      ~next:(fun m ->
        match delete_selection m with
        | Some m -> m
        | None ->
            if m.cursor >= model_length m then m
            else
              let m = push_undo m in
              {
                m with
                text = drop_range m.text m.cursor (m.cursor + 1);
                anchor = None;
              })
      (fun m editor ->
        let before = step Editor.delete_forward editor in
        equal ~msg:"delete_forward changes the text" bool
          (model_selection m <> None || m.cursor < model_length m)
          (text_changed before !editor));
    command "move_left" Gen.bool
      ~next:(fun m select ->
        match model_selection m with
        | Some (lo, _) when not select -> { m with cursor = lo; anchor = None }
        | _ -> move_to m ~select (max 0 (m.cursor - 1)))
      (fun m select editor ->
        let before = step (Editor.move_left ~select) editor in
        equal ~msg:"move_left moves the cursor or selection" bool
          ((model_selection m <> None && not select) || m.cursor > 0)
          (caret_moved before !editor));
    command "move_right" Gen.bool
      ~next:(fun m select ->
        match model_selection m with
        | Some (_, hi) when not select -> { m with cursor = hi; anchor = None }
        | _ -> move_to m ~select (min (model_length m) (m.cursor + 1)))
      (fun m select editor ->
        let before = step (Editor.move_right ~select) editor in
        equal ~msg:"move_right moves the cursor or selection" bool
          ((model_selection m <> None && not select)
          || m.cursor < model_length m)
          (caret_moved before !editor));
    command "set_cursor"
      (Gen.pair gen_offset Gen.bool)
      ~next:(fun m (pos, select) -> move_to m ~select (clamp m pos))
      (fun _ (pos, select) editor ->
        ignore (step (Editor.set_cursor ~select pos) editor : Editor.t));
    call "select_all"
      ~next:(fun m ->
        let cursor = model_length m in
        { m with cursor; anchor = anchor_of ~cursor (Some 0) })
      (fun _ editor -> ignore (step Editor.select_all editor : Editor.t));
    call "clear_selection"
      ~next:(fun m -> { m with anchor = None })
      (fun _ editor -> ignore (step Editor.clear_selection editor : Editor.t));
    call "undo"
      ~next:(fun m ->
        match m.undo with
        | [] -> m
        | snap :: rest ->
            {
              (restore m snap) with
              undo = rest;
              redo = snapshot_of m :: m.redo;
            })
      (fun m editor ->
        let before = step Editor.undo editor in
        equal ~msg:"undo changes the text iff there is history" bool
          (m.undo <> [])
          (text_changed before !editor));
    call "redo"
      ~next:(fun m ->
        match m.redo with
        | [] -> m
        | snap :: rest ->
            {
              (restore m snap) with
              redo = rest;
              undo = snapshot_of m :: m.undo;
            })
      (fun m editor ->
        let before = step Editor.redo editor in
        equal ~msg:"redo changes the text iff there is history" bool
          (m.redo <> [])
          (text_changed before !editor));
  ]

let model_invariant m editor =
  let e = !editor in
  equal ~msg:"text" string m.text (Editor.text e);
  equal ~msg:"length" int (model_length m) (Editor.length e);
  equal ~msg:"cursor" int m.cursor (Editor.cursor e);
  equal ~msg:"selection" sel (model_selection m) (Editor.selection e);
  equal ~msg:"selected_text" string
    (match model_selection m with
    | None -> ""
    | Some (lo, hi) -> String.sub m.text lo (hi - lo))
    (Editor.selected_text e);
  (* The interesting states are the ones a shorter program never reaches:
     without these the suite could pass while only ever seeing an empty
     value with no selection. *)
  cover "a selection is live" (model_selection m <> None);
  cover "undo history is non-empty" (m.undo <> []);
  cover "redo history is non-empty" (m.redo <> [])

(* ── History equivalence ── *)

(* The undo history is the sequence of states before each text change: undo
   restores the latest one, including caret, affinity and selection, and redo
   reapplies. The reference keeps those states whole, as snapshots; the editor
   must agree with it after every step of any script. *)
type op =
  | Insert of string
  | Delete_backward
  | Delete_forward
  | Delete_word_backward
  | Delete_word_forward
  | Delete_to_line_start
  | Delete_to_line_end
  | Delete_line
  | Clear
  | Move_left of bool
  | Move_right of bool
  | Move_word_backward of bool
  | Move_word_forward of bool
  | Move_line_start of bool
  | Move_line_end of bool
  | Set_cursor of int * bool
  | Set_caret_upstream of int
  | Select of int * int
  | Select_all
  | Clear_selection
  | Undo
  | Redo

let pp_op ppf = function
  | Insert s -> Format.fprintf ppf "insert %S" s
  | Delete_backward -> Format.pp_print_string ppf "delete_backward"
  | Delete_forward -> Format.pp_print_string ppf "delete_forward"
  | Delete_word_backward -> Format.pp_print_string ppf "delete_word_backward"
  | Delete_word_forward -> Format.pp_print_string ppf "delete_word_forward"
  | Delete_to_line_start -> Format.pp_print_string ppf "delete_to_line_start"
  | Delete_to_line_end -> Format.pp_print_string ppf "delete_to_line_end"
  | Delete_line -> Format.pp_print_string ppf "delete_line"
  | Clear -> Format.pp_print_string ppf "clear"
  | Move_left s -> Format.fprintf ppf "move_left %b" s
  | Move_right s -> Format.fprintf ppf "move_right %b" s
  | Move_word_backward s -> Format.fprintf ppf "move_word_backward %b" s
  | Move_word_forward s -> Format.fprintf ppf "move_word_forward %b" s
  | Move_line_start s -> Format.fprintf ppf "move_line_start %b" s
  | Move_line_end s -> Format.fprintf ppf "move_line_end %b" s
  | Set_cursor (p, s) -> Format.fprintf ppf "set_cursor %d %b" p s
  | Set_caret_upstream p -> Format.fprintf ppf "set_caret_upstream %d" p
  | Select (a, c) -> Format.fprintf ppf "select %d %d" a c
  | Select_all -> Format.pp_print_string ppf "select_all"
  | Clear_selection -> Format.pp_print_string ppf "clear_selection"
  | Undo -> Format.pp_print_string ppf "undo"
  | Redo -> Format.pp_print_string ppf "redo"

(* Fragments that segment interestingly: a combining mark joins the previous
   grapheme, regional indicators pair, CJK and spaces change word breaks. *)
let fragment =
  Gen.of_list
    [ "a"; "b"; " "; "\n"; "\r\n"; "\204\129"; "🇦"; "🇧"; "日本"; "word"; "x y" ]

let gen_op =
  let flag = Gen.bool and pos = Gen.int_range (-1) 24 in
  Gen.frequency
    [
      (6, Gen.map (fun s -> Insert s) fragment);
      (2, Gen.pure Delete_backward);
      (1, Gen.pure Delete_forward);
      (1, Gen.pure Delete_word_backward);
      (1, Gen.pure Delete_word_forward);
      (1, Gen.pure Delete_to_line_start);
      (1, Gen.pure Delete_to_line_end);
      (1, Gen.pure Delete_line);
      (1, Gen.pure Clear);
      (1, Gen.map (fun s -> Move_left s) flag);
      (1, Gen.map (fun s -> Move_right s) flag);
      (1, Gen.map (fun s -> Move_word_backward s) flag);
      (1, Gen.map (fun s -> Move_word_forward s) flag);
      (1, Gen.map (fun s -> Move_line_start s) flag);
      (1, Gen.map (fun s -> Move_line_end s) flag);
      (2, Gen.map (fun (p, s) -> Set_cursor (p, s)) (Gen.pair pos flag));
      (1, Gen.map (fun p -> Set_caret_upstream p) pos);
      (1, Gen.map (fun (a, c) -> Select (a, c)) (Gen.pair pos pos));
      (1, Gen.pure Select_all);
      (1, Gen.pure Clear_selection);
      (4, Gen.pure Undo);
      (3, Gen.pure Redo);
    ]

let edit = function
  | Insert s -> Editor.insert s
  | Delete_backward -> Editor.delete_backward
  | Delete_forward -> Editor.delete_forward
  | Delete_word_backward -> Editor.delete_word_backward
  | Delete_word_forward -> Editor.delete_word_forward
  | Delete_to_line_start -> Editor.delete_to_line_start
  | Delete_to_line_end -> Editor.delete_to_line_end
  | Delete_line -> Editor.delete_line
  | Clear -> Editor.clear
  | Move_left select -> Editor.move_left ~select
  | Move_right select -> Editor.move_right ~select
  | Move_word_backward select -> Editor.move_word_backward ~select
  | Move_word_forward select -> Editor.move_word_forward ~select
  | Move_line_start select -> Editor.move_line_start ~select
  | Move_line_end select -> Editor.move_line_end ~select
  | Set_cursor (pos, select) -> Editor.set_cursor ~select pos
  | Set_caret_upstream grapheme ->
      Editor.set_caret Matrix.Text.Layout.{ grapheme; affinity = Upstream }
  | Select (anchor, cursor) -> Editor.select ~anchor ~cursor
  | Select_all -> Editor.select_all
  | Clear_selection -> Editor.clear_selection
  | Undo -> Editor.undo
  | Redo -> Editor.redo

type reference = {
  state : Editor.t; (* used only for its text, caret and selection *)
  undo_states : Editor.t list;
  redo_states : Editor.t list;
}

let reference_step r = function
  | Undo -> (
      match r.undo_states with
      | [] -> r
      | previous :: rest ->
          {
            state = previous;
            undo_states = rest;
            redo_states = r.state :: r.redo_states;
          })
  | Redo -> (
      match r.redo_states with
      | [] -> r
      | next :: rest ->
          {
            state = next;
            redo_states = rest;
            undo_states = r.state :: r.undo_states;
          })
  | op ->
      let next = edit op r.state in
      if String.equal (Editor.text next) (Editor.text r.state) then
        { r with state = next }
      else
        {
          state = next;
          undo_states = r.state :: r.undo_states;
          redo_states = [];
        }

let same_state ~step expected actual =
  let msg what = Printf.sprintf "step %d: %s" step what in
  equal ~msg:(msg "text") string (Editor.text expected) (Editor.text actual);
  let caret e =
    let c = Editor.caret e in
    (c.grapheme, c.affinity = Matrix.Text.Layout.Upstream)
  in
  equal ~msg:(msg "caret") (pair int bool) (caret expected) (caret actual);
  equal ~msg:(msg "selection") sel
    (Editor.selection expected)
    (Editor.selection actual)

let history_matches_snapshots (initial, script) =
  let editor = ref (Editor.of_string initial) in
  let reference = ref { state = !editor; undo_states = []; redo_states = [] } in
  List.iteri
    (fun step op ->
      editor := edit op !editor;
      reference := reference_step !reference op;
      same_state ~step !reference.state !editor)
    script

let gen_script =
  Gen.with_pp
    (fun ppf (initial, ops) ->
      Format.fprintf ppf "%S: %a" initial
        (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_op)
        ops)
    (Gen.pair
       (Gen.of_list [ ""; "hello world"; "a\nbc\n\ndef"; "日本語 text\n🇦🇧" ])
       (Gen.list ~size:(Gen.int_range 0 80) gen_op))

let () =
  run "Editor"
    [
      group "Persistence"
        [
          test "persistent text and selection" persistent_edits;
          test "independent history branches" independent_history_branches;
          test "clear restores original caret" clear_restores_original_caret;
          test "large cursor transitions share source"
            cursor_shares_large_source;
          test "restored source shared by cursor branches"
            restored_source_is_shared_by_cursor_branches;
          test "stale same-source proposals" stale_same_source_proposal;
          test "stale affinity proposal" stale_affinity_proposal;
          test "stale text proposal" stale_text_proposal;
          test "combining insertion at limit" combining_insertion_and_limit;
          test "no-op edits preserve identity" no_op_identity;
        ];
      group "Construction"
        [
          test "empty" create_empty;
          test "hello" create_hello;
          test "preserves newlines" create_preserves_newlines;
          test "max_length truncates initial"
            create_max_length_truncates_initial;
          test "negative max_length is clamped"
            create_negative_max_length_clamped;
          test "zero max_length" create_zero_max_length;
          test "multibyte Unicode" create_multibyte_unicode;
          test "default length is unlimited" default_length_is_unlimited;
        ];
      group "Cursor"
        [
          test "set_cursor moves" set_cursor_moves;
          test "set_cursor clamps" set_cursor_clamps;
          test "set_cursor clears selection" set_cursor_clears_selection;
          test "set_cursor with select" set_cursor_with_select;
        ];
      group "Selection"
        [
          test "no selection initially" no_selection_initially;
          test "select_all sets selection" select_all_sets_selection;
          test "selected_text without selection" selected_text_no_selection;
          test "clear_selection keeps the cursor" clear_selection_keeps_cursor;
          test "selection by movement" selection_by_movement;
          test "select_all on empty" select_all_empty;
          test "select_all on empty leaves no phantom selection"
            select_all_on_empty_leaves_no_phantom_selection;
        ];
      group "Insertion"
        [
          test "at end appends" insert_at_end_appends;
          test "at middle" insert_at_middle;
          test "empty is a no-op" insert_empty_is_noop;
          test "same selected text is a no-op" insert_same_selected_text_is_noop;
          test "replaces selection" insert_replaces_selection;
          test "preserves newlines" insert_preserves_newlines;
          test "enforces max_length" insert_enforces_max_length;
          test "at the limit preserves the suffix"
            insert_at_limit_preserves_suffix;
          test "moves the cursor" insert_moves_cursor;
          test "wide characters" insert_wide_chars;
        ];
      group "Deletion"
        [
          test "delete_backward" delete_backward;
          test "delete_forward" delete_forward;
          test "delete_word_backward" delete_word_backward;
          test "delete_word_backward and newlines"
            delete_word_backward_and_newlines;
          test "delete_word_forward" delete_word_forward;
          test "CJK words stay grouped" cjk_words_stay_grouped;
          test "delete_line" delete_line;
          test "delete to line edges" delete_to_line_edges;
          test "newline editing" newline_editing;
        ];
      group "Movement"
        [
          test "left and right" move_left_and_right;
          test "select creates selection" move_select_creates_selection;
          test "home and end" move_home_and_end;
          test "by word" move_by_word;
          test "by line" move_by_line;
        ];
      group "Undo and redo"
        [
          test "no history" undo_no_history;
          test "history keeps every edit" undo_history_keeps_every_edit;
          test "restores text and cursor" undo_restores_text_and_cursor;
          test "restores no selection" undo_restores_no_selection;
          test "restores selection" undo_restores_selection;
          test "redo after undo" redo_after_undo;
          test "edit after undo clears redo" edit_after_undo_clears_redo;
          test "multiple steps" multiple_undo_steps;
          test "deletions" deletions_undo;
        ];
      group "History"
        [
          prop ~count:500 "matches snapshot history over edit scripts"
            gen_script history_matches_snapshots;
        ];
      group "Shared source index"
        [
          test "insertion merges a combining mark at the length limit"
            resegment_inserted_combining_mark;
          test "deletion rebases a merged flag cluster"
            resegment_deleted_separator;
          test "length limits retain invisible clusters"
            invisible_clusters_count_towards_limit;
        ];
      stateful "text, cursor, selection and history stay consistent"
        ~model:{ text = ""; cursor = 0; anchor = None; undo = []; redo = [] }
        ~scope:(fun run -> run (ref Editor.empty))
        ~pp_model ~invariant:model_invariant commands;
    ]
