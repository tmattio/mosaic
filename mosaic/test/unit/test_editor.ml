open Windtrap
open Mosaic_ui

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

let imperative_copy_is_independent () =
  let original = Edit_buffer.create "ab" in
  let branch = Edit_buffer.copy original in
  ignore (Edit_buffer.insert branch "x" : bool);
  Edit_buffer.set_cursor original 0;
  equal string "ab" (Edit_buffer.text original);
  equal string "abx" (Edit_buffer.text branch);
  equal int 3 (Edit_buffer.cursor branch);
  ignore (Edit_buffer.undo branch : bool);
  equal string "ab" (Edit_buffer.text branch);
  equal int 0 (Edit_buffer.cursor original)

let () =
  run "Editor"
    [
      test "persistent text and selection" persistent_edits;
      test "independent history branches" independent_history_branches;
      test "clear restores original caret" clear_restores_original_caret;
      test "large cursor transitions share source" cursor_shares_large_source;
      test "restored source shared by cursor branches"
        restored_source_is_shared_by_cursor_branches;
      test "stale same-source proposals" stale_same_source_proposal;
      test "stale affinity proposal" stale_affinity_proposal;
      test "combining insertion at limit" combining_insertion_and_limit;
      test "no-op edits preserve identity" no_op_identity;
      test "imperative copies remain independent" imperative_copy_is_independent;
    ]
