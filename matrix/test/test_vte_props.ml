(** Property tests for the virtual terminal emulator over untrusted bytes.

    A VTE consumes whatever the child process writes, so every byte string is a
    valid input. Three laws are checked over inputs biased toward ANSI
    escape-sequence shapes:

    - {b Total safety}: [Vte.feed] never raises.
    - {b Grid invariants}: after any input, the cursor stays in bounds, every
      wide-character start is paired with its continuation cells within the row,
      and scrollback never exceeds its capacity.
    - {b Split invariance}: feeding a byte string whole or in arbitrary chunks
      produces the same terminal state. *)

module Grid = Matrix_grid
open Windtrap
module Gen = Windtrap_prop.Gen

(* Generators ------------------------------------------------------------- *)

let csi_gen =
  let open Gen in
  let* private_marker = bool in
  let* params = list_size (int_range 0 3) (int_range 0 200) in
  let+ final =
    oneofl
      [
        'A';
        'B';
        'C';
        'D';
        'E';
        'F';
        'G';
        'H';
        'J';
        'K';
        'L';
        'M';
        'P';
        'S';
        'T';
        'X';
        '@';
        'd';
        'f';
        'h';
        'l';
        'm';
        'n';
        'r';
        's';
        'u';
      ]
  in
  Printf.sprintf "\x1b[%s%s%c"
    (if private_marker then "?" else "")
    (String.concat ";" (List.map string_of_int params))
    final

let mode_gen =
  let open Gen in
  let* set = bool in
  let* private_mode = bool in
  let+ mode = oneofl [ 1; 4; 6; 7; 12; 25; 47; 1047; 1048; 1049; 2004 ] in
  Printf.sprintf "\x1b[%s%d%c"
    (if private_mode then "?" else "")
    mode
    (if set then 'h' else 'l')

let structural_fragments =
  [
    "\r\n";
    "\n";
    "\r";
    "\t";
    "\b";
    "\x07";
    "\x1b7";
    "\x1b8";
    "\x1bM";
    "\x1bc";
    "\x1b[2J";
    "\x1b[0m";
    "\x1b[1;31;42m";
    "\x1b[38;2;1;2;3m";
    "\x1b[38;5;100m";
    "\x1b]0;title\x07";
    "\x1b]2;t\x1b\\";
    "\x1b]8;;https://x\x1b\\";
  ]

let valid_utf8_fragments =
  [
    "\xE4\xBD\xA0"; "\xE5\xA5\xBD"; "\xF0\x9F\x98\x80"; "\xC3\xA9"; "e\xCC\x81";
  ]

let invalid_utf8_fragments =
  [ "\xE4\xBD"; "\xC3"; "\xF0\x9F"; "\x80"; "\xFF"; "\xC0"; "\xE0\x80\x80" ]

let partial_escape_fragments =
  [ "\x1b"; "\x1b["; "\x1b[3"; "\x1b]0;x"; "\x1bP1$r" ]

(* One generator for every property: Ansi.Parser's UTF-8 recovery and
   charset-designation handling are split-invariant, so invalid UTF-8 and
   pure random bytes run through split invariance as well as safety. *)
let fragment_gen =
  Gen.frequency
    [
      (4, Gen.string_size (Gen.int_range 0 8) (Gen.char_range ' ' '~'));
      (3, csi_gen);
      (2, mode_gen);
      (3, Gen.oneofl structural_fragments);
      (2, Gen.oneofl (valid_utf8_fragments @ invalid_utf8_fragments));
      (1, Gen.oneofl partial_escape_fragments);
      (1, Gen.string_size (Gen.int_range 0 6) Gen.char);
    ]

let case_gen =
  let open Gen in
  let* rows = int_range 1 6 in
  let* cols = int_range 1 8 in
  let* frags = list_size (int_range 0 12) fragment_gen in
  let s = String.concat "" frags in
  let+ cuts = list_size (int_range 0 6) (int_range 0 (String.length s)) in
  (rows, cols, s, cuts)

let chunks_of s cuts =
  let len = String.length s in
  let cuts = List.sort_uniq compare (0 :: len :: cuts) in
  let rec slices = function
    | a :: (b :: _ as rest) -> String.sub s a (b - a) :: slices rest
    | _ -> []
  in
  slices cuts

let pp_case fmt (rows, cols, s, cuts) =
  Format.fprintf fmt "%dx%d %S cut at [%s]" rows cols s
    (String.concat ";" (List.map string_of_int cuts))

let vte_case = testable ~pp:pp_case ~gen:case_gen ()

(* Invariants ------------------------------------------------------------- *)

let check_invariants vte =
  let rows = Vte.rows vte and cols = Vte.cols vte in
  let row, col = Vte.cursor_pos vte in
  if not (row >= 0 && row < rows) then
    failf "cursor row %d out of bounds (rows=%d)" row rows;
  if not (col >= 0 && col <= cols) then
    failf "cursor col %d out of bounds (cols=%d)" col cols;
  if Vte.scrollback_size vte > Vte.scrollback_capacity vte then
    failf "scrollback size %d exceeds capacity %d" (Vte.scrollback_size vte)
      (Vte.scrollback_capacity vte);
  (* Wide-character span integrity: every start cell of width w is followed
     by exactly w - 1 continuation cells, continuations never appear without
     a start, and spans never cross the row edge. *)
  let grid = Vte.grid vte in
  let width = Grid.width grid and height = Grid.height grid in
  for r = 0 to height - 1 do
    let expected = ref 0 in
    for c = 0 to width - 1 do
      let idx = (r * width) + c in
      let continuation = Grid.is_continuation grid idx in
      if !expected > 0 then begin
        if not continuation then
          failf "row %d col %d: wide start is missing its continuation" r c;
        decr expected
      end
      else begin
        if continuation then failf "row %d col %d: orphan continuation" r c;
        let w = Grid.cell_width grid idx in
        if w > 1 then expected := w - 1
      end
    done;
    if !expected > 0 then failf "row %d: wide span crosses the row edge" r
  done

let feed_chunks vte chunks =
  List.iter (fun chunk -> Vte.feed_string vte chunk) chunks

(* Properties ------------------------------------------------------------- *)

let prop_safety_and_invariants (rows, cols, s, cuts) =
  let vte = Vte.create ~scrollback:16 ~rows ~cols () in
  List.iter
    (fun chunk ->
      Vte.feed_string vte chunk;
      check_invariants vte)
    (chunks_of s cuts)

let prop_split_invariance (rows, cols, s, cuts) =
  let whole = Vte.create ~scrollback:16 ~rows ~cols () in
  Vte.feed_string whole s;
  let split = Vte.create ~scrollback:16 ~rows ~cols () in
  feed_chunks split (chunks_of s cuts);
  equal ~msg:"screen content" string (Vte.to_string whole) (Vte.to_string split);
  equal ~msg:"cursor position" (pair int int) (Vte.cursor_pos whole)
    (Vte.cursor_pos split);
  equal ~msg:"cursor visibility" bool (Vte.cursor_visible whole)
    (Vte.cursor_visible split);
  equal ~msg:"alternate screen" bool
    (Vte.is_alternate_screen whole)
    (Vte.is_alternate_screen split);
  equal ~msg:"title" string (Vte.title whole) (Vte.title split);
  equal ~msg:"scrollback" string
    (String.concat "\n" (Vte.scrollback_lines whole))
    (String.concat "\n" (Vte.scrollback_lines split))

let props =
  let config =
    { Windtrap_prop.Prop.default_config with count = 1000; max_gen = 1500 }
  in
  [
    prop' ~config "feed is total and preserves grid invariants" vte_case
      prop_safety_and_invariants;
    prop' ~config "split invariance" vte_case prop_split_invariance;
  ]

let () = run "matrix.vte.props" [ group "vte" props ]
