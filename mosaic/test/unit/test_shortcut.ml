open Windtrap
open Mosaic_ui
module Keystroke = Matrix_input.Keystroke

let event ?(mods = Keystroke.no_mods) ?(repeat = false) ?base ?(keypad = false)
    key =
  Event.Key.of_input { Keystroke.key; mods; text = ""; base; keypad; repeat }

let char c = Keystroke.Char (Uchar.of_char c)

(* A release is a [Keystroke.release], not a key event: no shortcut can see
   one, so none is tested here. *)
let exact_modifiers () =
  let shortcut = Shortcut.ctrl 'c' in
  let key = char 'c' in
  let mods = { Keystroke.no_mods with ctrl = true } in
  is_true (Shortcut.matches shortcut (event ~mods key));
  is_true ~msg:"lock state is ignored"
    (Shortcut.matches shortcut
       (event ~mods:{ mods with caps_lock = true; num_lock = true } key));
  List.iter
    (fun mods ->
      is_false ~msg:"extra non-lock modifiers do not match"
        (Shortcut.matches shortcut (event ~mods key)))
    [
      { mods with shift = true };
      { mods with alt = true };
      { mods with super = true };
      { mods with hyper = true };
      { mods with meta = true };
    ];
  is_true ~msg:"repeat matches"
    (Shortcut.matches shortcut (event ~mods ~repeat:true key));
  is_true ~msg:"Alt matches Alt alone"
    (Shortcut.matches (Shortcut.alt 'c')
       (event ~mods:{ Keystroke.no_mods with alt = true } key));
  is_false ~msg:"Alt with Meta is not Alt"
    (Shortcut.matches (Shortcut.alt 'c')
       (event ~mods:{ Keystroke.no_mods with alt = true; meta = true } key));
  is_false ~msg:"Meta alone is distinct from Alt"
    (Shortcut.matches (Shortcut.alt 'c')
       (event ~mods:{ Keystroke.no_mods with meta = true } key))

let shift_and_case () =
  let ctrl = { Keystroke.no_mods with ctrl = true } in
  is_false ~msg:"a lowercase shortcut does not match the capital"
    (Shortcut.matches (Shortcut.ctrl 'c') (event ~mods:ctrl (char 'C')));
  is_false ~msg:"an uppercase shortcut does not match the lowercase letter"
    (Shortcut.matches (Shortcut.ctrl 'C') (event ~mods:ctrl (char 'c')));
  is_false ~msg:"char 'r' does not match Shift+r"
    (Shortcut.matches (Shortcut.char 'r') (event (char 'R')));
  is_true ~msg:"char ~shift:true 'r' matches Shift+r, the capital"
    (Shortcut.matches (Shortcut.char ~shift:true 'r') (event (char 'R')));
  is_true ~msg:"char ~shift:true 'r' is char 'R'"
    (Shortcut.equal (Shortcut.char ~shift:true 'r') (Shortcut.char 'R'));
  is_true ~msg:"Shift on a capital is consumed too"
    (Shortcut.equal (Shortcut.char ~shift:true 'R') (Shortcut.char 'R'));
  is_true ~msg:"Ctrl+Shift+g matches Char 'G' with Ctrl"
    (Shortcut.matches
       (Shortcut.char ~ctrl:true ~shift:true 'g')
       (event ~mods:ctrl (char 'G')));
  is_false ~msg:"Shift stays on a key it does not change"
    (Shortcut.equal (Shortcut.shift ' ') Shortcut.space);
  is_true ~msg:"Shift+Space matches with its shift"
    (Shortcut.matches (Shortcut.shift ' ')
       (event ~mods:{ Keystroke.no_mods with shift = true } (char ' ')));
  is_true ~msg:"Caps Lock leaves a chord's key, and is ignored"
    (Shortcut.matches (Shortcut.ctrl 's')
       (event ~mods:{ ctrl with caps_lock = true } (char 's')))

let base_layout () =
  let mods = { Keystroke.no_mods with ctrl = true } in
  is_true ~msg:"the base-layout key matches with the same modifiers"
    (Shortcut.matches (Shortcut.ctrl 'c')
       (event ~mods ~base:(Uchar.of_char 'c')
          (Keystroke.Char (Uchar.of_int 0x0441))));
  is_false ~msg:"the base-layout key is not lowercased"
    (Shortcut.matches (Shortcut.ctrl 'c')
       (event ~mods ~base:(Uchar.of_char 'C')
          (Keystroke.Char (Uchar.of_int 0x0441))))

let keypad_and_line_feed () =
  List.iter
    (fun (shortcut, key) ->
      is_true ~msg:"a keypad key matches its main-keyboard shortcut"
        (Shortcut.matches shortcut (event ~keypad:true key)))
    [
      (Shortcut.enter, Keystroke.Enter);
      (Shortcut.left, Keystroke.Left);
      (Shortcut.key Keystroke.Page_down, Keystroke.Page_down);
      (Shortcut.char '1', char '1');
      (Shortcut.char '.', char '.');
      (Shortcut.char '+', char '+');
    ];
  is_false ~msg:"keypad Begin is not Home"
    (Shortcut.matches
       (Shortcut.key Keystroke.Home)
       (event ~keypad:true Keystroke.Begin));
  is_true ~msg:"keypad Begin is bindable"
    (Shortcut.matches
       (Shortcut.key Keystroke.Begin)
       (event ~keypad:true Keystroke.Begin));
  is_true ~msg:"unidentified protocol keys remain bindable"
    (Shortcut.matches
       (Shortcut.key (Keystroke.Unidentified 63000))
       (event (Keystroke.Unidentified 63000)));
  is_true ~msg:"a line feed (0x0A) is Ctrl+J"
    (Shortcut.matches (Shortcut.ctrl 'j')
       (event ~mods:{ Keystroke.no_mods with ctrl = true } (char 'j')))

let matches_does_not_allocate_per_event () =
  let shortcut = Shortcut.ctrl 'c' in
  let event =
    event
      ~mods:{ Keystroke.no_mods with ctrl = true }
      ~base:(Uchar.of_char 'c')
      (Keystroke.Char (Uchar.of_int 0x0441))
  in
  let matches = ref 0 in
  let before = Gc.allocated_bytes () in
  for i = 1 to 10000 do
    ignore i;
    if Sys.opaque_identity (Shortcut.matches shortcut event) then incr matches
  done;
  let allocated = Gc.allocated_bytes () -. before in
  equal ~msg:"all repeated matches use the base layout" int 10000 !matches;
  is_true ~msg:"matching allocates no per-event key list or string"
    (allocated < 1024.)

let () =
  run "mosaic.shortcut"
    [
      test "exact non-lock modifiers and repeats" exact_modifiers;
      test "Shift is consumed into the key" shift_and_case;
      test "base-layout matching" base_layout;
      test "keypad keys and line feed" keypad_and_line_feed;
      test "matching allocation is independent of event count"
        matches_does_not_allocate_per_event;
    ]
