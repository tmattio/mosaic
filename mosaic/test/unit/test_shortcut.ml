open Windtrap
open Mosaic_ui
module Key = Matrix_input.Key
module Modifier = Matrix_input.Modifier

let event ?modifier ?event_type ?base_key key =
  Event.Key.of_input (Key.make ?modifier ?event_type ?base_key key)

let exact_modifiers () =
  let shortcut = Shortcut.ctrl 'c' in
  let key = Key.Char (Uchar.of_char 'c') in
  let modifiers = { Modifier.none with ctrl = true } in
  is_true (Shortcut.matches shortcut (event ~modifier:modifiers key));
  is_true ~msg:"lock state is ignored"
    (Shortcut.matches shortcut
       (event
          ~modifier:{ modifiers with caps_lock = true; num_lock = true }
          key));
  List.iter
    (fun modifier ->
      is_false ~msg:"extra non-lock modifiers do not match"
        (Shortcut.matches shortcut (event ~modifier key)))
    [
      { modifiers with shift = true };
      { modifiers with alt = true; meta = true };
      { modifiers with super = true };
      { modifiers with hyper = true };
      { modifiers with meta = true };
    ];
  is_false ~msg:"release is ignored"
    (Shortcut.matches shortcut
       (event ~modifier:modifiers ~event_type:Release key));
  is_true ~msg:"repeat matches"
    (Shortcut.matches shortcut
       (event ~modifier:modifiers ~event_type:Repeat key));
  is_true ~msg:"Alt uses Matrix's Alt and Meta pair"
    (Shortcut.matches (Shortcut.alt 'c')
       (event ~modifier:{ Modifier.none with alt = true; meta = true } key));
  is_false ~msg:"Meta alone is distinct from Alt"
    (Shortcut.matches (Shortcut.alt 'c')
       (event ~modifier:{ Modifier.none with meta = true } key))

let base_layout_and_case () =
  let modifier = { Modifier.none with ctrl = true } in
  is_true ~msg:"lowercase shortcuts match uppercase event characters"
    (Shortcut.matches (Shortcut.ctrl 'c')
       (event ~modifier (Key.Char (Uchar.of_char 'C'))));
  is_false ~msg:"uppercase shortcuts do not match lowercase event characters"
    (Shortcut.matches (Shortcut.ctrl 'C')
       (event ~modifier (Key.Char (Uchar.of_char 'c'))));
  is_true ~msg:"base-layout fallback is normalized"
    (Shortcut.matches (Shortcut.ctrl 'c')
       (event ~modifier ~base_key:(Uchar.of_char 'C')
          (Key.Char (Uchar.of_int 0x0441))))

let keypad_equivalences () =
  List.iter
    (fun (shortcut, key) ->
      is_true ~msg:"keypad match" (Shortcut.matches shortcut (event key));
      is_true ~msg:"keypad shortcut equality"
        (Shortcut.equal shortcut (Shortcut.key key)))
    [
      (Shortcut.enter, Key.KP_enter);
      (Shortcut.left, Key.KP_left);
      (Shortcut.key Key.Page_down, Key.KP_page_down);
      (Shortcut.char '1', Key.KP_1);
      (Shortcut.char '.', Key.KP_decimal);
      (Shortcut.char '+', Key.KP_add);
    ];
  is_false ~msg:"keypad Begin is not Home"
    (Shortcut.matches (Shortcut.key Key.Home) (event Key.KP_begin));
  is_true ~msg:"unknown protocol keys remain bindable"
    (Shortcut.matches
       (Shortcut.key (Key.Unknown 99999))
       (event (Key.Unknown 99999)))

let matches_does_not_allocate_per_event () =
  let shortcut = Shortcut.ctrl 'c' in
  let event =
    event
      ~modifier:{ Modifier.none with ctrl = true }
      ~base_key:(Uchar.of_char 'C')
      (Key.Char (Uchar.of_int 0x0441))
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
      test "exact non-lock modifiers and event phases" exact_modifiers;
      test "base-layout and case matching" base_layout_and_case;
      test "typed keypad equivalences" keypad_equivalences;
      test "matching allocation is independent of event count"
        matches_does_not_allocate_per_event;
    ]
