(* Subscription constructors, mappings and a small timer model. Complete-loop
   tests in test_loop cover dispatch ordering: earlier command messages drain
   before input, and accepted declarations refresh before the next event. *)

module Input = Matrix_input
open Windtrap
open Mosaic

(* ── Timer Model (minimal reproduction) ── *)

type timer_state = Idle | Running

type model = {
  time_remaining : int;
  state : timer_state;
  elapsed_time : float;
  tick_count : int;
}

type msg = Tick of float | Every_fired | Quit

let update msg m =
  match msg with
  | Tick dt ->
      if m.state = Running then
        let elapsed = m.elapsed_time +. dt in
        if elapsed >= 1.0 then
          let new_time = max 0 (m.time_remaining - 1) in
          let new_state = if new_time = 0 then Idle else Running in
          {
            time_remaining = new_time;
            state = new_state;
            elapsed_time = 0.0;
            tick_count = m.tick_count + 1;
          }
        else { m with elapsed_time = elapsed; tick_count = m.tick_count + 1 }
      else m
  | Every_fired -> { m with tick_count = m.tick_count + 1 }
  | Quit -> m

(* ── Subscription collectors ── *)

(* Flatten a Sub.t into a list of individual subs. *)
let rec flatten_sub (sub : msg Sub.t) : msg Sub.t list =
  match sub with
  | Sub.None -> []
  | Sub.Batch subs -> List.concat_map flatten_sub subs
  | other -> [ other ]

(* ── Tests ── *)

let sub_on_tick_produces_messages () =
  (* Verify that Sub.on_tick produces a message with the correct dt. *)
  let sub = Sub.on_tick ~id:"frame" (fun ~dt -> Tick dt) in
  match sub with
  | Sub.On_tick (id, f) -> (
      equal string "frame" id;
      let msg = f ~dt:0.016 in
      match msg with
      | Tick dt -> equal ~msg:"dt" (float 0.001) 0.016 dt
      | _ -> fail "expected Tick msg")
  | _ -> fail "expected On_tick sub"

let sub_every_produces_messages () =
  (* Verify that Sub.every produces a message when called. *)
  let sub = Sub.every ~id:"countdown" 1.0 (fun () -> Every_fired) in
  match sub with
  | Sub.Every (id, interval, f) -> (
      equal ~msg:"stable timer identity" string "countdown" id;
      equal ~msg:"interval" (float 0.001) 1.0 interval;
      let msg = f () in
      match msg with Every_fired -> () | _ -> fail "expected Every_fired msg")
  | _ -> fail "expected Every sub"

let sub_every_map_preserves_identity () =
  let original = Sub.every ~id:"refresh" 0.5 (fun () -> 3) in
  match Sub.map (fun n -> n + 1) original with
  | Sub.Every (id, interval, f) ->
      equal string "refresh" id;
      equal float_exact 0.5 interval;
      equal int 4 (f ())
  | _ -> fail "expected Every sub"

let timer_model_accumulates_elapsed () =
  (* Verify the timer model accumulates elapsed time across ticks and decrements
     time_remaining after 1 second. *)
  let m =
    { time_remaining = 5; state = Running; elapsed_time = 0.0; tick_count = 0 }
  in
  (* Simulate 60 ticks at ~16.67ms each (= 1 second) *)
  let m = ref m in
  for _ = 1 to 59 do
    m := update (Tick (1.0 /. 60.0)) !m
  done;
  (* After 59 ticks (~983ms), time_remaining should still be 5 *)
  equal ~msg:"remaining after 59 ticks" int 5 !m.time_remaining;
  (* 60th tick pushes us past 1.0s *)
  let m' = update (Tick (1.0 /. 60.0)) !m in
  equal ~msg:"remaining after 60 ticks" int 4 m'.time_remaining;
  equal ~msg:"elapsed resets" (float 0.001) 0.0 m'.elapsed_time

let timer_model_goes_idle_at_zero () =
  (* When time_remaining reaches 0, state should become Idle. *)
  let m =
    { time_remaining = 1; state = Running; elapsed_time = 0.99; tick_count = 0 }
  in
  let m' = update (Tick 0.02) m in
  equal ~msg:"remaining" int 0 m'.time_remaining;
  is_true ~msg:"idle" (m'.state = Idle)

let sub_batch_flattens () =
  (* Verify that Sub.batch correctly groups subscriptions. *)
  let subs =
    Sub.batch
      [
        Sub.on_tick ~id:"frame" (fun ~dt -> Tick dt);
        Sub.on_key (fun _ -> None);
        Sub.every ~id:"batch-timer" 2.0 (fun () -> Every_fired);
      ]
  in
  let flat = flatten_sub subs in
  equal ~msg:"3 subs" int 3 (List.length flat)

let sub_map_transforms_messages () =
  (* Verify that Sub.map correctly transforms messages. *)
  let sub = Sub.on_tick ~id:"frame" (fun ~dt -> Tick dt) in
  let mapped : msg Sub.t = Sub.map (fun msg -> msg) sub in
  match mapped with
  | Sub.On_tick (id, f) -> (
      equal string "frame" id;
      match f ~dt:0.5 with
      | Tick dt -> equal ~msg:"mapped dt" (float 0.001) 0.5 dt
      | _ -> fail "expected Tick")
  | _ -> fail "expected On_tick"

let key ?modifier ?event_type ?base_key key =
  Event.Key.of_input (Input.Key.make ?modifier ?event_type ?base_key key)

let char ?modifier ?event_type ?base_key c =
  key ?modifier ?event_type ?base_key (Input.Key.Char (Uchar.of_char c))

let shortcut_matches_plain_keys () =
  is_true ~msg:"char shortcut" (Shortcut.matches (Shortcut.char 'q') (char 'q'));
  is_true ~msg:"escape shortcut"
    (Shortcut.matches Shortcut.escape (key Input.Key.Escape));
  is_false ~msg:"wrong char" (Shortcut.matches (Shortcut.char 'q') (char 'x'))

let shortcut_ignores_key_release () =
  let ev = char ~event_type:Input.Key.Release 'q' in
  is_false ~msg:"release ignored" (Shortcut.matches (Shortcut.char 'q') ev)

let shortcut_matches_ctrl_base_key () =
  let ev =
    key
      ~modifier:{ Input.Modifier.none with ctrl = true }
      ~base_key:(Uchar.of_char 'c')
      (Input.Key.Char (Uchar.of_int 0x0441))
  in
  is_true ~msg:"base-layout fallback" (Shortcut.matches (Shortcut.ctrl 'c') ev)

let shortcut_matches_legacy_shift_char () =
  let ev = char ~modifier:{ Input.Modifier.none with shift = true } 'A' in
  is_true ~msg:"legacy uppercase fallback"
    (Shortcut.matches (Shortcut.char ~shift:true 'a') ev)

let shortcut_alt_matches_matrix_alt_meta () =
  let ev =
    char ~modifier:{ Input.Modifier.none with alt = true; meta = true } 'x'
  in
  is_true ~msg:"Alt also requires Matrix meta"
    (Shortcut.matches (Shortcut.alt 'x') ev)

let sub_on_keys_resolution () =
  let resolve bindings event =
    match Sub.on_keys bindings with
    | Sub.On_key f -> f event
    | _ -> fail "expected On_key"
  in
  let check message expected bindings event =
    equal ~msg:message (option int) expected (resolve bindings event);
    equal ~msg:"only bound events prevent defaults" bool
      (Option.is_some expected)
      (Event.Key.default_prevented event)
  in
  check "later equivalent bindings override earlier ones" (Some 2)
    [ (Shortcut.char 'q', 1); (Shortcut.char 'q', 2) ]
    (char 'q');
  check "keypad equivalents override earlier ones" (Some 2)
    [ (Shortcut.enter, 1); (Shortcut.key Input.Key.KP_enter, 2) ]
    (key Input.Key.Enter);
  check "exact uppercase precedes lowercase regardless of order" (Some 1)
    [ (Shortcut.char 'Q', 1); (Shortcut.char 'q', 2) ]
    (char 'Q');
  check "direct character precedes base layout regardless of order" (Some 1)
    [ (Shortcut.char 'x', 1); (Shortcut.char 'q', 2) ]
    (char ~base_key:(Uchar.of_char 'q') 'x');
  check "normalized direct character precedes base layout" (Some 1)
    [ (Shortcut.char 'x', 1); (Shortcut.char 'q', 2) ]
    (char ~base_key:(Uchar.of_char 'q') 'X');
  check "base-layout fallback remains available" (Some 2)
    [ (Shortcut.char 'q', 1); (Shortcut.char 'q', 2) ]
    (char ~base_key:(Uchar.of_char 'Q') 'x');
  check "release never resolves" None
    [ (Shortcut.char 'q', 1) ]
    (char ~event_type:Input.Key.Release 'q');
  check "unbound modifiers remain available" None
    [ (Shortcut.char 'q', 1) ]
    (char ~modifier:{ Input.Modifier.none with ctrl = true } 'q')

let sub_on_keys_does_not_compile_or_allocate_per_override () =
  let shortcut = Shortcut.char 'q' in
  let bindings = List.init 128 (fun value -> (shortcut, value)) in
  let before = Gc.allocated_bytes () in
  let sub = Sys.opaque_identity (Sub.on_keys bindings) in
  let constructed = Gc.allocated_bytes () -. before in
  is_true ~msg:"model updates do not compile a binding table"
    (constructed < 1024.);
  let measure bindings =
    let dispatch =
      match Sub.on_keys bindings with
      | Sub.On_key f -> f
      | _ -> fail "expected key subscription"
    in
    let event = char 'q' in
    let before = Gc.allocated_bytes () in
    for i = 1 to 10000 do
      ignore i;
      ignore (Sys.opaque_identity (dispatch event))
    done;
    Gc.allocated_bytes () -. before
  in
  ignore sub;
  let single = measure [ (shortcut, 127) ] in
  let layered = measure bindings in
  is_true ~msg:"overrides do not add allocation to event dispatch"
    (layered <= single +. 1024.)

let cmd_copy_to_clipboard_maps_without_changing_payload () =
  match Cmd.map (fun msg -> msg) (Cmd.copy_to_clipboard "hello") with
  | Cmd.Copy_to_clipboard "hello" -> ()
  | Cmd.Copy_to_clipboard other ->
      failf "unexpected clipboard payload: %S" other
  | Cmd.None | Cmd.Batch _ | Cmd.Perform _ | Cmd.Quit | Cmd.Set_title _
  | Cmd.Clear_selection | Cmd.Static_commit _ | Cmd.Static_clear
  | Cmd.Copy_selection | Cmd.Query_color_scheme | Cmd.Bell | Cmd.Notify _ ->
      fail "expected Copy_to_clipboard"

let cmd_clear_selection_maps_to_itself () =
  match Cmd.map (fun msg -> msg) Cmd.clear_selection with
  | Cmd.Clear_selection -> ()
  | Cmd.None | Cmd.Batch _ | Cmd.Perform _ | Cmd.Quit | Cmd.Set_title _
  | Cmd.Copy_to_clipboard _ | Cmd.Static_commit _ | Cmd.Static_clear
  | Cmd.Copy_selection | Cmd.Query_color_scheme | Cmd.Bell | Cmd.Notify _ ->
      fail "expected Clear_selection"

let cmd_copy_selection_maps_to_itself () =
  match Cmd.map (fun msg -> msg) Cmd.copy_selection with
  | Cmd.Copy_selection -> ()
  | Cmd.None | Cmd.Batch _ | Cmd.Perform _ | Cmd.Quit | Cmd.Set_title _
  | Cmd.Copy_to_clipboard _ | Cmd.Clear_selection | Cmd.Static_commit _
  | Cmd.Static_clear | Cmd.Query_color_scheme | Cmd.Bell | Cmd.Notify _ ->
      fail "expected Copy_selection"

let cmd_query_color_scheme_maps_to_itself () =
  match Cmd.map (fun msg -> msg) Cmd.query_color_scheme with
  | Cmd.Query_color_scheme -> ()
  | Cmd.None | Cmd.Batch _ | Cmd.Perform _ | Cmd.Quit | Cmd.Set_title _
  | Cmd.Copy_to_clipboard _ | Cmd.Clear_selection | Cmd.Static_commit _
  | Cmd.Static_clear | Cmd.Copy_selection | Cmd.Bell | Cmd.Notify _ ->
      fail "expected Query_color_scheme"

let sub_on_color_scheme_delivers_reports () =
  let sub =
    Sub.on_color_scheme (function `Dark -> Some Quit | `Light -> None)
  in
  match sub with
  | Sub.On_color_scheme f ->
      (match f `Dark with
      | Some Quit -> ()
      | Some _ -> fail "expected Quit for dark"
      | None -> fail "expected a message for dark");
      is_true ~msg:"light ignored" (f `Light = None)
  | _ -> fail "expected On_color_scheme sub"

let sub_on_color_scheme_maps_messages () =
  let sub = Sub.on_color_scheme (fun _ -> Some Quit) in
  let mapped : msg Sub.t = Sub.map (fun msg -> msg) sub in
  match mapped with
  | Sub.On_color_scheme f -> (
      match f `Dark with Some Quit -> () | _ -> fail "expected mapped Quit")
  | _ -> fail "expected On_color_scheme"

let () =
  Windtrap.run "subscriptions"
    [
      test "Sub.on_tick produces messages" sub_on_tick_produces_messages;
      test "Sub.every produces messages" sub_every_produces_messages;
      test "Sub.every mapping preserves identity"
        sub_every_map_preserves_identity;
      test "timer model accumulates elapsed" timer_model_accumulates_elapsed;
      test "timer model goes idle at zero" timer_model_goes_idle_at_zero;
      test "Sub.batch flattens" sub_batch_flattens;
      test "Sub.map transforms messages" sub_map_transforms_messages;
      test "Shortcut.matches plain keys" shortcut_matches_plain_keys;
      test "Shortcut ignores key release" shortcut_ignores_key_release;
      test "Shortcut matches ctrl base key" shortcut_matches_ctrl_base_key;
      test "Shortcut matches legacy shift char"
        shortcut_matches_legacy_shift_char;
      test "Shortcut Alt matches Matrix Alt/Meta"
        shortcut_alt_matches_matrix_alt_meta;
      test "Sub.on_keys binding resolution" sub_on_keys_resolution;
      test "Sub.on_keys allocation across overrides"
        sub_on_keys_does_not_compile_or_allocate_per_override;
      test "Cmd.copy_to_clipboard maps without changing payload"
        cmd_copy_to_clipboard_maps_without_changing_payload;
      test "Cmd.clear_selection maps to itself"
        cmd_clear_selection_maps_to_itself;
      test "Cmd.copy_selection maps to itself" cmd_copy_selection_maps_to_itself;
      test "Cmd.query_color_scheme maps to itself"
        cmd_query_color_scheme_maps_to_itself;
      test "Sub.on_color_scheme delivers reports"
        sub_on_color_scheme_delivers_reports;
      test "Sub.on_color_scheme maps messages" sub_on_color_scheme_maps_messages;
    ]
