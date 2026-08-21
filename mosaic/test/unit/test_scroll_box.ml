open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let make_scroll_box ?scroll_x ?scroll_y ?sticky_scroll ?sticky_start ?background
    ?scroll_accel ?on_scroll ?on_scroll_by_applied ?on_reset_sticky_applied () =
  let t = make_ctx () in
  let root = make_root t in
  let sb =
    Scroll_box.create ~parent:root ?scroll_x ?scroll_y ?sticky_scroll
      ?sticky_start ?background ?scroll_accel ?on_scroll ?on_scroll_by_applied
      ?on_reset_sticky_applied ()
  in
  (t, sb)

(* ── Scroll_accel ── *)

let accel_linear_always_one () =
  let a = Scroll_box.Scroll_accel.linear () in
  let m1 = Scroll_box.Scroll_accel.tick a ~now:0. in
  let m2 = Scroll_box.Scroll_accel.tick a ~now:100. in
  let m3 = Scroll_box.Scroll_accel.tick a ~now:200. in
  is_true ~msg:"first = 1.0" (Float.equal m1 1.0);
  is_true ~msg:"second = 1.0" (Float.equal m2 1.0);
  is_true ~msg:"third = 1.0" (Float.equal m3 1.0)

let accel_macos_first_tick_one () =
  let a = Scroll_box.Scroll_accel.macos () in
  let m = Scroll_box.Scroll_accel.tick a ~now:0. in
  is_true ~msg:"first = 1.0" (Float.equal m 1.0)

let accel_macos_accelerates_on_rapid_events () =
  let a = Scroll_box.Scroll_accel.macos () in
  let _ = Scroll_box.Scroll_accel.tick a ~now:0. in
  let _ = Scroll_box.Scroll_accel.tick a ~now:20. in
  let _ = Scroll_box.Scroll_accel.tick a ~now:40. in
  let m = Scroll_box.Scroll_accel.tick a ~now:60. in
  satisfies ~claim:"multiplier above 1.0" float_exact (fun m -> m > 1.0) m

let accel_macos_resets_after_timeout () =
  let a = Scroll_box.Scroll_accel.macos () in
  let _ = Scroll_box.Scroll_accel.tick a ~now:0. in
  let _ = Scroll_box.Scroll_accel.tick a ~now:20. in
  let _ = Scroll_box.Scroll_accel.tick a ~now:40. in
  (* Long pause exceeding streak_timeout (150ms) *)
  let m = Scroll_box.Scroll_accel.tick a ~now:500. in
  is_true ~msg:"reset to 1.0" (Float.equal m 1.0)

let accel_macos_respects_cap () =
  let a = Scroll_box.Scroll_accel.macos ~max_multiplier:2.0 () in
  (* Rapid events to build up velocity *)
  let m = ref 1.0 in
  for i = 1 to 30 do
    m := Scroll_box.Scroll_accel.tick a ~now:(float_of_int (i * 10))
  done;
  satisfies ~claim:"capped at 2.0" float_exact (fun m -> m <= 2.0) !m

let accel_reset_clears_history () =
  let a = Scroll_box.Scroll_accel.macos () in
  let _ = Scroll_box.Scroll_accel.tick a ~now:0. in
  let _ = Scroll_box.Scroll_accel.tick a ~now:20. in
  Scroll_box.Scroll_accel.reset a;
  let m = Scroll_box.Scroll_accel.tick a ~now:100. in
  is_true ~msg:"back to 1.0 after reset" (Float.equal m 1.0)

(* ── Props ── *)

let props_defaults () =
  let p = Scroll_box.Props.default in
  is_true ~msg:"equal to make()"
    (Scroll_box.Props.equal p (Scroll_box.Props.make ()))

let props_equal_identical () =
  let a = Scroll_box.Props.make () in
  let b = Scroll_box.Props.make () in
  is_true ~msg:"equal" (Scroll_box.Props.equal a b)

let props_detects_scroll_x_diff () =
  let a = Scroll_box.Props.make ~scroll_x:true () in
  let b = Scroll_box.Props.make ~scroll_x:false () in
  is_false ~msg:"different" (Scroll_box.Props.equal a b)

let props_detects_scroll_y_diff () =
  let a = Scroll_box.Props.make ~scroll_y:true () in
  let b = Scroll_box.Props.make ~scroll_y:false () in
  is_false ~msg:"different" (Scroll_box.Props.equal a b)

let props_detects_sticky_scroll_diff () =
  let a = Scroll_box.Props.make ~sticky_scroll:true () in
  let b = Scroll_box.Props.make ~sticky_scroll:false () in
  is_false ~msg:"different" (Scroll_box.Props.equal a b)

let props_detects_sticky_start_diff () =
  let a = Scroll_box.Props.make ~sticky_start:`Top () in
  let b = Scroll_box.Props.make ~sticky_start:`Bottom () in
  is_false ~msg:"different" (Scroll_box.Props.equal a b)

let props_detects_background_diff () =
  let a = Scroll_box.Props.make ~background:Ansi.Color.red () in
  let b = Scroll_box.Props.make ~background:Ansi.Color.blue () in
  is_false ~msg:"different" (Scroll_box.Props.equal a b)

let scroll_request key y : Scroll_box.scroll_by =
  { key; x = None; y = Some y; unit = `Viewport }

let props_detects_scroll_by_diff () =
  let request = scroll_request "page-1" (-1.) in
  let without = Scroll_box.Props.make () in
  let with_request = Scroll_box.Props.make ~scroll_by:request () in
  let changed_key =
    Scroll_box.Props.make ~scroll_by:(scroll_request "page-2" (-1.)) ()
  in
  let changed_delta =
    Scroll_box.Props.make ~scroll_by:(scroll_request "page-1" 1.) ()
  in
  is_false ~msg:"presence differs" (Scroll_box.Props.equal without with_request);
  is_false ~msg:"key differs" (Scroll_box.Props.equal with_request changed_key);
  is_false ~msg:"delta differs"
    (Scroll_box.Props.equal with_request changed_delta)

let props_detects_reset_sticky_diff () =
  let without = Scroll_box.Props.make () in
  let first = Scroll_box.Props.make ~reset_sticky:"turn-1" () in
  let second = Scroll_box.Props.make ~reset_sticky:"turn-2" () in
  is_false ~msg:"presence differs" (Scroll_box.Props.equal without first);
  is_false ~msg:"key differs" (Scroll_box.Props.equal first second)

let props_rejects_invalid_scroll_by () =
  raises_match ~msg:"an axis is required"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () ->
      ignore
        (Scroll_box.Props.make
           ~scroll_by:{ key = "empty"; x = None; y = None; unit = `Viewport }
           ()));
  raises_match ~msg:"deltas are finite"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () ->
      ignore
        (Scroll_box.Props.make
           ~scroll_by:
             { key = "nan"; x = None; y = Some Float.nan; unit = `Viewport }
           ()))

(* ── Construction ── *)

let create_attaches () =
  let _t, sb = make_scroll_box () in
  match Renderable.parent (Scroll_box.node sb) with
  | Some _ -> ()
  | None -> fail "expected parent"

let create_default_scroll_zero () =
  let _t, sb = make_scroll_box () in
  equal ~msg:"scroll_top = 0" int 0 (Scroll_box.scroll_top sb);
  equal ~msg:"scroll_left = 0" int 0 (Scroll_box.scroll_left sb)

let create_child_target_set () =
  let _t, sb = make_scroll_box () in
  let target = Renderable.child_target (Scroll_box.node sb) in
  let content = Scroll_box.content sb in
  is_true ~msg:"target is content node"
    (Renderable.id target = Renderable.id content)

let create_vertical_bar_attached_to_root () =
  let _t, sb = make_scroll_box () in
  let bar_node = Scroll_bar.node (Scroll_box.vertical_bar sb) in
  match Renderable.parent bar_node with
  | None -> fail "expected vertical bar parent"
  | Some parent ->
      is_true ~msg:"vertical bar is attached to scroll box root"
        (Renderable.id parent = Renderable.id (Scroll_box.node sb))

let create_viewport_has_overflow_hidden () =
  let _t, sb = make_scroll_box () in
  let vp = Scroll_box.viewport sb in
  let style = Renderable.style vp in
  let overflow = Toffee.Style.overflow style in
  is_true ~msg:"overflow x hidden" (overflow.x = Toffee.Style.Overflow.Hidden);
  is_true ~msg:"overflow y hidden" (overflow.y = Toffee.Style.Overflow.Hidden)

let vertical_bar_visible_when_content_overflows () =
  let t = make_ctx () in
  let root = make_root t in
  let sb = Scroll_box.create ~parent:root () in
  layout_node root ~x:0 ~y:0 ~width:20 ~height:8;
  layout_node (Scroll_box.node sb) ~x:0 ~y:0 ~width:20 ~height:8;
  layout_node (Scroll_box.viewport sb) ~x:0 ~y:0 ~width:19 ~height:8;
  layout_node (Scroll_box.content sb) ~x:0 ~y:0 ~width:19 ~height:20;
  Renderable.Private.pre_render_update (Scroll_box.content sb) ~delta:0.;
  is_true ~msg:"vertical bar visible on overflow"
    (Renderable.visible (Scroll_bar.node (Scroll_box.vertical_bar sb)))

(* ── Scroll Position ── *)

let set_scroll_top_clamps () =
  let _t, sb = make_scroll_box () in
  (* Without layout, max_scroll_y is 0, so everything clamps to 0 *)
  Scroll_box.set_scroll_top sb 100;
  equal ~msg:"clamped to 0" int 0 (Scroll_box.scroll_top sb);
  Scroll_box.set_scroll_top sb (-10);
  equal ~msg:"clamped to 0" int 0 (Scroll_box.scroll_top sb)

let scroll_to_sets_both () =
  let _t, sb = make_scroll_box ~scroll_x:true () in
  Scroll_box.scroll_to sb ~x:5 ~y:10 ();
  (* Both clamped to 0 without layout *)
  equal ~msg:"x clamped" int 0 (Scroll_box.scroll_left sb);
  equal ~msg:"y clamped" int 0 (Scroll_box.scroll_top sb)

let scroll_by_adjusts_relative () =
  let _t, sb = make_scroll_box () in
  Scroll_box.scroll_by sb ~y:10 ();
  (* Clamped to max_scroll_y = 0 *)
  equal ~msg:"clamped" int 0 (Scroll_box.scroll_top sb)

let on_scroll_fires () =
  let log = ref [] in
  let _t, sb =
    make_scroll_box ~on_scroll:(fun ~x ~y -> log := (x, y) :: !log) ()
  in
  (* Even though position won't change (clamped), the callback should not fire
     unless there's an actual change *)
  Scroll_box.set_scroll_top sb 0;
  equal ~msg:"no spurious fire" int 0 (List.length !log)

let render_scroll_box sb ~vh ~ch =
  layout_node (Scroll_box.node sb) ~x:0 ~y:0 ~width:20 ~height:vh;
  layout_node (Scroll_box.viewport sb) ~x:0 ~y:0 ~width:19 ~height:vh;
  layout_node (Scroll_box.content sb) ~x:0 ~y:0 ~width:19 ~height:ch;
  Renderable.Private.pre_render_update (Scroll_box.content sb) ~delta:0.

let wheel ?(shift = false) direction =
  let modifiers = { Event.Mouse.no_modifier with shift } in
  Event.Mouse.make ~x:0 ~y:0 ~modifiers (Scroll { direction; delta = 1 })

let emit_wheel sb event =
  Renderable.Private.emit_mouse (Scroll_box.content sb) event

let parent_node sb =
  match Renderable.parent (Scroll_box.node sb) with
  | Some parent -> parent
  | None -> fail "expected scroll box parent"

let wheel_consumes_only_when_scroll_position_changes () =
  let _test_context, sb = make_scroll_box () in
  let bubbled = ref 0 in
  Renderable.on_mouse (parent_node sb) (fun _event -> incr bubbled);
  render_scroll_box sb ~vh:4 ~ch:12;
  let down = wheel Scroll_down in
  emit_wheel sb down;
  equal ~msg:"wheel advances scroll position" int 1 (Scroll_box.scroll_top sb);
  is_true ~msg:"movement consumes wheel event"
    (Event.Mouse.propagation_stopped down);
  equal ~msg:"consumed event does not reach parent" int 0 !bubbled;
  Scroll_box.set_scroll_top sb 8;
  let clamped_down = wheel Scroll_down in
  emit_wheel sb clamped_down;
  equal ~msg:"boundary does not change position" int 8
    (Scroll_box.scroll_top sb);
  is_false ~msg:"clamped event remains available to parent"
    (Event.Mouse.propagation_stopped clamped_down);
  equal ~msg:"clamped event bubbles" int 1 !bubbled

let wheel_without_usable_axis_bubbles () =
  let _test_context, sb = make_scroll_box ~scroll_x:false ~scroll_y:true () in
  let bubbled = ref 0 in
  Renderable.on_mouse (parent_node sb) (fun _event -> incr bubbled);
  render_scroll_box sb ~vh:4 ~ch:12;
  let horizontal = wheel Scroll_right in
  emit_wheel sb horizontal;
  equal ~msg:"horizontal wheel leaves vertical position unchanged" int 0
    (Scroll_box.scroll_top sb);
  is_false ~msg:"disabled horizontal axis does not consume"
    (Event.Mouse.propagation_stopped horizontal);
  let shifted_vertical = wheel ~shift:true Scroll_down in
  emit_wheel sb shifted_vertical;
  equal ~msg:"shift-remapped wheel leaves vertical position unchanged" int 0
    (Scroll_box.scroll_top sb);
  is_false ~msg:"shift-remapped disabled axis does not consume"
    (Event.Mouse.propagation_stopped shifted_vertical);
  equal ~msg:"both unusable wheel events bubble" int 2 !bubbled

let wheel_without_overflow_bubbles () =
  let _test_context, sb = make_scroll_box () in
  let bubbled = ref 0 in
  Renderable.on_mouse (parent_node sb) (fun _event -> incr bubbled);
  render_scroll_box sb ~vh:4 ~ch:4;
  let down = wheel Scroll_down in
  emit_wheel sb down;
  equal ~msg:"content without overflow stays at origin" int 0
    (Scroll_box.scroll_top sb);
  is_false ~msg:"non-scrollable box does not consume"
    (Event.Mouse.propagation_stopped down);
  equal ~msg:"event bubbles past non-scrollable box" int 1 !bubbled

let nested_wheel_bubbles_at_inner_boundary () =
  let test_context = make_ctx () in
  let root = make_root test_context in
  let outer = Scroll_box.create ~parent:root () in
  let inner = Scroll_box.create ~parent:(Scroll_box.content outer) () in
  let escaped = ref 0 in
  Renderable.on_mouse root (fun _event -> incr escaped);
  render_scroll_box outer ~vh:8 ~ch:24;
  render_scroll_box inner ~vh:4 ~ch:12;
  let handled_by_inner = wheel Scroll_down in
  emit_wheel inner handled_by_inner;
  equal ~msg:"inner scrolls first" int 1 (Scroll_box.scroll_top inner);
  equal ~msg:"outer remains parked while inner can scroll" int 0
    (Scroll_box.scroll_top outer);
  equal ~msg:"handled event does not escape nested scroll boxes" int 0 !escaped;
  Scroll_box.set_scroll_top inner 8;
  let handled_by_outer = wheel Scroll_down in
  emit_wheel inner handled_by_outer;
  equal ~msg:"inner stays at its boundary" int 8 (Scroll_box.scroll_top inner);
  equal ~msg:"outer takes over at inner boundary" int 1
    (Scroll_box.scroll_top outer);
  is_true ~msg:"outer movement consumes bubbled event"
    (Event.Mouse.propagation_stopped handled_by_outer);
  equal ~msg:"outer consumption prevents escape" int 0 !escaped;
  Scroll_box.set_scroll_top outer 16;
  let both_clamped = wheel Scroll_down in
  emit_wheel inner both_clamped;
  equal ~msg:"inner remains clamped" int 8 (Scroll_box.scroll_top inner);
  equal ~msg:"outer remains clamped" int 16 (Scroll_box.scroll_top outer);
  is_false ~msg:"event remains unconsumed when no scroll box moves"
    (Event.Mouse.propagation_stopped both_clamped);
  equal ~msg:"event escapes after every scroll box declines it" int 1 !escaped

let scroll_by_viewport_uses_measured_height () =
  let test_context, sb = make_scroll_box () in
  render_scroll_box sb ~vh:8 ~ch:40;
  Scroll_box.set_scroll_top sb 24;
  let scheduled_before = !(test_context.schedule_count) in
  Scroll_box.set_scroll_by sb (Some (scroll_request "page-up" (-1.)));
  gt ~msg:"scheduled by the request" ~than:scheduled_before
    !(test_context.schedule_count);
  render_scroll_box sb ~vh:8 ~ch:40;
  equal ~msg:"one measured viewport up" int 16 (Scroll_box.scroll_top sb);
  Scroll_box.set_scroll_by sb (Some (scroll_request "page-down" 0.5));
  render_scroll_box sb ~vh:8 ~ch:40;
  equal ~msg:"half a measured viewport down" int 20 (Scroll_box.scroll_top sb)

let scroll_by_key_applies_at_most_once () =
  let test_context, sb = make_scroll_box () in
  render_scroll_box sb ~vh:8 ~ch:40;
  Scroll_box.set_scroll_top sb 24;
  let first = scroll_request "page" (-1.) in
  let scheduled_before = !(test_context.schedule_count) in
  Scroll_box.apply_props sb (Scroll_box.Props.make ~scroll_by:first ());
  gt ~msg:"scheduled by the new key" ~than:scheduled_before
    !(test_context.schedule_count);
  render_scroll_box sb ~vh:8 ~ch:40;
  equal ~msg:"first application" int 16 (Scroll_box.scroll_top sb);
  Scroll_box.set_scroll_top sb 20;
  Scroll_box.apply_props sb
    (Scroll_box.Props.make ~background:Ansi.Color.blue ~scroll_by:first ());
  render_scroll_box sb ~vh:8 ~ch:40;
  equal ~msg:"unrelated property update does not replay" int 20
    (Scroll_box.scroll_top sb);
  Scroll_box.apply_props sb
    (Scroll_box.Props.make ~scroll_by:(scroll_request "page" 1.) ());
  render_scroll_box sb ~vh:8 ~ch:40;
  equal ~msg:"changed payload cannot reuse a key" int 20
    (Scroll_box.scroll_top sb);
  Scroll_box.apply_props sb
    (Scroll_box.Props.make ~scroll_by:(scroll_request "page-next" 0.5) ());
  render_scroll_box sb ~vh:8 ~ch:40;
  equal ~msg:"new key applies" int 24 (Scroll_box.scroll_top sb)

let scroll_by_acknowledges_consumption_at_boundary () =
  let acknowledgements = ref [] in
  let scrolls = ref [] in
  let _test_context, sb =
    make_scroll_box
      ~on_scroll:(fun ~x ~y -> scrolls := (x, y) :: !scrolls)
      ~on_scroll_by_applied:(fun ~key ->
        acknowledgements := key :: !acknowledgements)
      ()
  in
  render_scroll_box sb ~vh:8 ~ch:40;
  Scroll_box.set_scroll_by sb (Some (scroll_request "at-top" (-1.)));
  render_scroll_box sb ~vh:8 ~ch:40;
  equal ~msg:"clamped request does not report a position change" int 0
    (List.length !scrolls);
  equal ~msg:"clamped request is acknowledged" (list string) [ "at-top" ]
    (List.rev !acknowledgements);
  render_scroll_box sb ~vh:8 ~ch:40;
  equal ~msg:"consumed request is acknowledged once" (list string) [ "at-top" ]
    (List.rev !acknowledgements)

(* ── Sticky Scroll ── *)

let set_sticky_scroll_updates () =
  let _t, sb = make_scroll_box ~sticky_scroll:true ~sticky_start:`Bottom () in
  Scroll_box.set_sticky_scroll sb false;
  (* Just verify it doesn't crash *)
  Scroll_box.set_sticky_scroll sb true;
  ()

let reset_sticky_clears_manual () =
  let _t, sb = make_scroll_box ~sticky_scroll:true ~sticky_start:`Top () in
  Scroll_box.reset_sticky sb;
  equal ~msg:"scroll_top reset" int 0 (Scroll_box.scroll_top sb)

(* Lay the box out at the given viewport/content heights and render once, so
   the render path (recalc → reengage → sticky) runs against the new sizes. *)
let render_sticky_box sb ~vh ~ch = render_scroll_box sb ~vh ~ch

let scroll_by_viewport_parks_sticky_content () =
  let test_context, sb =
    make_scroll_box ~sticky_scroll:true ~sticky_start:`Bottom ()
  in
  render_sticky_box sb ~vh:8 ~ch:40;
  equal ~msg:"initially follows bottom" int 32 (Scroll_box.scroll_top sb);
  let scheduled_before = !(test_context.schedule_count) in
  Scroll_box.apply_props sb
    (Scroll_box.Props.make ~sticky_scroll:true ~sticky_start:`Bottom
       ~scroll_by:(scroll_request "page-up" (-1.))
       ());
  gt ~msg:"scheduled by the page request" ~than:scheduled_before
    !(test_context.schedule_count);
  render_sticky_box sb ~vh:8 ~ch:40;
  equal ~msg:"page request uses viewport" int 24 (Scroll_box.scroll_top sb);
  render_sticky_box sb ~vh:8 ~ch:48;
  equal ~msg:"manual page request disengages sticky following" int 24
    (Scroll_box.scroll_top sb)

let reset_sticky_request_reengages_once_per_key () =
  let acknowledgements = ref [] in
  let _test_context, sb =
    make_scroll_box ~sticky_scroll:true ~sticky_start:`Bottom
      ~on_reset_sticky_applied:(fun ~key ->
        acknowledgements := key :: !acknowledgements)
      ()
  in
  render_sticky_box sb ~vh:8 ~ch:40;
  equal ~msg:"initially follows bottom" int 32 (Scroll_box.scroll_top sb);
  Scroll_box.apply_props sb
    (Scroll_box.Props.make ~sticky_scroll:true ~sticky_start:`Bottom
       ~scroll_by:(scroll_request "page-up" (-1.))
       ());
  render_sticky_box sb ~vh:8 ~ch:40;
  equal ~msg:"page up parks one viewport from the tail" int 24
    (Scroll_box.scroll_top sb);
  render_sticky_box sb ~vh:8 ~ch:48;
  equal ~msg:"append preserves manual reading position" int 24
    (Scroll_box.scroll_top sb);
  Scroll_box.apply_props sb
    (Scroll_box.Props.make ~sticky_scroll:true ~sticky_start:`Bottom
       ~reset_sticky:"turn-2" ());
  render_sticky_box sb ~vh:8 ~ch:48;
  equal ~msg:"new reset key returns to tail" int 40 (Scroll_box.scroll_top sb);
  equal ~msg:"new reset key is acknowledged once" (list string) [ "turn-2" ]
    (List.rev !acknowledgements);
  Scroll_box.set_scroll_top sb 32;
  Scroll_box.apply_props sb
    (Scroll_box.Props.make ~sticky_scroll:true ~sticky_start:`Bottom
       ~background:Ansi.Color.blue ~reset_sticky:"turn-2" ());
  render_sticky_box sb ~vh:8 ~ch:56;
  equal ~msg:"stable reset key preserves later manual scroll" int 32
    (Scroll_box.scroll_top sb);
  equal ~msg:"stable reset key emits no second acknowledgement" (list string)
    [ "turn-2" ]
    (List.rev !acknowledgements);
  Scroll_box.apply_props sb
    (Scroll_box.Props.make ~sticky_scroll:true ~sticky_start:`Bottom
       ~reset_sticky:"turn-3" ());
  render_sticky_box sb ~vh:8 ~ch:56;
  equal ~msg:"changed reset key returns to the new tail" int 48
    (Scroll_box.scroll_top sb);
  equal ~msg:"changed reset key is acknowledged" (list string)
    [ "turn-2"; "turn-3" ]
    (List.rev !acknowledgements)

let reflow_reengages_within_one_row () =
  let _t, sb = make_scroll_box ~sticky_scroll:true ~sticky_start:`Bottom () in
  render_sticky_box sb ~vh:8 ~ch:20;
  equal ~msg:"following at bottom" int 12 (Scroll_box.scroll_top sb);
  (* A one-row scroll up latches the manual flag and parks the box. *)
  Scroll_box.set_scroll_top sb 11;
  render_sticky_box sb ~vh:8 ~ch:20;
  equal ~msg:"steady-state frame keeps the park" int 11
    (Scroll_box.scroll_top sb);
  (* A reflow that moves the edge to within one row re-engages following. *)
  render_sticky_box sb ~vh:10 ~ch:20;
  equal ~msg:"re-pinned to the new edge" int 10 (Scroll_box.scroll_top sb);
  render_sticky_box sb ~vh:10 ~ch:30;
  equal ~msg:"follows appended content again" int 20 (Scroll_box.scroll_top sb)

let reflow_keeps_deep_park () =
  let _t, sb = make_scroll_box ~sticky_scroll:true ~sticky_start:`Bottom () in
  render_sticky_box sb ~vh:8 ~ch:20;
  Scroll_box.set_scroll_top sb 5;
  (* Parked well above the edge: neither the reflow nor later appends may
     steal the reading position. *)
  render_sticky_box sb ~vh:10 ~ch:20;
  equal ~msg:"reflow keeps the park" int 5 (Scroll_box.scroll_top sb);
  render_sticky_box sb ~vh:10 ~ch:30;
  equal ~msg:"appended content keeps the park" int 5 (Scroll_box.scroll_top sb)

(* ── Setters ── *)

let set_background_updates () =
  let _t, sb = make_scroll_box () in
  Scroll_box.set_background sb (Some Ansi.Color.red);
  Scroll_box.set_background sb None;
  ()

let set_on_scroll_replaces () =
  let first_count = ref 0 in
  let second_count = ref 0 in
  let _t, sb =
    make_scroll_box ~on_scroll:(fun ~x:_ ~y:_ -> incr first_count) ()
  in
  Scroll_box.set_on_scroll sb (Some (fun ~x:_ ~y:_ -> incr second_count));
  (* No actual scroll changes, just verify no crash *)
  ()

let set_scroll_accel_replaces () =
  let _t, sb = make_scroll_box () in
  let new_accel = Scroll_box.Scroll_accel.macos ~max_multiplier:3.0 () in
  Scroll_box.set_scroll_accel sb new_accel;
  ()

(* ── apply_props ── *)

let apply_props_updates () =
  let _t, sb = make_scroll_box () in
  let props =
    Scroll_box.Props.make ~sticky_scroll:true ~sticky_start:`Bottom
      ~background:Ansi.Color.blue ()
  in
  Scroll_box.apply_props sb props;
  ()

(* ── Pretty-printing ── *)

let pp_produces_output () =
  let _t, sb = make_scroll_box () in
  let s = Format.asprintf "%a" Scroll_box.pp sb in
  satisfies ~claim:"non-empty" string (fun s -> String.length s > 0) s

let pp_contains_scroll_box () =
  let _t, sb = make_scroll_box () in
  let s = Format.asprintf "%a" Scroll_box.pp sb in
  is_true ~msg:"has ScrollBox prefix"
    (String.length s >= 9 && String.sub s 0 9 = "ScrollBox")

(* ── Runner ── *)

let () =
  run "mosaic.scroll_box"
    [
      group "Scroll_accel"
        [
          test "linear always 1.0" accel_linear_always_one;
          test "macos first tick 1.0" accel_macos_first_tick_one;
          test "macos accelerates on rapid events"
            accel_macos_accelerates_on_rapid_events;
          test "macos resets after timeout" accel_macos_resets_after_timeout;
          test "macos respects cap" accel_macos_respects_cap;
          test "reset clears history" accel_reset_clears_history;
        ];
      group "Props"
        [
          test "default values" props_defaults;
          test "equal on identical" props_equal_identical;
          test "detects scroll_x diff" props_detects_scroll_x_diff;
          test "detects scroll_y diff" props_detects_scroll_y_diff;
          test "detects sticky_scroll diff" props_detects_sticky_scroll_diff;
          test "detects sticky_start diff" props_detects_sticky_start_diff;
          test "detects background diff" props_detects_background_diff;
          test "detects scroll_by diff" props_detects_scroll_by_diff;
          test "detects reset_sticky diff" props_detects_reset_sticky_diff;
          test "rejects invalid scroll_by" props_rejects_invalid_scroll_by;
        ];
      group "Construction"
        [
          test "attaches to parent" create_attaches;
          test "default scroll is 0" create_default_scroll_zero;
          test "child_target set to content" create_child_target_set;
          test "vertical bar attached to root"
            create_vertical_bar_attached_to_root;
          test "viewport has overflow hidden"
            create_viewport_has_overflow_hidden;
          test "vertical bar visible on overflow"
            vertical_bar_visible_when_content_overflows;
        ];
      group "Scroll position"
        [
          test "set_scroll_top clamps" set_scroll_top_clamps;
          test "scroll_to sets both" scroll_to_sets_both;
          test "scroll_by adjusts relative" scroll_by_adjusts_relative;
          test "scroll_by viewport uses measured height"
            scroll_by_viewport_uses_measured_height;
          test "scroll_by key applies at most once"
            scroll_by_key_applies_at_most_once;
          test "scroll_by acknowledges consumption at boundary"
            scroll_by_acknowledges_consumption_at_boundary;
          test "on_scroll fires" on_scroll_fires;
        ];
      group "Mouse wheel"
        [
          test "consumes only when position changes"
            wheel_consumes_only_when_scroll_position_changes;
          test "bubbles without a usable axis" wheel_without_usable_axis_bubbles;
          test "bubbles without overflow" wheel_without_overflow_bubbles;
          test "nested boxes bubble at inner boundary"
            nested_wheel_bubbles_at_inner_boundary;
        ];
      group "Sticky scroll"
        [
          test "set_sticky_scroll updates" set_sticky_scroll_updates;
          test "reset_sticky clears manual" reset_sticky_clears_manual;
          test "scroll_by viewport parks sticky content"
            scroll_by_viewport_parks_sticky_content;
          test "reset_sticky request reengages once per key"
            reset_sticky_request_reengages_once_per_key;
          test "reflow reengages within one row" reflow_reengages_within_one_row;
          test "reflow keeps deep park" reflow_keeps_deep_park;
        ];
      group "Setters"
        [
          test "set_background" set_background_updates;
          test "set_on_scroll replaces" set_on_scroll_replaces;
          test "set_scroll_accel replaces" set_scroll_accel_replaces;
        ];
      group "apply_props" [ test "updates" apply_props_updates ];
      group "Pretty-printing"
        [
          test "produces output" pp_produces_output;
          test "contains ScrollBox" pp_contains_scroll_box;
        ];
    ]
