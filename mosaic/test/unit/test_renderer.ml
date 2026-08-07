open Windtrap
open Mosaic_ui

(* ── Helpers ── *)

let make_renderer () = Renderer.create ()

let do_frame ?(width = 80) ?(height = 24) ?(delta = 16.0) t =
  Renderer.render_frame t ~width ~height ~delta;
  ignore (Renderer.render ~full:true t : string)

let make_child ~parent ~x ~y ~w ~h ?focusable () =
  let lpa = Toffee.Style.Length_percentage_auto.length in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_position Absolute
    |> Toffee.Style.set_inset
         {
           left = lpa (Float.of_int x);
           top = lpa (Float.of_int y);
           right = Toffee.Style.Length_percentage_auto.auto;
           bottom = Toffee.Style.Length_percentage_auto.auto;
         }
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length (Float.of_int w))
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length (Float.of_int h))
  in
  let child = Renderable.create ~parent ~style () in
  (match focusable with
  | Some v -> Renderable.set_focusable child v
  | None -> ());
  child

let mouse_press ?(button = Input.Mouse.Left) ?(modifiers = Input.Modifier.none)
    ~x ~y () =
  Input.Mouse.make ~x ~y ~modifiers (Down { button })

let mouse_release ?(button = Input.Mouse.Left)
    ?(modifiers = Input.Modifier.none) ~x ~y () =
  Input.Mouse.make ~x ~y ~modifiers (Up { button = Some button })

let mouse_motion ?(left = false) ?(modifiers = Input.Modifier.none) ~x ~y () =
  let kind = if left then Input.Mouse.Drag { button = Left } else Move in
  Input.Mouse.make ~x ~y ~modifiers kind

let mouse_scroll ?modifiers ~x ~y direction =
  match Input.mouse_scroll ?modifiers x y direction with
  | Input.Mouse mouse -> mouse
  | _ -> assert false

(* Unit-returning dispatch wrappers: these tests assert via tree state, not
   the event [Renderer.dispatch_mouse]/[dispatch_paste] return. *)
let dispatch_mouse t ev = ignore (Renderer.dispatch_mouse t ev : Event.mouse)

let dispatch_paste t text =
  ignore (Renderer.dispatch_paste t text : Event.paste)

let record_mouse node =
  let log = ref [] in
  Renderable.on_mouse node (fun ev -> log := ev :: !log);
  log

let record_keys node =
  let log = ref [] in
  Renderable.on_key node (fun ev -> log := ev :: !log);
  log

let key_a = Input.Key.of_char 'a'

(* ── Lifecycle ── *)

let creates_with_default_style () =
  let t = make_renderer () in
  let root = Renderer.root t in
  is_true ~msg:"root exists" (not (Renderable.destroyed root));
  let _screen = Renderer.screen t in
  ()

let creates_with_custom_style () =
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 40.)
  in
  let t = Renderer.create ~style () in
  let _root = Renderer.root t in
  ()

let width_method_reaches_widgets () =
  (* Text machinery follows the screen's width method: widgets read it through
     the renderable context when creating text buffers. *)
  let t = Renderer.create ~width_method:`Wcwidth () in
  is_true ~msg:"owned screen's method"
    (Renderable.Private.width_method (Renderer.root t) = `Wcwidth);
  let host = Screen.create ~width_method:`Wcwidth () in
  let adopted = Renderer.create ~screen:host () in
  is_true ~msg:"adopted screen's method"
    (Renderable.Private.width_method (Renderer.root adopted) = `Wcwidth)

let adopted_screen_render_is_guarded () =
  let host = Screen.create () in
  let t = Renderer.create ~screen:host () in
  let _child = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:4 ~h:2 () in
  Renderer.render_frame t ~width:10 ~height:4 ~delta:0.;
  is_true ~msg:"renderer adopted the host screen" (Renderer.screen t == host);
  is_true ~msg:"frame built into the host's hit grid"
    (Screen.Hit_grid.get (Screen.next_hit_grid host) ~x:1 ~y:1 > 0);
  raises_match ~msg:"presentation belongs to the host"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> ignore (Renderer.render t : string))

let zero_sized_clipping_node_hides_children () =
  (* A zero-width overflow-hidden container must report honest bounds and skip
     its children rather than fabricating a phantom 1-cell scissor. *)
  let t = make_renderer () in
  let hidden =
    Toffee.Geometry.Point.
      { x = Toffee.Style.Overflow.Hidden; y = Toffee.Style.Overflow.Hidden }
  in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_overflow hidden
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 0.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 3.)
  in
  let container = Renderable.create ~parent:(Renderer.root t) ~style () in
  let child_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 5.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 1.)
  in
  let child = Renderable.create ~parent:container ~style:child_style () in
  let child_num = Renderable.Private.num child in
  do_frame t;
  equal ~msg:"honest zero width" int 0 (Renderable.width container);
  let hit = Screen.query_hit (Renderer.screen t) ~x:0 ~y:0 in
  is_false ~msg:"child not hit-testable through the empty clip" (hit = child_num)

let starts_dirty () =
  let t = make_renderer () in
  is_true ~msg:"needs_render" (Renderer.needs_render t)

let clean_after_render () =
  let t = make_renderer () in
  do_frame t;
  is_false ~msg:"not needs_render" (Renderer.needs_render t)

let dirty_after_schedule () =
  let t = make_renderer () in
  do_frame t;
  is_false ~msg:"clean before" (Renderer.needs_render t);
  let child = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 () in
  Renderable.request_render child;
  is_true ~msg:"dirty after schedule" (Renderer.needs_render t)

let render_request_during_frame_survives () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 () in
  let requested = ref false in
  Renderer.add_frame_callback t (fun _delta ->
      if not !requested then begin
        requested := true;
        Renderable.request_render child
      end);
  do_frame t;
  is_true ~msg:"follow-up frame requested" (Renderer.needs_render t);
  is_false ~msg:"renderer remains unsettled" (Renderer.is_settled t);
  do_frame t;
  is_false ~msg:"follow-up frame consumed" (Renderer.needs_render t);
  is_true ~msg:"renderer settled" (Renderer.is_settled t)

let callback_runs_each_frame () =
  let t = make_renderer () in
  let count = ref 0 in
  Renderer.add_frame_callback t (fun _delta -> incr count);
  do_frame t;
  equal ~msg:"one frame" int 1 !count;
  do_frame t;
  equal ~msg:"two frames" int 2 !count

let callback_receives_delta () =
  let t = make_renderer () in
  let received = ref 0.0 in
  Renderer.add_frame_callback t (fun delta -> received := delta);
  do_frame ~delta:42.0 t;
  is_true ~msg:"delta" (Float.equal !received 42.0)

let remove_callback_stops_it () =
  let t = make_renderer () in
  let count = ref 0 in
  let f delta =
    ignore (delta : float);
    incr count
  in
  Renderer.add_frame_callback t f;
  do_frame t;
  equal ~msg:"ran once" int 1 !count;
  Renderer.remove_frame_callback t f;
  do_frame t;
  equal ~msg:"not called again" int 1 !count

let clear_removes_all_callbacks () =
  let t = make_renderer () in
  let c1 = ref 0 in
  let c2 = ref 0 in
  Renderer.add_frame_callback t (fun _ -> incr c1);
  Renderer.add_frame_callback t (fun _ -> incr c2);
  do_frame t;
  equal ~msg:"c1 ran" int 1 !c1;
  equal ~msg:"c2 ran" int 1 !c2;
  Renderer.clear_frame_callbacks t;
  do_frame t;
  equal ~msg:"c1 not called" int 1 !c1;
  equal ~msg:"c2 not called" int 1 !c2

(* ── Settlement ── *)

type async_highlight_job = {
  notify : unit -> unit;
  mutable outcome : Code.Highlighter.result option;
}

let async_highlighter () =
  let jobs = ref [] in
  let highlighter =
    Code.Highlighter.async (fun _request ~notify ->
        let job = { notify; outcome = None } in
        jobs := !jobs @ [ job ];
        Code.Highlighter.job
          ~poll:(fun () ->
            match job.outcome with
            | None -> None
            | Some outcome ->
                job.outcome <- None;
                Some outcome)
          ~cancel:(fun () -> ()))
  in
  (jobs, highlighter)

let complete_highlight job outcome =
  job.outcome <- Some outcome;
  job.notify ()

let pending_provider_reports_work () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 () in
  Renderable.set_pending_provider child
    (Some
       (fun () ->
         Some (Renderable.Pending.make ~label:"example" ~kind:"test.work" ())));
  match Renderer.pending_work t with
  | [ pending ] ->
      is_true ~msg:"node" (Renderer.Pending.node pending == child);
      let work = Renderer.Pending.work pending in
      equal ~msg:"kind" string "test.work" work.kind;
      equal ~msg:"label" (option string) (Some "example") work.label
  | pending ->
      fail
        (Printf.sprintf "expected one pending work item, got %d"
           (List.length pending))

let render_frame_until_settled_reports_pending_code () =
  let t = make_renderer () in
  let _jobs, highlighter = async_highlighter () in
  let syntax = Code.with_highlighter ~language:"ocaml" highlighter in
  let _code =
    Code.create ~parent:(Renderer.root t) ~content:"let x" ~syntax ()
  in
  match
    Renderer.render_frame_until_settled ~max_passes:1 t ~width:40 ~height:5
      ~delta:0.
  with
  | `Settled -> fail "expected pending highlighting"
  | `Pending [ pending ] ->
      let work = Renderer.Pending.work pending in
      equal ~msg:"kind" string "code.highlight" work.kind;
      equal ~msg:"label" (option string) (Some "ocaml") work.label
  | `Pending pending ->
      fail
        (Printf.sprintf "expected one pending work item, got %d"
           (List.length pending))

let render_frame_until_settled_completes_code () =
  let t = make_renderer () in
  let jobs, highlighter = async_highlighter () in
  let highlights = Syntax_highlight.of_triples [ (0, 3, "keyword") ] in
  let syntax = Code.with_highlighter ~language:"ocaml" highlighter in
  let code =
    Code.create ~parent:(Renderer.root t) ~content:"let x" ~syntax ()
  in
  complete_highlight (List.hd !jobs) (Ok highlights);
  match Renderer.render_frame_until_settled t ~width:40 ~height:5 ~delta:0. with
  | `Pending pending ->
      fail
        (Printf.sprintf "expected settlement, got %d pending item(s)"
           (List.length pending))
  | `Settled ->
      is_false ~msg:"not highlighting" (Code.is_highlighting code);
      equal ~msg:"pending work" int 0 (List.length (Renderer.pending_work t))

let settlement_replaces_superseded_frame_cells () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:1 ~h:1 () in
  let passes = ref 0 in
  Renderable.set_render child (fun self grid ~delta:_ ->
      incr passes;
      if !passes = 1 then begin
        Grid.draw_text grid ~x:0 ~y:0 ~text:"X";
        Renderable.request_render self
      end);
  (match Renderer.render_frame_until_settled t ~width:1 ~height:1 ~delta:0. with
  | `Settled -> ()
  | `Pending pending ->
      fail
        (Printf.sprintf "expected settlement, got %d pending item(s)"
           (List.length pending)));
  equal ~msg:"render passes" int 2 !passes;
  let grid = Screen.next_grid (Renderer.screen t) in
  equal ~msg:"superseded glyph cleared" string " " (Grid.get_text grid 0)

let post_process_runs () =
  let t = make_renderer () in
  let ran = ref false in
  let _id = Renderer.add_post_process t (fun _grid ~delta:_ -> ran := true) in
  do_frame t;
  is_true ~msg:"post-process ran" !ran

let remove_post_process () =
  let t = make_renderer () in
  let count = ref 0 in
  let f _grid ~delta:_ = incr count in
  let id = Renderer.add_post_process t f in
  do_frame t;
  equal ~msg:"ran once" int 1 !count;
  Renderer.remove_post_process t id;
  do_frame t;
  equal ~msg:"not called again" int 1 !count

(* ── Viewport culling ── *)

let overflow_visible_descendant_reaching_viewport_renders () =
  let t = make_renderer () in
  let parent = make_child ~parent:(Renderer.root t) ~x:80 ~y:0 ~w:10 ~h:5 () in
  let child = make_child ~parent ~x:(-80) ~y:0 ~w:10 ~h:5 () in
  let renders = ref 0 in
  Renderable.set_render child (fun _ _ ~delta:_ -> incr renders);
  do_frame ~width:20 ~height:10 t;
  equal ~msg:"overflow-visible descendant callback" int 1 !renders

let clipped_offscreen_subtree_is_not_traversed_for_rendering () =
  let t = make_renderer () in
  let hidden = Toffee.Style.Overflow.Hidden in
  let parent_style =
    Toffee.Style.default
    |> Toffee.Style.set_position Absolute
    |> Toffee.Style.set_inset
         {
           left = Toffee.Style.Length_percentage_auto.length 80.;
           top = Toffee.Style.Length_percentage_auto.length 0.;
           right = Toffee.Style.Length_percentage_auto.auto;
           bottom = Toffee.Style.Length_percentage_auto.auto;
         }
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 10.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 5.)
    |> Toffee.Style.set_overflow { x = hidden; y = hidden }
  in
  let parent =
    Renderable.create ~parent:(Renderer.root t) ~style:parent_style ()
  in
  let child = make_child ~parent ~x:(-80) ~y:0 ~w:10 ~h:5 () in
  let renders = ref 0 in
  Renderable.set_render child (fun _ _ ~delta:_ -> incr renders);
  do_frame ~width:20 ~height:10 t;
  equal ~msg:"clipped descendant callback" int 0 !renders

(* A scroll box whose rows report render invocations. Rows are 1 cell tall,
   the viewport is [vh] rows, scrollbars are hidden so rows span the full
   width. *)
let make_culled_transcript ?(rows = 40) ?(vw = 20) ?(vh = 5) t =
  let box_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length (Float.of_int vw))
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length (Float.of_int vh))
  in
  let sb =
    Scroll_box.create ~parent:(Renderer.root t) ~style:box_style
      ~show_scrollbars:false ()
  in
  let row_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length (Float.of_int vw))
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 1.)
  in
  let renders = Array.make rows 0 in
  let row_nodes =
    Array.init rows (fun i ->
        let row =
          Renderable.create ~parent:(Scroll_box.node sb) ~style:row_style ()
        in
        Renderable.set_render row (fun _ _ ~delta:_ ->
            renders.(i) <- renders.(i) + 1);
        row)
  in
  (sb, renders, row_nodes)

let scroll_box_culls_offscreen_children () =
  let t = make_renderer () in
  let _sb, renders, _rows = make_culled_transcript t in
  do_frame ~width:20 ~height:5 t;
  equal ~msg:"first visible row rendered" int 1 renders.(0);
  equal ~msg:"last visible row rendered" int 1 renders.(4);
  equal ~msg:"offscreen row not rendered" int 0 renders.(20);
  equal ~msg:"final row not rendered" int 0 renders.(39)

let scrolling_paints_revealed_children () =
  let t = make_renderer () in
  let sb, renders, _rows = make_culled_transcript t in
  do_frame ~width:20 ~height:5 t;
  equal ~msg:"row 20 culled before scroll" int 0 renders.(20);
  Scroll_box.scroll_to sb ~y:20 ();
  do_frame ~width:20 ~height:5 t;
  equal ~msg:"row 20 painted after scroll" int 1 renders.(20);
  equal ~msg:"row 24 painted after scroll" int 1 renders.(24);
  equal ~msg:"row 0 culled after scroll" int 1 renders.(0)

let culled_child_is_not_hit_testable () =
  let t = make_renderer () in
  let sb, _renders, rows = make_culled_transcript t in
  let count_downs log =
    List.length
      (List.filter
         (fun ev ->
           match Event.Mouse.kind ev with
           | Event.Mouse.Down _ -> true
           | _ -> false)
         !log)
  in
  let hits_visible = record_mouse rows.(2) in
  let hits_hidden = record_mouse rows.(22) in
  do_frame ~width:20 ~height:5 t;
  Renderer.dispatch_mouse t (mouse_press ~x:3 ~y:2 ());
  equal ~msg:"visible row hit" int 1 (count_downs hits_visible);
  equal ~msg:"hidden row not hit" int 0 (count_downs hits_hidden);
  Scroll_box.scroll_to sb ~y:20 ();
  do_frame ~width:20 ~height:5 t;
  Renderer.dispatch_mouse t (mouse_press ~x:3 ~y:2 ());
  equal ~msg:"revealed row hit after scroll" int 1 (count_downs hits_hidden)

let culled_live_child_does_not_tick () =
  let t = make_renderer () in
  let sb, _renders, rows = make_culled_transcript t in
  let visible_ticks = ref 0 and hidden_ticks = ref 0 in
  Renderable.set_live rows.(1) true;
  Renderable.set_on_frame rows.(1) (Some (fun _ ~delta:_ -> incr visible_ticks));
  Renderable.set_live rows.(30) true;
  Renderable.set_on_frame rows.(30) (Some (fun _ ~delta:_ -> incr hidden_ticks));
  do_frame ~width:20 ~height:5 t;
  equal ~msg:"visible live row ticked" int 1 !visible_ticks;
  equal ~msg:"culled live row did not tick" int 0 !hidden_ticks;
  Scroll_box.scroll_to sb ~y:30 ();
  do_frame ~width:20 ~height:5 t;
  equal ~msg:"revealed live row ticks" int 1 !hidden_ticks

(* ── Layout dirtiness ──

   Layout computation is skipped on clean frames; each mutation kind must
   re-enable it. Every test renders two frames first so the skip is armed
   before the mutation. *)

let sized_child ~parent w h =
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length (Float.of_int w))
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length (Float.of_int h))
  in
  Renderable.create ~parent ~style ()

let style_change_relayouts_after_clean_frame () =
  let t = make_renderer () in
  let a = sized_child ~parent:(Renderer.root t) 10 2 in
  let b = sized_child ~parent:(Renderer.root t) 5 2 in
  do_frame t;
  do_frame t;
  equal ~msg:"b after a" int 10 (Renderable.x b);
  let style =
    Renderable.style a
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 15.)
  in
  Renderable.set_style a style;
  do_frame t;
  equal ~msg:"a resized" int 15 (Renderable.width a);
  equal ~msg:"b moved" int 15 (Renderable.x b)

let measure_change_relayouts_after_clean_frame () =
  let t = make_renderer () in
  let child = Renderable.create ~parent:(Renderer.root t) () in
  let w = ref 10. in
  Renderable.set_measure child
    (Some
       (fun ~known_dimensions:_ ~available_space:_ ~style:_ ->
         Toffee.Geometry.Size.{ width = !w; height = 1. }));
  do_frame t;
  do_frame t;
  equal ~msg:"measured width" int 10 (Renderable.width child);
  w := 14.;
  (* Content-driven size changes invalidate via mark_dirty, as
     text surfaces do. *)
  Renderable.mark_dirty child;
  do_frame t;
  equal ~msg:"remeasured width" int 14 (Renderable.width child)

let add_remove_child_relayouts_after_clean_frame () =
  let t = make_renderer () in
  let a = sized_child ~parent:(Renderer.root t) 10 2 in
  do_frame t;
  do_frame t;
  let b = sized_child ~parent:(Renderer.root t) 5 2 in
  do_frame t;
  equal ~msg:"new child laid out" int 10 (Renderable.x b);
  Renderable.detach a;
  do_frame t;
  equal ~msg:"sibling reflows on removal" int 0 (Renderable.x b)

let resize_relayouts_after_clean_frame () =
  let t = make_renderer () in
  do_frame ~width:80 ~height:24 t;
  do_frame ~width:80 ~height:24 t;
  equal ~msg:"root at 80" int 80 (Renderable.width (Renderer.root t));
  do_frame ~width:100 ~height:30 t;
  equal ~msg:"root at 100" int 100 (Renderable.width (Renderer.root t))

let visibility_toggle_relayouts_after_clean_frame () =
  let t = make_renderer () in
  let a = sized_child ~parent:(Renderer.root t) 10 2 in
  let b = sized_child ~parent:(Renderer.root t) 5 2 in
  do_frame t;
  do_frame t;
  equal ~msg:"b after a" int 10 (Renderable.x b);
  Renderable.set_visible a false;
  do_frame t;
  equal ~msg:"b moves to front" int 0 (Renderable.x b)

(* ── Focus ── *)

let focus_returns_true_for_focusable () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  is_true ~msg:"focus ok" (Renderer.focus t child)

let focus_returns_false_for_non_focusable () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 () in
  is_false ~msg:"not focusable" (Renderer.focus t child)

let focused_returns_focused_node () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  ignore (Renderer.focus t child : bool);
  match Renderer.focused t with
  | Some n -> is_true ~msg:"same node" (n == child)
  | None -> fail "expected focused node"

let blur_clears_focus () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  ignore (Renderer.focus t child : bool);
  Renderer.blur t;
  is_none ~msg:"no focus" (Renderer.focused t)

let focus_different_node_blurs_previous () =
  let t = make_renderer () in
  let a =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let b =
    make_child ~parent:(Renderer.root t) ~x:10 ~y:0 ~w:10 ~h:5 ~focusable:true
      ()
  in
  ignore (Renderer.focus t a : bool);
  ignore (Renderer.focus t b : bool);
  is_false ~msg:"a blurred" (Renderable.focused a);
  is_true ~msg:"b focused" (Renderable.focused b)

let left_click_focuses_focusable_node () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 ~focusable:true ()
  in
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  match Renderer.focused t with
  | Some n -> is_true ~msg:"child focused" (n == child)
  | None -> fail "expected focus after click"

let left_click_on_non_focusable_does_not_focus () =
  let t = make_renderer () in
  let _child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  is_none ~msg:"no focus" (Renderer.focused t)

let click_walks_up_to_focusable_parent () =
  let t = make_renderer () in
  let parent =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:20 ~h:20 ~focusable:true
      ()
  in
  let _child = make_child ~parent ~x:2 ~y:2 ~w:5 ~h:5 () in
  do_frame t;
  dispatch_mouse t (mouse_press ~x:3 ~y:3 ());
  match Renderer.focused t with
  | Some n -> is_true ~msg:"parent focused" (n == parent)
  | None -> fail "expected parent to be focused"

let left_click_focuses_selectable_node () =
  (* Starting a text selection consumes the Down event; auto-focus must still
     run, mirroring OpenTUI's dispatchMouseEvent. *)
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 ~focusable:true ()
  in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  is_some ~msg:"selection started" (Renderer.selection t);
  match Renderer.focused t with
  | Some n -> is_true ~msg:"selectable child focused" (n == child)
  | None -> fail "expected focus after click on selectable node"

let right_click_does_not_auto_focus () =
  let t = make_renderer () in
  let _child =
    make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 ~focusable:true ()
  in
  do_frame t;
  dispatch_mouse t (mouse_press ~button:Right ~x:7 ~y:7 ());
  is_none ~msg:"no focus from right click" (Renderer.focused t)

let shift_left_click_preserves_existing_focus () =
  let t = make_renderer () in
  let composer =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let _target =
    make_child ~parent:(Renderer.root t) ~x:20 ~y:0 ~w:10 ~h:5 ~focusable:true
      ()
  in
  do_frame t;
  ignore (Renderer.focus t composer : bool);
  let modifiers = { Input.Modifier.none with shift = true } in
  dispatch_mouse t (mouse_press ~modifiers ~x:25 ~y:2 ());
  match Renderer.focused t with
  | Some n -> is_true ~msg:"composer remains focused" (n == composer)
  | None -> fail "expected focus to be preserved"

let prevent_default_mouse_down_blocks_auto_focus () =
  let t = make_renderer () in
  let composer =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let target =
    make_child ~parent:(Renderer.root t) ~x:20 ~y:0 ~w:10 ~h:5 ~focusable:true
      ()
  in
  Renderable.on_mouse target (fun ev ->
      match Event.Mouse.kind ev with
      | Event.Mouse.Down { button = Left } -> Event.Mouse.prevent_default ev
      | _ -> ());
  do_frame t;
  ignore (Renderer.focus t composer : bool);
  dispatch_mouse t (mouse_press ~x:25 ~y:2 ());
  match Renderer.focused t with
  | Some n -> is_true ~msg:"composer remains focused" (n == composer)
  | None -> fail "expected focus to be preserved"

let focus_fires_on_node () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  ignore (Renderer.focus t child : bool);
  is_true ~msg:"child focused" (Renderable.focused child)

let blur_fires_on_previous () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  ignore (Renderer.focus t child : bool);
  Renderer.blur t;
  is_false ~msg:"child blurred" (Renderable.focused child)

let refocus_fires_blur_then_focus () =
  let t = make_renderer () in
  let a =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let b =
    make_child ~parent:(Renderer.root t) ~x:10 ~y:0 ~w:10 ~h:5 ~focusable:true
      ()
  in
  ignore (Renderer.focus t a : bool);
  is_true ~msg:"a focused" (Renderable.focused a);
  ignore (Renderer.focus t b : bool);
  is_false ~msg:"a blurred" (Renderable.focused a);
  is_true ~msg:"b focused" (Renderable.focused b)

(* ── Input ── *)

let key_dispatches_to_focused () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let log = record_keys child in
  ignore (Renderer.focus t child : bool);
  ignore (Renderer.dispatch_key t key_a : Event.key);
  equal ~msg:"received one key" int 1 (List.length !log)

let key_does_nothing_when_no_focus () =
  let t = make_renderer () in
  ignore (Renderer.dispatch_key t key_a : Event.key)

let key_multiple_handlers_run () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let h1 = ref 0 in
  let h2 = ref 0 in
  Renderable.on_key child (fun _ev -> incr h1);
  Renderable.on_key child (fun _ev -> incr h2);
  ignore (Renderer.focus t child : bool);
  ignore (Renderer.dispatch_key t key_a : Event.key);
  equal ~msg:"handler 1 ran" int 1 !h1;
  equal ~msg:"handler 2 ran" int 1 !h2

let prevent_default_stops_default_handler () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let default_ran = ref false in
  Renderable.on_key child (fun ev -> Event.Key.prevent_default ev);
  Renderable.set_default_key_handler child
    (Some (fun _ev -> default_ran := true));
  ignore (Renderer.focus t child : bool);
  ignore (Renderer.dispatch_key t key_a : Event.key);
  is_false ~msg:"default not called" !default_ran

let paste_dispatches_to_focused () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let received = ref None in
  Renderable.set_paste_handler child
    (Some (fun ev -> received := Some (Event.Paste.text ev)));
  ignore (Renderer.focus t child : bool);
  dispatch_paste t "hello";
  match !received with
  | Some s -> equal ~msg:"text" string "hello" s
  | None -> fail "expected paste event"

let paste_does_nothing_when_no_focus () =
  let t = make_renderer () in
  dispatch_paste t "hello"

let paste_contains_correct_text () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let received = ref "" in
  Renderable.set_paste_handler child
    (Some (fun ev -> received := Event.Paste.text ev));
  ignore (Renderer.focus t child : bool);
  dispatch_paste t "world\n123";
  equal ~msg:"text" string "world\n123" !received

(* ── Mouse ── *)

let click_dispatches_to_positioned_child () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  is_true ~msg:"received event" (List.length !log > 0);
  let ev = List.hd (List.rev !log) in
  match Event.Mouse.kind ev with
  | Event.Mouse.Down { button = Left } -> ()
  | _ -> fail "expected Down Left"

let click_on_empty_area_does_not_crash () =
  let t = make_renderer () in
  let _child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  (* Click outside all children — should not crash *)
  dispatch_mouse t (mouse_press ~x:0 ~y:0 ())

let overlapping_children_last_child_wins () =
  let t = make_renderer () in
  let a = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:20 ~h:20 () in
  let b = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:20 ~h:20 () in
  let log_a = record_mouse a in
  let log_b = record_mouse b in
  do_frame t;
  dispatch_mouse t (mouse_press ~x:5 ~y:5 ());
  is_true ~msg:"b received" (List.length !log_b > 0);
  (* a should not receive the event directly — it may get it via bubbling from
     b's event dispatch, but the target hit should be b *)
  let _ = log_a in
  ()

let click_coordinates_are_absolute () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:8 ());
  is_true ~msg:"has events" (List.length !log > 0);
  let ev = List.hd (List.rev !log) in
  equal ~msg:"x" int 7 (Event.Mouse.x ev);
  equal ~msg:"y" int 8 (Event.Mouse.y ev)

let press_and_release_sequence () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  dispatch_mouse t (mouse_release ~x:7 ~y:7 ());
  equal ~msg:"two events" int 2 (List.length !log);
  let events = List.rev !log in
  (match Event.Mouse.kind (List.nth events 0) with
  | Event.Mouse.Down { button = Left } -> ()
  | _ -> fail "expected Down");
  match Event.Mouse.kind (List.nth events 1) with
  | Event.Mouse.Up { button = Left; _ } -> ()
  | _ -> fail "expected Up"

let move_fires_move_event () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  do_frame t;
  dispatch_mouse t (mouse_motion ~x:7 ~y:7 ());
  let found_move =
    List.exists
      (fun ev -> match Event.Mouse.kind ev with Move -> true | _ -> false)
      !log
  in
  is_true ~msg:"got Move" found_move

let mouse_event_has_correct_modifiers () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  let mods = { Input.Modifier.none with shift = true } in
  do_frame t;
  dispatch_mouse t (mouse_press ~modifiers:mods ~x:7 ~y:7 ());
  is_true ~msg:"received" (List.length !log > 0);
  let ev = List.hd !log in
  is_true ~msg:"shift" (Event.Mouse.modifiers ev).shift;
  log := [];
  dispatch_mouse t (mouse_motion ~modifiers:mods ~x:7 ~y:7 ());
  is_true ~msg:"motion received" (List.length !log > 0);
  let ev = List.hd !log in
  is_true ~msg:"motion shift" (Event.Mouse.modifiers ev).shift

let move_over_child_fires_over () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  do_frame t;
  dispatch_mouse t (mouse_motion ~x:7 ~y:7 ());
  let found_over =
    List.exists
      (fun ev -> match Event.Mouse.kind ev with Over _ -> true | _ -> false)
      !log
  in
  is_true ~msg:"got Over" found_over

let move_away_fires_out_then_over () =
  let t = make_renderer () in
  let a = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:10 () in
  let b = make_child ~parent:(Renderer.root t) ~x:20 ~y:0 ~w:10 ~h:10 () in
  let log_a = record_mouse a in
  let log_b = record_mouse b in
  do_frame t;
  (* Move to a *)
  dispatch_mouse t (mouse_motion ~x:5 ~y:5 ());
  (* Move to b *)
  dispatch_mouse t (mouse_motion ~x:25 ~y:5 ());
  let a_got_out =
    List.exists
      (fun ev -> match Event.Mouse.kind ev with Out -> true | _ -> false)
      !log_a
  in
  let b_got_over =
    List.exists
      (fun ev -> match Event.Mouse.kind ev with Over _ -> true | _ -> false)
      !log_b
  in
  is_true ~msg:"a got Out" a_got_out;
  is_true ~msg:"b got Over" b_got_over

let move_within_same_child_no_over_out () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  dispatch_mouse t (mouse_motion ~x:7 ~y:7 ());
  let log = record_mouse child in
  dispatch_mouse t (mouse_motion ~x:8 ~y:8 ());
  let got_over_or_out =
    List.exists
      (fun ev ->
        match Event.Mouse.kind ev with Over _ | Out -> true | _ -> false)
      !log
  in
  is_false ~msg:"no Over/Out" got_over_or_out

let hover_returns_hovered_node () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  dispatch_mouse t (mouse_motion ~x:7 ~y:7 ());
  match Renderer.hover t with
  | Some n -> is_true ~msg:"hover is child" (n == child)
  | None -> fail "expected hover node"

let hover_none_when_pointer_leaves () =
  let t = make_renderer () in
  let _child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  dispatch_mouse t (mouse_motion ~x:7 ~y:7 ());
  is_some ~msg:"hovering" (Renderer.hover t);
  (* Move outside all children — root hover is technically not None since root
     covers everything, but hover tracks specific children. The hover node will
     be whatever the hit grid returns. Let's just move to a corner that the
     child doesn't cover. *)
  dispatch_mouse t (mouse_motion ~x:0 ~y:0 ());
  (* Hover should change from the child *)
  match Renderer.hover t with
  | Some n -> is_true ~msg:"not the child" (not (n == _child))
  | None -> ()

let scroll_dispatches_to_hit_target () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  do_frame t;
  dispatch_mouse t (mouse_scroll ~x:7 ~y:7 Input.Mouse.Scroll_up);
  let found_scroll =
    List.exists
      (fun ev -> match Event.Mouse.kind ev with Scroll _ -> true | _ -> false)
      !log
  in
  is_true ~msg:"got Scroll" found_scroll

let scroll_with_modifiers () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  let mods = { Input.Modifier.none with shift = true } in
  do_frame t;
  dispatch_mouse t
    (mouse_scroll ~modifiers:mods ~x:7 ~y:7 Input.Mouse.Scroll_down);
  is_true ~msg:"received" (List.length !log > 0);
  let ev = List.hd !log in
  is_true ~msg:"shift" (Event.Mouse.modifiers ev).shift

let scroll_on_empty_area () =
  let t = make_renderer () in
  let _child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  (* Should not crash *)
  dispatch_mouse t (mouse_scroll ~x:0 ~y:0 Input.Mouse.Scroll_up)

(* ── Drag ── *)

let drag_sets_captured () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  (* Drag = motion with left button pressed *)
  dispatch_mouse t (mouse_motion ~left:true ~x:7 ~y:7 ());
  match Renderer.captured t with
  | Some n -> is_true ~msg:"captured child" (n == child)
  | None -> fail "expected drag capture"

let captured_receives_all_mouse_events () =
  let t = make_renderer () in
  let a = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:10 () in
  let b = make_child ~parent:(Renderer.root t) ~x:20 ~y:0 ~w:10 ~h:10 () in
  let log_a = record_mouse a in
  do_frame t;
  (* Start drag on a *)
  dispatch_mouse t (mouse_motion ~left:true ~x:5 ~y:5 ());
  is_some ~msg:"captured" (Renderer.captured t);
  let count_before = List.length !log_a in
  (* Move to b's area — a should still receive *)
  dispatch_mouse t (mouse_motion ~left:true ~x:25 ~y:5 ());
  is_true ~msg:"a received more" (List.length !log_a > count_before);
  ignore b

let release_clears_capture () =
  let t = make_renderer () in
  let _child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  dispatch_mouse t (mouse_motion ~left:true ~x:7 ~y:7 ());
  is_some ~msg:"captured" (Renderer.captured t);
  dispatch_mouse t (mouse_release ~x:7 ~y:7 ());
  is_none ~msg:"released" (Renderer.captured t)

let drag_end_fires_on_release () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  do_frame t;
  dispatch_mouse t (mouse_motion ~left:true ~x:7 ~y:7 ());
  dispatch_mouse t (mouse_release ~x:7 ~y:7 ());
  let found_drag_end =
    List.exists
      (fun ev ->
        match Event.Mouse.kind ev with Drag_end _ -> true | _ -> false)
      !log
  in
  is_true ~msg:"got Drag_end" found_drag_end

let drop_fires_on_target () =
  let t = make_renderer () in
  let a = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:10 () in
  let b = make_child ~parent:(Renderer.root t) ~x:20 ~y:0 ~w:10 ~h:10 () in
  let log_b = record_mouse b in
  do_frame t;
  (* Start drag on a *)
  dispatch_mouse t (mouse_motion ~left:true ~x:5 ~y:5 ());
  is_some ~msg:"captured a" (Renderer.captured t);
  ignore a;
  (* Release over b *)
  dispatch_mouse t (mouse_release ~x:25 ~y:5 ());
  let found_drop =
    List.exists
      (fun ev -> match Event.Mouse.kind ev with Drop _ -> true | _ -> false)
      !log_b
  in
  is_true ~msg:"b got Drop" found_drop

let drop_source_identifies_origin () =
  let t = make_renderer () in
  let a = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:10 () in
  let b = make_child ~parent:(Renderer.root t) ~x:20 ~y:0 ~w:10 ~h:10 () in
  let log_b = record_mouse b in
  do_frame t;
  dispatch_mouse t (mouse_motion ~left:true ~x:5 ~y:5 ());
  let a_num = Renderable.Private.num a in
  dispatch_mouse t (mouse_release ~x:25 ~y:5 ());
  let drop_source =
    List.find_map
      (fun ev ->
        match Event.Mouse.kind ev with
        | Drop { source; _ } -> source
        | _ -> None)
      !log_b
  in
  match drop_source with
  | Some src -> equal ~msg:"source" int a_num src
  | None -> fail "expected Drop with source"

let right_button_does_not_capture () =
  let t = make_renderer () in
  let _child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  (* Right button motion *)
  dispatch_mouse t
    (Input.Mouse.make ~x:7 ~y:7 ~modifiers:Input.Modifier.none
       (Drag { button = Right }));
  is_none ~msg:"no capture from right" (Renderer.captured t)

(* ── Selection ── *)

let selection_starts_none () =
  let t = make_renderer () in
  is_none ~msg:"no selection" (Renderer.selection t)

let click_on_selectable_starts_selection () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  is_some ~msg:"selection started" (Renderer.selection t)

let drag_updates_selection_focus () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  is_some ~msg:"selection active" (Renderer.selection t);
  dispatch_mouse t (mouse_motion ~left:true ~x:12 ~y:8 ());
  match Renderer.selection t with
  | Some sel ->
      let f = Selection.focus sel in
      equal ~msg:"focus x" int 12 f.x;
      equal ~msg:"focus y" int 8 f.y
  | None -> fail "expected active selection"

let release_finishes_selection () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  dispatch_mouse t (mouse_motion ~left:true ~x:12 ~y:8 ());
  dispatch_mouse t (mouse_release ~x:12 ~y:8 ());
  match Renderer.selection t with
  | Some sel -> is_false ~msg:"not dragging" (Selection.is_dragging sel)
  | None -> fail "expected selection"

let clear_selection_resets () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  is_some ~msg:"selection exists" (Renderer.selection t);
  Renderer.clear_selection t;
  is_none ~msg:"cleared" (Renderer.selection t)

let scroll_on_dead_space_reaches_focused_scrollable () =
  let t = make_renderer () in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 10.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 3.)
  in
  let sb = Scroll_box.create ~parent:(Renderer.root t) ~style () in
  let content_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 8.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 30.)
  in
  let _content =
    Renderable.create ~parent:(Scroll_box.node sb) ~style:content_style ()
  in
  do_frame t;
  ignore (Renderer.focus t (Scroll_box.node sb) : bool);
  dispatch_mouse t (mouse_scroll ~x:30 ~y:8 Input.Mouse.Scroll_down);
  is_true ~msg:"focused scroll box scrolled" (Scroll_box.scroll_top sb > 0)

let selection_drag_auto_scrolls_scroll_box () =
  let t = make_renderer () in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 20.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 8.)
  in
  let sb = Scroll_box.create ~parent:(Renderer.root t) ~style () in
  let content_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 18.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 30.)
  in
  let content =
    Renderable.create ~parent:(Scroll_box.node sb) ~style:content_style ()
  in
  let changes = ref 0 in
  Renderable.set_selection content
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ ->
      incr changes;
      true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame ~width:20 ~height:8 t;
  dispatch_mouse t (mouse_press ~x:1 ~y:1 ());
  dispatch_mouse t (mouse_motion ~left:true ~x:1 ~y:7 ());
  do_frame ~width:20 ~height:8 ~delta:1000. t;
  is_true ~msg:"scroll box moved down" (Scroll_box.scroll_top sb > 0);
  is_true ~msg:"selection was refreshed" (!changes > 2)

let selection_drag_auto_scrolls_scroll_box_when_pointer_leaves_it () =
  let t = make_renderer () in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 20.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 8.)
  in
  let sb = Scroll_box.create ~parent:(Renderer.root t) ~style () in
  let content_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 18.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 30.)
  in
  let content =
    Renderable.create ~parent:(Scroll_box.node sb) ~style:content_style ()
  in
  Renderable.set_selection content
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame ~width:20 ~height:12 t;
  dispatch_mouse t (mouse_press ~x:1 ~y:1 ());
  dispatch_mouse t (mouse_motion ~left:true ~x:1 ~y:10 ());
  do_frame ~width:20 ~height:12 ~delta:1000. t;
  is_true ~msg:"scroll box moved down" (Scroll_box.scroll_top sb > 0)

let selection_release_after_leaving_scroll_box_goes_to_anchor_ancestors () =
  let t = make_renderer () in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 20.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 8.)
  in
  let sb = Scroll_box.create ~parent:(Renderer.root t) ~style () in
  let content_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 18.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 30.)
  in
  let content =
    Renderable.create ~parent:(Scroll_box.node sb) ~style:content_style ()
  in
  let released = ref false in
  Renderable.on_mouse (Scroll_box.node sb) (fun event ->
      match Event.Mouse.kind event with
      | Up { button = Left; is_dragging = true } -> released := true
      | _ -> ());
  Renderable.set_selection content
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame ~width:20 ~height:12 t;
  dispatch_mouse t (mouse_press ~x:1 ~y:1 ());
  dispatch_mouse t (mouse_motion ~left:true ~x:1 ~y:10 ());
  dispatch_mouse t (mouse_release ~x:1 ~y:10 ());
  is_true ~msg:"release bubbled through scroll box" !released

let selectable_child_receives_changed () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let changed = ref false in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _sel ->
      changed := true;
      true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  is_true ~msg:"on_change called" !changed

let clear_notifies_selectables () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let clear_called = ref false in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> clear_called := true)
    ~get_text:(fun () -> "");
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  Renderer.clear_selection t;
  is_true ~msg:"clear called" !clear_called

let click_on_non_selectable_clears_selection () =
  let t = make_renderer () in
  let selectable =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:10 ()
  in
  let non_selectable =
    make_child ~parent:(Renderer.root t) ~x:20 ~y:0 ~w:10 ~h:10 ()
  in
  Renderable.set_selection selectable
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame t;
  dispatch_mouse t (mouse_press ~x:5 ~y:5 ());
  is_some ~msg:"selection active" (Renderer.selection t);
  ignore non_selectable;
  (* Click on non-selectable area *)
  dispatch_mouse t (mouse_press ~x:25 ~y:5 ());
  is_none ~msg:"selection cleared" (Renderer.selection t)

(* ── Run ── *)

let () =
  run "mosaic.renderer"
    [
      group "Lifecycle"
        [
          test "creates with default style" creates_with_default_style;
          test "creates with custom style" creates_with_custom_style;
          test "width method reaches widgets" width_method_reaches_widgets;
          test "adopted screen render is guarded"
            adopted_screen_render_is_guarded;
          test "zero-sized clipping node hides children"
            zero_sized_clipping_node_hides_children;
          test "starts dirty" starts_dirty;
          test "clean after render" clean_after_render;
          test "dirty after schedule" dirty_after_schedule;
          test "render request during frame survives"
            render_request_during_frame_survives;
          test "callback runs each frame" callback_runs_each_frame;
          test "callback receives delta" callback_receives_delta;
          test "remove callback stops it" remove_callback_stops_it;
          test "clear removes all callbacks" clear_removes_all_callbacks;
          test "post-process runs" post_process_runs;
          test "remove post-process" remove_post_process;
        ];
      group "Viewport culling"
        [
          test "overflow-visible descendant reaching viewport renders"
            overflow_visible_descendant_reaching_viewport_renders;
          test "clipped offscreen subtree is not traversed for rendering"
            clipped_offscreen_subtree_is_not_traversed_for_rendering;
          test "scroll box culls offscreen children"
            scroll_box_culls_offscreen_children;
          test "scrolling paints revealed children"
            scrolling_paints_revealed_children;
          test "culled child is not hit-testable"
            culled_child_is_not_hit_testable;
          test "culled live child does not tick" culled_live_child_does_not_tick;
        ];
      group "Layout dirtiness"
        [
          test "style change relayouts after clean frame"
            style_change_relayouts_after_clean_frame;
          test "measure change relayouts after clean frame"
            measure_change_relayouts_after_clean_frame;
          test "add/remove child relayouts after clean frame"
            add_remove_child_relayouts_after_clean_frame;
          test "resize relayouts after clean frame"
            resize_relayouts_after_clean_frame;
          test "visibility toggle relayouts after clean frame"
            visibility_toggle_relayouts_after_clean_frame;
        ];
      group "Settlement"
        [
          test "pending provider reports work" pending_provider_reports_work;
          test "render_frame_until_settled reports pending code"
            render_frame_until_settled_reports_pending_code;
          test "render_frame_until_settled completes code"
            render_frame_until_settled_completes_code;
          test "settlement replaces superseded frame cells"
            settlement_replaces_superseded_frame_cells;
        ];
      group "Focus"
        [
          test "focus returns true for focusable"
            focus_returns_true_for_focusable;
          test "focus returns false for non-focusable"
            focus_returns_false_for_non_focusable;
          test "focused returns focused node" focused_returns_focused_node;
          test "blur clears focus" blur_clears_focus;
          test "focus different node blurs previous"
            focus_different_node_blurs_previous;
          test "left click focuses focusable node"
            left_click_focuses_focusable_node;
          test "left click on non-focusable does not focus"
            left_click_on_non_focusable_does_not_focus;
          test "click walks up to focusable parent"
            click_walks_up_to_focusable_parent;
          test "left click focuses selectable node"
            left_click_focuses_selectable_node;
          test "right click does not auto-focus" right_click_does_not_auto_focus;
          test "shift left click preserves existing focus"
            shift_left_click_preserves_existing_focus;
          test "prevent default mouse down blocks auto-focus"
            prevent_default_mouse_down_blocks_auto_focus;
          test "focus fires on node" focus_fires_on_node;
          test "blur fires on previous" blur_fires_on_previous;
          test "refocus fires blur then focus" refocus_fires_blur_then_focus;
        ];
      group "Input"
        [
          test "key dispatches to focused" key_dispatches_to_focused;
          test "key does nothing when no focus" key_does_nothing_when_no_focus;
          test "key multiple handlers run" key_multiple_handlers_run;
          test "prevent default stops default handler"
            prevent_default_stops_default_handler;
          test "paste dispatches to focused" paste_dispatches_to_focused;
          test "paste does nothing when no focus"
            paste_does_nothing_when_no_focus;
          test "paste contains correct text" paste_contains_correct_text;
        ];
      group "Mouse"
        [
          test "click dispatches to positioned child"
            click_dispatches_to_positioned_child;
          test "click on empty area does not crash"
            click_on_empty_area_does_not_crash;
          test "overlapping children last child wins"
            overlapping_children_last_child_wins;
          test "click coordinates are absolute" click_coordinates_are_absolute;
          test "press and release sequence" press_and_release_sequence;
          test "move fires Move event" move_fires_move_event;
          test "mouse event has correct modifiers"
            mouse_event_has_correct_modifiers;
          test "move over child fires Over" move_over_child_fires_over;
          test "move away fires Out then Over" move_away_fires_out_then_over;
          test "move within same child no Over/Out"
            move_within_same_child_no_over_out;
          test "hover returns hovered node" hover_returns_hovered_node;
          test "hover None when pointer leaves" hover_none_when_pointer_leaves;
          test "scroll dispatches to hit target" scroll_dispatches_to_hit_target;
          test "scroll with modifiers" scroll_with_modifiers;
          test "scroll on empty area" scroll_on_empty_area;
          test "scroll on dead space reaches focused scrollable"
            scroll_on_dead_space_reaches_focused_scrollable;
        ];
      group "Drag"
        [
          test "drag sets captured" drag_sets_captured;
          test "captured receives all mouse events"
            captured_receives_all_mouse_events;
          test "release clears capture" release_clears_capture;
          test "drag-end fires on release" drag_end_fires_on_release;
          test "drop fires on target" drop_fires_on_target;
          test "drop source identifies origin" drop_source_identifies_origin;
          test "right button does not capture" right_button_does_not_capture;
        ];
      group "Selection"
        [
          test "selection starts None" selection_starts_none;
          test "click on selectable starts selection"
            click_on_selectable_starts_selection;
          test "drag updates selection focus" drag_updates_selection_focus;
          test "release finishes selection" release_finishes_selection;
          test "clear selection resets" clear_selection_resets;
          test "selection drag auto-scrolls scroll box"
            selection_drag_auto_scrolls_scroll_box;
          test "selection drag auto-scrolls scroll box after leaving it"
            selection_drag_auto_scrolls_scroll_box_when_pointer_leaves_it;
          test
            "selection release after leaving scroll box goes to anchor \
             ancestors"
            selection_release_after_leaving_scroll_box_goes_to_anchor_ancestors;
          test "selectable child receives changed"
            selectable_child_receives_changed;
          test "clear notifies selectables" clear_notifies_selectables;
          test "click on non-selectable clears selection"
            click_on_non_selectable_clears_selection;
        ];
    ]
