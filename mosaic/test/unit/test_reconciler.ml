open Mosaic_ui
open Mosaic
open Windtrap

(* ── Helpers ── *)

let make () =
  let renderer = Renderer.create () in
  let container = Renderer.root renderer in
  let reconciler = Reconciler.create ~container in
  (renderer, reconciler)

let render_view ?(viewport_width = 40) reconciler vnode =
  Reconciler.render reconciler ~viewport_width vnode

let do_frame ?(width = 40) ?(height = 20) renderer =
  Renderer.render_frame renderer ~width ~height ~delta:0.

let children_of renderer = Renderable.children (Renderer.root renderer)
let child_count renderer = List.length (children_of renderer)

(* ── Construction ── *)

let create_returns_container () =
  let renderer, reconciler = make () in
  is_true ~msg:"container is root"
    (Reconciler.container reconciler == Renderer.root renderer)

let render_rejects_nonpositive_viewport_width () =
  let _renderer, reconciler = make () in
  raises_match ~msg:"positive viewport width"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> Reconciler.render reconciler ~viewport_width:0 (Vnode.box []))

(* ── Basic Rendering ── *)

let render_single_box () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box []);
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer)

let render_single_text () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.text "hello");
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer)

let render_empty () =
  let renderer, reconciler = make () in
  render_view reconciler Vnode.empty;
  do_frame renderer;
  equal ~msg:"no children" int 0 (child_count renderer)

let render_fragment () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.fragment [ Vnode.box []; Vnode.text "hi" ]);
  do_frame renderer;
  equal ~msg:"two children" int 2 (child_count renderer)

let render_nested_fragments () =
  let renderer, reconciler = make () in
  render_view reconciler
    (Vnode.fragment
       [ Vnode.fragment [ Vnode.text "a"; Vnode.text "b" ]; Vnode.text "c" ]);
  do_frame renderer;
  equal ~msg:"three children" int 3 (child_count renderer)

let viewport_switch_selects_only_matching_branch () =
  let renderer, reconciler = make () in
  let narrow_node = ref None in
  let wide_node = ref None in
  let view =
    Vnode.viewport_switch ~at_least_width:80
      ~wide:(Vnode.spinner ~ref:(fun node -> wide_node := Some node) ())
      ~narrow:
        (Vnode.input ~autofocus:true
           ~ref:(fun node -> narrow_node := Some node)
           ())
  in
  render_view ~viewport_width:79 reconciler view;
  do_frame ~width:79 renderer;
  is_some ~msg:"narrow branch mounted" !narrow_node;
  is_none ~msg:"wide branch not mounted" !wide_node;
  equal ~msg:"hidden live branch inactive" int 0
    (Renderable.Private.live_count (Renderer.root renderer));
  match (!narrow_node, Renderer.focused renderer) with
  | Some narrow, Some focused ->
      is_true ~msg:"selected branch receives autofocus" (narrow == focused)
  | _ -> fail "expected the narrow input to be focused"

let viewport_switch_reconciles_selected_branch_on_resize () =
  let renderer, reconciler = make () in
  let last_scroll = ref None in
  let scroll_style =
    Toffee.Style.default
    |> Toffee.Style.set_size (Vnode.size ~width:10 ~height:4)
  in
  let column_style =
    Toffee.Style.default
    |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
  in
  let content =
    Vnode.box ~key:"transcript-content"
      ~style:(column_style |> Toffee.Style.set_flex_shrink 0.)
      (List.init 20 (fun index -> Vnode.text (string_of_int index)))
  in
  let transcript =
    Vnode.scroll_box ~key:"transcript" ~style:scroll_style ~focusable:false
      ~show_scrollbars:false
      ~on_scroll:(fun ~x:_ ~y -> last_scroll := Some y)
      [ content ]
  in
  let root children =
    Vnode.box ~key:"responsive-root" ~style:column_style children
  in
  let view =
    Vnode.viewport_switch ~at_least_width:80
      ~wide:(root [ transcript; Vnode.spinner () ])
      ~narrow:(root [ transcript; Vnode.input ~autofocus:true () ])
  in
  render_view ~viewport_width:79 reconciler view;
  do_frame ~width:79 renderer;
  ignore (Renderer.render ~full:true renderer : string);
  let first_root = List.hd (children_of renderer) in
  let first_transcript = List.hd (Renderable.children first_root) in
  let first_narrow = List.nth (Renderable.children first_root) 1 in
  let mouse =
    match Input.mouse_scroll 1 1 Input.Mouse.Scroll_down with
    | Input.Mouse mouse -> mouse
    | _ -> assert false
  in
  Renderer.dispatch_mouse renderer mouse;
  let first_scroll =
    match !last_scroll with
    | Some y when y > 0 -> y
    | _ -> fail "expected the transcript to scroll before resize"
  in
  render_view ~viewport_width:80 reconciler view;
  do_frame ~width:80 renderer;
  ignore (Renderer.render ~full:true renderer : string);
  let wide_root = List.hd (children_of renderer) in
  let wide_transcript = List.hd (Renderable.children wide_root) in
  let wide_only = List.nth (Renderable.children wide_root) 1 in
  is_true ~msg:"compatible keyed root retained" (first_root == wide_root);
  is_true ~msg:"keyed stateful child retained"
    (first_transcript == wide_transcript);
  is_true ~msg:"narrow branch destroyed" (Renderable.destroyed first_narrow);
  is_none ~msg:"focus cleared with narrow branch" (Renderer.focused renderer);
  equal ~msg:"only wide live branch active" int 1
    (Renderable.Private.live_count (Renderer.root renderer));
  Renderer.dispatch_mouse renderer mouse;
  (match !last_scroll with
  | Some y ->
      is_true ~msg:"retained transcript continues from its scroll position"
        (y > first_scroll)
  | None -> fail "expected the retained transcript to keep scrolling");
  render_view ~viewport_width:79 reconciler view;
  do_frame ~width:79 renderer;
  let second_root = List.hd (children_of renderer) in
  let second_transcript = List.hd (Renderable.children second_root) in
  let second_narrow = List.nth (Renderable.children second_root) 1 in
  is_true ~msg:"root survives both crossings" (first_root == second_root);
  is_true ~msg:"stateful child survives both crossings"
    (first_transcript == second_transcript);
  is_true ~msg:"wide branch destroyed" (Renderable.destroyed wide_only);
  is_false ~msg:"narrow branch remounted" (first_narrow == second_narrow);
  equal ~msg:"hidden wide live branch stopped" int 0
    (Renderable.Private.live_count (Renderer.root renderer));
  match Renderer.focused renderer with
  | Some focused ->
      is_true ~msg:"remounted narrow branch receives focus"
        (focused == second_narrow)
  | None -> fail "expected remounted narrow branch to be focused"

let non_focusable_scroll_box_still_scrolls_with_wheel () =
  let renderer, reconciler = make () in
  let last_scroll = ref None in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_size (Vnode.size ~width:10 ~height:3)
  in
  let content =
    Vnode.box
      ~style:
        (Toffee.Style.default
        |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
        |> Toffee.Style.set_flex_shrink 0.)
      (List.init 10 (fun index -> Vnode.text (string_of_int index)))
  in
  render_view reconciler
    (Vnode.scroll_box ~style ~focusable:false ~show_scrollbars:false
       ~on_scroll:(fun ~x:_ ~y -> last_scroll := Some y)
       [ content ]);
  do_frame renderer;
  ignore (Renderer.render ~full:true renderer : string);
  let node = List.hd (children_of renderer) in
  is_false ~msg:"scroll box is not focusable" (Renderable.focusable node);
  let mouse =
    match Input.mouse_scroll 1 1 Input.Mouse.Scroll_down with
    | Input.Mouse mouse -> mouse
    | _ -> assert false
  in
  Renderer.dispatch_mouse renderer mouse;
  is_none ~msg:"wheel does not take focus" (Renderer.focused renderer);
  match !last_scroll with
  | Some y -> is_true ~msg:"wheel advances scroll position" (y > 0)
  | None -> fail "expected wheel scroll callback"

let keyed_scroll_by_applies_once_after_layout () =
  let renderer, reconciler = make () in
  let last_scroll = ref None in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_size (Vnode.size ~width:10 ~height:4)
  in
  let content =
    Vnode.box
      ~style:
        (Toffee.Style.default
        |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
        |> Toffee.Style.set_flex_shrink 0.)
      (List.init 20 (fun index -> Vnode.text (string_of_int index)))
  in
  let scroll_request key : Scroll_box.scroll_by =
    { key; x = None; y = Some 1.; unit = `Viewport }
  in
  let view ?background request =
    Vnode.scroll_box ~key:"paged" ~style ~show_scrollbars:false ?background
      ~scroll_by:request
      ~on_scroll:(fun ~x ~y -> last_scroll := Some (x, y))
      [ content ]
  in
  let expect_scroll message expected =
    match !last_scroll with
    | Some (x, y) ->
        equal ~msg:(message ^ ": horizontal offset") int 0 x;
        equal ~msg:message int expected y
    | None -> fail (message ^ ": expected a scroll callback")
  in
  let request = scroll_request "page-1" in
  render_view reconciler (view request);
  do_frame renderer;
  expect_scroll "first request uses the measured viewport" 4;
  render_view reconciler (view ~background:Ansi.Color.blue request);
  do_frame renderer;
  expect_scroll "unrelated prop update does not replay the key" 4;
  render_view reconciler (view (scroll_request "page-2"));
  do_frame renderer;
  expect_scroll "a new key applies exactly once" 8

let acknowledged_scroll_by_does_not_replay_after_remount () =
  let renderer, reconciler = make () in
  let request = ref (Some "page-1") in
  let acknowledgements = ref [] in
  let scrolls = ref [] in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_size (Vnode.size ~width:10 ~height:4)
  in
  let content =
    Vnode.box
      ~style:
        (Toffee.Style.default
        |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
        |> Toffee.Style.set_flex_shrink 0.)
      (List.init 20 (fun index -> Vnode.text (string_of_int index)))
  in
  let view instance_key =
    let scroll_by =
      Option.map
        (fun key ->
          ({ key; x = None; y = Some 1.; unit = `Viewport }
            : Scroll_box.scroll_by))
        !request
    in
    Vnode.scroll_box ~key:instance_key ~style ~show_scrollbars:false ?scroll_by
      ~on_scroll:(fun ~x:_ ~y -> scrolls := y :: !scrolls)
      ~on_scroll_by_applied:(fun ~key ->
        acknowledgements := key :: !acknowledgements;
        if Option.equal String.equal !request (Some key) then request := None)
      [ content ]
  in
  render_view reconciler (view "first-mount");
  do_frame renderer;
  equal ~msg:"request moved by one viewport" (list int) [ 4 ]
    (List.rev !scrolls);
  equal ~msg:"request acknowledged" (list string) [ "page-1" ]
    (List.rev !acknowledgements);
  is_none ~msg:"caller retired acknowledged request" !request;
  render_view reconciler (view "second-mount");
  do_frame renderer;
  equal ~msg:"remount cannot replay retired request" (list int) [ 4 ]
    (List.rev !scrolls);
  equal ~msg:"remount emits no stale acknowledgement" (list string) [ "page-1" ]
    (List.rev !acknowledgements)

(* ── Re-rendering (Update in Place) ── *)

let rerender_reuses_box () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box []);
  do_frame renderer;
  let node_before = List.hd (children_of renderer) in
  render_view reconciler (Vnode.box ~border:true []);
  do_frame renderer;
  let node_after = List.hd (children_of renderer) in
  is_true ~msg:"same node" (node_before == node_after)

let rerender_reuses_text () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.text "hello");
  do_frame renderer;
  let node_before = List.hd (children_of renderer) in
  render_view reconciler (Vnode.text "world");
  do_frame renderer;
  let node_after = List.hd (children_of renderer) in
  is_true ~msg:"same node" (node_before == node_after)

let rerender_updates_visibility () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box ~visible:true []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  is_true ~msg:"visible before" (Renderable.visible node);
  render_view reconciler (Vnode.box ~visible:false []);
  do_frame renderer;
  is_false ~msg:"hidden after" (Renderable.visible node)

let rerender_updates_z_index () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box ~z_index:0 []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  equal ~msg:"z_index before" int 0 (Renderable.z_index node);
  render_view reconciler (Vnode.box ~z_index:5 []);
  do_frame renderer;
  equal ~msg:"z_index after" int 5 (Renderable.z_index node)

let rerender_updates_opacity () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box ~opacity:1.0 []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  render_view reconciler (Vnode.box ~opacity:0.5 []);
  do_frame renderer;
  is_true ~msg:"opacity" (Float.equal (Renderable.opacity node) 0.5)

let controlled_input_reapplies_equal_value () =
  let renderer, reconciler = make () in
  let inputs = ref [] in
  let submissions = ref [] in
  let view () =
    Vnode.input ~key:"controlled-input" ~autofocus:true ~value:""
      ~on_input:(fun value -> inputs := value :: !inputs)
      ~on_submit:(fun value -> submissions := value :: !submissions)
      ()
  in
  render_view reconciler (view ());
  ignore (Renderer.dispatch_key renderer (Input.Key.of_char 'x') : Event.key);
  equal ~msg:"live input accepted the edit" (list string) [ "x" ]
    (List.rev !inputs);
  render_view reconciler (view ());
  ignore
    (Renderer.dispatch_key renderer (Input.Key.make Input.Key.Enter)
      : Event.key);
  equal ~msg:"equal controlled value replaced live input state" (list string)
    [ "" ] (List.rev !submissions)

let controlled_textarea_reapplies_equal_value () =
  let renderer, reconciler = make () in
  let inputs = ref [] in
  let submissions = ref [] in
  let node = ref None in
  let view () =
    Vnode.textarea ~key:"controlled-textarea" ~autofocus:true ~value:""
      ~ref:(fun renderable -> node := Some renderable)
      ~on_input:(fun value -> inputs := value :: !inputs)
      ~on_submit:(fun value -> submissions := value :: !submissions)
      ()
  in
  render_view reconciler (view ());
  ignore (Renderer.dispatch_key renderer (Input.Key.of_char 'x') : Event.key);
  equal ~msg:"live textarea accepted the edit" (list string) [ "x" ]
    (List.rev !inputs);
  do_frame renderer;
  render_view reconciler (view ());
  (match !node with
  | Some renderable ->
      is_true ~msg:"controlled replacement invalidates intrinsic layout"
        (Renderable.Private.layout_dirty renderable)
  | None -> fail "expected textarea ref");
  let modifier = { Input.Modifier.none with ctrl = true } in
  ignore
    (Renderer.dispatch_key renderer (Input.Key.make ~modifier Input.Key.Enter)
      : Event.key);
  equal ~msg:"equal controlled value replaced live textarea state" (list string)
    [ "" ] (List.rev !submissions)

let converged_controlled_value_preserves_cursor () =
  let renderer, reconciler = make () in
  let inputs = ref [] in
  let view =
    Vnode.textarea ~key:"controlled-cursor" ~autofocus:true ~value:"ab"
      ~on_input:(fun value -> inputs := value :: !inputs)
      ()
  in
  render_view reconciler view;
  ignore
    (Renderer.dispatch_key renderer (Input.Key.make Input.Key.Left) : Event.key);
  render_view reconciler view;
  ignore (Renderer.dispatch_key renderer (Input.Key.of_char 'x') : Event.key);
  equal ~msg:"same vnode preserves the live cursor when value has converged"
    (list string) [ "axb" ] (List.rev !inputs)

(* ── Keyed Reconciliation ── *)

let keyed_reorder () =
  let renderer, reconciler = make () in
  render_view reconciler
    (Vnode.fragment [ Vnode.box ~key:"a" []; Vnode.box ~key:"b" [] ]);
  do_frame renderer;
  let children_before = children_of renderer in
  let node_a = List.nth children_before 0 in
  let node_b = List.nth children_before 1 in
  render_view reconciler
    (Vnode.fragment [ Vnode.box ~key:"b" []; Vnode.box ~key:"a" [] ]);
  do_frame renderer;
  let children_after = children_of renderer in
  let first = List.nth children_after 0 in
  let second = List.nth children_after 1 in
  is_true ~msg:"b is first" (first == node_b);
  is_true ~msg:"a is second" (second == node_a)

let keyed_removal () =
  let renderer, reconciler = make () in
  render_view reconciler
    (Vnode.fragment [ Vnode.box ~key:"a" []; Vnode.box ~key:"b" [] ]);
  do_frame renderer;
  let node_b = List.nth (children_of renderer) 1 in
  render_view reconciler (Vnode.fragment [ Vnode.box ~key:"b" [] ]);
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer);
  is_true ~msg:"b survives" (List.hd (children_of renderer) == node_b)

let keyed_addition () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.fragment [ Vnode.box ~key:"a" [] ]);
  do_frame renderer;
  let node_a = List.hd (children_of renderer) in
  render_view reconciler
    (Vnode.fragment [ Vnode.box ~key:"a" []; Vnode.box ~key:"b" [] ]);
  do_frame renderer;
  equal ~msg:"two children" int 2 (child_count renderer);
  is_true ~msg:"a survives" (List.nth (children_of renderer) 0 == node_a)

let keyed_reorder_preserves_focus () =
  let renderer, reconciler = make () in
  render_view reconciler
    (Vnode.fragment
       [
         Vnode.box ~key:"a" ~focusable:true [];
         Vnode.box ~key:"b" ~focusable:true [];
       ]);
  do_frame renderer;
  let node_a = List.nth (children_of renderer) 0 in
  let node_b = List.nth (children_of renderer) 1 in
  ignore (Renderer.focus renderer node_b : bool);
  render_view reconciler
    (Vnode.fragment
       [
         Vnode.box ~key:"b" ~focusable:true [];
         Vnode.box ~key:"a" ~focusable:true [];
       ]);
  do_frame renderer;
  is_true ~msg:"b remains focused" (Renderable.focused node_b);
  is_true ~msg:"renderer still points at b"
    (match Renderer.focused renderer with
    | Some node -> node == node_b
    | None -> false);
  is_false ~msg:"a remains unfocused" (Renderable.focused node_a)

let unkeyed_positional () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.fragment [ Vnode.box []; Vnode.text "x" ]);
  do_frame renderer;
  let box_node = List.nth (children_of renderer) 0 in
  let text_node = List.nth (children_of renderer) 1 in
  render_view reconciler
    (Vnode.fragment [ Vnode.box ~border:true []; Vnode.text "y" ]);
  do_frame renderer;
  is_true ~msg:"box reused" (List.nth (children_of renderer) 0 == box_node);
  is_true ~msg:"text reused" (List.nth (children_of renderer) 1 == text_node)

(* ── Kind Mismatch ── *)

let kind_mismatch_destroys_old () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box []);
  do_frame renderer;
  let old_node = List.hd (children_of renderer) in
  render_view reconciler (Vnode.text "replaced");
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer);
  is_true ~msg:"old destroyed" (Renderable.destroyed old_node);
  is_true ~msg:"new is different" (List.hd (children_of renderer) != old_node)

let kind_mismatch_text_to_slider () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.text "hello");
  do_frame renderer;
  let old_node = List.hd (children_of renderer) in
  render_view reconciler (Vnode.slider ());
  do_frame renderer;
  is_true ~msg:"old destroyed" (Renderable.destroyed old_node);
  equal ~msg:"one child" int 1 (child_count renderer)

let render_single_spinner () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.spinner ());
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer)

let render_single_progress_bar () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.progress_bar ());
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer)

let rerender_reuses_spinner () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.spinner ());
  do_frame renderer;
  let node_before = List.hd (children_of renderer) in
  render_view reconciler (Vnode.spinner ~frame_set:Spinner.line ());
  do_frame renderer;
  let node_after = List.hd (children_of renderer) in
  is_true ~msg:"same node" (node_before == node_after)

let rerender_reuses_progress_bar () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.progress_bar ());
  do_frame renderer;
  let node_before = List.hd (children_of renderer) in
  render_view reconciler (Vnode.progress_bar ~value:0.5 ());
  do_frame renderer;
  let node_after = List.hd (children_of renderer) in
  is_true ~msg:"same node" (node_before == node_after)

let kind_mismatch_spinner_to_progress_bar () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.spinner ());
  do_frame renderer;
  let old_node = List.hd (children_of renderer) in
  render_view reconciler (Vnode.progress_bar ());
  do_frame renderer;
  is_true ~msg:"old destroyed" (Renderable.destroyed old_node);
  equal ~msg:"one child" int 1 (child_count renderer)

(* ── Children ── *)

let fewer_children_destroys_extra () =
  let renderer, reconciler = make () in
  render_view reconciler
    (Vnode.box [ Vnode.text "a"; Vnode.text "b"; Vnode.text "c" ]);
  do_frame renderer;
  let box_node = List.hd (children_of renderer) in
  equal ~msg:"three kids" int 3 (List.length (Renderable.children box_node));
  let old_c = List.nth (Renderable.children box_node) 2 in
  render_view reconciler (Vnode.box [ Vnode.text "a" ]);
  do_frame renderer;
  equal ~msg:"one kid" int 1 (List.length (Renderable.children box_node));
  is_true ~msg:"c destroyed" (Renderable.destroyed old_c)

let more_children_creates_new () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box [ Vnode.text "a" ]);
  do_frame renderer;
  let box_node = List.hd (children_of renderer) in
  equal ~msg:"one kid" int 1 (List.length (Renderable.children box_node));
  render_view reconciler (Vnode.box [ Vnode.text "a"; Vnode.text "b" ]);
  do_frame renderer;
  equal ~msg:"two kids" int 2 (List.length (Renderable.children box_node))

let nested_boxes () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box [ Vnode.box [ Vnode.text "inner" ] ]);
  do_frame renderer;
  let outer = List.hd (children_of renderer) in
  equal ~msg:"outer has 1 child" int 1 (List.length (Renderable.children outer));
  let inner = List.hd (Renderable.children outer) in
  equal ~msg:"inner has 1 child" int 1 (List.length (Renderable.children inner))

(* ── Embed ── *)

let embed_attaches () =
  let renderer, reconciler = make () in
  let embedded = Renderable.create ~parent:(Renderer.root renderer) () in
  Renderable.detach embedded;
  render_view reconciler (Vnode.embed embedded);
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer)

let embed_removed_detaches () =
  let renderer, reconciler = make () in
  let embedded = Renderable.create ~parent:(Renderer.root renderer) () in
  Renderable.detach embedded;
  render_view reconciler (Vnode.embed embedded);
  do_frame renderer;
  equal ~msg:"attached" int 1 (child_count renderer);
  render_view reconciler Vnode.empty;
  do_frame renderer;
  equal ~msg:"detached" int 0 (child_count renderer)

(* ── Callbacks ── *)

let ref_fires_on_creation () =
  let _renderer, reconciler = make () in
  let received = ref None in
  render_view reconciler (Vnode.box ~ref:(fun n -> received := Some n) []);
  is_some ~msg:"ref called" !received

let ref_receives_correct_node () =
  let renderer, reconciler = make () in
  let received = ref None in
  render_view reconciler (Vnode.box ~ref:(fun n -> received := Some n) []);
  do_frame renderer;
  match !received with
  | Some n -> is_true ~msg:"same node" (n == List.hd (children_of renderer))
  | None -> fail "ref not called"

let on_key_handler_fires () =
  let renderer, reconciler = make () in
  let received = ref false in
  render_view reconciler
    (Vnode.box ~focusable:true ~on_key:(fun _ -> received := true) []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  ignore (Renderer.focus renderer node : bool);
  ignore (Renderer.dispatch_key renderer (Input.Key.of_char 'a') : Event.key);
  is_true ~msg:"handler fired" !received

let on_key_handler_updates_on_rerender () =
  let renderer, reconciler = make () in
  let log = ref [] in
  render_view reconciler
    (Vnode.box ~focusable:true ~on_key:(fun _ -> log := "first" :: !log) []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  ignore (Renderer.focus renderer node : bool);
  ignore (Renderer.dispatch_key renderer (Input.Key.of_char 'a') : Event.key);
  equal ~msg:"first handler" (list string) [ "first" ] !log;
  render_view reconciler
    (Vnode.box ~focusable:true ~on_key:(fun _ -> log := "second" :: !log) []);
  ignore (Renderer.dispatch_key renderer (Input.Key.of_char 'b') : Event.key);
  equal ~msg:"second handler" (list string) [ "second"; "first" ] !log

(* ── Unmount ── *)

let unmount_empties_container () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.fragment [ Vnode.box []; Vnode.text "hi" ]);
  do_frame renderer;
  equal ~msg:"two children" int 2 (child_count renderer);
  Reconciler.unmount reconciler;
  do_frame renderer;
  equal ~msg:"no children" int 0 (child_count renderer)

let unmount_destroys_nodes () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  Reconciler.unmount reconciler;
  is_true ~msg:"destroyed" (Renderable.destroyed node)

let unmount_allows_reuse () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.text "first");
  do_frame renderer;
  Reconciler.unmount reconciler;
  do_frame renderer;
  equal ~msg:"empty after unmount" int 0 (child_count renderer);
  render_view reconciler (Vnode.text "second");
  do_frame renderer;
  equal ~msg:"one child after reuse" int 1 (child_count renderer)

(* ── Runner ── *)

let () =
  run "mosaic.reconciler"
    [
      group "Construction"
        [
          test "container returns root" create_returns_container;
          test "render rejects nonpositive viewport width"
            render_rejects_nonpositive_viewport_width;
        ];
      group "Basic rendering"
        [
          test "single box" render_single_box;
          test "single text" render_single_text;
          test "empty" render_empty;
          test "fragment" render_fragment;
          test "nested fragments" render_nested_fragments;
          test "viewport switch selects only matching branch"
            viewport_switch_selects_only_matching_branch;
          test "viewport switch reconciles across resize threshold"
            viewport_switch_reconciles_selected_branch_on_resize;
          test "non-focusable scroll box still wheels"
            non_focusable_scroll_box_still_scrolls_with_wheel;
          test "keyed scroll_by applies once after layout"
            keyed_scroll_by_applies_once_after_layout;
          test "acknowledged scroll_by does not replay after remount"
            acknowledged_scroll_by_does_not_replay_after_remount;
          test "single spinner" render_single_spinner;
          test "single progress_bar" render_single_progress_bar;
        ];
      group "Re-rendering"
        [
          test "reuses box" rerender_reuses_box;
          test "reuses text" rerender_reuses_text;
          test "reuses spinner" rerender_reuses_spinner;
          test "reuses progress_bar" rerender_reuses_progress_bar;
          test "updates visibility" rerender_updates_visibility;
          test "updates z_index" rerender_updates_z_index;
          test "updates opacity" rerender_updates_opacity;
          test "controlled input reapplies equal value"
            controlled_input_reapplies_equal_value;
          test "controlled textarea reapplies equal value"
            controlled_textarea_reapplies_equal_value;
          test "converged controlled value preserves cursor"
            converged_controlled_value_preserves_cursor;
        ];
      group "Keyed reconciliation"
        [
          test "reorder" keyed_reorder;
          test "removal" keyed_removal;
          test "addition" keyed_addition;
          test "reorder preserves focus" keyed_reorder_preserves_focus;
          test "unkeyed positional" unkeyed_positional;
        ];
      group "Kind mismatch"
        [
          test "box to text" kind_mismatch_destroys_old;
          test "text to slider" kind_mismatch_text_to_slider;
          test "spinner to progress_bar" kind_mismatch_spinner_to_progress_bar;
        ];
      group "Children"
        [
          test "fewer children destroys extra" fewer_children_destroys_extra;
          test "more children creates new" more_children_creates_new;
          test "nested boxes" nested_boxes;
        ];
      group "Embed"
        [
          test "attaches" embed_attaches;
          test "removed detaches" embed_removed_detaches;
        ];
      group "Callbacks"
        [
          test "ref fires on creation" ref_fires_on_creation;
          test "ref receives correct node" ref_receives_correct_node;
          test "on_key handler fires" on_key_handler_fires;
          test "on_key handler updates on rerender"
            on_key_handler_updates_on_rerender;
        ];
      group "Unmount"
        [
          test "empties container" unmount_empties_container;
          test "destroys nodes" unmount_destroys_nodes;
          test "allows reuse" unmount_allows_reuse;
        ];
    ]
