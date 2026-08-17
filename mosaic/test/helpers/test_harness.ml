open Mosaic_ui

(* ── Renderer Context ── *)

type ctx = {
  ctx : Renderable.Private.context;
  schedule_count : int ref;
  focus_log : string list ref;
  blur_log : string list ref;
  unregister_log : string list ref;
}

let next_num = ref 0

let make_ctx ?(width_method = `Unicode) () =
  let tree = Toffee.new_tree () in
  let schedule_count = ref 0 in
  let focus_log = ref [] in
  let blur_log = ref [] in
  let unregister_log = ref [] in
  let ctx : Renderable.Private.context =
    {
      tree;
      schedule = (fun () -> incr schedule_count);
      focus =
        (fun n ->
          focus_log := Renderable.id n :: !focus_log;
          Renderable.Private.focus_direct n);
      blur =
        (fun n ->
          blur_log := Renderable.id n :: !blur_log;
          Renderable.Private.blur_direct n);
      get_selection = (fun () -> None);
      request_selection_update = (fun () -> ());
      register_lifecycle = (fun _ -> ());
      unregister_lifecycle = (fun _ -> ());
      alloc_num =
        (fun () ->
          let n = !next_num in
          incr next_num;
          n);
      register = (fun _ -> ());
      unregister =
        (fun n -> unregister_log := Renderable.id n :: !unregister_log);
      width_method = (fun () -> width_method);
      report_scroll = (fun _ ~region:_ ~dx:_ ~dy:_ -> ());
    }
  in
  { ctx; schedule_count; focus_log; blur_log; unregister_log }

(* ── Root and Layout ── *)

let make_root ?style t =
  let root = Renderable.Private.create_root t.ctx ?style () in
  Renderable.Private.set_is_root root true;
  root

let layout_node node ~x ~y ~width ~height =
  Renderable.Private.update_layout node ~x:(Float.of_int x) ~y:(Float.of_int y)
    ~width:(Float.of_int width) ~height:(Float.of_int height)

(* ── Box Helpers ── *)

let lp = Toffee.Style.Length_percentage.equal
let one = Toffee.Style.Length_percentage.length 1.
let zero_lp = Toffee.Style.Length_percentage.zero
let border_of node = Toffee.Style.border (Renderable.style node)

(* ── Grid ── *)

let make_grid ?(width = 80) ?(height = 24) ?(width_method = `Unicode) () =
  Matrix_grid.create ~width ~height ~width_method ~respect_alpha:false ()

(* ── Input events ── *)

(* Every widget test driving the keyboard wrote this record out by hand, all
   eight modifier fields included, once per file. The defaults are "no
   modifier, a press, no associated text" — what a test means by "the user
   pressed this key" — so the exceptions name only what differs. *)
let make_key ?(ctrl = false) ?(alt = false) ?(shift = false) ?(super = false)
    ?(hyper = false) ?(meta = false) ?(caps_lock = false) ?(num_lock = false)
    ?(event_type = Matrix_input.Key.Press) ?(associated_text = "") ?shifted_key
    ?base_key key : Matrix_input.Key.event =
  {
    key;
    modifier = { ctrl; alt; shift; super; hyper; meta; caps_lock; num_lock };
    event_type;
    associated_text;
    shifted_key;
    base_key;
  }

(* UI-layer mouse events (Event.Mouse). Widget tests that work at the input
   layer instead — Matrix_input.Mouse, as text_input and textarea do — build
   their own; the two layers are different types, not duplicates. *)
let no_mod = Event.Mouse.no_modifier

let mouse_down ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod (Down { button = Left })

let mouse_up ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Up { button = Left; is_dragging = false })

let mouse_drag ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Drag { button = Left; is_dragging = true })

let mouse_move ~x ~y = Event.Mouse.make ~x ~y ~modifiers:no_mod Move
let mouse_out ~x ~y = Event.Mouse.make ~x ~y ~modifiers:no_mod Out

let mouse_scroll_down ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Scroll { direction = Scroll_down; delta = 1 })

let mouse_scroll_up ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Scroll { direction = Scroll_up; delta = 1 })
