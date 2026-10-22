(* ───── Render Commands ───── *)

(* A flat array of commands built by depth-first tree traversal, executed
   sequentially. This separates layout extraction (pass 2) from drawing (pass
   3), so traversal and drawing stay decoupled. *)
module Grid = Matrix_grid
module Input = Matrix_input
module Screen = Matrix_screen

type render_command =
  | Render of Renderable.t
  | Push_scissor of Grid.region
  | Pop_scissor
  | Push_opacity of float
  | Pop_opacity

type pointer_state = {
  mutable position : (int * int) option;
  mutable modifiers : Input.Modifier.t;
  mutable rejected_press : bool;
  mutable press_target : Renderable.t option;
}

type selection_state = {
  mutable current : Selection.t option;
  mutable anchor_node : Renderable.t option;
  mutable containers : Renderable.t list;
  mutable touched_selectables : Renderable.t list;
}

(* Scroll movements reported since the last presentation. A hardware scroll
   hint is only safe when exactly one scroll container moved, purely
   vertically, within one stable row region — anything else disqualifies the
   whole cycle and presentation falls back to the ordinary cell diff. *)
type scroll_hint_state =
  | Hint_none
  | Hint_one of { num : int; region : Grid.region; mutable delta : int }
  | Hint_disqualified

(* ───── Types ───── *)

type t = {
  screen : Screen.t;
  (* [true] when the screen was adopted from a host runtime; presentation
     then belongs to the host, and [render] refuses to run. *)
  adopted : bool;
  root : Renderable.t;
  tree : unit Toffee.tree;
  (* Node registry: maps numeric IDs to renderables for hit testing. *)
  node_map : (int, Renderable.t) Hashtbl.t;
  (* Reverse index: maps Toffee.Node_id index to renderable for O(1) measure
     lookups. *)
  toffee_map : (int, Renderable.t) Hashtbl.t;
  (* Focus — shared ref between record and context closures (single source of
     truth). *)
  focused : Renderable.t option ref;
  (* Lifecycle *)
  lifecycle_set : (int, Renderable.t) Hashtbl.t;
  (* Worker notifications only mutate this scheduling flag. *)
  dirty : bool Atomic.t;
  (* Available space of the last layout compute, as (width, height).
     Layout is recomputed only when this changes or the Toffee root is dirty
     (any set_style/measure/mark_dirty/attach/detach marks ancestors up to
     the root), skipping the full sizing and rounding passes on clean
     frames. *)
  mutable last_layout : (int * int) option;
  (* The frame laid out by [layout_frame] and not yet painted. *)
  mutable frame : frame option;
  (* Render command buffer — reused across frames. *)
  mutable commands : render_command array;
  mutable cmd_len : int;
  mutable input_geometry : (int * int) option;
  (* Hover tracking *)
  mutable hover_node : Renderable.t option;
  mutable hover_num : int;
  pointer : pointer_state;
  (* Drag capture *)
  mutable captured : Renderable.t option;
  (* Selection *)
  selection_state : selection_state;
  (* Scroll hint accumulation — shared ref with the context closure, reset
     when a presentation consumes it. *)
  scroll_hint : scroll_hint_state ref;
  (* Frame callbacks *)
  mutable frame_callbacks : (float -> unit) list;
  (* Size reports of the current frame — shared with the context closure. *)
  size_round : size_round;
}

(* Size reports settle by the depth rule of CSS Resize Observer (W3C, §3.4.1
   and §3.6.1): once a layout pass delivers reports, later passes of the same
   frame deliver only reports of nodes deeper than the shallowest node already
   reported. Depth grows with each pass that delivers, so a frame's passes
   terminate even when an observer's response keeps resizing its own node.
   Refused reports stay pending; the next frame's layout starts a new round
   and delivers them. *)
and frame = { width : int; height : int; delta : float }

and size_round = {
  mutable limit : int;
      (* Reports are admitted from nodes deeper than [limit]; [-1] admits all. *)
  mutable shallowest : int;
      (* Depth of the shallowest node reported in the current pass. *)
}

module Pending = struct
  type t = { node : Renderable.t; work : Renderable.Pending.t }

  let node t = t.node
  let work t = t.work
end

type settle_result = [ `Settled | `Pending of Pending.t list ]

(* ───── Helpers ───── *)

let toffee_exn = function
  | Ok x -> x
  | Error e -> invalid_arg (Toffee.Error.to_string e)

let clip_rect_intersect (a : Grid.region) (b : Grid.region) : Grid.region =
  let x1 = max a.x b.x and y1 = max a.y b.y in
  let x2 = min (a.x + a.width) (b.x + b.width) in
  let y2 = min (a.y + a.height) (b.y + b.height) in
  { x = x1; y = y1; width = max 0 (x2 - x1); height = max 0 (y2 - y1) }

(* ───── Command Buffer ───── *)

let ensure_cmd_capacity t needed =
  let len = Array.length t.commands in
  if needed > len then (
    let cap =
      let rec grow c = if c >= needed then c else grow (c * 2) in
      grow (max 64 (len * 2))
    in
    let arr = Array.make cap Pop_scissor in
    Array.blit t.commands 0 arr 0 t.cmd_len;
    t.commands <- arr)

let emit_cmd t cmd =
  ensure_cmd_capacity t (t.cmd_len + 1);
  t.commands.(t.cmd_len) <- cmd;
  t.cmd_len <- t.cmd_len + 1

(* ───── Node Lookup ───── *)

let find_node t num = Hashtbl.find_opt t.node_map num

let rec find_focusable node =
  if Renderable.focusable node then Some node
  else
    match Renderable.parent node with
    | Some p -> find_focusable p
    | None -> None

(* ───── Focus ───── *)

let blur_current t =
  match !(t.focused) with
  | None -> ()
  | Some node ->
      Renderable.Private.blur_direct node;
      t.focused := None

let focus_node t node =
  if not (Renderable.focusable node) then false
  else (
    if match !(t.focused) with Some f -> not (f == node) | None -> true then (
      blur_current t;
      ignore (Renderable.Private.focus_direct node : bool);
      t.focused := Some node);
    true)

(* ───── Selection Helpers ───── *)

let rec iter_selectables node f =
  if Renderable.selectable node then f node;
  Renderable.Private.iter_children_z node (fun child ->
      iter_selectables child f)

let rec is_within_container node container =
  if node == container then true
  else
    match Renderable.parent node with
    | Some p -> is_within_container p container
    | None -> false

let list_phys_index x lst =
  let rec go i = function
    | [] -> -1
    | hd :: _ when hd == x -> i
    | _ :: tl -> go (i + 1) tl
  in
  go 0 lst

let list_take n lst =
  let rec go acc n = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | hd :: tl -> go (hd :: acc) (n - 1) tl
  in
  go [] n lst

let parent_or_self node =
  match Renderable.parent node with Some p -> p | None -> node

let notify_selectables state sel =
  match state.containers with
  | [] -> ()
  | container :: _ ->
      let new_touched = ref [] in
      iter_selectables container (fun node ->
          ignore (Renderable.Private.emit_selection_changed node sel : bool);
          new_touched := node :: !new_touched);
      List.iter
        (fun prev ->
          if
            (not (List.exists (fun n -> n == prev) !new_touched))
            && not (Renderable.destroyed prev)
          then
            ignore (Renderable.Private.emit_selection_changed prev None : bool))
        state.touched_selectables;
      state.touched_selectables <- !new_touched

let clear_active_selection t =
  match t.selection_state.current with
  | None -> ()
  | Some _ ->
      let touched = t.selection_state.touched_selectables in
      t.selection_state.current <- None;
      t.selection_state.anchor_node <- None;
      t.selection_state.containers <- [];
      t.selection_state.touched_selectables <- [];
      List.iter
        (fun node ->
          if not (Renderable.destroyed node) then begin
            ignore (Renderable.Private.emit_selection_changed node None : bool);
            if not (Renderable.destroyed node) then
              Renderable.Private.clear_selection node
          end)
        touched

let update_selection_state state ~x ~y ~target_node =
  match state.current with
  | Some sel when Selection.is_dragging sel ->
      Selection.set_focus sel { x; y };
      Selection.set_is_start sel false;
      (match state.containers with
      | [] -> ()
      | current_container :: _ as containers -> (
          match target_node with
          | None ->
              let parent_container = parent_or_self current_container in
              if not (parent_container == current_container) then
                state.containers <- parent_container :: containers
          | Some node ->
              if not (is_within_container node current_container) then (
                let parent_container = parent_or_self current_container in
                if not (parent_container == current_container) then
                  state.containers <- parent_container :: containers)
              else if List.length containers > 1 then
                let idx = list_phys_index node state.containers in
                let idx =
                  if idx >= 0 then idx
                  else
                    let p = parent_or_self node in
                    list_phys_index p state.containers
                in
                if idx >= 0 then
                  state.containers <- list_take (idx + 1) state.containers));
      notify_selectables state (Some sel);
      true
  | _ -> false

let request_selection_update ~selection_state ~pointer ~screen ~node_map =
  match (selection_state.current, pointer.position) with
  | Some sel, Some (x, y) when Selection.is_dragging sel ->
      let hit_num = Screen.query_hit screen ~x ~y in
      let target_node =
        if hit_num > 0 then Hashtbl.find_opt node_map hit_num else None
      in
      ignore (update_selection_state selection_state ~x ~y ~target_node : bool)
  | _ -> ()

let emit_selection_mouse t ~x ~y ~modifiers ~target_node ~target_id kind =
  let node, target =
    match t.selection_state.anchor_node with
    | Some anchor when not (Renderable.destroyed anchor) ->
        (Some anchor, Some (Renderable.Private.num anchor))
    | _ -> (target_node, target_id)
  in
  let ev = Event.Mouse.make ~x ~y ~modifiers ?target kind in
  (match node with
  | Some node -> Renderable.Private.emit_mouse node ev
  | None -> ());
  ev

(* ───── Hover Tracking ───── *)

let update_hover t ~x ~y ~modifiers ~target_node ~target_num kind =
  let is_move_like =
    match kind with Event.Mouse.Move | Event.Mouse.Drag _ -> true | _ -> false
  in
  if is_move_like && Option.is_none t.captured && target_num <> t.hover_num then (
    (* Fire Out on old hover target *)
    (match t.hover_node with
    | Some old ->
        let ev = Event.Mouse.make ~x ~y ~modifiers Event.Mouse.Out in
        Renderable.Private.emit_mouse old ev
    | None -> ());
    t.hover_node <- target_node;
    t.hover_num <- target_num;
    (* Fire Over on new hover target *)
    match target_node with
    | Some node ->
        let ev =
          Event.Mouse.make ~x ~y ~modifiers (Event.Mouse.Over { source = None })
        in
        Renderable.Private.emit_mouse node ev
    | None -> ())

(* Recheck hover against the hit grid of the frame just built (the next hit
   grid), so it runs before the host presents the frame. The data is identical
   to what [Screen.query_hit] returns after the swap. *)
let recheck_hover t =
  if Option.is_some t.captured then ()
  else
    match t.pointer.position with
    | None -> ()
    | Some (x, y) ->
        let hit_num =
          Screen.Hit_grid.get (Screen.next_hit_grid t.screen) ~x ~y
        in
        let target_node = if hit_num > 0 then find_node t hit_num else None in
        let target_num =
          match target_node with
          | Some n -> Renderable.Private.num n
          | None -> 0
        in
        if target_num <> t.hover_num then (
          let modifiers = t.pointer.modifiers in
          (match t.hover_node with
          | Some old ->
              let ev = Event.Mouse.make ~x ~y ~modifiers Event.Mouse.Out in
              Renderable.Private.emit_mouse old ev
          | None -> ());
          t.hover_node <- target_node;
          t.hover_num <- target_num;
          match target_node with
          | Some node ->
              let ev =
                Event.Mouse.make ~x ~y ~modifiers
                  (Event.Mouse.Over { source = None })
              in
              Renderable.Private.emit_mouse node ev
          | None -> ())

let rec alive_visible node =
  (not (Renderable.destroyed node))
  && Renderable.visible node
  &&
  match Renderable.parent node with
  | None -> true
  | Some parent -> alive_visible parent

(* Between frames, a point hits the last node the command list draws over it,
   as in a hit grid built from the commands: the node's box and every
   enclosing scissor contain the point. Walking the commands costs one test
   per command, where filling a grid would cost one store per covered cell. *)
let query_input_hit t ~x ~y =
  match t.input_geometry with
  | None -> Screen.query_hit t.screen ~x ~y
  | Some (width, height) ->
      if x < 0 || y < 0 || x >= width || y >= height then 0
      else begin
        let contains ~rx ~ry ~rw ~rh =
          x >= rx && x < rx + rw && y >= ry && y < ry + rh
        in
        (* [depth] counts open scissors; [outside] is the depth of the first
           one that excludes the point, or 0. *)
        let hit = ref 0 and depth = ref 0 and outside = ref 0 in
        for i = 0 to t.cmd_len - 1 do
          match t.commands.(i) with
          | Render node ->
              if
                !outside = 0
                && contains ~rx:(Renderable.x node) ~ry:(Renderable.y node)
                     ~rw:(Renderable.width node) ~rh:(Renderable.height node)
              then hit := Renderable.Private.num node
          | Push_scissor clip ->
              incr depth;
              if
                !outside = 0
                && not
                     (contains ~rx:clip.x ~ry:clip.y ~rw:clip.width
                        ~rh:clip.height)
              then outside := !depth
          | Pop_scissor ->
              if !outside = !depth then outside := 0;
              decr depth
          | Push_opacity _ | Pop_opacity -> ()
        done;
        !hit
      end

let focus_request_accepted requested target =
  match requested with
  | None -> true
  | Some focus -> (
      alive_visible focus && Renderable.focused focus
      &&
      match target with
      | None -> false
      | Some node -> (
          match find_focusable node with
          | Some ancestor -> ancestor == focus
          | None -> false))

let valid_selection_target t ~requested ~x ~y node =
  alive_visible node && Renderable.selectable node
  && query_input_hit t ~x ~y = Renderable.Private.num node
  && focus_request_accepted requested (Some node)

let reject_press t ~x ~y ~modifiers kind =
  t.pointer.rejected_press <- true;
  let event = Event.Mouse.make ~x ~y ~modifiers kind in
  Event.Mouse.prevent_default event;
  event

(* ───── Selection Event Handling ───── *)

(* When the selection state machine consumes the event, returns [Some ev]
   where [ev] is the event the tree saw; [None] means not consumed. *)
let handle_selection t ~requested ~x ~y ~(modifiers : Input.Modifier.t)
    ~target_node ~target_id kind =
  match kind with
  | Event.Mouse.Down { button = Left } when not modifiers.ctrl -> (
      match target_node with
      | Some node
        when Renderable.selectable node
             && Renderable.Private.should_start_selection node ~x ~y ->
          clear_active_selection t;
          if not (valid_selection_target t ~requested ~x ~y node) then
            Some (reject_press t ~x ~y ~modifiers kind)
          else
            let p = Selection.{ x; y } in
            let local_x = x - Renderable.x node in
            let local_y = y - Renderable.y node in
            let anchor_position () =
              Selection.
                {
                  x = Renderable.x node + local_x;
                  y = Renderable.y node + local_y;
                }
            in
            let sel = Selection.create ~anchor_position ~anchor:p ~focus:p () in
            t.selection_state.current <- Some sel;
            t.selection_state.anchor_node <- Some node;
            t.selection_state.containers <-
              [
                (match Renderable.parent node with
                | Some p -> p
                | None -> t.root);
              ];
            t.selection_state.touched_selectables <- [];
            notify_selectables t.selection_state (Some sel);
            if not (valid_selection_target t ~requested ~x ~y node) then begin
              (match t.selection_state.current with
              | Some current when current == sel -> clear_active_selection t
              | Some _ | None -> ());
              Some (reject_press t ~x ~y ~modifiers kind)
            end
            else
              let ev =
                Event.Mouse.make ~x ~y ~modifiers ?target:target_id
                  (Event.Mouse.Down { button = Left })
              in
              Renderable.Private.emit_mouse node ev;
              Some ev
      | _ -> None)
  | Event.Mouse.Drag { button = Left; _ } -> (
      match t.selection_state.current with
      | Some sel when Selection.is_dragging sel ->
          ignore
            (update_selection_state t.selection_state ~x ~y ~target_node : bool);
          Some
            (emit_selection_mouse t ~x ~y ~modifiers ~target_node ~target_id
               (Event.Mouse.Drag { button = Left; is_dragging = true }))
      | _ -> None)
  | Event.Mouse.Up { button = Left; _ } -> (
      match t.selection_state.current with
      | Some sel when Selection.is_dragging sel ->
          let ev =
            emit_selection_mouse t ~x ~y ~modifiers ~target_node ~target_id
              (Event.Mouse.Up { button = Left; is_dragging = true })
          in
          Selection.set_is_dragging sel false;
          notify_selectables t.selection_state (Some sel);
          Some ev
      | _ -> None)
  | _ -> None

(* ───── Drag Capture Handling ───── *)

(* When drag capture consumes the event, returns [Some ev] where [ev] is the
   event the captured node saw; [None] means not consumed. *)
let handle_drag_capture t ~x ~y ~modifiers ~target_node ~target_id kind =
  match t.captured with
  | None -> None
  | Some captured_node -> (
      let captured_id = Some (Renderable.Private.num captured_node) in
      match kind with
      | Event.Mouse.Scroll _ ->
          (* Wheel events keep targeting the hit node (or the focused
             fallback) during a capture drag; OpenTUI handles scroll before
             capture redirection. *)
          None
      | Event.Mouse.Up { button; _ } ->
          let drag_end_ev =
            Event.Mouse.make ~x ~y ~modifiers ?target:captured_id
              (Event.Mouse.Drag_end { button })
          in
          Renderable.Private.emit_mouse captured_node drag_end_ev;
          let up_ev =
            Event.Mouse.make ~x ~y ~modifiers ?target:captured_id
              (Event.Mouse.Up { button; is_dragging = false })
          in
          Renderable.Private.emit_mouse captured_node up_ev;
          (match target_node with
          | Some node ->
              let drop_ev =
                Event.Mouse.make ~x ~y ~modifiers ?target:target_id
                  (Event.Mouse.Drop { button; source = captured_id })
              in
              Renderable.Private.emit_mouse node drop_ev
          | None -> ());
          t.captured <- None;
          Atomic.set t.dirty true;
          Some up_ev
      | _ ->
          let ev = Event.Mouse.make ~x ~y ~modifiers ?target:captured_id kind in
          Renderable.Private.emit_mouse captured_node ev;
          Some ev)

(* ───── Mouse Dispatch Pipeline ───── *)

let map_button = function
  | Input.Mouse.Left -> Event.Mouse.Left
  | Input.Mouse.Right -> Event.Mouse.Right
  | Input.Mouse.Middle -> Event.Mouse.Middle
  | Input.Mouse.Button n -> Event.Mouse.Button n

(* Full mouse dispatch pipeline in a fixed order: 1. Update pointer →
   2. Hit test → 3. Selection → 4. Hover → 5. Drag capture → 6. Normal dispatch
   → 7. Selection clear.

   Returns the event the tree saw (mirroring [dispatch_key]), so a host can
   inspect [Event.Mouse.default_prevented] to tell whether a widget consumed
   the input. *)
let dispatch_mouse_internal t ~x ~y ~modifiers kind =
  t.pointer.position <- Some (x, y);
  t.pointer.modifiers <- modifiers;
  let suppress =
    match kind with
    | Event.Mouse.Down { button = Left } ->
        t.pointer.rejected_press <- false;
        false
    | Event.Mouse.Up { button = Left | Button 0; _ } ->
        let rejected = t.pointer.rejected_press in
        t.pointer.rejected_press <- false;
        rejected
    | Event.Mouse.Drag { button = Left; _ } -> t.pointer.rejected_press
    | _ -> false
  in
  if suppress then begin
    let event = Event.Mouse.make ~x ~y ~modifiers kind in
    Event.Mouse.prevent_default event;
    event
  end
  else
    let hit_num = query_input_hit t ~x ~y in
    let target_node = if hit_num > 0 then find_node t hit_num else None in
    (* Accept declarative focus before computing a caret. The request may change
     layout, so query again and require the same focus target afterwards. *)
    let requested =
      match (kind, target_node) with
      | Event.Mouse.Down { button = Left }, Some node when not modifiers.shift
        -> (
          match find_focusable node with
          | Some target when not (Renderable.focused target) ->
              if Renderable.Private.request_focus target then Some target
              else None
          | Some target -> Some target
          | None -> None)
      | _ -> None
    in
    let target_node =
      match requested with
      | None -> target_node
      | Some _ ->
          let hit = query_input_hit t ~x ~y in
          if hit > 0 then find_node t hit else None
    in
    if not (focus_request_accepted requested target_node) then
      reject_press t ~x ~y ~modifiers kind
    else
      (* A scroll over dead space still means "scroll": fall back to the focused
     renderable, whose ancestor chain reaches the nearest scrollable. *)
      let target_node =
        match (kind, target_node) with
        | Event.Mouse.Scroll _, None -> (
            match !(t.focused) with
            | Some node when Renderable.focused node -> Some node
            | _ -> None)
        | _ -> target_node
      in
      let target_num =
        Option.fold ~none:0 ~some:Renderable.Private.num target_node
      in
      let target_id = if target_num > 0 then Some target_num else None in
      (* Selection state machine *)
      match
        handle_selection t ~requested ~x ~y ~modifiers ~target_node ~target_id
          kind
      with
      | Some ev -> ev
      | None -> (
          (* Hover tracking *)
          update_hover t ~x ~y ~modifiers ~target_node ~target_num kind;
          (* Drag capture *)
          match
            handle_drag_capture t ~x ~y ~modifiers ~target_node ~target_id kind
          with
          | Some ev -> ev
          | None ->
              (* Set up new capture on left drag *)
              (match kind with
              | Event.Mouse.Drag { button = Left; _ } -> (
                  match target_node with
                  | Some node -> t.captured <- Some node
                  | None -> ())
              | _ -> ());
              (* Normal dispatch *)
              let ev =
                Event.Mouse.make ~x ~y ~modifiers ?target:target_id kind
              in
              (match target_node with
              | Some node -> Renderable.Private.emit_mouse node ev
              | None -> ());
              (* Clear selection on left click if not prevented *)
              (match kind with
              | Event.Mouse.Down { button = Left }
                when Option.is_some t.selection_state.current ->
                  if not (Event.Mouse.default_prevented ev) then
                    clear_active_selection t
              | _ -> ());
              ev)

(* ───── Creation ───── *)

let create ?width_method ?clock ?screen ?style ?(request_render = fun () -> ())
    () =
  let tree = Toffee.new_tree () in
  let adopted = Option.is_some screen in
  let screen =
    match screen with
    | Some screen ->
        (* Adopted screens come from a host runtime; the renderer requires
           alpha blending on the grids it paints. *)
        screen
    | None -> Screen.create ?width_method ?clock ()
  in
  let node_map = Hashtbl.create 256 in
  let toffee_map = Hashtbl.create 256 in
  let lifecycle_set = Hashtbl.create 16 in
  let next_num = ref 0 in
  let dirty = Atomic.make true in
  let focused = ref None in
  let pointer =
    {
      position = None;
      modifiers = Input.Modifier.none;
      rejected_press = false;
      press_target = None;
    }
  in
  let selection_state =
    {
      current = None;
      anchor_node = None;
      containers = [];
      touched_selectables = [];
    }
  in
  let scroll_hint = ref Hint_none in
  let report_scroll node ~(region : Grid.region) ~dx ~dy =
    (* Horizontal movement and degenerate regions cannot be expressed as a
       row shift; either disqualifies the cycle outright. *)
    if dx <> 0 || region.height <= 1 then scroll_hint := Hint_disqualified
    else
      let num = Renderable.Private.num node in
      match !scroll_hint with
      | Hint_none -> scroll_hint := Hint_one { num; region; delta = dy }
      | Hint_one one when one.num = num && one.region = region ->
          one.delta <- one.delta + dy
      | Hint_one _ | Hint_disqualified -> scroll_hint := Hint_disqualified
  in
  let size_round = { limit = -1; shallowest = max_int } in
  let admit_size_report node =
    let rec depth node acc =
      match Renderable.parent node with
      | Some parent -> depth parent (acc + 1)
      | None -> acc
    in
    let depth = depth node 0 in
    if depth > size_round.limit then begin
      size_round.shallowest <- min size_round.shallowest depth;
      true
    end
    else begin
      (* The paint that delivers the refused report must happen. *)
      if not (Atomic.exchange dirty true) then request_render ();
      false
    end
  in
  let ctx : Renderable.Private.context =
    {
      tree;
      schedule =
        (fun () -> if not (Atomic.exchange dirty true) then request_render ());
      focus =
        (fun node ->
          if not (Renderable.focusable node) then false
          else (
            (match !focused with
            | Some f when not (f == node) ->
                Renderable.Private.blur_direct f;
                focused := None
            | Some _ | None -> ());
            ignore (Renderable.Private.focus_direct node : bool);
            focused := Some node;
            true));
      blur =
        (fun node ->
          match !focused with
          | Some f when f == node ->
              Renderable.Private.blur_direct node;
              focused := None
          | _ -> ());
      get_selection = (fun () -> selection_state.current);
      request_selection_update =
        (fun () ->
          request_selection_update ~selection_state ~pointer ~screen ~node_map);
      register_lifecycle =
        (fun node ->
          Hashtbl.replace lifecycle_set (Renderable.Private.num node) node);
      unregister_lifecycle =
        (fun node -> Hashtbl.remove lifecycle_set (Renderable.Private.num node));
      alloc_num =
        (fun () ->
          let n = !next_num in
          incr next_num;
          n);
      register =
        (fun node ->
          Hashtbl.replace node_map (Renderable.Private.num node) node;
          let toffee_idx =
            Toffee.Node_id.index (Renderable.Private.toffee_node node)
          in
          Hashtbl.replace toffee_map toffee_idx node);
      unregister =
        (fun node ->
          Hashtbl.remove node_map (Renderable.Private.num node);
          let toffee_idx =
            Toffee.Node_id.index (Renderable.Private.toffee_node node)
          in
          Hashtbl.remove toffee_map toffee_idx);
      width_method = (fun () -> Grid.width_method (Screen.next_grid screen));
      report_scroll;
      admit_size_report;
    }
  in
  let root_style =
    match style with
    | Some s -> s
    | None ->
        Toffee.Style.set_size
          (Toffee.Style.Size_dim.pct ~w:100. ~h:100.)
          Toffee.Style.default
  in
  let root =
    Renderable.Private.create_root ctx ~style:root_style ~id:"root" ()
  in
  Renderable.Private.set_is_root root true;
  Hashtbl.replace node_map (Renderable.Private.num root) root;
  Hashtbl.replace toffee_map
    (Toffee.Node_id.index (Renderable.Private.toffee_node root))
    root;
  {
    screen;
    adopted;
    root;
    tree;
    node_map;
    toffee_map;
    dirty;
    last_layout = None;
    frame = None;
    focused;
    lifecycle_set;
    commands = Array.make 64 Pop_scissor;
    cmd_len = 0;
    input_geometry = None;
    hover_node = None;
    hover_num = 0;
    pointer;
    captured = None;
    selection_state;
    scroll_hint;
    frame_callbacks = [];
    size_round;
  }

(* ───── Accessors ───── *)

let root t = t.root
let screen t = t.screen
let focused t = !(t.focused)
let blur t = blur_current t
let focus t node = focus_node t node
let selection t = t.selection_state.current

let selection_text t =
  match t.selection_state.current with
  | None -> None
  | Some _ -> (
      match t.selection_state.containers with
      | [] -> None
      | container :: _ ->
          let buf = Buffer.create 256 in
          iter_selectables container (fun node ->
              let text = Renderable.Private.get_selected_text node in
              if text <> "" then Buffer.add_string buf text);
          let text = Buffer.contents buf in
          if text = "" then None else Some text)

let captured t = t.captured
let hover t = t.hover_node

(* ───── Pending Render Work ───── *)

let pending_work t =
  let rec collect acc node =
    if Renderable.destroyed node || not (Renderable.visible node) then acc
    else
      let acc =
        match Renderable.Private.pending_work node with
        | None -> acc
        | Some work -> { Pending.node; work } :: acc
      in
      Array.fold_left collect acc (Renderable.Private.children_z node)
  in
  List.rev (collect [] t.root)

let is_settled t =
  (not (Atomic.get t.dirty))
  && match pending_work t with [] -> true | _ :: _ -> false

(* ───── Measure Function ───── *)

(* O(1) lookup via toffee_map reverse index. *)
let measure_fn (t : t) (known_dimensions : float option Toffee.Geometry.Size.t)
    (available_space : Toffee.Available_space.t Toffee.Geometry.Size.t)
    (node_id : Toffee.Node_id.t) (_context : unit option)
    (style : Toffee.Style.t) : float Toffee.Geometry.Size.t =
  match Hashtbl.find_opt t.toffee_map (Toffee.Node_id.index node_id) with
  | None -> Toffee.Geometry.Size.{ width = 0.; height = 0. }
  | Some node -> (
      match Renderable.Private.measure node with
      | None -> Toffee.Geometry.Size.{ width = 0.; height = 0. }
      | Some m -> m ~known_dimensions ~available_space ~style)

(* ───── Pass 2: Layout Extraction + Command Generation ───── *)

(* Depth-first walk from a node, computing absolute positions and emitting
   render commands. [parent_x] and [parent_y] are the absolute position of the
   parent's content box origin. *)
let rec build_commands (t : t) (node : Renderable.t) ~parent_x ~parent_y
    ~(clip : Grid.region) ~frame ~(delta : float) : unit =
  if not (Renderable.visible node) then ()
  else
    let toffee_node = Renderable.Private.toffee_node node in
    let layout = toffee_exn (Toffee.layout t.tree toffee_node) in
    (* Absolute position = parent content box origin + node's relative location
       (which is relative to parent's content box). *)
    let abs_x = parent_x +. layout.location.x in
    let abs_y = parent_y +. layout.location.y in
    let abs_w = layout.size.width in
    let abs_h = layout.size.height in
    Renderable.Private.update_layout node ~x:abs_x ~y:abs_y ~width:abs_w
      ~height:abs_h;
    Renderable.Private.pre_render_update ~frame node ~delta;
    (* Opacity *)
    let opacity = Renderable.opacity node in
    let has_opacity = opacity < 1.0 in
    if has_opacity then emit_cmd t (Push_opacity opacity);
    (* Emit render command for this node *)
    emit_cmd t (Render node);
    (* Child clipping — only applied when overflow is not Visible, following
       CSS-style overflow semantics where "visible" means no clipping. *)
    let overflow = Toffee.Style.overflow (Renderable.style node) in
    let should_clip =
      overflow.x <> Toffee.Style.Overflow.Visible
      || overflow.y <> Toffee.Style.Overflow.Visible
    in
    let child_clip =
      if should_clip then Renderable.Private.child_clip node else None
    in
    let render_children child_clip =
      (* Recurse into children in z-index order. Toffee's layout.location
         already positions children relative to the parent's border box (i.e.
         it includes border+padding offsets), so we pass abs_x/abs_y directly —
         no additional inset is needed. Viewport-culling nodes (scroll-box
         content) restrict the walk to children intersecting the effective
         clip, so offscreen children skip layout extraction, frame hooks,
         rendering, and hit registration entirely. *)
      let recurse child =
        build_commands t child ~parent_x:abs_x ~parent_y:abs_y ~clip:child_clip
          ~frame ~delta
      in
      if Renderable.Private.viewport_cull node then
        Renderable.Private.iter_children_in_viewport ~parent:node
          ~viewport:child_clip recurse
      else Renderable.Private.iter_children_z node recurse
    in
    (match child_clip with
    | None -> render_children clip
    | Some child_scissor ->
        let child_clip = clip_rect_intersect clip child_scissor in
        if child_clip.width > 0 && child_clip.height > 0 then begin
          emit_cmd t (Push_scissor child_scissor);
          render_children child_clip;
          emit_cmd t Pop_scissor
        end);
    if has_opacity then emit_cmd t Pop_opacity

(* ───── Pass 3: Execute Commands ───── *)

let execute_commands (t : t) ~(grid : Grid.t) ~(hits : Screen.Hit_grid.t)
    ~(delta : float) : unit =
  for i = 0 to t.cmd_len - 1 do
    match t.commands.(i) with
    | Render node ->
        Renderable.Private.render_full node ~grid ~delta;
        let bounds = Renderable.bounds node in
        Screen.Hit_grid.add hits ~x:bounds.x ~y:bounds.y ~width:bounds.width
          ~height:bounds.height
          ~id:(Renderable.Private.num node)
    | Push_scissor clip ->
        Grid.push_clip grid clip;
        Screen.Hit_grid.push_clip hits clip
    | Pop_scissor ->
        Grid.pop_clip grid;
        Screen.Hit_grid.pop_clip hits
    | Push_opacity opacity -> Grid.push_opacity grid opacity
    | Pop_opacity -> Grid.pop_opacity grid
  done

(* ───── Render Frame ───── *)

let layout_dirty t =
  toffee_exn (Toffee.dirty t.tree (Renderable.Private.toffee_node t.root))

let compute_layout t ~width ~height =
  let root_toffee = Renderable.Private.toffee_node t.root in
  let needs_layout =
    (match t.last_layout with
      | Some (w, h) -> w <> width || h <> height
      | None -> true)
    ||
    match Toffee.dirty t.tree root_toffee with
    | Ok dirty -> dirty
    | Error _ -> true
  in
  if needs_layout then begin
    let available_space =
      Toffee.Geometry.Size.
        {
          width = Toffee.Available_space.Definite (Float.of_int width);
          height = Toffee.Available_space.Definite (Float.of_int height);
        }
    in
    toffee_exn
      (Toffee.compute_layout_with_measure t.tree root_toffee available_space
         (measure_fn t));
    t.last_layout <- Some (width, height)
  end

(* A layout callback may resize a sibling (for example, reveal a scrollbar).
   Resolve those dependencies before hit testing or painting. Animations run
   only on the first pass of a frame, never while preparing input geometry. *)
let prepare_commands t ~width ~height ~frame ~delta =
  let clip : Grid.region = { x = 0; y = 0; width; height } in
  let rec settle remaining frame delta =
    compute_layout t ~width ~height;
    t.cmd_len <- 0;
    build_commands t t.root ~parent_x:0. ~parent_y:0. ~clip ~frame ~delta;
    if layout_dirty t then begin
      if remaining = 0 then
        invalid_arg "Mosaic.Renderer: layout callbacks did not converge";
      settle (remaining - 1) false 0.
    end
  in
  settle 16 frame delta

let prepare_layout t ~width ~height =
  t.frame <- None;
  t.size_round.shallowest <- max_int;
  prepare_commands t ~width ~height ~frame:false ~delta:0.;
  if t.size_round.shallowest < max_int then
    t.size_round.limit <- t.size_round.shallowest;
  t.input_geometry <- Some (width, height)

let layout_frame t ~width ~height ~delta =
  t.input_geometry <- None;
  (* A frame starts a new round of size reports and admits every pending one,
     including those its layout passes refused. *)
  t.size_round.limit <- -1;
  (* Consume the request that led to this pass before running any code that can
     schedule its successor. In particular, widgets may discover geometry in
     their render function and change a sibling's visibility after the command
     list has already been built. That change must survive for another pass. *)
  Atomic.set t.dirty false;
  (* Pass 0: Lifecycle passes *)
  Hashtbl.iter
    (fun _num node -> Renderable.Private.run_lifecycle_pass node)
    t.lifecycle_set;
  (* Frame callbacks *)
  List.iter (fun f -> f delta) t.frame_callbacks;
  prepare_commands t ~width ~height ~frame:true ~delta;
  t.frame <- Some { width; height; delta }

let paint_frame t ~height =
  let frame =
    match t.frame with
    | Some frame -> frame
    | None -> invalid_arg "Renderer.paint_frame: no frame is laid out"
  in
  t.frame <- None;
  let width = frame.width in
  (* A paint owns the complete next frame, including blank cells:
     Screen.build guarantees cleared buffers before drawing, so when
     settlement performs another pass before presenting the first one,
     transparent text blanks do not preserve glyphs drawn by that superseded
     pass. *)
  let rec paint remaining delta =
    Screen.build t.screen ~width ~height (fun grid hits ->
        Screen.Hit_grid.clear_clip hits;
        execute_commands t ~grid ~hits ~delta);
    if layout_dirty t then begin
      if remaining = 0 then
        invalid_arg "Mosaic.Renderer: rendering repeatedly invalidated layout";
      prepare_commands t ~width ~height:frame.height ~frame:false ~delta:0.;
      paint (remaining - 1) 0.
    end
  in
  paint 16 frame.delta;
  (* Update cursor from focused node. Visibility is derived from renderable
     intent — a cursor is visible iff a focused renderable exposes one — so
     the state is authoritative regardless of the screen's previous policy
     (a host runtime's screen starts hidden). *)
  (match !(t.focused) with
  | Some node when Renderable.focused node -> (
      match Renderable.cursor node with
      | Some c ->
          let r, g, b = Ansi.Color.to_rgb c.color in
          Screen.set_cursor t.screen
            {
              position = Some (c.x, c.y);
              style = c.style;
              blinking = c.blinking;
              color = Some (r, g, b);
              visible = true;
            }
      | None ->
          let cursor = Screen.cursor t.screen in
          Screen.set_cursor t.screen
            { cursor with position = None; visible = false })
  | _ ->
      let cursor = Screen.cursor t.screen in
      Screen.set_cursor t.screen
        { cursor with position = None; visible = false });
  recheck_hover t

let render_frame t ~width ~height ~delta =
  layout_frame t ~width ~height ~delta;
  paint_frame t ~height

(* A hint is only forwarded when the scrolled region owns the full grid
   width: DECSTBM shifts entire terminal rows, so a narrower container would
   drag unrelated side-by-side content and the diff would rewrite it anyway.
   A shift of the whole region height reveals every row, which the plain diff
   handles just as well. *)
let take_scroll_hint t =
  let hint = !(t.scroll_hint) in
  t.scroll_hint := Hint_none;
  match hint with
  | Hint_one { region; delta; _ }
    when delta <> 0
         && abs delta < region.height
         && region.x <= 0
         && region.x + region.width >= Grid.width (Screen.next_grid t.screen) ->
      Some
        { Screen.top = region.y; bottom = region.y + region.height - 1; delta }
  | Hint_none | Hint_one _ | Hint_disqualified -> None

let render ?full t =
  if t.adopted then
    invalid_arg
      "Renderer.render: this renderer adopted a host screen; presentation \
       belongs to the host runtime";
  let scroll_hint = take_scroll_hint t in
  Screen.render ?full ?scroll_hint t.screen

let needs_render t =
  Atomic.get t.dirty || Renderable.Private.live_count t.root > 0

let render_frame_until_settled ?(max_passes = 4) t ~width ~height ~delta =
  let max_passes = max 1 max_passes in
  let rec loop remaining =
    render_frame t ~width ~height ~delta;
    if is_settled t then `Settled
    else if remaining = 1 then `Pending (pending_work t)
    else loop (remaining - 1)
  in
  loop max_passes

(* ───── Event Dispatch ───── *)

let dispatch_key ?(capture = Fun.const ()) t (key : Input.Key.event) =
  let ev = Event.Key.of_input key in
  capture ev;
  (match !(t.focused) with
  | None -> ()
  | Some node when not (Event.Key.propagation_stopped ev) ->
      Renderable.Private.emit_key node ev;
      if
        (not (Renderable.destroyed node))
        && Renderable.focused node
        && not (Event.Key.default_prevented ev)
      then Renderable.Private.emit_default_key node ev
  | Some _ -> ());
  ev

let rec ancestor_of ancestor node =
  ancestor == node
  ||
  match Renderable.parent node with
  | None -> false
  | Some parent -> ancestor_of ancestor parent

let dispatch_click t pressed event =
  if alive_visible pressed then
    let hit =
      query_input_hit t ~x:(Event.Mouse.x event) ~y:(Event.Mouse.y event)
    in
    let rec bubble node =
      if
        alive_visible node
        && (not (Event.Mouse.default_prevented event))
        && not (Event.Mouse.propagation_stopped event)
      then begin
        if ancestor_of node pressed then
          Renderable.Private.emit_click node event;
        if alive_visible node then Option.iter bubble (Renderable.parent node)
      end
    in
    if hit > 0 then Option.iter bubble (find_node t hit)

let dispatch_mouse t (mouse : Input.Mouse.event) =
  let kind =
    match mouse.kind with
    | Input.Mouse.Down { button } ->
        Event.Mouse.Down { button = map_button button }
    | Input.Mouse.Up { button } ->
        let button =
          Option.fold ~none:(Event.Mouse.Button 0) ~some:map_button button
        in
        Event.Mouse.Up { button; is_dragging = false }
    | Input.Mouse.Move -> Event.Mouse.Move
    | Input.Mouse.Drag { button } ->
        Event.Mouse.Drag { button = map_button button; is_dragging = true }
    | Input.Mouse.Scroll { direction; delta } ->
        Event.Mouse.Scroll { direction; delta }
  in
  let pressed = t.pointer.press_target in
  (match kind with
  | Event.Mouse.Down _ | Event.Mouse.Drag _
  | Event.Mouse.Up { button = Left | Button 0; _ } ->
      t.pointer.press_target <- None
  | _ -> ());
  let event =
    dispatch_mouse_internal t ~x:mouse.x ~y:mouse.y ~modifiers:mouse.modifiers
      kind
  in
  if not (Event.Mouse.default_prevented event) then
    begin match kind with
    | Event.Mouse.Down { button = Left } -> (
        match Event.Mouse.target event with
        | Some num when num = query_input_hit t ~x:mouse.x ~y:mouse.y -> (
            match find_node t num with
            | Some node when alive_visible node ->
                t.pointer.press_target <- Some node
            | _ -> ())
        | _ -> ())
    | Event.Mouse.Up { button = Left | Button 0; _ } ->
        Option.iter (fun node -> dispatch_click t node event) pressed
    | _ -> ()
    end;
  event

let dispatch_paste ?(capture = Fun.const ()) t text =
  let ev = Event.Paste.of_text text in
  capture ev;
  (match !(t.focused) with
  | None -> ()
  | Some node when not (Event.Paste.propagation_stopped ev) ->
      Renderable.Private.emit_paste node ev
  | Some _ -> ());
  ev

(* ───── Selection ───── *)

let clear_selection t = clear_active_selection t

(* ───── Frame Callbacks ───── *)

let add_frame_callback t f = t.frame_callbacks <- t.frame_callbacks @ [ f ]

let remove_frame_callback t f =
  t.frame_callbacks <- List.filter (fun e -> not (e == f)) t.frame_callbacks

let clear_frame_callbacks t = t.frame_callbacks <- []
