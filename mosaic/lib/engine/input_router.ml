module Attr = Ui.Attr
module Layout_snapshot = Ui.Layout_snapshot

let src = Logs.Src.create "input" ~doc:"Input handling events"

module Log = (val Logs.src_log src : Logs.LOG)

type mouse_event =
  | Button_down of Input.mouse_button
  | Button_up of Input.mouse_button
  | Motion

let pp_mouse_event ppf = function
  | Button_down b ->
      Format.fprintf ppf "Button_down(%a)" Input.pp_mouse_button b
  | Button_up b -> Format.fprintf ppf "Button_up(%a)" Input.pp_mouse_button b
  | Motion -> Format.fprintf ppf "Motion"

type phase = [ `Start | `Move | `End ]

let pp_phase ppf = function
  | `Start -> Format.fprintf ppf "Start"
  | `Move -> Format.fprintf ppf "Move"
  | `End -> Format.fprintf ppf "End"

type drag_event = {
  x : int;
  y : int;
  phase : phase;
  start_x : int;
  start_y : int;
  dx : int;
  dy : int;
}

let pp_drag_event ppf evt =
  Format.fprintf ppf
    "DragEvent { x=%d; y=%d; phase=%a; start=(%d,%d); delta=(%d,%d) }" evt.x
    evt.y pp_phase evt.phase evt.start_x evt.start_y evt.dx evt.dy

type handler =
  | Click of Attr.key * (unit -> unit)
  | Hover of Attr.key * (bool -> unit)
  | Drag of Attr.key * (drag_event -> unit)
  | Focus of Attr.key * (bool -> unit)
  | KeyPress of Attr.key * (Input.key_event -> unit)
  | Scroll of Attr.key * (int -> unit)
(* delta: positive for down, negative for up *)

type subscription_id = int

type handlers = {
  mutable clicks : (subscription_id * (unit -> unit)) list;
  mutable hovers : (subscription_id * (bool -> unit)) list;
  mutable drags : (subscription_id * (drag_event -> unit)) list;
  mutable focuses : (subscription_id * (bool -> unit)) list;
  mutable keypresses : (subscription_id * (Input.key_event -> unit)) list;
  mutable scrolls : (subscription_id * (int -> unit)) list;
}

let create_handlers () =
  {
    clicks = [];
    hovers = [];
    drags = [];
    focuses = [];
    keypresses = [];
    scrolls = [];
  }

module Key_tbl = Hashtbl.Make (struct
  type t = Attr.key

  let equal = ( = )
  let hash = Hashtbl.hash
end)

type t = {
  mutable snapshot : Layout_snapshot.t option;
  by_key : handlers Key_tbl.t;
  mutable next_id : int;
  mutable hovered : Attr.key option;
  mutable dragging : (Attr.key * int * int) option;
  mutable focused : Attr.key option;
}

let create () =
  {
    snapshot = None;
    by_key = Key_tbl.create 64;
    next_id = 0;
    hovered = None;
    dragging = None;
    focused = None;
  }

let set_snapshot t snapshot = t.snapshot <- Some snapshot

let subscribe t handler =
  let id = t.next_id in
  t.next_id <- t.next_id + 1;

  let get_or_create_handlers key =
    match Key_tbl.find_opt t.by_key key with
    | Some h -> h
    | None ->
        let h = create_handlers () in
        Key_tbl.add t.by_key key h;
        h
  in

  (match handler with
  | Click (key, f) ->
      let handlers = get_or_create_handlers key in
      handlers.clicks <- (id, f) :: handlers.clicks
  | Hover (key, f) ->
      let handlers = get_or_create_handlers key in
      handlers.hovers <- (id, f) :: handlers.hovers
  | Drag (key, f) ->
      let handlers = get_or_create_handlers key in
      handlers.drags <- (id, f) :: handlers.drags
  | Focus (key, f) ->
      let handlers = get_or_create_handlers key in
      handlers.focuses <- (id, f) :: handlers.focuses
  | KeyPress (key, f) ->
      let handlers = get_or_create_handlers key in
      handlers.keypresses <- (id, f) :: handlers.keypresses
  | Scroll (key, f) ->
      let handlers = get_or_create_handlers key in
      handlers.scrolls <- (id, f) :: handlers.scrolls);
  id

let unsubscribe t id =
  Key_tbl.iter
    (fun _key handlers ->
      handlers.clicks <- List.filter (fun (sid, _) -> sid <> id) handlers.clicks;
      handlers.hovers <- List.filter (fun (sid, _) -> sid <> id) handlers.hovers;
      handlers.drags <- List.filter (fun (sid, _) -> sid <> id) handlers.drags;
      handlers.focuses <-
        List.filter (fun (sid, _) -> sid <> id) handlers.focuses;
      handlers.keypresses <-
        List.filter (fun (sid, _) -> sid <> id) handlers.keypresses;
      handlers.scrolls <-
        List.filter (fun (sid, _) -> sid <> id) handlers.scrolls)
    t.by_key

let get_hovered t = t.hovered

let get_dragging t =
  match t.dragging with Some (key, _, _) -> Some key | None -> None

let set_focused t key =
  let old_focused = t.focused in
  t.focused <- key;

  (match old_focused with
  | Some old_key when Some old_key <> key -> (
      match Key_tbl.find_opt t.by_key old_key with
      | Some handlers -> List.iter (fun (_, f) -> f false) handlers.focuses
      | None -> ())
  | _ -> ());

  match key with
  | Some new_key -> (
      match Key_tbl.find_opt t.by_key new_key with
      | Some handlers -> List.iter (fun (_, f) -> f true) handlers.focuses
      | None -> ())
  | None -> ()

let get_focused t = t.focused

let reset t =
  t.hovered <- None;
  t.dragging <- None;
  t.focused <- None

let on_mouse t ~x ~y event =
  Log.debug (fun m -> m "Mouse event at (%d,%d): %a" x y pp_mouse_event event);
  match t.snapshot with
  | None ->
      Log.debug (fun m -> m "No snapshot available for mouse event routing");
      ()
  | Some snapshot -> (
      Log.debug (fun m -> m "Routing mouse event through snapshot");
      match event with
      | Button_down b when List.mem b [ Input.Left; Input.Right; Input.Middle ]
        -> (
          match Layout_snapshot.hit_test snapshot ~x ~y with
          | Some key -> (
              t.dragging <- Some (key, x, y);

              (match Key_tbl.find_opt t.by_key key with
              | Some handlers when handlers.focuses <> [] ->
                  set_focused t (Some key)
              | _ -> ());

              let evt =
                {
                  x;
                  y;
                  phase = `Start;
                  start_x = x;
                  start_y = y;
                  dx = 0;
                  dy = 0;
                }
              in
              match Key_tbl.find_opt t.by_key key with
              | Some handlers -> List.iter (fun (_, f) -> f evt) handlers.drags
              | None -> ())
          | None -> ())
      | Button_down Input.Wheel_up -> (
          match Layout_snapshot.hit_test snapshot ~x ~y with
          | Some key -> (
              match Key_tbl.find_opt t.by_key key with
              | Some handlers ->
                  List.iter (fun (_, f) -> f (-3)) handlers.scrolls
              | None -> ())
          | None -> ())
      | Button_down Input.Wheel_down -> (
          match Layout_snapshot.hit_test snapshot ~x ~y with
          | Some key -> (
              match Key_tbl.find_opt t.by_key key with
              | Some handlers -> List.iter (fun (_, f) -> f 3) handlers.scrolls
              | None -> ())
          | None -> ())
      | Button_up b when List.mem b [ Input.Left; Input.Right; Input.Middle ] ->
          (match t.dragging with
          | Some (drag_key, start_x, start_y) -> (
              let evt =
                {
                  x;
                  y;
                  phase = `End;
                  start_x;
                  start_y;
                  dx = x - start_x;
                  dy = y - start_y;
                }
              in
              (match Key_tbl.find_opt t.by_key drag_key with
              | Some handlers -> List.iter (fun (_, f) -> f evt) handlers.drags
              | None -> ());

              match Layout_snapshot.hit_test snapshot ~x ~y with
              | Some key when key = drag_key -> (
                  match Key_tbl.find_opt t.by_key key with
                  | Some handlers ->
                      Log.info (fun m ->
                          m "Click event delivered to component with key");
                      List.iter (fun (_, f) -> f ()) handlers.clicks
                  | None ->
                      Log.debug (fun m ->
                          m "Click event on component with no click handler"))
              | _ -> ())
          | None -> ());
          t.dragging <- None
      | Button_down _ -> () (* Other button types not handled *)
      | Button_up _ -> () (* Button up without dragging - ignore *)
      | Motion -> (
          (* Handle dragging immediately - drag events need real-time feedback *)
          (match t.dragging with
          | Some (key, start_x, start_y) -> (
              let evt =
                {
                  x;
                  y;
                  phase = `Move;
                  start_x;
                  start_y;
                  dx = x - start_x;
                  dy = y - start_y;
                }
              in
              match Key_tbl.find_opt t.by_key key with
              | Some handlers -> List.iter (fun (_, f) -> f evt) handlers.drags
              | None -> ())
          | None -> ());

          (* Process hover state changes immediately without debouncing *)
          match Layout_snapshot.hit_test snapshot ~x ~y with
          | Some key ->
              let prev_hovered = t.hovered in
              if Some key <> prev_hovered then (
                t.hovered <- Some key;

                (match prev_hovered with
                | Some prev_key -> (
                    match Key_tbl.find_opt t.by_key prev_key with
                    | Some handlers ->
                        List.iter (fun (_, f) -> f false) handlers.hovers
                    | None -> ())
                | None -> ());

                match Key_tbl.find_opt t.by_key key with
                | Some handlers ->
                    List.iter (fun (_, f) -> f true) handlers.hovers
                | None -> ())
          | None -> (
              match t.hovered with
              | Some prev_key -> (
                  t.hovered <- None;
                  match Key_tbl.find_opt t.by_key prev_key with
                  | Some handlers ->
                      List.iter (fun (_, f) -> f false) handlers.hovers
                  | None -> ())
              | None -> ())))

let on_keyboard t event =
  Log.debug (fun m -> m "Keyboard event: %a" Input.pp_key_event event);
  match t.focused with
  | Some key -> (
      match Key_tbl.find_opt t.by_key key with
      | Some handlers -> List.iter (fun (_, f) -> f event) handlers.keypresses
      | None -> ())
  | None -> ()
