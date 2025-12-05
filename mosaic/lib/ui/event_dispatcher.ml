type 'a event_handler = 'a -> unit
type handler_id = int

type t = {
  mutable next_handler_id : int;
  mutable global_key_handlers : (handler_id * Event.key event_handler) list;
  mutable global_paste_handlers : (handler_id * Event.paste event_handler) list;
  mutable focused_renderable : Renderable.t option;
}

let create () =
  {
    next_handler_id = 1;
    global_key_handlers = [];
    global_paste_handlers = [];
    focused_renderable = None;
  }

let alloc_handler_id t =
  let id = t.next_handler_id in
  t.next_handler_id <- id + 1;
  id

let add_global_key_handler t handler =
  let id = alloc_handler_id t in
  t.global_key_handlers <- (id, handler) :: t.global_key_handlers;
  id

let remove_global_key_handler t id =
  t.global_key_handlers <-
    List.filter (fun (hid, _) -> hid <> id) t.global_key_handlers

let add_global_paste_handler t handler =
  let id = alloc_handler_id t in
  t.global_paste_handlers <- (id, handler) :: t.global_paste_handlers;
  id

let remove_global_paste_handler t id =
  t.global_paste_handlers <-
    List.filter (fun (hid, _) -> hid <> id) t.global_paste_handlers

let set_focused_renderable t renderable = t.focused_renderable <- renderable

let dispatch_key t (event : Event.key) =
  (* Tier 1: Global key handlers *)
  List.iter (fun (_, handler) -> handler event) t.global_key_handlers;

  (* Only proceed if global handlers didn't prevent default *)
  if not (Event.Key.default_prevented event) then
    match t.focused_renderable with
    | Some renderable ->
        (* Tier 2: onKeyDown handlers *)
        Renderable.Internal.emit_key_event renderable event;

        (* Only proceed if onKeyDown didn't prevent default *)
        if not (Event.Key.default_prevented event) then
          (* Tier 3: Default handleKeyPress handler *)
          Renderable.Internal.emit_default_key_event renderable event
    | None -> ()

let dispatch_paste t (event : Event.paste) =
  (* Tier 1: Global paste handlers *)
  List.iter (fun (_, handler) -> handler event) t.global_paste_handlers;

  (* Only proceed if global handlers didn't prevent default *)
  if not (Event.Paste.default_prevented event) then
    match t.focused_renderable with
    | Some renderable ->
        (* Tier 2: Focused renderable paste handler *)
        Renderable.Internal.emit_paste_event renderable event
    | None -> ()
