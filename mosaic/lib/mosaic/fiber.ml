module Renderable = Mosaic_ui.Renderable

(* Child type - unified representation preserving order of fibers and raw
   nodes *)
type child = C_fiber of t | C_raw of Renderable.t

(* Fiber Type - simplified to use children list instead of sibling pointers *)
and t = {
  tag : Vnode.tag;
  key : string option;
  mutable props : unit Vnode.props;
  instance : Host_config.instance;
  mutable child_list : child list;
  (* Handler refs - allow updating handlers without re-registering *)
  mouse_handler_ref : (Mosaic_ui.Event.mouse -> unit) option ref;
  key_handler_ref : (Mosaic_ui.Event.key -> unit) option ref;
  paste_handler_ref : (Mosaic_ui.Event.paste -> unit) option ref;
}

(* Creation *)

let create ~tag ~key ~props ~instance : t =
  let mouse_handler_ref = ref props.Vnode.handlers.on_mouse in
  let key_handler_ref = ref props.Vnode.handlers.on_key in
  let paste_handler_ref = ref props.Vnode.handlers.on_paste in
  let node = Host_config.node_of instance in
  (* Register handlers exactly once that dispatch through refs *)
  Renderable.on_mouse node (fun ev ->
      match !mouse_handler_ref with Some h -> h ev | None -> ());
  Renderable.on_key_down node (fun ev ->
      match !key_handler_ref with Some h -> h ev | None -> ());
  Renderable.on_paste node (fun ev ->
      match !paste_handler_ref with Some h -> h ev | None -> ());
  {
    tag;
    key;
    props;
    instance;
    child_list = [];
    mouse_handler_ref;
    key_handler_ref;
    paste_handler_ref;
  }

(* Accessors *)

let instance fiber = fiber.instance
let node fiber = Host_config.node_of fiber.instance
let tag fiber = fiber.tag
let key fiber = fiber.key
let props fiber = fiber.props
let child_list fiber = fiber.child_list

(* Extract only fiber children (for backward compat with reconciler) *)
let children fiber =
  List.filter_map
    (function C_fiber f -> Some f | C_raw _ -> None)
    fiber.child_list

(* Mutations *)

let set_child_list fiber new_children = fiber.child_list <- new_children

let update_props fiber new_props =
  fiber.props <- new_props;
  (* Update handler refs so registered handlers pick up new behavior *)
  fiber.mouse_handler_ref := new_props.Vnode.handlers.on_mouse;
  fiber.key_handler_ref := new_props.Vnode.handlers.on_key;
  fiber.paste_handler_ref := new_props.Vnode.handlers.on_paste

(* Destruction - use remove (not detach) to properly destroy the node *)

let rec destroy fiber =
  (* Destroy fiber children first (depth-first), detach raw children *)
  List.iter
    (function
      | C_fiber f -> destroy f | C_raw n -> Host_config.detach_if_attached n)
    fiber.child_list;
  (* Remove from renderable tree - this actually destroys the node *)
  Host_config.remove_child (node fiber)
