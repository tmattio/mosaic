(** Event dispatcher with tiered handler execution.

    This module is used internally by {!Renderer} to implement keyboard and
    paste routing.

    Events flow through multiple tiers where each tier can prevent subsequent
    tiers by calling [Event.Key.prevent_default] or
    [Event.Paste.prevent_default].

    {2 Keyboard Event Tiers}

    - {b Tier 1}: Global key handlers
    - {b Tier 2}: Focused renderable's key handlers
    - {b Tier 3}: Focused renderable's default key handler

    {2 Paste Event Tiers}

    - {b Tier 1}: Global paste handlers
    - {b Tier 2}: Focused renderable's paste handler

    If no renderable is focused, only Tier 1 executes. Global handlers execute
    in reverse registration order (newest first). *)

type 'a event_handler = 'a -> unit
(** Event handler function. *)

type handler_id
(** Opaque identifier for removing registered handlers. *)

type t
(** Event dispatcher managing global handlers and focused renderable routing. *)

val create : unit -> t
(** [create ()] creates a dispatcher with no handlers and no focus. *)

val add_global_key_handler : t -> Event.key event_handler -> handler_id
(** [add_global_key_handler t handler] registers a Tier 1 key handler. *)

val remove_global_key_handler : t -> handler_id -> unit
(** [remove_global_key_handler t id] unregisters a key handler. No-op if [id] is
    invalid. *)

val add_global_paste_handler : t -> Event.paste event_handler -> handler_id
(** [add_global_paste_handler t handler] registers a Tier 1 paste handler. *)

val remove_global_paste_handler : t -> handler_id -> unit
(** [remove_global_paste_handler t id] unregisters a paste handler. No-op if
    [id] is invalid. *)

val set_focused_renderable : t -> Renderable.t option -> unit
(** [set_focused_renderable t r] sets which renderable receives Tier 2/3 events.
    [None] clears focus. *)

val dispatch_key : t -> Event.key -> unit
(** [dispatch_key t event] dispatches through all three keyboard tiers. *)

val dispatch_paste : t -> Event.paste -> unit
(** [dispatch_paste t event] dispatches through both paste tiers. *)
