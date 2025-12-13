(** Renderer orchestrating layout, rendering, and event dispatch.

    Renderer manages the entire rendering pipeline, from Toffee-based layout
    computation to efficient screen rendering with hit testing for mouse
    interactions. It coordinates focus management, text selection, and event
    bubbling across the renderable tree.

    {1 Overview}

    The renderer serves as the central coordinator for terminal UI rendering:
    - Manages a root renderable and its descendants
    - Computes layout using Toffee's flexbox engine
    - Renders visible nodes into a grid buffer
    - Handles mouse, keyboard, and paste events
    - Maintains focus state and text selection
    - Provides hit testing for mouse interaction

    All mutations are deferred until {!render_frame} executes, enabling
    efficient batching of layout and rendering operations.

    {1 Rendering Pipeline}

    Each frame follows this sequence: 1. Run lifecycle passes for nodes
    requesting updates 2. Compute layout if dirty (size changes or explicit
    marks) 3. Blur invisible focused nodes 4. Render visible nodes depth-first
    with clipping 5. Generate differential ANSI output via {!Screen} 6. Update
    selection highlighting if active

    Layout computation is skipped when no nodes are dirty and the renderer size
    is unchanged. This makes static UIs zero-cost between renders.

    {1 Event Dispatch}

    Mouse events bubble from the hit node up through ancestors until
    {!Event.Mouse.stop_propagation} is called or the root is reached.
    {!Event.Mouse.prevent_default} affects renderer behaviors (like clearing
    selection) without stopping bubbling.

    Key events run in three tiers: 1. Global key handlers (first registered,
    first executed) 2. Focused renderable's key handlers 3. Focused renderable's
    default key handler

    Any handler can call {!Event.Key.prevent_default} to stop subsequent tiers.

    Paste events run global handlers first, then the focused renderable's paste
    handler. {!Event.Paste.prevent_default} stops delivery to the focused
    renderable.

    {1 Selection}

    Text selection is managed automatically when mouse-down occurs on a
    selectable node. The renderer tracks anchor and focus points, updating
    touched nodes on drag. Selection containers bubble outward when the pointer
    moves outside the current container.

    Selection clearing happens on mouse-down unless
    {!Event.Mouse.prevent_default} is called.

    {1 Live Regions}

    Nodes marked as [live] in their props trigger {!on_live_change} callbacks
    when the aggregate live count transitions between zero and non-zero. This
    enables external systems to react to active animations or async operations.
*)

type t
(** Renderer state managing layout, rendering, focus, and event dispatch. *)

type error =
  | Layout_error of Toffee.Error.t
      (** Layout computation failed due to invalid style or node structure. *)
  | Tree_mismatch  (** Node does not belong to this renderer's layout tree. *)
  | Root_already_assigned
      (** Attempted to set root when one is already assigned. *)

type handler_id
(** Opaque subscription token for global event handlers. *)

(** {1 Construction} *)

val create :
  ?glyph_pool:Glyph.pool ->
  ?width_method:Glyph.width_method ->
  ?respect_alpha:bool ->
  ?mouse_enabled:bool ->
  ?cursor_visible:bool ->
  ?auto_focus_on_mouse_down:bool ->
  ?explicit_width:bool ->
  unit ->
  t
(** [create ()] creates a renderer with default configuration.

    Optional parameters:
    - [glyph_pool]: Shared glyph pool for character storage (defaults to a new
      pool)
    - [width_method]: Character width computation method (defaults to
      [`Wcwidth])
    - [respect_alpha]: Enable alpha blending for semi-transparent colors
      (defaults to [true])
    - [mouse_enabled]: Include mouse metadata in ANSI output (defaults to
      [true])
    - [cursor_visible]: Show terminal cursor (defaults to [true])
    - [auto_focus_on_mouse_down]: Automatically focus clicked focusable nodes
      (defaults to [true])
    - [explicit_width]: Use explicit width values for ANSI sequences (defaults
      to [false])

    The renderer starts with no root, zero dimensions, and no dirty state. Call
    {!resize} before rendering to set terminal dimensions.

    @raise Invalid_argument if [width_method] is unsupported by the platform. *)

(** {1 Root Management} *)

val root : t -> Renderable.t option
(** [root t] returns the current root renderable, if any.

    The root defines the top-level layout container. All rendering begins from
    the root and traverses its descendants. *)

val clear_root : t -> unit
(** [clear_root t] removes the root association.

    This unsets the root's [is_root] flag, clears its live count change
    callback, sets live state to [false], and marks the renderer dirty.
    Subsequent renders produce no output until a new root is set. *)

val set_root : t -> Renderable.t -> (unit, error) result
(** [set_root t node] assigns [node] as the root renderable.

    Preconditions:
    - No root is currently assigned.
    - [node] belongs to this renderer's layout tree.

    Postconditions:
    - [node.is_root] is [true].
    - Live count changes propagate to {!on_live_change} callbacks.
    - Renderer is marked dirty.

    @return [Error Root_already_assigned] if a root is already set.
    @return [Error Tree_mismatch] if [node] is not in this renderer's tree. *)

(** {1 Node Creation} *)

val create_node :
  t ->
  ?id:string ->
  ?props:Renderable.Props.t ->
  ?on_frame:(Renderable.t -> delta:float -> unit) ->
  ?on_size_change:(Renderable.t -> unit) ->
  ?render:Renderable.render ->
  unit ->
  (Renderable.t, Renderable.error) result
(** [create_node t ()] creates a detached renderable on this renderer's layout
    tree.

    The node is fully wired to the renderer's scheduling, focus, and lifecycle
    systems. It must be attached via {!Renderable.append_child} or similar to
    appear in the rendered output.

    Optional parameters:
    - [id]: Unique identifier (defaults to a generated ID via {!gen_id})
    - [props]: Styling and visibility properties
    - [on_frame]: Frame callback invoked during lifecycle passes
    - [on_size_change]: Callback invoked when layout dimensions change
    - [render]: Custom render function for this node

    @return [Error (Layout_error _)] if Toffee node creation fails. *)

val gen_id : t -> string
(** [gen_id t] generates a unique stable ID for node identification.

    IDs are of the form ["_fiber_N"] where [N] is a monotonically increasing
    integer. IDs remain stable across re-renders but are not preserved across
    renderer instances. *)

(** {1 Sizing and Rendering} *)

val resize : t -> width:int -> height:int -> (unit, error) result
(** [resize t ~width ~height] updates terminal dimensions and marks dirty.

    Negative dimensions are clamped to zero. Resizing clears mouse capture,
    hover state, cached frames, and hit grids. Pointer coordinates reset to (0,
    0) only when dimensions actually change.

    This does not trigger an immediate render; call {!render_frame} to produce
    updated output. *)

val request_render : t -> unit
(** [request_render t] marks the renderer dirty unless currently rendering.

    Requests during render are ignored to prevent infinite loops. Use this to
    schedule a frame when state changes outside the normal event flow. *)

val needs_render : t -> bool
(** [needs_render t] returns [true] if the renderer is marked dirty.

    A dirty renderer will recompute layout and redraw nodes on the next
    {!render_frame} call. *)

val render_frame : t -> delta:float -> string
(** [render_frame t ~delta] executes the rendering pipeline and returns ANSI
    output.

    This runs lifecycle passes, computes layout if needed, blurs invisible
    focused nodes, renders all visible nodes into the screen buffer, and
    generates differential ANSI sequences.

    The [delta] parameter is the time elapsed since the last frame in seconds,
    passed to node [on_frame] callbacks and post-processors.

    Postconditions:
    - Dirty flag is cleared.
    - Layout cache is updated if layout was computed.
    - Last frame and hit grid are updated.
    - Selection highlighting is refreshed if active.

    The returned string is empty if the frame is unchanged from the last render
    (differential rendering). Write it to stdout to update the terminal. *)

val snapshot_frame : t -> delta:float -> string
(** [snapshot_frame t ~delta] renders a snapshot without differential output.

    Identical to {!render_frame} except it returns a full frame snapshot via
    {!Grid.snapshot} instead of differential ANSI sequences. This is useful for
    capturing the complete rendered state for testing or debugging.

    The returned string contains all cell data, not just changes from the
    previous frame. *)

val render_into :
  t ->
  Grid.t ->
  Screen.Hit_grid.t ->
  delta:float ->
  Renderable.hardware_cursor option
(** [render_into t grid hits ~delta] renders directly into external buffers.

    This executes lifecycle passes, computes layout, and renders nodes into the
    provided [grid] and [hits] buffers without generating ANSI output. The
    buffers are not cleared; nodes render on top of existing content. Returns
    the resolved hardware cursor for the currently focused node (if any) with
    coordinates clamped to the provided [grid].

    Use this to compose renderer output into larger grids or when integrating
    with custom rendering pipelines. *)

val query_hit : t -> x:int -> y:int -> int
(** [query_hit t ~x ~y] returns the numeric ID of the node at screen
    coordinates.

    Returns the node number from the most recent render's hit grid. Returns [0]
    if no node occupies the position or no frame has been rendered.

    The returned ID can be resolved to a renderable via {!find_by_number}. *)

(** {1 Input Handling} *)

val handle_mouse : t -> Event.mouse -> unit
(** [handle_mouse t event] routes mouse events through the dispatch pipeline.

    Behavior by event kind:

    - [Down]: Handles auto-focus (if enabled), starts selection on selectable
      nodes (if left button, no selection active, and not ctrl), or continues
      selection if ctrl is held with active selection.

    - [Drag]: Updates selection focus if selecting, or captures the target node
      for left button drags. Emits hover state changes ([Over]/[Out] events).

    - [Up] or [Drag_end]: Ends selection if selecting, or releases capture and
      emits [Drag_end] to the captured node and [Drop] to the current target.

    - [Move]: Updates hover state ([Over]/[Out] events) as the pointer moves.

    - [Scroll]: Dispatches directly to the hit node with no bubbling.

    Mouse events bubble from the hit node upward until
    {!Event.Mouse.stop_propagation} is called or the root is reached.
    {!Event.Mouse.prevent_default} affects renderer behaviors (like clearing
    selection on mouse-down) but does not stop bubbling.

    Selection is automatically cleared on mouse-down if no default prevention
    occurs and a selection exists. *)

val handle_key : t -> Event.key -> unit
(** [handle_key t event] routes keyboard events through tiered handlers.

    Event dispatch follows this order: 1. Global key handlers (registered via
    {!add_global_key_handler}) 2. Focused renderable's key handlers 3. Focused
    renderable's default key handler

    Any handler can call {!Event.Key.prevent_default} to stop subsequent tiers
    from executing. Use tier-1 for application-wide shortcuts, tier-2 for
    node-specific handlers, and tier-3 for fallback behavior. *)

val handle_paste : t -> Event.paste -> unit
(** [handle_paste t event] routes paste events to registered handlers.

    Event dispatch follows this order: 1. Global paste handlers (registered via
    {!add_global_paste_handler}) 2. Focused renderable's paste handler

    {!Event.Paste.prevent_default} stops delivery to the focused renderable
    after global handlers execute. Use global handlers for application-wide
    paste processing; focused handlers receive events only when globals pass
    them through. *)

(** {1 Focus Management} *)

val focused_node : t -> Renderable.t option
(** [focused_node t] returns the currently focused renderable, if any.

    The focused node receives keyboard and paste events. Only one node can be
    focused at a time. *)

val focus : t -> Renderable.t -> unit
(** [focus t node] attempts to focus [node].

    If [node] is not focusable (via {!Renderable.focusable}), this is a no-op.
    Otherwise, it blurs the previously focused node (if different), calls
    {!Renderable.Internal.focus_direct} on [node], and marks the renderer dirty.

    Focus changes trigger [on_focus] and [on_blur] callbacks on affected nodes.
*)

val blur_focused : t -> unit
(** [blur_focused t] removes focus from the currently focused node, if any.

    This calls {!Renderable.Internal.blur_direct} and marks the renderer dirty.
*)

val find_by_number : t -> int -> Renderable.t option
(** [find_by_number t num] looks up a renderable by its numeric ID.

    Returns [None] if no renderable with the given number exists. Numeric IDs
    are assigned during {!create_node} and remain stable for the node's
    lifetime. *)

val find_by_id : t -> string -> Renderable.t option
(** [find_by_id t id] looks up a renderable by its string ID.

    Returns [None] if no renderable with the given ID exists. This performs a
    linear scan of all renderables. *)

(** {1 Global Event Handlers} *)

val add_global_key_handler : t -> (Event.key -> unit) -> handler_id
(** [add_global_key_handler t handler] registers a tier-1 keyboard handler.

    Global handlers execute before focused renderable handlers. They run in
    registration order and can call {!Event.Key.prevent_default} to stop tier-2
    and tier-3 handlers from executing.

    Returns a handler ID for later removal via {!remove_global_key_handler}. *)

val remove_global_key_handler : t -> handler_id -> unit
(** [remove_global_key_handler t id] unregisters a global keyboard handler.

    If [id] does not correspond to a registered handler, this is a no-op. *)

val add_global_paste_handler : t -> (Event.paste -> unit) -> handler_id
(** [add_global_paste_handler t handler] registers a tier-1 paste handler.

    Global paste handlers execute before the focused renderable's handler. They
    can call {!Event.Paste.prevent_default} to stop delivery to the focused
    node.

    Returns a handler ID for later removal via {!remove_global_paste_handler}.
*)

val remove_global_paste_handler : t -> handler_id -> unit
(** [remove_global_paste_handler t id] unregisters a global paste handler.

    If [id] does not correspond to a registered handler, this is a no-op. *)

(** {1 Text Selection} *)

val on_selection :
  t ->
  (anchor_x:int -> anchor_y:int -> focus_x:int -> focus_y:int -> unit) ->
  unit
(** [on_selection t callback] registers a selection completion listener.

    The callback is invoked when a selection drag ends (mouse-up after dragging
    on a selectable node). It receives the final anchor and focus screen
    coordinates.

    Multiple callbacks can be registered; they execute in registration order.
    There is no mechanism to unregister callbacks. *)

val has_selection : t -> bool
(** [has_selection t] returns [true] if an active selection exists.

    A selection is active from mouse-down on a selectable node until the
    selection is cleared (e.g., by clicking elsewhere) or the root is cleared.
*)

val get_selection : t -> (int * int * int * int) option
(** [get_selection t] returns the current selection's coordinates.

    Returns [Some (anchor_x, anchor_y, focus_x, focus_y)] if a selection is
    active, [None] otherwise. Coordinates are in screen space. *)

val get_selected_text : t -> string
(** [get_selected_text t] extracts text from selected nodes.

    Returns the concatenated text from all selected renderables, joined by
    newlines and sorted by screen position (top-to-bottom, left-to-right).
    Returns [""] if no selection is active.

    Nodes must implement {!Renderable.Internal.get_selected_text} to contribute
    text to the selection. *)

val request_selection_update : t -> unit
(** [request_selection_update t] refreshes selection to the last pointer
    position.

    If a selection drag is active, this updates the selection focus point to the
    last recorded mouse coordinates and recomputes touched nodes. Has no effect
    if no selection is in progress.

    Use this when nodes move or resize during a selection drag to ensure the
    selection remains accurate. *)

(** {1 Live Regions} *)

val has_live_requests : t -> bool
(** [has_live_requests t] returns [true] if any node in the tree is marked live.

    Nodes with [Props.live = true] increment the root's live count. This returns
    [true] when the aggregate count is non-zero, indicating active animations,
    async operations, or other time-sensitive activity.

    Returns [false] if no root is set. *)

val on_live_change : t -> (bool -> unit) -> unit
(** [on_live_change t callback] registers a callback for live state transitions.

    The callback is invoked when the aggregate live count transitions between
    zero and non-zero. It receives [true] when the first node becomes live,
    [false] when the last live node is unmarked or removed.

    Use this to start or stop frame loops based on whether the UI has active
    content requiring updates. Only one callback can be registered; subsequent
    calls replace the previous callback. *)
