(** Renderable nodes with Toffee layout integration.

    Renderable provides the core node abstraction for Mosaic's UI tree. Each
    node combines layout state (managed by Toffee, a flexbox layout engine) with
    rendering callbacks, event handlers, and lifecycle hooks. Nodes form a tree
    hierarchy where parent-child relationships drive both layout computation and
    rendering order.

    The public API focuses on tree construction, styling, measurement, event
    handling, and rendering. Internal renderer integration lives in {!Internal}.

    {1 Overview}

    Create nodes with {!create_child}, style them with {!set_style}, attach
    render callbacks with {!set_render}, and organize them hierarchically with
    {!append_child} or {!insert_child}. The layout engine computes positions and
    sizes; rendering traverses the tree in z-index order.

    {1 Usage Basics}

    Create a child node and attach it to a parent:
    {[
      let child =
        Renderable.create_child ~parent
          ~props:(Renderable.Props.make ~id:"my-node" ())
          ()
      in
      match child with
      | Ok child ->
          Renderable.set_render child (fun _self grid ~delta ->
              Grid.draw_text grid ~x:0 ~y:0 ~text:"Hello, world!");
          let _ = Renderable.append_child ~parent ~child in
          ()
      | Error _ -> ()
    ]}

    Update styles dynamically:
    {[
      let open Toffee.Style in
      let style =
        default |> set_width (Length 80.) |> set_height (Length 24.)
        |> set_flex_direction Flex_direction.Column
      in
      let _ = Renderable.set_style node style in
      ()
    ]}

    {1 Key Concepts}

    {2 Layout Integration}

    Each renderable wraps a Toffee layout node. Toffee computes flexbox-based
    positions and dimensions; {!val-x}, {!val-y}, {!width}, and {!height} expose
    the computed layout. Modifications via {!set_style} or {!mark_layout_dirty}
    trigger re-layout on the next frame.

    {2 Rendering Pipeline}

    Rendering proceeds in three phases per node:
    + {i Before}: Optional [render_before_hook] for setup (e.g., clipping)
    + {i Main}: The node's {!type-render} callback draws into a grid
    + {i After}: Optional [render_after_hook] for cleanup or overlays

    Children render recursively after the parent's main phase, sorted by
    z-index. Nodes with [`Self] buffering render into an offscreen grid, then
    blit to the parent.

    {2 Event Handling}

    Events flow in tiers:
    + {b Tier 1}: Global handlers via renderer
    + {b Tier 2}: Per-node {!on_key_down} or {!on_mouse} handlers
    + {b Tier 3}: Default handler via {!set_default_key_handler}

    Mouse events bubble up the tree unless {!Event.Mouse.stop_propagation} is
    called. Keyboard and paste events do not bubble; they target the focused
    node only.

    {2 Focus Management}

    Mark nodes focusable with {!set_focusable}. Call {!val-focus} to request
    focus (delegated to the renderer's focus controller). Only one node is
    focused at a time. Focused nodes receive keyboard and paste events.

    {2 Lifecycle Hooks}

    + {!set_on_frame}: Called every frame with delta time (ms)
    + {!set_on_size_change}: Called when {!width} or {!height} changes
    + {!set_on_lifecycle_pass}: Called during renderer's lifecycle pass

    Lifecycle passes run for nodes with {!set_live} enabled or ancestor counts
    propagated via {!Internal.live_count}.

    {2 Selection Support}

    Nodes provide text selection via {!set_selection}. Implement
    {!Select.capability} to handle selection start, change, clear, and text
    extraction. The renderer coordinates global selection state across nodes.

    {1 Invariants}

    - Every node belongs to exactly one Toffee tree (validated via
      [Error.Tree_mismatch])
    - A node's parent reference matches its position in the parent's child list
    - Layout dirty flags propagate upward on style or hierarchy changes
    - Sorted children caches (z-index, primary axis) rebuild lazily on access
    - Buffered nodes maintain a framebuffer resized to match layout dimensions
    - Live counts equal the sum of self-live (1 if live and visible, else 0)
      plus all descendants' live counts
    - Focused nodes are always focusable; blurring unfocusable nodes is a no-op

    {1 Performance Considerations}

    - Child sorting is O(n log n) via insertion sort, cached until z-index or
      position changes
    - Tree mutations ({!append_child}, {!detach}) are O(1) but invalidate caches
    - Layout queries ({!val-x}, {!val-y}, {!width}, {!height}) are O(1) after
      caching
    - Viewport culling via {!Internal.children_in_viewport} uses binary search
      for large child lists (>= 16)

    {1 Standards and Compatibility}

    Layout semantics align with Toffee's flexbox implementation, which follows
    the CSS Flexible Box Layout specification. Refer to Toffee documentation for
    flexbox properties and behavior. *)

type t
(** Mutable renderable node.

    Combines layout state (via Toffee), rendering callbacks, event handlers,
    focus state, and hierarchy metadata. Nodes are mutable and stateful;
    modifications trigger re-renders and layout updates. *)

type render = t -> Grid.t -> delta:float -> unit
(** [render self grid ~delta] draws the node into [grid].

    [self] is the node being rendered; [grid] is the target framebuffer (either
    the main grid or a node's buffered grid); [delta] is elapsed time in
    milliseconds since the last frame.

    Rendering operates within the node's computed layout bounds ({!val-x},
    {!val-y}, {!width}, {!height}) but may draw outside if not clipped. Use
    {!Grid.with_scissor} or {!set_child_clip} for constrained rendering. *)

type measure =
  known_dimensions:float option Toffee.Geometry.Size.t ->
  available_space:Toffee.Available_space.t Toffee.Geometry.Size.t ->
  style:Toffee.Style.t ->
  float Toffee.Geometry.Size.t
(** [measure ~known_dimensions ~available_space ~style] computes intrinsic size.

    Called by Toffee during layout when a node's dimensions are not explicitly
    set. [known_dimensions] contains width/height if already resolved;
    [available_space] describes constraints from the parent; [style] is the
    node's current Toffee style.

    Returns a size in layout units (typically terminal cells). Used for leaf
    nodes like text or images that need content-based sizing.

    @see <https://docs.rs/taffy/latest/taffy/tree/trait.LayoutTree.html#tymethod.measure_child>
      Taffy measure_child documentation *)

type 'a event_handler = 'a -> unit
(** ['a event_handler] is a callback for events of type ['a].

    Handlers run synchronously during event dispatch. For mouse events, multiple
    handlers may run (bubbling); for keyboard events, handlers run until one
    calls {!Event.Key.prevent_default}. *)

type cursor_style = [ `Block | `Line | `Underline ]
(** Cursor style variants used for hardware cursor control. *)

type hardware_cursor = {
  x : int;
  y : int;
  color : Ansi.Color.t;
  style : cursor_style;
  blinking : bool;
}
(** Hardware cursor description in absolute, 1-based coordinates. *)

module Props : sig
  type t
  (** Immutable properties for node initialization.

      Props bundle initial configuration: ID, style, visibility, z-index,
      buffering mode, and live flag. Use {!make} to construct and accessors to
      read. Props are copied into the node on creation; subsequent modifications
      use direct setters like {!set_visible} or {!set_style}. *)

  type buffer_mode = [ `None | `Self ]
  (** Buffering mode for offscreen rendering.

      [`None] renders directly into the parent's grid. [`Self] allocates a
      framebuffer and renders the node (but not children) offscreen, then blits
      to the parent. Useful for caching expensive rendering or applying effects.
  *)

  val make :
    id:string ->
    ?style:Toffee.Style.t ->
    ?visible:bool ->
    ?z_index:int ->
    ?buffer:buffer_mode ->
    ?live:bool ->
    unit ->
    t
  (** [make ~id ?style ?visible ?z_index ?buffer ?live ()] creates props.

      [style] defaults to {!Toffee.Style.default}. [visible] defaults to [true].
      [z_index] defaults to [0]. [buffer] defaults to [`None]. [live] defaults
      to [false]. *)

  val id : t -> string
  (** [id t] returns the node's string identifier. *)

  val visible : t -> bool
  (** [visible t] returns whether the node is visible. *)

  val z_index : t -> int
  (** [z_index t] returns the rendering order. *)

  val buffer : t -> buffer_mode
  (** [buffer t] returns the buffering mode. *)

  val live : t -> bool
  (** [live t] returns whether lifecycle hooks are enabled. *)
end

type error =
  | Layout_error of Toffee.Error.t
  | Tree_mismatch
      (** Errors from tree or layout operations.

          [Layout_error e] wraps a Toffee layout engine error. [Tree_mismatch]
          occurs when attaching nodes from different Toffee trees (each tree is
          isolated). *)

(** {1 Tree Construction} *)

val create_child :
  parent:t ->
  ?id:string ->
  ?props:Props.t ->
  ?on_frame:(t -> delta:float -> unit) ->
  ?on_size_change:(t -> unit) ->
  ?render:render ->
  unit ->
  (t, error) result
(** [create_child ~parent ?id ?props ?on_frame ?on_size_change ?render ()]
    creates a detached child.

    The child belongs to the same Toffee tree and renderer as [parent] but is
    not attached to any parent until {!append_child} or {!insert_child} is
    called. [id] defaults to ["renderable-N"] where N is an internal counter.
    [props] defaults to [Props.make ~id ()]. [render] defaults to a no-op.

    The child inherits scheduling, focus, and lifecycle callbacks from [parent].

    Returns [Error Tree_mismatch] if [parent] is from a different Toffee tree
    (should not occur in practice). *)

(** {1 Tree Mutation} *)

val append_child : parent:t -> child:t -> (unit, error) result
(** [append_child ~parent ~child] attaches [child] as the last child of
    [parent].

    Detaches [child] from its current parent (if any) before attaching. Updates
    the Toffee layout tree, marks layouts dirty, and invalidates sorted caches.
    Live counts propagate upward.

    @raise Tree_mismatch
      if [child] belongs to a different Toffee tree than [parent]. *)

val insert_child : parent:t -> index:int -> child:t -> (unit, error) result
(** [insert_child ~parent ~index ~child] attaches [child] at position [index].

    [index] is clamped to [[0, child_count]]. Detaches [child] from its current
    parent first. Updates the Toffee layout tree, marks layouts dirty, and
    invalidates sorted caches.

    @raise Tree_mismatch
      if [child] belongs to a different Toffee tree than [parent]. *)

val detach : t -> (unit, error) result
(** [detach t] removes [t] from its parent without destroying the Toffee node.

    If [t] is focused, blurs it first. The node remains valid and can be
    reattached. Marks the parent's layout dirty and updates live counts. Returns
    [Ok ()] if [t] has no parent. *)

val remove : t -> (unit, error) result
(** [remove t] detaches [t] and destroys its Toffee node.

    The node becomes unusable after removal. Use {!detach} if you plan to
    reattach. *)

(** {1 Introspection} *)

val id : t -> string
(** [id t] returns [t]'s string identifier. *)

val parent : t -> t option
(** [parent t] returns [t]'s parent, or [None] if detached or root. *)

val children : t -> t list
(** [children t] returns [t]'s children in insertion order.

    The returned list is a snapshot; subsequent mutations are not reflected. *)

val set_child_sink :
  t -> (child:t -> index:int option -> t * int option) option -> unit
(** [set_child_sink t sink] installs an optional redirect for child mutations.

    When set, calls to {!append_child} or {!insert_child} on [t] forward the
    mutation to the target parent (and optional index) returned by [sink].
    Useful for composite renderables (e.g. scroll boxes) whose logical children
    live under an internal container instead of the root. *)

val reconcile_parent : t -> t
(** [reconcile_parent t] returns the logical parent to use when reconciling
    user-provided children. Defaults to [t] unless overridden via
    {!set_reconcile_parent}. *)

val set_reconcile_parent : t -> t -> unit
(** [set_reconcile_parent t p] sets [p] as the logical parent used by the
    reconciler for child placement. *)

val clear_reconcile_parent : t -> unit
(** [clear_reconcile_parent t] removes any logical reconciliation parent, so
    reconciliation uses [t] directly. *)

(** {1 Rendering + Layout} *)

val set_render : t -> render -> unit
(** [set_render t fn] replaces [t]'s render callback.

    Schedules a re-render. The callback is invoked during the renderer's draw
    pass. *)

val request_render : t -> unit
(** [request_render t] schedules a re-render for [t] and its descendants.

    The renderer processes pending renders on the next frame. This is a no-op if
    [t] is detached (no renderer). *)

val set_measure : t -> measure option -> unit
(** [set_measure t fn] assigns a custom measure function for intrinsic sizing.

    [fn] is called by Toffee when [t] lacks explicit width or height. Setting
    [None] clears the measure function. Marks layout dirty. *)

val mark_layout_dirty : t -> (unit, error) result
(** [mark_layout_dirty t] flags [t] for re-layout on the next frame.

    Invalidates cached layout and propagates dirty flags to the Toffee tree.
    Returns [Error (Layout_error e)] if Toffee rejects the operation. *)

val set_style : t -> Toffee.Style.t -> (unit, error) result
(** [set_style t style] updates [t]'s flexbox style.

    Applies a heuristic: if [style] has explicit width or height and
    [flex_shrink] is 1.0, sets [flex_shrink] to 0.0 (prevents unexpected
    shrinking). Marks layout dirty and schedules a re-render. Preserves the
    display mode in [original_display] if not [Display.None].

    Returns [Error (Layout_error e)] if Toffee rejects the style. *)

val style : t -> Toffee.Style.t
(** [style t] returns [t]'s current Toffee style.

    Falls back to the cached style if Toffee queries fail (should not occur). *)

val x : t -> int
(** [x t] returns [t]'s absolute X position in terminal cells.

    Computed from cached layout plus accumulated render offsets. Returns [0] if
    layout is not cached. *)

val y : t -> int
(** [y t] returns [t]'s absolute Y position in terminal cells.

    Computed from cached layout plus accumulated render offsets. Returns [0] if
    layout is not cached. *)

val width : t -> int
(** [width t] returns [t]'s layout width in terminal cells.

    Returns [max 1 (floor cached_width)] if layout is cached, else [0]. *)

val height : t -> int
(** [height t] returns [t]'s layout height in terminal cells.

    Returns [max 1 (floor cached_height)] if layout is cached, else [0]. *)

val bounds : t -> Grid.clip_rect
(** [bounds t] returns [t]'s bounding rectangle.

    Equivalent to [{ x = x t; y = y t; width = width t; height = height t }]. *)

val set_render_offset : t -> x:int -> y:int -> unit
(** [set_render_offset t ~x ~y] shifts [t]'s rendering position.

    Offsets are added to layout positions during rendering. Useful for scrolling
    without modifying layout. Invalidates the parent's primary axis sort cache
    and schedules a re-render. *)

val render_offset : t -> int * int
(** [render_offset t] returns [t]'s current render offset as [(x, y)]. *)

val set_child_clip : t -> (t -> Grid.clip_rect option) option -> unit
(** [set_child_clip t fn] overrides child clipping for [t].

    [fn] receives [t] and returns a clipping rectangle (or [None] for no clip).
    Useful for scroll containers. The renderer applies the clip before rendering
    children. *)

val set_visible_children_selector : t -> (t -> int list) option -> unit
(** [set_visible_children_selector t fn] filters visible children by index.

    [fn] receives [t] and returns a list of indices (in insertion order) to
    render. Children not in the list are skipped. Use for virtualized lists or
    conditional rendering. *)

(** {1 Props-like convenience setters} *)

val set_visible : t -> bool -> unit
(** [set_visible t visible] shows or hides [t].

    Updates the Toffee display mode ([Display.None] when hidden, restored
    original mode when shown). Marks layout dirty and updates live counts. Blurs
    [t] if hiding while focused. *)

val visible : t -> bool
(** [visible t] returns whether [t] is visible. *)

val set_z_index : t -> int -> unit
(** [set_z_index t z] changes [t]'s rendering order.

    Higher z-index nodes render on top. Invalidates the parent's z-index sort
    cache and schedules a re-render. *)

val z_index : t -> int
(** [z_index t] returns [t]'s z-index. *)

val set_buffer : t -> Props.buffer_mode -> unit
(** [set_buffer t mode] changes [t]'s buffering mode.

    Setting [`None] releases the framebuffer. Setting [`Self] allocates a
    framebuffer on the next render. Schedules a re-render. *)

val buffer : t -> Props.buffer_mode
(** [buffer t] returns [t]'s buffering mode. *)

val set_live : t -> bool -> unit
(** [set_live t live] enables or disables lifecycle hooks.

    When [live] is true and [t] is visible, increments live count (propagates to
    ancestors). Lifecycle passes ({!set_on_lifecycle_pass}) run for live nodes.
*)

val live : t -> bool
(** [live t] returns whether [t] has lifecycle hooks enabled. *)

(** {1 Focus} *)

val set_focusable : t -> bool -> unit
(** [set_focusable t focusable] marks [t] as focusable.

    Focusable nodes can receive focus via {!val-focus} and accept keyboard/paste
    events. *)

val focusable : t -> bool
(** [focusable t] returns whether [t] can receive focus. *)

val focused : t -> bool
(** [focused t] returns whether [t] currently has focus. *)

val focus : t -> bool
(** [focus t] requests focus for [t].

    Returns [false] if [t] is not focusable. Delegates to the renderer's focus
    controller (if attached), which handles global focus state. Returns [true]
    if focus was granted. *)

val blur : t -> unit
(** [blur t] removes focus from [t].

    No-op if [t] is not focused. Delegates to the renderer's blur controller. *)

val set_hardware_cursor_provider :
  t -> (t -> hardware_cursor option) option -> unit
(** [set_hardware_cursor_provider t provider] registers a hardware cursor
    provider for [t].

    The provider is consulted when [t] is focused to position and style the
    terminal cursor. Pass [None] to clear the provider. *)

val hardware_cursor : t -> hardware_cursor option
(** [hardware_cursor t] returns the current hardware cursor state if a provider
    is set and returns a value. *)

(** {1 Events} *)

val on_mouse : t -> Event.mouse event_handler -> unit
(** [on_mouse t handler] registers a mouse event handler.

    [handler] is called for all mouse events targeting [t] or bubbling from
    children. Multiple handlers accumulate (newest registered first). Handlers
    run until one calls {!Event.Mouse.stop_propagation}. *)

val on_key_down : t -> Event.key event_handler -> unit
(** [on_key_down t handler] registers a keyboard event handler.

    Runs when [t] is focused and a key is pressed. Multiple handlers accumulate
    (newest registered first). If a handler calls {!Event.Key.prevent_default},
    remaining handlers and the default handler (Tier-3) are skipped. *)

val set_default_key_handler : t -> (Event.key -> unit) option -> unit
(** [set_default_key_handler t handler] assigns a fallback key handler.

    Runs after {!on_key_down} handlers (Tier-3). Only one default handler per
    node; setting [None] clears it. *)

val on_paste : t -> Event.paste event_handler -> unit
(** [on_paste t handler] registers a paste event handler.

    Runs when [t] is focused and text is pasted. Multiple handlers are not
    supported; last registration wins. *)

(** {1 Selection Capability} *)

module Select : sig
  type capability = {
    should_start : x:int -> y:int -> bool;
        (** [should_start ~x ~y] returns whether selection should begin.

            Called when the user presses a mouse button. [x] and [y] are global
            terminal coordinates. Return [true] to start selecting. *)
    on_change : Selection.t option -> bool;
        (** [on_change selection] notifies the node of selection changes.

            Called when selection bounds change or are cleared ([None]). Return
            [true] if this node owns the selection (for renderer tracking). *)
    clear : unit -> unit;
        (** [clear ()] clears the node's internal selection state.

            Called when the renderer clears global selection. *)
    get_text : unit -> string;
        (** [get_text ()] extracts the selected text.

            Returns [""] if no selection. Used for clipboard operations. *)
  }
  (** Selection capability for text selection support.

      Implement this interface and register via {!set_selection} to enable text
      selection on a node. The renderer coordinates selection across nodes;
      capabilities handle node-specific logic (hit-testing, bounds, text
      extraction). *)
end

val set_selection : t -> Select.capability option -> unit
(** [set_selection t cap] enables text selection on [t].

    Setting [None] disables selection. The renderer calls [cap] methods during
    mouse interactions and clipboard operations. *)

val selectable : t -> bool
(** [selectable t] returns whether [t] has selection capability. *)

(** {1 Runtime Hooks} *)

val set_on_frame : t -> (t -> delta:float -> unit) option -> unit
(** [set_on_frame t callback] registers a per-frame update hook.

    [callback t ~delta] is called every frame with elapsed milliseconds since
    the last frame. Use for animations, time-based updates, or polling. Setting
    [None] clears the hook. *)

val set_on_size_change : t -> (t -> unit) option -> unit
(** [set_on_size_change t callback] registers a size change hook.

    [callback t] is called when {!width} or {!height} changes after layout.
    Useful for resizing internal buffers or recomputing derived state. Setting
    [None] clears the hook. *)

val set_on_lifecycle_pass : t -> (t -> unit) option -> unit
(** [set_on_lifecycle_pass t callback] registers a lifecycle pass hook.

    [callback t] is called during the renderer's lifecycle pass. Lifecycle
    passes run for nodes with {!set_live} enabled or descendants with live
    counts > 0. Use for cleanup, state updates, or deferred operations. Setting
    [None] clears the hook and unregisters from lifecycle passes. *)

(** Renderer and reconciler integration (unstable).

    Internal functions for renderer implementation. Applications should not use
    these directly; they exist for renderer/reconciler plumbing and may change
    between minor versions. *)
module Internal : sig
  (** Renderer-facing operations.

      This module exposes lifecycle management, rendering pipeline hooks, event
      emission, and low-level queries needed by the renderer. Public
      applications should not depend on these functions; they are intended for
      Mosaic's internal renderer and advanced custom renderers only. *)

  type visible_children = [ `All | `Subset of int list ]
  (** Visible children discriminator.

      [`All] renders all children. [`Subset indices] renders only children at
      the given indices (in insertion order). *)

  val create :
    layout:unit Toffee.tree ->
    ?id:string ->
    ?props:Props.t ->
    ?on_frame:(t -> delta:float -> unit) ->
    ?on_size_change:(t -> unit) ->
    ?render:render ->
    ?render_before:render ->
    ?render_after:render ->
    ?glyph_pool:Glyph.pool ->
    num:int ->
    unit ->
    (t, error) result
  (** [create ~layout ?id ?props ?on_frame ?on_size_change ?render
       ?render_before ?render_after ?glyph_pool ~num ()] creates a root node.

      [layout] is the Toffee tree. [num] is a unique integer ID (managed by the
      renderer). [render_before] and [render_after] are optional hooks for
      pre/post rendering. [glyph_pool] is shared for character storage. *)

  val node_id : t -> Toffee.Node_id.t
  (** [node_id t] returns [t]'s Toffee node identifier. *)

  val number : t -> int
  (** [number t] returns [t]'s numeric ID (unique per renderer). *)

  val set_schedule : t -> (unit -> unit) -> unit
  (** [set_schedule t fn] assigns the scheduling callback.

      [fn] is called to request re-renders (typically enqueues [t] for the next
      frame). *)

  val set_registration_hooks :
    t -> register:(t -> unit) -> alloc_num:(unit -> int) -> unit
  (** [set_registration_hooks t ~register ~alloc_num] configures node
      registration.

      [register] is called when a node is created via {!create_child}.
      [alloc_num] allocates unique numeric IDs for children. *)

  val register_with_renderer : t -> unit
  (** [register_with_renderer t] registers [t] with the renderer.

      Calls the [register] hook set by {!set_registration_hooks}. *)

  val set_focus_controller : t -> focus:(t -> bool) -> blur:(t -> unit) -> unit
  (** [set_focus_controller t ~focus ~blur] assigns focus delegation.

      [focus] grants focus to [t]. [blur] removes focus. Used by the renderer to
      enforce single-focus invariant. *)

  val set_hardware_cursor_provider :
    t -> (t -> hardware_cursor option) option -> unit
  (** Internal setter for hardware cursor providers. *)

  val hardware_cursor : t -> hardware_cursor option
  (** Internal accessor for hardware cursor state. *)

  val focus_direct : t -> bool
  (** [focus_direct t] focuses [t] without delegation.

      Returns [false] if [t] is not focusable. Does not notify the renderer; use
      {!val-focus} for normal focus requests. *)

  val blur_direct : t -> unit
  (** [blur_direct t] blurs [t] without delegation.

      Does not notify the renderer; use {!blur} for normal blur requests. *)

  val set_lifecycle_controllers :
    t -> register:(t -> unit) -> unregister:(t -> unit) -> unit
  (** [set_lifecycle_controllers t ~register ~unregister] configures lifecycle
      tracking.

      [register] is called when [t] gains a lifecycle hook and is attached.
      [unregister] is called when [t] loses lifecycle hooks or is detached. *)

  val set_is_root : t -> bool -> unit
  (** [set_is_root t is_root] marks [t] as a root node.

      Root nodes are always attached (for lifecycle purposes). *)

  val set_on_live_count_change : t -> (t -> unit) option -> unit
  (** [set_on_live_count_change t callback] registers a live count observer.

      [callback t] is called when {!live_count} changes. Used by the renderer to
      track which subtrees need lifecycle passes. *)

  val live_count : t -> int
  (** [live_count t] returns the total live count for [t] and descendants.

      Equals self-live (1 if live and visible, else 0) plus sum of children's
      live counts. *)

  val layout_dirty : t -> bool
  (** [layout_dirty t] returns whether [t] needs re-layout. *)

  val clear_layout_dirty : t -> unit
  (** [clear_layout_dirty t] clears [t]'s layout dirty flag.

      Called by the renderer after computing layout. *)

  val update_cached_layout :
    t -> x:float -> y:float -> width:float -> height:float -> unit
  (** [update_cached_layout t ~x ~y ~width ~height] caches Toffee layout
      results.

      Stores computed position and size for O(1) queries via {!val-x}, {!val-y},
      {!width}, {!height}. Invalidates parent's primary axis cache if position
      changed. *)

  val pre_render_update : t -> delta:float -> unit
  (** [pre_render_update t ~delta] runs pre-render lifecycle hooks.

      Calls {!set_on_frame} and {!set_on_size_change} if conditions are met. The
      renderer calls this before rendering [t]. *)

  val render : t -> Grid.t -> delta:float -> unit
  (** [render t grid ~delta] invokes [t]'s render callback.

      Calls the function set by {!set_render}. The renderer calls this during
      the draw pass. *)

  val ensure_frame_buffer : t -> parent:Grid.t -> Grid.t option
  (** [ensure_frame_buffer t ~parent] allocates or resizes [t]'s framebuffer.

      Returns [Some buffer] if [t] has [`Self] buffering and dimensions > 0.
      Returns [None] otherwise. Reuses existing buffer if size matches. *)

  val blit_frame_buffer : t -> dst:Grid.t -> unit
  (** [blit_frame_buffer t ~dst] copies [t]'s framebuffer to [dst].

      No-op if [t] has no buffer. Blits from [(0, 0)] in the buffer to
      [(x t, y t)] in [dst]. *)

  val render_before_hook : t -> render option
  (** [render_before_hook t] returns [t]'s pre-render hook. *)

  val render_after_hook : t -> render option
  (** [render_after_hook t] returns [t]'s post-render hook. *)

  val set_render_before : t -> render option -> unit
  (** [set_render_before t hook] assigns a pre-render hook. *)

  val set_render_after : t -> render option -> unit
  (** [set_render_after t hook] assigns a post-render hook. *)

  val sorted_children : t -> t array
  (** [sorted_children t] returns [t]'s children sorted by z-index.

      Cached via insertion sort (O(n^2) worst case, O(n) for sorted input).
      Rebuilds when z-index changes or children mutate. *)

  val iter_sorted_children : t -> (t -> unit) -> unit
  (** [iter_sorted_children t f] applies [f] to each child in z-index order.

      Equivalent to [Array.iter f (sorted_children t)] but avoids exposing the
      array. *)

  val children_in_viewport :
    parent:t -> viewport:Grid.clip_rect -> padding:int -> t list
  (** [children_in_viewport ~parent ~viewport ~padding] returns visible
      children.

      Filters [parent]'s children to those intersecting [viewport] (expanded by
      [padding] in all directions). Uses binary search for large child lists (>=
      16). Returns children sorted by z-index. *)

  val child_clip_rect : t -> Grid.clip_rect option
  (** [child_clip_rect t] computes the clip rectangle for [t]'s children.

      Calls the function set by {!set_child_clip}, or returns [None] if unset.
  *)

  val visible_children : t -> visible_children
  (** [visible_children t] returns which children to render.

      Calls the function set by {!set_visible_children_selector}, or returns
      [`All] if unset. *)

  val run_lifecycle_pass : t -> unit
  (** [run_lifecycle_pass t] invokes [t]'s lifecycle callback.

      Calls the function set by {!set_on_lifecycle_pass}, or no-op if unset. *)

  val measure_of : t -> measure option
  (** [measure_of t] returns [t]'s measure function. *)

  val set_glyph_pool : t -> Glyph.pool -> unit
  (** [set_glyph_pool t pool] assigns a glyph pool for text rendering. *)

  val glyph_pool : t -> Glyph.pool option
  (** [glyph_pool t] returns [t]'s glyph pool. *)

  val emit_mouse_event : t -> Event.mouse -> unit
  (** [emit_mouse_event t event] dispatches a mouse event to [t]'s handlers.

      Runs all handlers registered via {!on_mouse}, then bubbles to [t]'s parent
      unless {!Event.Mouse.stop_propagation} was called. *)

  val emit_key_event : t -> Event.key -> unit
  (** [emit_key_event t event] dispatches a key event to [t]'s handlers.

      Runs handlers registered via {!on_key_down} in reverse registration order
      until one calls {!Event.Key.prevent_default}. *)

  val emit_default_key_event : t -> Event.key -> unit
  (** [emit_default_key_event t event] dispatches to [t]'s default key handler.

      Runs the handler set by {!set_default_key_handler}, or no-op if unset. *)

  val emit_paste_event : t -> Event.paste -> unit
  (** [emit_paste_event t event] dispatches a paste event to [t]'s handler.

      Runs the handler registered via {!on_paste}, or no-op if unset. *)

  val emit_selection_changed : t -> Selection.t option -> bool
  (** [emit_selection_changed t selection] notifies [t]'s selection capability.

      Calls [on_change] from the capability set by {!set_selection}. Returns
      [false] if [t] has no selection capability. *)

  val clear_selection_by_node : t -> unit
  (** [clear_selection_by_node t] clears [t]'s internal selection state.

      Calls [clear] from the capability set by {!set_selection}. No-op if [t]
      has no selection capability. *)

  val should_start_selection : t -> x:int -> y:int -> bool
  (** [should_start_selection t ~x ~y] queries whether selection should start.

      Calls [should_start] from the capability set by {!set_selection}. Returns
      [false] if [t] has no selection capability. *)

  val get_selected_text : t -> string
  (** [get_selected_text t] extracts selected text from [t].

      Calls [get_text] from the capability set by {!set_selection}. Returns [""]
      if [t] has no selection capability. *)
end
