(** UI event wrappers for keyboard, paste, and mouse interactions.

    Wraps terminal input events from {!Input} with default prevention,
    propagation control, and hit-testing metadata. *)

(** {1 Keyboard Events} *)

module Key : sig
  type t
  (** Keyboard event with default prevention flag. *)

  val of_input : Input.Key.event -> t
  (** [of_input event] wraps a terminal keyboard event. *)

  val data : t -> Input.Key.event
  (** [data t] returns the wrapped terminal keyboard event. *)

  val prevent_default : t -> unit
  (** [prevent_default t] marks renderer-level default behavior as prevented.

      When used with {!Renderer.handle_key}, this stops Tier-2/Tier-3 key
      handling (focused renderable and its default handler) after global
      handlers have run. *)

  val default_prevented : t -> bool
  (** [default_prevented t] returns whether default was prevented. *)
end

(** {1 Paste Events} *)

module Paste : sig
  type t
  (** Text paste event with default prevention flag. *)

  val of_text : string -> t
  (** [of_text s] creates a paste event containing [s]. *)

  val text : t -> string
  (** [text t] returns the pasted text. *)

  val prevent_default : t -> unit
  (** [prevent_default t] marks renderer-level default behavior as prevented.

      When used with {!Renderer.handle_paste}, this stops delivery to the
      focused renderable after global paste handlers have run. *)

  val default_prevented : t -> bool
  (** [default_prevented t] returns whether default was prevented. *)
end

(** {1 Mouse Events} *)

module Mouse : sig
  type t
  (** Mouse event with coordinates, modifiers, propagation control, and
      renderer-managed metadata. *)

  type scroll_direction = Input.Mouse.scroll_direction =
    | Scroll_up
    | Scroll_down
    | Scroll_left
    | Scroll_right

  type kind =
    | Down
    | Up
    | Move
    | Drag
    | Drag_end
    | Drop
    | Over
    | Out
    | Scroll  (** Mouse event discriminator. *)

  val kind : t -> kind
  (** [kind t] returns the event's kind. *)

  (** {2 Constructors}

      All constructors initialize target/source metadata to [None], default
      prevented to [false], and propagation stopped to [false]. *)

  val down :
    x:int ->
    y:int ->
    button:Input.Mouse.button ->
    modifiers:Input.Key.modifier ->
    t
  (** [down ~x ~y ~button ~modifiers] creates a mouse button press event. *)

  val up :
    x:int ->
    y:int ->
    button:Input.Mouse.button ->
    modifiers:Input.Key.modifier ->
    t
  (** [up ~x ~y ~button ~modifiers] creates a mouse button release event. *)

  val move : x:int -> y:int -> modifiers:Input.Key.modifier -> t
  (** [move ~x ~y ~modifiers] creates a mouse motion event. *)

  val drag :
    x:int ->
    y:int ->
    button:Input.Mouse.button ->
    modifiers:Input.Key.modifier ->
    t
  (** [drag ~x ~y ~button ~modifiers] creates a mouse drag event. *)

  val drag_end :
    x:int ->
    y:int ->
    button:Input.Mouse.button ->
    modifiers:Input.Key.modifier ->
    t
  (** [drag_end ~x ~y ~button ~modifiers] creates a drag termination event. *)

  val drop :
    x:int ->
    y:int ->
    button:Input.Mouse.button ->
    modifiers:Input.Key.modifier ->
    t
  (** [drop ~x ~y ~button ~modifiers] creates a drop event. *)

  val over : x:int -> y:int -> modifiers:Input.Key.modifier -> t
  (** [over ~x ~y ~modifiers] creates a mouse-over event. *)

  val out : x:int -> y:int -> modifiers:Input.Key.modifier -> t
  (** [out ~x ~y ~modifiers] creates a mouse-out event. *)

  val scroll :
    x:int ->
    y:int ->
    direction:scroll_direction ->
    delta:int ->
    modifiers:Input.Key.modifier ->
    t
  (** [scroll ~x ~y ~direction ~delta ~modifiers] creates a scroll event. *)

  (** {2 Default and Propagation Control} *)

  val stop_propagation : t -> unit
  (** [stop_propagation t] prevents the event from reaching ancestor nodes.

      Propagation only applies to mouse events; keyboard and paste events do not
      bubble through the render tree. *)

  val propagation_stopped : t -> bool
  (** [propagation_stopped t] returns whether propagation was stopped. *)

  val prevent_default : t -> unit
  (** [prevent_default t] marks default behavior as prevented.

      For mouse events, this only affects renderer-defined defaults (for
      example, clearing selection on mouse down) and does not stop bubbling. Use
      [stop_propagation] to stop bubbling to ancestors. *)

  val default_prevented : t -> bool
  (** [default_prevented t] returns whether default was prevented. *)

  (** {2 Payload Accessors} *)

  val x : t -> int
  (** [x t] returns the horizontal cursor position. *)

  val y : t -> int
  (** [y t] returns the vertical cursor position. *)

  val modifiers : t -> Input.Key.modifier
  (** [modifiers t] returns the modifier key state. *)

  val button : t -> Input.Mouse.button option
  (** [button t] returns the button for [Down], [Up], [Drag], [Drag_end], and
      [Drop] events; [None] otherwise. *)

  val scroll_delta : t -> (scroll_direction * int) option
  (** [scroll_delta t] returns [(direction, delta)] for [Scroll] events; [None]
      otherwise. *)

  val is_selecting : t -> bool
  (** [is_selecting t] returns whether this event is part of a text selection.

      Only [Drag] and [Up] events track selection state. *)

  (** {2 Hit-Testing Metadata}

      Set by the renderer during hit-testing. Target identifies the node under
      the cursor; source identifies the drag origin for [Over] and [Drop]. *)

  val target_id : t -> string option
  (** [target_id t] returns the target node's string ID. *)

  val target_number : t -> int option
  (** [target_number t] returns the target node's numeric ID. *)

  val source_id : t -> string option
  (** [source_id t] returns the drag source's string ID. *)

  val source_number : t -> int option
  (** [source_number t] returns the drag source's numeric ID. *)

  (** {2 Internal}

      Renderer-only mutation hooks. Applications should not call these. *)

  module Internal : sig
    val set_target : t -> id:string option -> number:int option -> unit
    (** [set_target t ~id ~number] updates the target metadata. *)

    val set_source : t -> id:string option -> number:int option -> unit
    (** [set_source t ~id ~number] updates source metadata for [Over]/[Drop]. *)

    val set_is_selecting : t -> bool -> unit
    (** [set_is_selecting t flag] updates selection state for [Drag]/[Up]. *)
  end
end

(** {1 Type Aliases} *)

type key = Key.t
(** Alias for {!Key.t}. *)

type paste = Paste.t
(** Alias for {!Paste.t}. *)

type mouse = Mouse.t
(** Alias for {!Mouse.t}. *)
