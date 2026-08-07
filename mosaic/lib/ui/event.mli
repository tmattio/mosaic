(** Keyboard, paste, and mouse UI events.

    Each event type wraps terminal input from {!Input} with the dispatch control
    the renderer consults. Key and paste events target the focused renderable
    only: its handlers run newest-first until one calls [prevent_default], which
    also suppresses the widget's default behaviour and marks the event as
    consumed for the host runtime. Mouse events bubble from the hit-tested node
    to its ancestors — {!Mouse.stop_propagation} stops the bubbling — and
    additionally carry hit-testing metadata. *)

(** {1:keyboard Keyboard events} *)

module Key : sig
  type t
  (** The type for keyboard events.

      Wraps an {!Input.Key.event} with a default-prevention flag consulted while
      dispatching to the focused renderable. *)

  val of_input : Input.Key.event -> t
  (** [of_input ev] is a keyboard event wrapping [ev]. The default-prevention
      flag starts as [false]. *)

  val data : t -> Input.Key.event
  (** [data t] is the underlying terminal key event. *)

  val prevent_default : t -> unit
  (** [prevent_default t] marks [t] as consumed: remaining handlers on the
      focused renderable and its default key behaviour are skipped, and the host
      runtime treats the key as handled. *)

  val default_prevented : t -> bool
  (** [default_prevented t] is [true] iff {!prevent_default} has been called on
      [t]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] wrap the same key event. Dispatch
      control state is ignored. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a keyboard event for debugging. *)
end

(** {1:paste Paste events} *)

module Paste : sig
  type t
  (** The type for text paste events.

      Wraps pasted text with a default-prevention flag. See {!Key.t} for a
      description of the dispatch model. *)

  val of_text : string -> t
  (** [of_text s] is a paste event containing [s]. The default-prevention flag
      starts as [false]. *)

  val text : t -> string
  (** [text t] is the pasted text carried by [t]. *)

  val prevent_default : t -> unit
  (** [prevent_default t] marks [t] as consumed: remaining handlers on the
      focused renderable and its default paste behaviour are skipped. *)

  val default_prevented : t -> bool
  (** [default_prevented t] is [true] iff {!prevent_default} has been called on
      [t]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] contain the same text. Dispatch
      control state is ignored. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a paste event for debugging. *)
end

(** {1:mouse Mouse events} *)

module Mouse : sig
  type t
  (** The type for mouse events.

      Carries cursor coordinates, modifier keys, propagation control, and
      hit-testing metadata. Variant-specific data (button, source, scroll
      direction) is accessed by pattern-matching on {!kind}.

      Only {!stop_propagation} and {!prevent_default} mutate the event; all
      other fields are immutable after construction. *)

  (** {2:types Supporting types} *)

  (** The type for mouse buttons. *)
  type button =
    | Left  (** Primary (left) button. *)
    | Middle  (** Middle button. *)
    | Right  (** Secondary (right) button. *)
    | Button of int  (** Extended button with one-based index [n]. *)

  val equal_button : button -> button -> bool
  (** [equal_button a b] is [true] iff [a] and [b] are the same button. *)

  val pp_button : Format.formatter -> button -> unit
  (** [pp_button] formats a {!button} value. *)

  type modifier = Input.Modifier.t = {
    ctrl : bool;  (** Control key held. *)
    alt : bool;  (** Alt / Option key held. *)
    shift : bool;  (** Shift key held. *)
    super : bool;  (** Super / Windows / Command key held. *)
    hyper : bool;  (** Hyper key held. *)
    meta : bool;  (** Meta key held. *)
    caps_lock : bool;  (** Caps Lock active. *)
    num_lock : bool;  (** Num Lock active. *)
  }
  (** The type for modifier key state. Re-exported from {!Input.Modifier.t}. *)

  val no_modifier : modifier
  (** [no_modifier] is the modifier state with every field set to [false]. *)

  val equal_modifier : modifier -> modifier -> bool
  (** [equal_modifier a b] is [true] iff all modifier fields of [a] and [b] are
      equal. *)

  val pp_modifier : Format.formatter -> modifier -> unit
  (** [pp_modifier] formats a {!modifier} value. *)

  (** The type for scroll-wheel directions. Re-exported from
      {!Input.Mouse.scroll_direction}. *)
  type scroll_direction = Input.Mouse.scroll_direction =
    | Scroll_up  (** Scroll towards the top of the content. *)
    | Scroll_down  (** Scroll towards the bottom of the content. *)
    | Scroll_left  (** Scroll towards the left of the content. *)
    | Scroll_right  (** Scroll towards the right of the content. *)

  val equal_scroll_direction : scroll_direction -> scroll_direction -> bool
  (** [equal_scroll_direction a b] is [true] iff [a] and [b] are the same
      direction. *)

  val pp_scroll_direction : Format.formatter -> scroll_direction -> unit
  (** [pp_scroll_direction] formats a {!scroll_direction} value. *)

  (** {2:kinds Event kinds}

      Each variant carries exactly the data relevant to that kind of event.
      Common fields (coordinates, modifiers, target) are accessed via {!x},
      {!y}, {!modifiers}, and {!target}. *)

  (** The type for mouse event kinds. *)
  type kind =
    | Down of { button : button }  (** Button pressed. *)
    | Up of { button : button; is_dragging : bool }
        (** Button released. [is_dragging] is [true] iff a drag was in progress
            when the button was released. *)
    | Move  (** Cursor moved with no button pressed. *)
    | Drag of { button : button; is_dragging : bool }
        (** Cursor moved with [button] held. [is_dragging] is [true] iff the
            drag threshold has been exceeded. *)
    | Drag_end of { button : button }
        (** Drag gesture ended; [button] was the dragging button. *)
    | Drop of { button : button; source : int option }
        (** Drop target reached. [source] is the node identifier of the drag
            source, if known. *)
    | Over of { source : int option }
        (** Cursor moved over a potential drop target. [source] is the node
            identifier of the drag source, if known. *)
    | Out  (** Cursor left a node during a drag. *)
    | Scroll of { direction : scroll_direction; delta : int }
        (** Scroll-wheel event. [delta] is the number of steps in [direction].
        *)

  val equal_kind : kind -> kind -> bool
  (** [equal_kind a b] is [true] iff [a] and [b] are the same constructor and
      carry equal payloads. *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind] formats a {!kind} value. *)

  (** {2:constructors Construction} *)

  val make : x:int -> y:int -> modifiers:modifier -> ?target:int -> kind -> t
  (** [make ~x ~y ~modifiers ?target kind] is a mouse event at [(x, y)] with
      modifier state [modifiers], optional hit-test node [target], and event
      kind [kind]. Propagation and default-prevention flags start as [false].

      [target] defaults to [None]. *)

  (** {2:accessors Accessors} *)

  val kind : t -> kind
  (** [kind t] is [t]'s event kind with its variant-specific payload. See
      {!type-kind}. *)

  val x : t -> int
  (** [x t] is [t]'s horizontal cursor position (0-based column). *)

  val y : t -> int
  (** [y t] is [t]'s vertical cursor position (0-based row). *)

  val modifiers : t -> modifier
  (** [modifiers t] is [t]'s modifier key state at the time of the event. *)

  val target : t -> int option
  (** [target t] is the hit-test target node identifier of [t], if any. *)

  (** {2:dispatch Dispatch control} *)

  val stop_propagation : t -> unit
  (** [stop_propagation t] prevents [t] from bubbling to ancestor nodes. *)

  val propagation_stopped : t -> bool
  (** [propagation_stopped t] is [true] iff {!stop_propagation} has been called
      on [t]. *)

  val prevent_default : t -> unit
  (** [prevent_default t] marks renderer-level default behaviour as prevented.
      This only suppresses renderer-defined defaults (for example, starting a
      text selection on mouse-down); it does not stop bubbling. Use
      {!stop_propagation} to prevent bubbling to ancestors. *)

  val default_prevented : t -> bool
  (** [default_prevented t] is [true] iff {!prevent_default} has been called on
      [t]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same kind, coordinates,
      modifiers, and target. Dispatch control state is ignored. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a mouse event for debugging. *)
end

(** {1:aliases Type aliases} *)

type key = Key.t
(** The type for keyboard events. Alias for {!Key.t}. *)

type paste = Paste.t
(** The type for paste events. Alias for {!Paste.t}. *)

type mouse = Mouse.t
(** The type for mouse events. Alias for {!Mouse.t}. *)
