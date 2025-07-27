(** Subscriptions declare external event sources.

    This module provides declarative event listeners for keyboard, mouse, and
    window events. Subscriptions are evaluated after each update to determine
    active event handlers.

    Subscriptions are pure values that describe event interests. The runtime
    manages actual event listening. Multiple subscriptions of the same type are
    composed with [batch]. Event handlers can filter events by returning [None].
*)

type window_size = { width : int; height : int }
(** [window_size] represents terminal dimensions in character cells.

    The width and height reflect the usable area excluding window decorations.
    Values update on terminal resize. *)

type 'msg t
(** [t] represents a subscription that may produce messages of type ['msg].

    Subscriptions are immutable descriptions of event interests. They can be
    composed, filtered, and transformed. *)

val none : 'msg t
(** [none] represents the absence of any subscription.

    Use when components have no external event interests. Default value for
    optional subscriptions. *)

val keyboard : (Input.key_event -> 'msg) -> 'msg t
(** [keyboard f] subscribes to all keyboard events.

    The function [f] receives every key press, release, and repeat event.
    Includes modifier keys and special keys.

    Example: Logs all keyboard input.
    {[
      Sub.keyboard (fun event -> `KeyPressed (Input.show_key_event event))
    ]} *)

val keyboard_filter : (Input.key_event -> 'msg option) -> 'msg t
(** [keyboard_filter f] subscribes to keyboard events with filtering.

    The function [f] examines each event and returns [Some msg] to produce a
    message or [None] to ignore. More efficient than [keyboard] when handling
    specific keys.

    Example: Handles only letter keys.
    {[
      Sub.keyboard_filter (fun event ->
          match event.key with
          | Char c when Uchar.is_char c -> Some (`CharTyped (Uchar.to_char c))
          | _ -> None)
    ]} *)

val mouse : (Input.mouse_event -> 'msg) -> 'msg t
(** [mouse f] subscribes to all mouse events.

    The function [f] receives clicks, releases, motion, and scroll events.
    Requires mouse support enabled in [run]. Coordinates are zero-based from
    top-left corner.

    Example: Tracks all mouse activity.
    {[
      Sub.mouse (fun event -> `Mouse_event event)
    ]} *)

val mouse_filter : (Input.mouse_event -> 'msg option) -> 'msg t
(** [mouse_filter f] subscribes to mouse events with filtering.

    The function [f] examines each event and returns [Some msg] to produce a
    message or [None] to ignore. Use for specific mouse interactions without
    processing all events.

    Example: Handles only right-click menu requests.
    {[
      Sub.mouse_filter (function
        | Press (x, y, Right, _) -> Some (`ShowMenu (x, y))
        | _ -> None)
    ]} *)

val window : (window_size -> 'msg) -> 'msg t
(** [window f] subscribes to terminal resize events.

    The function [f] receives new dimensions whenever the terminal window
    changes size. Initial size is provided on subscription. Useful for
    responsive layouts.

    Example: Adjusts layout on resize.
    {[
      Sub.window (fun size -> `Resized (size.width, size.height))
    ]} *)

val window_filter : (window_size -> 'msg option) -> 'msg t
(** [window_filter f] subscribes to window events with filtering.

    The function [f] can choose to react to specific size changes. Useful for
    threshold-based layout changes.

    Example: Detects when window becomes too narrow.
    {[
      Sub.window_filter (fun size ->
          if size.width < 80 then Some `CompactMode else None)
    ]} *)

val focus : (unit -> 'msg) -> 'msg t
(** [focus f] subscribes to terminal focus gain events.

    The function [f] is called when the terminal window receives input focus.
    Support varies by terminal. Useful for pausing/resuming activities.

    Example: Resumes animation on focus.
    {[
      Sub.focus (fun () -> `ResumeAnimation)
    ]} *)

val blur : (unit -> 'msg) -> 'msg t
(** [blur f] subscribes to terminal focus loss events.

    The function [f] is called when the terminal window loses input focus.
    Support varies by terminal. Useful for pausing activities when not visible.

    Example: Pauses game when window loses focus.
    {[
      Sub.blur (fun () -> `PauseGame)
    ]} *)

val paste : (string -> 'msg) -> 'msg t
(** [paste f] subscribes to paste events.

    The function [f] receives the pasted text when the user pastes content.
    Requires bracketed paste mode support in the terminal.

    Example: Inserts pasted text.
    {[
      Sub.paste (fun text -> `InsertText text)
    ]} *)

val paste_filter : (string -> 'msg option) -> 'msg t
(** [paste_filter f] subscribes to paste events with filtering.

    The function [f] examines pasted text and returns [Some msg] to produce a
    message or [None] to ignore. Useful for validating or transforming pasted
    content.

    Example: Only accepts numeric pastes.
    {[
      Sub.paste_filter (fun text ->
          if String.for_all (fun c -> c >= '0' && c <= '9') text then
            Some (`PasteNumber text)
          else None)
    ]} *)

(** {2 Convenience Functions} *)

val on_mouse_motion : (int -> int -> 'msg) -> 'msg t
(** [on_mouse_motion f] subscribes to mouse movement events.

    The function [f] receives cursor coordinates on any motion. High-frequency
    events on fast mouse movement.

    Example: Implements hover effects.
    {[
      Sub.on_mouse_motion (fun x y -> `MouseAt (x, y))
    ]} *)

val on_mouse_click : (int -> int -> 'msg) -> 'msg t
(** [on_mouse_click f] subscribes to any mouse button press.

    The function [f] receives click coordinates regardless of button. Does not
    distinguish between buttons.

    Example: Handles click anywhere.
    {[
      Sub.on_mouse_click (fun x y -> `ClickedAt (x, y))
    ]} *)

val on_resize : (int -> int -> 'msg) -> 'msg t
(** [on_resize f] subscribes to terminal resize with direct dimensions.

    Convenience wrapper for [window]. The function [f] receives width and height
    directly.

    Example: Updates grid dimensions.
    {[
      Sub.on_resize (fun w h -> `GridSize (w / 10, h / 5))
    ]} *)

val on_key :
  ?ctrl:bool -> ?alt:bool -> ?shift:bool -> Input.key -> 'msg -> 'msg t
(** [on_key ?ctrl ?alt ?shift k msg] produces [msg] when key [k] is pressed with
    specified modifiers.

    Modifiers default to false. Only exact modifier combinations match.

    Example: Binds save command to Ctrl+S.
    {[
      Sub.on_key ~ctrl:true (Char (Uchar.of_char 's')) `Save
    ]} *)

val on_char : ?ctrl:bool -> ?alt:bool -> ?shift:bool -> char -> 'msg -> 'msg t
(** [on_char ?ctrl ?alt ?shift c msg] produces [msg] when character [c] is typed
    with modifiers.

    Convenience function for character keys. Case-sensitive matching.

    Example: Binds quit to 'q' without modifiers.
    {[
      Sub.on_char 'q' `Quit
    ]} *)

val on_click : (int -> int -> Input.mouse_button -> 'msg) -> 'msg t
(** [on_click f] subscribes to mouse button presses with button information.

    The function [f] receives coordinates and which button was pressed.
    Distinguishes all mouse buttons.

    Example: Handles different actions per button.
    {[
      Sub.on_click (fun x y button ->
          match button with
          | Left -> `Select (x, y)
          | Right -> `Menu (x, y)
          | _ -> `Other)
    ]} *)

val on_left_click : (int -> int -> 'msg) -> 'msg t
(** [on_left_click f] subscribes to left mouse button presses only.

    The function [f] receives click coordinates. Most common mouse interaction.

    Example: Selects item at position.
    {[
      Sub.on_left_click (fun x y -> `SelectAt (x, y))
    ]} *)

val on_right_click : (int -> int -> 'msg) -> 'msg t
(** [on_right_click f] subscribes to right mouse button presses only.

    The function [f] receives click coordinates. Typically used for context
    menus.

    Example: Shows context menu.
    {[
      Sub.on_right_click (fun x y -> `ShowContextMenu (x, y))
    ]} *)

val on_scroll_up : (int -> int -> 'msg) -> 'msg t
(** [on_scroll_up f] subscribes to mouse wheel scroll up events.

    The function [f] receives cursor position during scroll. Terminal support
    varies.

    Example: Scrolls content up.
    {[
      Sub.on_scroll_up (fun _ _ -> `ScrollUp)
    ]} *)

val on_scroll_down : (int -> int -> 'msg) -> 'msg t
(** [on_scroll_down f] subscribes to mouse wheel scroll down events.

    The function [f] receives cursor position during scroll. Terminal support
    varies.

    Example: Scrolls content down.
    {[
      Sub.on_scroll_down (fun _ _ -> `ScrollDown)
    ]} *)

val on_focus : 'msg -> 'msg t
(** [on_focus msg] produces [msg] when terminal gains focus.

    Simpler alternative to [focus] when no computation needed.

    Example: Resumes on focus.
    {[
      Sub.on_focus `Resume
    ]} *)

val on_blur : 'msg -> 'msg t
(** [on_blur msg] produces [msg] when terminal loses focus.

    Simpler alternative to [blur] when no computation needed.

    Example: Pauses on blur.
    {[
      Sub.on_blur `Pause
    ]} *)

val on_paste : (string -> 'msg) -> 'msg t
(** [on_paste f] produces a message when text is pasted.

    Convenience wrapper for [paste]. The function [f] receives the pasted text.

    Example: Handles pasted input.
    {[
      Sub.on_paste (fun text -> `PastedText text)
    ]} *)

(** {3 Common Key Shortcuts} *)

val on_enter : 'msg -> 'msg t
(** [on_enter msg] produces [msg] on Enter key press.

    Common for form submission and confirmations. *)

val on_escape : 'msg -> 'msg t
(** [on_escape msg] produces [msg] on Escape key press.

    Common for canceling operations or closing dialogs. *)

val on_tab : 'msg -> 'msg t
(** [on_tab msg] produces [msg] on Tab key press.

    Common for focus navigation between fields. *)

val on_backspace : 'msg -> 'msg t
(** [on_backspace msg] produces [msg] on Backspace key press.

    Common for deleting previous character or item. *)

val on_delete : 'msg -> 'msg t
(** [on_delete msg] produces [msg] on Delete key press.

    Common for deleting next character or selected item. *)

val on_up : 'msg -> 'msg t
(** [on_up msg] produces [msg] on Up arrow key press.

    Common for navigating lists or moving cursor up. *)

val on_down : 'msg -> 'msg t
(** [on_down msg] produces [msg] on Down arrow key press.

    Common for navigating lists or moving cursor down. *)

val on_left : 'msg -> 'msg t
(** [on_left msg] produces [msg] on Left arrow key press.

    Common for moving cursor left or navigating back. *)

val on_right : 'msg -> 'msg t
(** [on_right msg] produces [msg] on Right arrow key press.

    Common for moving cursor right or navigating forward. *)

val on_page_up : 'msg -> 'msg t
(** [on_page_up msg] produces [msg] on Page Up key press.

    Common for scrolling up by page or moving to previous screen. *)

val on_page_down : 'msg -> 'msg t
(** [on_page_down msg] produces [msg] on Page Down key press.

    Common for scrolling down by page or moving to next screen. *)

val on_home : 'msg -> 'msg t
(** [on_home msg] produces [msg] on Home key press.

    Common for moving to beginning of line or document. *)

val on_end : 'msg -> 'msg t
(** [on_end msg] produces [msg] on End key press.

    Common for moving to end of line or document. *)

(** {3 Common Control Key Combinations} *)

val on_ctrl_c : 'msg -> 'msg t
(** [on_ctrl_c msg] produces [msg] on Ctrl+C key combination.

    Common for copy operations. Note: may be intercepted by terminal for SIGINT.
*)

val on_ctrl_x : 'msg -> 'msg t
(** [on_ctrl_x msg] produces [msg] on Ctrl+X key combination.

    Common for cut operations in text editors. *)

val on_ctrl_v : 'msg -> 'msg t
(** [on_ctrl_v msg] produces [msg] on Ctrl+V key combination.

    Common for paste operations. Terminal clipboard support varies. *)

val on_ctrl_z : 'msg -> 'msg t
(** [on_ctrl_z msg] produces [msg] on Ctrl+Z key combination.

    Common for undo operations. Note: may suspend process on Unix terminals. *)

val on_ctrl_a : 'msg -> 'msg t
(** [on_ctrl_a msg] produces [msg] on Ctrl+A key combination.

    Common for select all or move to line start. *)

val on_ctrl_s : 'msg -> 'msg t
(** [on_ctrl_s msg] produces [msg] on Ctrl+S key combination.

    Common for save operations. *)

val on_ctrl_d : 'msg -> 'msg t
(** [on_ctrl_d msg] produces [msg] on Ctrl+D key combination.

    Common for EOF signal or delete operations. *)

val batch : 'msg t list -> 'msg t
(** [batch subs] combines multiple subscriptions into one.

    All subscriptions remain active. Empty lists return [none]. Single-element
    lists return the subscription directly. Essential for composing multiple
    event sources.

    Example: Subscribes to multiple events.
    {[
      Sub.batch
        [
          Sub.on_escape `Cancel;
          Sub.on_enter `Confirm;
          Sub.mouse_click (fun x y -> `Click (x, y));
        ]
    ]} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f sub] transforms all messages produced by [sub] using function [f].

    Preserves subscription structure while changing message types. Essential for
    component composition.

    Example: Wraps component subscriptions.
    {[
      let subs = Select.subscriptions model in
      Sub.map (fun msg -> `Select_msg msg) subs
    ]} *)

val collect_keyboard :
  (Input.key_event -> 'msg option) list ->
  'msg t ->
  (Input.key_event -> 'msg option) list
(** [collect_keyboard acc sub] extracts keyboard handlers from [sub].

    Internal function used by the runtime to gather active keyboard
    subscriptions. Accumulates handlers in [acc]. *)

val collect_mouse :
  (Input.mouse_event -> 'msg option) list ->
  'msg t ->
  (Input.mouse_event -> 'msg option) list
(** [collect_mouse acc sub] extracts mouse handlers from [sub].

    Internal function used by the runtime to gather active mouse subscriptions.
*)

val collect_window :
  (window_size -> 'msg option) list ->
  'msg t ->
  (window_size -> 'msg option) list
(** [collect_window acc sub] extracts window resize handlers from [sub].

    Internal function used by the runtime to gather active window subscriptions.
*)

val collect_focus :
  (unit -> 'msg option) list -> 'msg t -> (unit -> 'msg option) list
(** [collect_focus acc sub] extracts focus event handlers from [sub].

    Internal function used by the runtime to gather active focus subscriptions.
*)

val collect_blur :
  (unit -> 'msg option) list -> 'msg t -> (unit -> 'msg option) list
(** [collect_blur acc sub] extracts blur event handlers from [sub].

    Internal function used by the runtime to gather active blur subscriptions.
*)

val collect_paste :
  (string -> 'msg option) list -> 'msg t -> (string -> 'msg option) list
(** [collect_paste acc sub] extracts paste event handlers from [sub].

    Internal function used by the runtime to gather active paste subscriptions.
*)

val pp :
  (Format.formatter -> 'msg -> unit) -> Format.formatter -> 'msg t -> unit
(** [pp pp_msg fmt sub] pretty-prints subscription structure for debugging.

    The [pp_msg] function formats message values. Shows subscription types and
    nesting. Functions are displayed as "<fun>". Useful for debugging
    subscription composition. *)
