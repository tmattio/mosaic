(** Cross-platform event source for terminal input handling.

    This module provides a platform-agnostic interface for reading terminal
    events. Handles keyboard, mouse, and special events with automatic
    platform-specific behavior adaptation.

    Event sources are stateful and track terminal modes. Mouse support must be
    explicitly enabled at creation. Platform implementations handle encoding
    differences transparently. Events are delivered in order received. *)

type t
(** [t] represents an abstract event source for terminal input.

    Encapsulates platform-specific input handling. Manages terminal mode changes
    for mouse support. Thread-safe when used with proper Eio synchronization. *)

val create : ?mouse:bool -> Terminal.t -> t
(** [create ?mouse terminal] creates an event source for the given terminal.

    Selects appropriate implementation based on platform (Unix or Windows).
    Configures terminal for raw input mode. Enables mouse event capture when
    requested. Terminal must support the required input modes.

    @param mouse
      Enable mouse event capture including clicks, motion, and scroll (default:
      false)

    Example: Creates event source with mouse support.
    {[
      let source = Event_source.create ~mouse:true terminal
    ]} *)

val read :
  t ->
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  timeout:float option ->
  [ `Event of Input.event | `Timeout | `Eof ]
(** [read t ~sw ~clock ~timeout] reads the next input event with optional
    timeout.

    Blocks until an event arrives or timeout expires. Handles platform-specific
    input parsing. Returns immediately if events are buffered. Timeout of [None]
    blocks indefinitely.

    Platform differences:
    - [`Eof] typically only on Unix when input stream closes, not generated on
      Windows
    - [`Event (Input.Paste _)] from ANSI bracketed paste on Unix, heuristic
      timing-based detection on Windows (10ms threshold)
    - Mouse events only delivered if enabled in [create]
    - Function keys and modifiers may vary by terminal and platform

    @param sw Eio switch for resource management
    @param clock Eio clock for timeout handling
    @param timeout Maximum seconds to wait, [None] for indefinite blocking

    @raise End_of_file on unexpected termination (rare)

    Example: Reads events with 1-second timeout.
    {[
      match Event_source.read source ~sw ~clock ~timeout:(Some 1.0) with
      | `Event (Key key_event) -> handle_key key_event
      | `Timeout -> handle_idle ()
      | `Eof -> cleanup_and_exit ()
    ]} *)
