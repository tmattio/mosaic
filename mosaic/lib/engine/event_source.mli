(** Cross-platform event source for terminal input handling.

    Provides a platform-agnostic interface for reading terminal events,
    including keyboard, mouse, paste, resize, and focus. Automatically manages
    terminal modes with RAII via Eio.Switch. Handles platform differences
    transparently. *)

type t
(** Abstract event source. Manages internal stream and producer. *)

val create :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  ?mouse:bool ->
  ?paste_threshold:float ->
  ?paste_min_chars:int ->
  Tty_eio.t ->
  t
(** [create ~sw ~env ?mouse ?paste_threshold ?paste_min_chars terminal] creates
    an event source attached to the switch.

    Sets up raw mode, enables features (mouse, paste, focus), and spawns
    producer fibers. Cleans up on switch release.

    @param sw Eio switch for lifetime management and cleanup
    @param env Eio environment for clock and unix utilities
    @param mouse Enable mouse capture (default: false)
    @param paste_threshold Seconds for Windows paste heuristic (default: 0.01)
    @param paste_min_chars Minimum characters to detect as paste (default: 3)

    Platform notes:
    - Unix: Enables ANSI for mouse/paste/focus, handles SIGWINCH.
    - Windows: Uses native console API, timing-based paste.

    Example:
    {[
      Eio.Switch.run @@ fun sw ->
      let source = Event_source.create ~sw ~env ~mouse:true terminal in
      ...
    ]} *)

val read :
  t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  timeout:float option ->
  [ `Event of Input.event | `Timeout ]
(** [read t ~clock ~timeout] reads next event from stream with timeout.

    Blocks until event or timeout. Returns `Timeout if no event within time.

    Events include: Key, Mouse (Press/Release/Motion/Wheel), Paste, Resize,
    Focus.

    @param clock Eio clock
    @param timeout Seconds to wait, None for indefinite *)
