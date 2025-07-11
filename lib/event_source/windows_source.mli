(** Windows-specific event source implementation.

    This module provides event reading for Windows systems using native console
    input APIs. Directly reads keyboard and mouse events without ANSI parsing,
    using Windows console functions.

    Uses Windows ReadConsoleInput for direct event access. No ANSI parsing
    needed on Windows. Paste detection uses timing heuristics (10ms threshold).
    EOF not generated on Windows consoles. *)

type t
(** [t] represents a Windows event source.

    Encapsulates console handle and paste detection state. Manages timing for
    paste heuristics. *)

val create : mouse:bool -> Terminal.t -> t
(** [create ~mouse terminal] creates a Windows event source.

    Configures console input mode for the terminal. Mouse parameter controls
    whether mouse events are captured. Uses native Windows console API for
    configuration.

    @param mouse Enable mouse event capture
    @param terminal Terminal handle with console access *)

val read :
  t ->
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  timeout:float option ->
  [ `Event of Input.event | `Timeout | `Eof ]
(** [read t ~sw ~clock ~timeout] reads the next event from Windows console.

    Uses Windows-specific console input functions. Translates native events to
    Input.event. Implements paste detection through rapid keystroke timing
    analysis.

    @param sw Eio switch for resource management
    @param clock Eio clock for timeout and paste timing
    @param timeout Maximum seconds to wait, None for blocking

    Implementation details:
    - Direct console event reading, no ANSI parsing
    - Paste detected when keystrokes arrive within 10ms
    - Mouse events include extended buttons if available
    - `Eof never returned (Windows console behavior) *)
