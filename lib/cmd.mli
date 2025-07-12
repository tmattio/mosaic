(** Commands represent side effects and asynchronous operations.

    This module provides declarative effect descriptions that are executed by
    the Mosaic runtime. Commands can produce messages, perform I/O, manage
    timers, or trigger application lifecycle events.

    Commands are pure values describing effects, not performing them. The
    runtime interprets commands after each update cycle. Batch commands execute
    in parallel while sequence commands execute serially. Commands can be
    composed and transformed with [map]. *)

type 'msg exec_cmd = { run : unit -> unit; on_complete : 'msg }
(** [exec_cmd] describes an operation that temporarily releases terminal
    control.

    The [run] function executes with the terminal in normal mode. The
    [on_complete] message is produced after execution. *)

type 'msg t =
  | None
  | Msg of 'msg
  | Batch of 'msg t list
  | Perform of (unit -> 'msg option)
  | Exec of 'msg exec_cmd
  | Tick of float * (float -> 'msg)
  | Sequence of 'msg t list
  | Quit
  | Log of string
  | Print of string
  | Set_window_title of string
  | Enter_alt_screen
  | Exit_alt_screen
  | Repaint
      (** [t] represents a command that may produce messages of type ['msg].

          None performs no operation. Msg immediately produces a message. Batch
          runs commands in parallel. Perform executes an async function. Exec
          releases terminal for external programs. Tick creates a timer.
          Sequence runs commands serially. Quit terminates the application. Log
          writes debug output. Print writes to stdout for scrollback history.
          Set_window_title updates the terminal title. Enter_alt_screen switches
          to alternate screen buffer. Exit_alt_screen returns to normal screen.
          Repaint forces a full screen redraw. *)

val none : 'msg t
(** [none] represents the absence of any command.

    Use when update functions need to return a command but have no effects to
    perform. *)

val msg : 'msg -> 'msg t
(** [msg m] creates a command that immediately produces message [m].

    The message is processed in the same update cycle. Useful for triggering
    immediate state transitions.

    Example: Chains multiple updates.
    {[
      match msg with LoadData -> (Loading, Cmd.msg DataLoaded)
    ]} *)

val batch : 'msg t list -> 'msg t
(** [batch cmds] combines multiple commands for parallel execution.

    All commands in the batch execute concurrently. Empty lists and lists
    containing only [none] return [none]. Single-element lists return the
    command directly. Messages from batched commands arrive in non-deterministic
    order.

    Example: Loads multiple resources simultaneously.
    {[
      Cmd.batch
        [
          Cmd.perform (fun () -> load_user_data ());
          Cmd.perform (fun () -> load_settings ());
          Cmd.tick 2.0 (fun _ -> `Timeout);
        ]
    ]} *)

val perform : (unit -> 'msg option) -> 'msg t
(** [perform f] creates a command that executes [f] asynchronously.

    The function [f] runs in a separate fiber. If it returns [Some msg], that
    message is delivered to the update function. Returning [None] produces no
    message. Exceptions in [f] are caught and logged.

    Example: Performs async HTTP request.
    {[
      Cmd.perform (fun () ->
          match fetch_data url with
          | Ok data -> Some (`DataFetched data)
          | Error _ -> Some `FetchFailed)
    ]} *)

val exec : (unit -> unit) -> 'msg -> 'msg t
(** [exec f msg] temporarily releases terminal control to execute [f].

    The terminal is restored to normal mode before calling [f], allowing
    external programs to run with full terminal access. After [f] completes, the
    terminal returns to alternate screen mode and [msg] is produced.

    Example: Opens external editor.
    {[
      Cmd.exec
        (fun () -> Sys.command "vim /tmp/file.txt" |> ignore)
        `EditorClosed
    ]} *)

val release_and_run : (unit -> unit) -> 'msg -> 'msg t
(** [release_and_run f msg] temporarily releases terminal for external commands.

    Alias for [exec]. Provides a more descriptive name when launching
    interactive programs.

    Example: Opens pager to view logs.
    {[
      Cmd.release_and_run
        (fun () -> Sys.command "less /var/log/app.log" |> ignore)
        `PagerClosed
    ]} *)

val quit : 'msg t
(** [quit] creates a command that terminates the application gracefully.

    The runtime performs cleanup operations including restoring terminal state
    before exit. *)

val tick : float -> (float -> 'msg) -> 'msg t
(** [tick delay f] creates a timer that calls [f] after [delay] seconds.

    The function [f] receives the actual elapsed time, which may differ slightly
    from [delay] due to runtime scheduling. Multiple tick commands can run
    concurrently. Negative delays are treated as 0.

    Example: Implements a timeout with actual duration logging.
    {[
      Cmd.tick 5.0 (fun elapsed ->
          `Timeout (Printf.sprintf "Timed out after %.2fs" elapsed))
    ]} *)

val sequence : 'msg t list -> 'msg t
(** [sequence cmds] creates a command that executes [cmds] in order.

    Each command must complete before the next begins. Empty lists return
    [none]. Single-element lists return the command directly. Useful for
    operations that depend on previous results.

    Example: Saves data then shows confirmation.
    {[
      Cmd.sequence [ Cmd.perform save_to_disk; Cmd.msg `ShowSaveConfirmation ]
    ]} *)

val seq : 'msg t list -> 'msg t
(** [seq cmds] runs commands in sequence.

    Alias for [sequence]. Provides shorter name for common sequential
    operations. *)

val after : float -> 'msg -> 'msg t
(** [after delay msg] produces [msg] after [delay] seconds.

    Convenience function equivalent to [tick delay (fun _ -> msg)]. Use when
    elapsed time is not needed.

    Example: Auto-dismisses notification after 3 seconds.
    {[
      Cmd.after 3.0 `DismissNotification
    ]} *)

val log : string -> 'msg t
(** [log s] writes debug message [s] to stderr.

    Messages are written directly to avoid interfering with the alternate screen
    buffer. In debug mode, logs also append to mosaic-debug.log with timestamps.

    Example: Logs state transitions for debugging.
    {[
      match msg with
      | Click (x, y) -> (model, Cmd.log (Printf.sprintf "Click at (%d, %d)" x y))
    ]} *)

val print : string -> 'msg t
(** [print s] writes message [s] to stdout in non-alt-screen mode.

    In non-alternate screen mode, the message is written above the TUI view and
    becomes part of the terminal's scrollback history. In alternate screen mode,
    this behaves like [log]. Use for persistent output that should remain
    visible after the app exits.

    Example: Shows completion message in scrollback.
    {[
      Cmd.print "Task completed successfully!"
    ]} *)

val set_window_title : string -> 'msg t
(** [set_window_title title] updates the terminal window title to [title].

    Uses ANSI escape sequences. Support varies by terminal emulator. Has no
    effect on terminals without title support.

    Example: Shows current file in title bar.
    {[
      Cmd.set_window_title (Printf.sprintf "Editor - %s" filename)
    ]} *)

val enter_alt_screen : 'msg t
(** [enter_alt_screen] switches the terminal to alternate screen buffer.

    The alternate screen provides a clean canvas for fullscreen applications.
    Previous terminal content is preserved and will be restored when exiting
    alternate screen mode. This command only takes effect if the program is
    currently in normal screen mode.

    Example: Switches to fullscreen mode for a file picker.
    {[
      Cmd.batch [ Cmd.enter_alt_screen; Cmd.msg `ShowFilePicker ]
    ]} *)

val exit_alt_screen : 'msg t
(** [exit_alt_screen] returns the terminal to normal screen buffer.

    Restores the terminal content that was present before entering alternate
    screen mode. This command only takes effect if the program is currently in
    alternate screen mode. Typically paired with [repaint] to redraw the inline
    UI.

    Example: Returns from fullscreen mode.
    {[
      Cmd.batch [ Cmd.exit_alt_screen; Cmd.repaint; Cmd.msg `FileSelected path ]
    ]} *)

val repaint : 'msg t
(** [repaint] forces a full redraw of the current view.

    Clears internal render caches and triggers a complete repaint. Useful after
    screen mode switches or when recovering from terminal corruption. This is
    more efficient than clearing and redrawing manually.

    Example: Refreshes display after returning from external program.
    {[
      Cmd.batch [ Cmd.repaint; Cmd.msg `EditorClosed ]
    ]} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f cmd] transforms all messages produced by [cmd] using function [f].

    Preserves command structure while changing message types. Essential for
    component composition. Commands that don't produce messages ([quit], [log],
    [set_window_title]) pass through unchanged.

    Example: Wraps component messages.
    {[
      let cmd = Select.init options in
      Cmd.map (fun msg -> `Select_msg msg) cmd
    ]} *)

val to_list : 'msg t -> 'msg t list
(** [to_list cmd] flattens [cmd] into a list of atomic commands.

    Expands [Batch] commands while preserving [Sequence] as atomic units. Used
    internally by the runtime for command execution. *)

val pp :
  (Format.formatter -> 'msg -> unit) -> Format.formatter -> 'msg t -> unit
(** [pp pp_msg fmt cmd] pretty-prints command structure for debugging.

    The [pp_msg] function formats message values. Shows command constructors and
    parameters. Functions are displayed as "<fun>". Useful for logging command
    flow during development. *)
